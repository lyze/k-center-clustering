{-# OPTIONS -fwarn-tabs -Wall #-}
{-# LANGUAGE DeriveDataTypeable, TupleSections, ScopedTypeVariables #-}

module Main (main) where

import Control.Applicative hiding ((<|>))
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ClosestPair
import Data.Heap (MinPrioHeap)
import qualified Data.Heap as Heap
import Data.KCenterClustering
import Data.MetricSpace
import Data.PSQueue (PSQ, Binding(..))
import qualified Data.PSQueue as PQ
import Data.Typeable.Internal
import Numeric
import Pipes
import qualified Pipes as P (yield)
import qualified Pipes.Prelude as P
import Pipes.Concurrent
import System.Console.GetOpt
import System.Environment
import Text.Parsec
import Text.Parsec.String
import Text.Printf
import Text.Read (readMaybe)

data Options = Options { optShowHelp :: Bool
                       , optK        :: Int
                       , optA        :: Double
                       , optM        :: Int
                       , optOutput   :: String
                       , optInput    :: String
                       } deriving Show

defaultOptions :: Options
defaultOptions = Options { optShowHelp = False
                         , optK        = 1
                         , optA        = 1.1
                         , optM        = 1
                         , optOutput   = "a.out"
                         , optInput    = ""
                         }

read'   :: Read a => String -> IO a
read' s = case readMaybe s of
            Nothing -> ioError . userError $ "Could not parse option argument `" ++ s ++ "'"
            Just a  -> return a

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option ['h'] ["help"]
    (NoArg (\opts -> return opts { optShowHelp = True }))
    "help"

  , Option ['k'] ["centers"]
    (ReqArg (\s opts -> do k <- read' s; return opts { optK = k }) "k")
    "number of centers"

  , Option ['a'] ["alpha", "factor", "scale"]
    (ReqArg (\s opts -> do a <- read' s; return opts { optA = a }) "a")
    "scaling factor (greater than 1)"

  , Option ['m'] ["instances"]
    (ReqArg (\s opts -> do m <- read' s; return opts { optM = m }) "m")
    "number of instances"

  , Option ['o'] ["output"]
    (ReqArg (\f opts -> return opts { optOutput = f })
     "FILE")
    "output FILE"
 ]

header :: String
header = "Usage: kcenters [OPTION...] FILE"

parseOptions      :: [String] -> IO Options
parseOptions argv =
  case getOpt Permute options argv of
    (_, [], [])  -> ioError . userError $ "No input file given.\n" ++ usageInfo header options
    (o, [n], []) -> foldM (flip id) defaultOptions { optInput = n } o
    (_, _, errs) -> ioError . userError $ concat errs ++ usageInfo header options

point2DList :: Parser [Point2D]
point2DList = sepEndBy1 (tuple float float) spaces
  where float :: Parser Double
        float = do
          s <- getInput
          case readSigned readFloat s of
            [(n, s')] -> n <$ setInput s'
            _         -> empty
        tuple     :: Parser a -> Parser b -> Parser (a, b)
        tuple x y = (,) <$> (spaces *> x <* spaces) <*> y <* spaces

main :: IO ()
main = do
  argv <- getArgs
  opts <- parseOptions argv
  if optShowHelp opts
    then putStrLn $ usageInfo header options
    else do
      parseResult <- parseFromFile point2DList $ optInput opts
      case parseResult of
        Left err -> ioError . userError $ show err
        Right ps -> do
          (centers, threads, seal) <- parKCenters (optK opts) (each ps) (optA opts) (optM opts)
          waitForThreads threads
          atomically seal
          writeFile (optOutput opts) .
                    concat $
                    map (uncurry $ printf "%30f%30f\n") centers


waitForThreads :: [Async ()] -> IO ()
waitForThreads = mapM_ wait

type Instance       = Int         -- what number instance is this worker thread?
type WorkerResult p = (Instance, [p])
type ThreadPool     = PSQ (Async ()) Instance

data ComputationException = HigherThreadTerminatedException Int
                            deriving (Show, Typeable)

instance Exception ComputationException

async' :: (Async () -> IO b) -> IO b
async' = withAsync (return ())

parKCenters :: (MetricSpace p d, Num d) =>
               Int                          -- ^ /k/
            -> Producer p IO ()             -- ^ the stream of points, assumed all distinct
            -> d                            -- ^ the factor by which to scale the radius
            -> Int                          -- ^ the number of parallel instances to run
            -> IO ([p], [Async ()], STM ()) -- ^ the list of /k/-centers
parKCenters k ps a m = do
  (resultsOut, resultsIn, sealResults) <- spawn' Unbounded
  (centers, threads) <- parKCenters' k ps a m resultsOut resultsIn
  return (centers, threads, sealResults)

parKCenters' :: forall p d. (Num d, MetricSpace p d) =>
               Int                     -- ^ /k/
            -> Producer p IO ()        -- ^ the stream of points, assumed all distinct
            -> d                       -- ^ the factor by which to scale the radius
            -> Int                     -- ^ the number of parallel instances to run
            -> Output (WorkerResult p) -- ^ the outbox for job results
            -> Input (WorkerResult p)  -- ^ the inbox for job results
            -> IO ([p], [Async ()])    -- ^ the list of /k/-centers and the running threads to wait on
parKCenters' k ps a m resultsOut resultsIn = do
  initial <- P.toListM $ ps >-> P.take (k + 1)
  pool    <- atomically $ newTVar PQ.empty

  let d0 = uncurry distance $ closestPair initial
  mapM_ (worker pool resultsOut d0) [0 .. m - 1]

  centers <- runEffect $ ([] <$ fromInput resultsIn) >-> resultsHandler pool
  threads <- readTVarIO pool

  return (centers, PQ.keys threads)

    where worker                 :: TVar ThreadPool -> Output (WorkerResult p) -> d -> Int -> IO ()
          worker pool msgBox d n =
            let work     :: Async () -> Instance -> IO ()
                work h i = do
                  job <- kCentersStreaming k ps $ d * a^i
                  case job of
                    Left _   ->
                      do ts <- atomically $ getThreadsLE n pool
                         mapM_ (`cancelWith` HigherThreadTerminatedException i) ts
                         let i' = i + fromIntegral m
                         adjust h i'
                         work h i'
                    Right cs ->
                      runEffect $ P.yield (n, cs) >-> toOutput msgBox

                adjust       :: Async () -> Instance -> IO ()
                adjust h pri = atomically . modifyTVar pool $ PQ.adjust (const pri) h

                deregister   :: Async () -> IO ()
                deregister h = atomically . modifyTVar pool $ PQ.delete h

                register     :: Async () -> Instance -> IO ()
                register h i = atomically . modifyTVar pool $ PQ.insert h i

                exceptionHandler :: Async ()
                                 -> Instance
                                 -> ComputationException
                                 -> IO ()
                exceptionHandler h lo (HigherThreadTerminatedException hi) =
                  let dbl = fromIntegral (hi - lo) / fromIntegral m
                      int = ceiling (dbl :: Double)
                      new = m * int
                  in  work h new
            in
              async' $ \h -> do
                register h n
                work h n `catch` exceptionHandler h n `finally` deregister h

          resultsHandler      :: TVar ThreadPool
                              -> Consumer (WorkerResult p) IO [p]
          resultsHandler pool = go (Heap.empty :: MinPrioHeap Instance [p])
            where go oldResults = do
                    result <- await -- received a result
                    let newResults = Heap.insert result oldResults
                    ts <- lift $ readTVarIO pool
                    case PQ.findMin ts of         -- are there jobs left?
                      Nothing         ->
                        let Just (_, cs) = Heap.viewHead newResults
                        in  return cs
                      Just (_ :-> lo) ->
                        let Just (i, cs) = Heap.viewHead newResults
                        in if i < lo
                             then return cs
                             else go newResults

          getThreadsLE           :: Int -> TVar ThreadPool -> STM [Async ()]
          getThreadsLE n pool = do
            ts <- readTVar pool
            return . map (\(h :-> _) -> h) $ PQ.atMost n ts
