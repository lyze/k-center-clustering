{-# OPTIONS -fwarn-tabs -Wall #-}
{-# LANGUAGE TupleSections, ScopedTypeVariables #-}

module Main (main) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.ClosestPair
import Data.Heap (MinPrioHeap)
import qualified Data.Heap as Heap
import Data.KCenterClustering
import Data.Map (Map)
import qualified Data.Map as Map
import Data.MetricSpace
import Numeric
import Pipes
import qualified Pipes.Prelude as P
import Pipes.Concurrent
import System.Console.GetOpt
import System.Environment
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String

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

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option ['h'] ["help"]
    (NoArg (\opts -> return opts { optShowHelp = True }))
    "help"

  , Option ['a'] ["alpha", "factor", "scale"]
    (ReqArg (\a opts -> return opts { optK = read a }) "a")
    "scaling factor (greater than 1)"

  , Option ['k'] ["centers"]
    (ReqArg (\k opts -> return opts { optK = read k }) "k")
    "number of centers"

  , Option ['m'] ["instances"]
    (ReqArg (\m opts -> return opts { optM = read m }) "m")
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

point2DP :: Parser [Point2D]
point2DP = endBy1 (tuple float float) spaces
  where float = do
          s <- getInput
          case readSigned readFloat s of
            [(n, s')] -> n <$ setInput s'
            _         -> empty
        tuple     :: Parser a -> Parser b -> Parser (a, b)
        tuple x y = parens $ do
                      spaces
                      a <- x
                      spaces
                      _ <- char ','
                      spaces
                      b <- y
                      spaces
                      return (a, b)
        parens :: Parser a -> Parser a
        parens = between (char '(') (char ')')

main :: IO ()
main = do
  argv <- getArgs
  opts <- parseOptions argv
  if optShowHelp opts
    then putStrLn $ usageInfo header options
    else do
      parseResult <- parseFromFile point2DP $ optInput opts
      case parseResult of
        Left err -> ioError . userError $ show err
        Right ps -> do
          centers <- parKCenters (optK opts) (each ps) (optA opts) (optM opts)
          writeFile (optOutput opts) $ show centers

type JobResult a = (Int, Either ([a], Producer a IO ()) [a])

parKCenters :: forall p d.
               (MetricSpace p d, Num d) =>
               Int              -- ^ /k/
            -> Producer p IO () -- ^ the stream of points, assumed all distinct
            -> d                -- ^ the factor by which to scale the radius
            -> Int              -- ^ the number of parallel instances to run
            -> IO [p]           -- ^ the list of /k/-centers
parKCenters k ps a m = do
  (resultsOut, resultsIn, sealResults) <- spawn' Unbounded
  let centers = parKCenters' k ps a m resultsOut resultsIn
  atomically sealResults
  centers

parKCenters' :: forall p d. (Num d, MetricSpace p d) =>
               Int                  -- ^ /k/
            -> Producer p IO ()     -- ^ the stream of points, assumed all distinct
            -> d                    -- ^ the factor by which to scale the radius
            -> Int                  -- ^ the number of parallel instances to run
            -> Output (JobResult p) -- ^ the outbox for job results
            -> Input (JobResult p)  -- ^ the inbox for job results
            -> IO [p]               -- ^ the list of /k/-centers
parKCenters' k ps a m resultsOut resultsIn = do
  initial <- P.toListM $ ps >-> P.take (k + 1)
  let d0 = uncurry distance $ closestPair initial

  ts      <- mapM (worker resultsOut d0) [0 .. m - 1]
  threads <- atomically . newTVar . Map.fromList . zip [0 .. m - 1] $ ts

  runEffect $ ([] <$ fromInput resultsIn) >-> handler d0 resultsOut threads

    where worker            :: Output (JobResult p) -> d -> Int -> IO (Async ())
          worker msgBox d n = async $ do
                                  job <- kCentersStreaming k ps $ d * a^n
                                  runEffect $ yield (n, job) >-> toOutput msgBox

          handler                  :: d -> Output (JobResult p) -> TVar (Map Int (Async ())) -> Consumer (JobResult p) IO [p]
          handler d msgBox threads = go (Heap.empty :: MinPrioHeap Int [p])
            where go successes = do
                    result <- await

                    case result of

                      (i, Left (cs, ps')) ->
                        do ts <- lift . atomically . removeThreadsLE i $ threads
                           lift . Map.foldr (\t _ -> cancel t) (return ()) $ ts
                           let i' = i + fromIntegral m
                           handle <- lift . async $ do
                                       job <- kCentersStreaming k (each cs >> ps') $ d * a^i'
                                       runEffect $ yield (i', job) >-> toOutput msgBox
                           lift . atomically $ modifyTVar threads (Map.insert i' handle . Map.delete i)
                           go successes

                      (i, Right cs)  ->
                        do handle <- lift . atomically $
                                     do ts <- readTVar threads
                                        if Map.null ts
                                          then return Nothing
                                          else let (low, _) = Map.findMin ts
                                               in return $ Just low
                           case handle of                    -- running threads?
                             Nothing ->
                               case Heap.viewHead successes of
                                 Nothing -> error "parKCenters: logic error"
                                 Just (_, cs') -> return cs'
                             Just h  ->
                               if i == h                -- is the lowest thread?
                                 then return cs
                                 else do lift . atomically . modifyTVar threads $ Map.delete i
                                         go (Heap.insert (i, cs) successes)

          removeThreadsLE           :: Int -> TVar (Map Int (Async ())) -> STM (Map Int (Async ()))
          removeThreadsLE n threads = do
                         ts <- readTVar threads
                         let (below, above) = Map.partitionWithKey f ts
                             f i _ = i <= n
                         writeTVar threads above
                         return below
