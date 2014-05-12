{-# OPTIONS -fwarn-tabs -Wall #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.MetricSpace
import System.Environment
import System.Console.GetOpt
import Test.QuickCheck
import Text.Printf

data Options = Options { optShowHelp :: Bool
                       , optOutput   :: String
                       , optN        :: Int } deriving Show

defaultOptions :: Options
defaultOptions = Options { optShowHelp = False
                         , optOutput   = "sample_points.txt"
                         , optN        = 1000
                         }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option ['h'] ["help"]
    (NoArg (\opts -> return opts { optShowHelp = True }))
    "help"

  , Option ['o'] ["output"]
    (ReqArg (\f opts -> return opts { optOutput = f })
     "FILE")
    "output FILE"

  , Option ['n'] ["number"]
    (ReqArg (\n opts -> return opts { optN = read n })
     "N")
    $ "number of points to generate (default "
        ++ show (optN defaultOptions) ++ ")"
 ]

header :: String
header = "Usage: kcenters-gensamples [OPTION...] N"

parseOptions      :: [String] -> IO Options
parseOptions argv =
  case getOpt Permute options argv of
    (o, _, [])   -> foldM (flip id) defaultOptions o
    (_, _, errs) -> ioError . userError $ concat errs ++ usageInfo header options

main :: IO ()
main = do
  argv <- getArgs
  opts <- parseOptions argv
  if optShowHelp opts
    then putStrLn $ usageInfo header options
    else do
      points <- replicateM (optN opts) $ generate (arbitrary :: Gen Point2D)
      writeFile (optOutput opts) .
                concat $
                map (uncurry $ printf "%30f%30f\n") points
