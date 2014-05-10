{-# OPTIONS -fwarn-tabs -Wall #-}

module Main (main) where

import Data.MetricSpace

import Control.Monad
import Data.Monoid
import Data.Traversable (Traversable)

import Control.Concurrent.Async
import Pipes
import qualified Pipes.Prelude as P
import Pipes.Concurrent

-- Additional resources consulted
main :: IO ()
main = undefined


foo = do
     (output1, input1) <- spawn Unbounded
     (output2, input2) <- spawn Unbounded
     a1 <- async $ do
         runEffect $ P.stdinLn >-> toOutput (output1 <> output2)
         performGC
     as <- forM [input1, input2] $ \input -> async $ do
         runEffect $ fromInput input >-> P.take 3 >-> P.stdoutLn
         performGC
     mapM_ wait (a1:as)



-- parKCenters :: (Traversable t, MetricSpace p d) =>
--                Int    -- ^ /k/
--             -> t p    -- ^ the collection of points, assumed all distinct
--             -> Int    -- ^ the number of parallel instances to run
--             -> IO [p] -- ^ the list of /k/-centers
-- parKCenters k ps m = replicateM m (spawn Unbounded)
