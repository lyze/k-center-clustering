{-# OPTIONS -fwarn-tabs -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Data.Monoid
import Pipes
import qualified Pipes.Prelude as P
import Pipes.Concurrent

import Debug.Trace

-- Additional resources consulted
main :: IO ()
main = undefined

foo :: IO ()
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

type JobResult a = (Int, Either ([a], Producer a IO ()) [a])

parKCenters :: forall p d. (Num d, MetricSpace p d) =>
               Int              -- ^ /k/
            -> Producer p IO () -- ^ the stream of points, assumed all distinct
            -> d                -- ^ the factor by which to scale the radius
            -> Int              -- ^ the number of parallel instances to run
            -> IO [p]           -- ^ the list of /k/-centers
parKCenters k ps a m = do
  initial <- P.toListM $ ps >-> P.take (k + 1)
  let d0 = uncurry distance $ closestPair initial


  (resultsOut, resultsIn, sealResults) <- spawn' Unbounded

  ts <- mapM (worker resultsOut d0) [0 .. m - 1]
  threads <- atomically . newTVar . Map.fromList . zip [0 .. m - 1] $ ts

  centers <- runEffect $ ([] <$ fromInput resultsIn) >-> handler d0 resultsOut threads
  -- let _ = map wait threads
  return centers

    where worker            :: Output (JobResult p) -> d -> Int -> IO (Async ())
          worker msgBox d n = async $ do
                                  job <- kCentersStreaming k ps $ d * a^n
                                  runEffect $ yield (n, job) >-> toOutput msgBox
                                  putTraceMsg $ "worker " ++ show n
                                  -- performGC

          handler                  :: d -> Output (JobResult p) -> TVar (Map Int (Async ())) -> Consumer (JobResult p) IO [p]
          handler d msgBox threads = go (Heap.empty :: MinPrioHeap Int [p])
            where go successes = do
                    result <- await

                    case result of

                      (i, Left (cs, ps')) ->
                        do ts <- lift . atomically . removeThreadsLE i $ threads
                           _  <- lift . Map.foldr (\t _ -> cancel t) (return ()) $ ts
                           let i' = i + fromIntegral m
                           handle <- lift . async $ do
                                       job <- kCentersStreaming k (each cs >> ps') $ d * a^i'
                                       runEffect $ yield (i', job) >-> toOutput msgBox
                           lift . atomically $ modifyTVar threads (Map.insert i' handle . Map.delete i)
                           -- performGC
                           go successes

                      (i, Right cs)  ->
                        do handle <- lift . atomically $
                                     do ts <- readTVar threads
                                        if Map.null ts
                                          then return Nothing
                                          else let (low, _) = Map.findMin ts
                                               in return $ Just low
                           case handle of -- running threads?
                             Nothing ->
                               case Heap.viewHead successes of
                                 Nothing -> error "parKCenters: logic error"
                                 Just (_, cs') -> return cs'
                             Just h  ->
                               if i == h        -- is the lowest thread?
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

-- Async (Either ([p], Producer p IO ()) [p])

-- parKCenters 3 (each [(0,0),(1,0),(3,0),(4,0)]) 1.0 3 :: IO [Point2D]
