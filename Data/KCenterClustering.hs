{-# OPTIONS -fwarn-tabs -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}

module Data.KCenterClustering
  ( kCenters
  , kCenters'
  , kCentersStreaming
  , kCentersStreaming'
  , farthest
  , farthestDistance
  , nearest
  , nearestDistance
  ) where

import Control.Applicative
import qualified Data.Foldable as F
import Data.List
import Data.MetricSpace
import Data.Monoid
import Data.Traversable
import Data.Ord

import Pipes
import qualified Pipes.Prelude as P

import Test.QuickCheck

tests :: IO Bool
tests = $quickCheckAll

errorEmptyList     :: String -> a
errorEmptyList fun = error $ "Data.KCenterClustering." ++ fun ++ ": empty list"

-- | Returns a point (from the given list of points) that is the farthest from
-- the given point.
farthest      :: MetricSpace p d => p -> [p] -> p
farthest _ [] = errorEmptyList "farthest"
farthest p ps = maximumBy (comparing $ distance p) ps -- maximumBy . comparing . distance

-- | Returns a point (from the given list of points) that is the nearest to the
-- given point.
nearest :: MetricSpace p d => p -> [p] -> p
nearest _ [] = errorEmptyList "nearest"
nearest p ps = minimumBy (comparing $ distance p) ps

-- | Returns the distance from the given point to the given list of points that
-- is the nearest.
nearestDistance      :: MetricSpace p d => p -> [p] -> d
nearestDistance _ [] = errorEmptyList "nearestDistance"
nearestDistance p ps = minimum . fmap (distance p) $ ps

-- | Returns the distance from the given point to the given list of points that
-- is the farthest.
farthestDistance      :: MetricSpace p d => p -> [p] -> d
farthestDistance _ [] = errorEmptyList "farthestDistance"
farthestDistance p ps = maximum . fmap (distance p) $ ps

-- | Returns a list of centers based on the Farthest Point Algorithm. The first
-- point is chosen as the initial center. The input collection of points is
-- assumed to contain no duplicates.
kCenters      :: (Ord d, MetricSpace p d, Traversable t) => Int -> t p -> [p]
kCenters k ps = case first ps of
                  Nothing -> errorEmptyList "kCenters"
                  Just c1 -> kCenters' k ps c1
  where first = getFirst . F.foldMap (First . Just)

-- | Returns a list of centers based on the Farthest Point Algorithm, given an
-- initial center. The input collection of points is assumed to contain no
-- duplicates.
kCenters'         :: (Ord d, MetricSpace p d, Traversable t) => Int -> t p -> p -> [p]
kCenters' k0 ps c1
  | k0 > 0     = c1 : go (k0 - 1) ps [c1]
  | otherwise = []
  where go k xs cs
         | k <= 0    = []
         | otherwise =
           let dFromCenters = fmap $ \x -> (farthestDistance x cs, x)
               (_, c) = F.maximumBy (comparing fst) $ dFromCenters xs
           in  c : go (k - 1) xs (c : cs)

prop_farthestPointNumberOfCenters :: Int
                                  -> NonEmptyList Point2D
                                  -> Property
prop_farthestPointNumberOfCenters k (NonEmpty ps) =
  let n = length ps
  in
    k <= n ==> do
      c1 <- elements ps
      let centers = kCenters' k ps c1
      return $ case () of _
                            | k <= 0    -> length centers == 0
                            | k <= n    -> length centers == k
                            | otherwise -> length centers == n

prop_farthestPointRadiusInvariant :: Positive Int
                                  -> NonEmptyList Point2D
                                  -> Property
prop_farthestPointRadiusInvariant (Positive k) (NonEmpty ps) =
  let n = length ps
  in
    k < n ==> do
      c1 <- elements ps
      let centers = kCenters' k ps c1
          d = farthestDistance (last $ kCenters' (k + 1) ps c1) centers
      return $ all (\p -> any (\c -> distance p c <= d) centers) ps

-- | Builds a list of centers given a radius. If more than @k@ centers are
-- found, the function returns the remainder of the stream. The first point is
-- chosen as the initial center. The input collection of points is assumed to
-- contain no duplicates.
kCentersStreaming        :: (Monad m, Ord d, MetricSpace p d) =>
                            Int
                         -> Producer p m ()
                         -> d
                         -> m (Either ([p], Producer p m ()) [p])
kCentersStreaming k ps d = do first <- P.head ps
                              case first of
                                Nothing -> errorEmptyList "kCentersStreaming"
                                Just c1 -> kCentersStreaming' k ps c1 d

-- | Builds a list of centers given an initial center and a radius. If more than
-- @k@ centers are found, the function returns the remainder of the stream. The
-- input collection of points is assumed to contain no duplicates.
kCentersStreaming'            :: (Monad m, Ord d, MetricSpace p d) =>
                                 Int
                              -> Producer p m ()
                              -> p
                              -> d
                              -> m (Either ([p], Producer p m ()) [p])
kCentersStreaming' k0 ps c1 d
  | k0 > 0     = go 1 ps [c1]
  | otherwise = return $ Right []
  where go k xs cs = do
          hd <- next xs
          case hd of
            Left _         -> return $ Right cs
            Right (p, xs') ->
              if nearestDistance p cs > d
              then if k == k0
                   then return $ Left (cs, xs')
                   else do result <- go (k + 1) xs' (p : cs)
                           return $ do rest <- result
                                       return $ p : rest
              else go k xs' cs

pointInRadius   :: Double -> Gen Point2D
pointInRadius r = pointInSquare r `suchThat` inCircle
  where inCircle (x, y) = x * x + y * y <= r * r
        pointInSquare s = (,) <$> range s <*> range s
        range x = choose (negate x, x)

prop_allPointsWithinRadiusYieldsOneCenter   :: Double -> Gen Bool
prop_allPointsWithinRadiusYieldsOneCenter d = do
  ps <- shuffle =<< (listOf1 $ pointInRadius d)
  c1 <- elements ps
  let stream  = each ps
      centers = kCentersStreaming' 1 stream c1 d
  return $ length centers == 1

shuffle :: (Eq a) => [a] -> Gen [a]
shuffle [] = return []
shuffle xs = do x  <- oneof $ map return xs
                ys <- shuffle $ delete x xs
                return (x:ys)
