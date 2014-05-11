{-# OPTIONS -fwarn-tabs -Wall #-}

module Data.ClosestPair
  ( closestPair
  , closestPair2D
  ) where

import Data.MetricSpace
import Data.List

import Test.QuickCheck

pairs        :: [a] -> [(a, a)]
pairs []     = []
pairs [_]    = []
pairs (z:zs) = [(x, y) | let x = z, y <- zs] ++ pairs zs

distOrd :: MetricSpace p d => (p, p) -> (p, p) -> Ordering
distOrd (p1, p2) (p3, p4)
  | distance p1 p2 < distance p3 p4  = LT
  | distance p1 p2 == distance p3 p4 = EQ
  | otherwise                = GT

closestPair :: MetricSpace p d => [p] -> (p, p)
closestPair = minimumBy distOrd . pairs

xOrd                 :: Point2D -> Point2D -> Ordering
xOrd (x1, _) (x2, _)
  | x1 < x2   = LT
  | x1 == x2  = EQ
  | otherwise = GT

yOrd                 :: Point2D -> Point2D -> Ordering
yOrd (_, y1) (_, y2)
  | y1 < y2   = LT
  | y1 == y2  = EQ
  | otherwise = GT

closestPair2D     :: [Point2D] -> (Point2D, Point2D)
closestPair2D []  = error "closestPairDivideAndConquer: empty list"
closestPair2D [_] = error "closestPairDivideAndConquer: singleton list"
closestPair2D ps
  | len <= 3  = closestPair ps
  | otherwise = go (sortByX ps) (sortByY ps) len
  where len = length ps
        sortByX = sortBy xOrd
        sortByY = sortBy yOrd

        go pxs pys n
          | n <= 3 = closestPair pxs
          | otherwise = select $ minZ sy
          where select Nothing
                  | distance q0 q1 < distance r0 r1 = (q0, q1)
                  | otherwise               = (r0, r1)
                select (Just (s, s'))
                  | distance s s' < d           = (s, s')
                  | distance q0 q1 < distance r0 r1 = (q0, q1)
                  | otherwise               = (r0, r1)
                nq         = ceiling (fromIntegral n / (2 :: Double))
                nr         = n - nq
                divide     = splitAt nq
                (qxs, rxs) = divide pxs
                (x', _)    = last qxs -- max x-coordinate of point in Q
                (qys, rys) = partition (\(x, _) -> x <= x') pys
                (q0, q1)   = go qxs qys nq
                (r0, r1)   = go rxs rys nr
                d          = min (distance q0 q1) (distance r0 r1)
                sy         = filter (\(x, _) -> x' - x <= d) $ pys

                minZ        :: [Point2D] -> Maybe (Point2D, Point2D)
                minZ []     = Nothing
                minZ [_]    = Nothing
                minZ (s:ss) =
                  let s' = minimumBy distFromHead $ take 15 ss in
                  case minZ ss of
                    Nothing      -> Just (s, s')
                    Just (t, t') -> if distance s s' <= distance t t'
                                    then Just (s, s')
                                    else Just (t, t')
                    where distFromHead s1 s2
                           | distance s s1 < distance s s2  = LT
                           | distance s s1 == distance s s2 = EQ
                           | otherwise              = GT

_prop_2DAlgMatchesBruteForce :: [Point2D] -> Property
_prop_2DAlgMatchesBruteForce = check closestPair2D
  where check alg ps =
          length ps > 1 ==> distance' (alg ps) == distance' (closestPair ps)
        distance' = uncurry distance
