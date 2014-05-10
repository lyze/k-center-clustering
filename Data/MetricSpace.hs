{-# OPTIONS -fwarn-tabs -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
  TypeSynonymInstances, FlexibleInstances #-}

module Data.MetricSpace
  ( MetricSpace(..)
  , Point2D
  ) where

class Ord d => MetricSpace p d | p -> d where
  distance :: p -> p -> d

type Point2D = (Double, Double)

instance MetricSpace Point2D Double where
  distance (x1, y1) (x2, y2) = sqrt $ (x1 - x2)^(2 :: Int) + (y1 - y2)^(2 :: Int)
