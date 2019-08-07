{-# LANGUAGE NoImplicitPrelude #-}

module Tuple
  ( first
  , second
  , mapFirst
  , mapSecond
  , pair
  ) where

pair :: a -> b -> (a, b)
pair x y = (x, y)

first :: (a, b) -> a
first (x, _) = x

second :: (a, b) -> b
second (_, x) = x

mapFirst :: (a -> x) -> (a, b) -> (x, b)
mapFirst f (x, y) = (f x, y)

mapSecond :: (b -> x) -> (a, b) -> (a, x)
mapSecond f (x, y) = (x, f y)
