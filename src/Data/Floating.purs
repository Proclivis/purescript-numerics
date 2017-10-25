module Data.Floating
  ( class Floating
    , sqrt
  ) where

import Prelude (class EuclideanRing)

import Data.Num (class Num)

class (EuclideanRing a) <= Floating a where
  sqrt :: a -> a