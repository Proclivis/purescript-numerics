-- | A `Num` typeclass.
module Data.Num
  ( class Num
  , negate, abs, signum, sqrt
  , fromBigInt
  ) where

import Prelude (id, ($), (<<<), class Eq, class Semiring, class Ring, class CommutativeRing)
import Data.BigInt as BI
import Data.Int as I
import Data.Maybe (fromMaybe)
import Data.Ord as O
import Data.Ring as R
import Math as M
       
-- | The `Num` class represents general numbers, similar to the Haskell version.
class (Eq a, Semiring a, Ring a, CommutativeRing a) <= Num a where
    negate :: a -> a
    abs :: a -> a
    signum :: a -> a
    sqrt :: a -> a
    fromBigInt :: BI.BigInt -> a

-- | `Num` instance for `Int`.
instance numInt :: Num Int where
    negate = R.negate
    abs = O.abs
    signum = O.signum
    -- The `fromMaybe` call should never return the default value in this usage
    sqrt = fromMaybe (0 :: Int) <<< I.fromNumber <<< M.sqrt <<< I.toNumber
    fromBigInt bi = fromMaybe 0 $ (I.fromNumber <<< BI.toNumber) bi
   
-- | `Num` instance for `Number`.
instance numNumber :: Num Number where
    negate = R.negate
    abs = O.abs
    signum = O.signum
    sqrt = M.sqrt
    fromBigInt bi = BI.toNumber bi

-- | `Num` instance for `BigInt`.
instance numBigInt :: Num BI.BigInt where
    negate = R.negate
    abs = O.abs
    signum = O.signum
    -- FIXME: Implement sqrt on BigInt.  This implementation looses precision.
    sqrt = BI.fromInt <<< fromMaybe (0 :: Int) <<< I.fromNumber <<< M.sqrt <<< BI.toNumber
    fromBigInt = id
