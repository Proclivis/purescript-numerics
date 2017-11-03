-- | A `Complex` number typeclass.
module Data.Complex
  ( Complex(..)
  , mkComplex
  , (:+)
  , magnitude
  , realPart
  , imagPart
  ) where

import Prelude (class Applicative
                , class Apply
                , class Bind
                , class CommutativeRing
                , class Eq
                , class EuclideanRing
                , class Functor
                , class Monad
                , class Ring
                , class Semiring
                , class Show
                , show
                , pure
                , one
                , zero
                , sub
                , mod
                , (&&), (==), (<>)
                , (*), (+), (-), (/), ($), (<)
                , (<<<))

import Control.Monad.Zip
import Control.Monad.Fix

import Data.BigInt (toNumber)
import Data.Int as I
import Math as M
import Data.Num (class Num, negate, abs, signum, fromBigInt)
import Data.Floating (class Floating, sqrt)

-- -----------------------------------------------------------------------------
-- The Complex type

-- | Complex numbers are an algebraic type.
--
-- For a complex number @z@, @'abs' z@ is a number with the magnitude of @z@,
-- but oriented in the positive real direction, whereas @'signum' z@
-- has the phase of @z@, but unit magnitude.
--
data Complex a = Complex a a

mkComplex :: forall a. a -> a -> Complex a
mkComplex r i = Complex r i

infix 6 mkComplex as :+

-- -----------------------------------------------------------------------------
-- Functions over Complex

-- | Extracts the real part of a complex number.
realPart :: forall a. Complex a -> a
realPart (Complex r _) = r

-- | Extracts the imaginary part of a complex number.
imagPart :: forall a. Complex a -> a
imagPart (Complex _ i) = i

magnitude :: forall a b. Semiring a => Eq a => (a -> a) -> Complex a -> a
magnitude sqrtFn (Complex r i) = if (zero == r) && (zero == i) then zero
                           else sqrtFn ((r*r) + (i*i))

-- -----------------------------------------------------------------------------
-- Typeclass instances for Complex

instance showComplex :: (Show a) => Show (Complex a) where
  show (Complex r i) = (show r) <> " + " <> (show i) <> "i"

instance functorComplex :: Functor Complex where
  map f (Complex r i) = f r :+ f i

instance applyComplex :: Apply Complex where
  apply (Complex f g) (Complex r i) = f r :+ g i

instance applicativeComplex :: Applicative Complex where
  pure x = x :+ x

instance bindComplex :: Bind Complex where
  bind (Complex r i) f = realPart (f r) :+ imagPart (f i)

instance monadComplex :: Monad Complex

instance zipComplex :: MonadZip Complex where
  mzip = mzip_
  mzipWith = mzipWith_
  munzip = munzip_

-- Implementing the typeclasses for Num

derive instance eqComplex :: (Eq a) => Eq (Complex a)

instance semiringComplex ::  Semiring a => Semiring (Complex a) where
  one = pure one
  mul (Complex r1 i1) (Complex r2 i2) = (r1 * r2) :+ (i1 * i2)
  zero = pure zero
  add (Complex r1 i1) (Complex r2 i2) = (r1 + r2) :+ (i1 + i2)

instance ringComplex ::  Ring a => Ring (Complex a) where
  sub (Complex r1 i1) (Complex r2 i2) = (r1 - r2) :+ (i1 - i2)

instance commringComplex :: CommutativeRing a => CommutativeRing (Complex a)

instance eucliadeanComplex :: EuclideanRing (Complex Number) where
  degree (Complex r i) = 2 -- TODO: Check this
  div (Complex r1 i1) (Complex r2 i2) = r1 / r2 :+ i1 / i2
  mod (Complex r1 i1) (Complex r2 i2) = r1 `mod` r2 :+ i1 `mod` i2

instance floatingComplex :: Floating (Complex Number) where
  sqrt (Complex r i)   = if (zero == r) && (zero == i) then zero :+ zero
                         else u :+ (if i < 0.0 then -v else v)
                           where
                             u'    = M.sqrt ((magnitude M.sqrt (r :+ i) + abs r) / 2.0)
                             v'    = abs i / (u'*2.0)
                             u     = if r < 0.0 then v' else u'
                             v     = if r < 0.0 then u' else v'