-- | A `Complex` number typeclass.
module Data.Complex
  ( Complex(..)
  , mkComplex
  , (:+)
  , realPart
  , imagPart
  ) where

import Prelude (class Applicative
                , class Apply
                , class Bind
                , class Eq
                , class Functor
                , class Monad
                , class Semiring
                , class Show
                , show
                , pure
                , one
                , zero
                , (<>)
                , (*)
                , (+))

import Control.Monad.Zip
import Control.Monad.Fix

import Data.Num (class Num)

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

-- -----------------------------------------------------------------------------
-- Typeclass instances for Complex

instance showComplex :: (Show a) => Show (Complex a) where
  show (Complex r i) = (show r) <> " + " <> (show i) <> "i"

derive instance eqComplex :: (Eq a) => Eq (Complex a)

instance functorComplex :: Functor Complex where
  map f (Complex r i) = f r :+ f i

instance applyComplex :: Apply Complex where
  apply (Complex f g) (Complex r i) = f r :+ g i

instance applicativeComplex :: Applicative Complex where
  pure x = x :+ x

instance bindComplex :: Bind Complex where
  bind (Complex r i) f = realPart (f r) :+ imagPart (f i)

instance monadComplex :: Monad Complex

instance zipMaybeInt :: MonadZip Complex where
  mzip = mzip_
  mzipWith = mzipWith_
  munzip = munzip_

instance semiringComplex :: Semiring a => Semiring (Complex a) where
  one = pure one
  mul (Complex r1 i1) (Complex r2 i2) = (r1 * r2) :+ (i1 * i2)
  zero = pure zero
  add (Complex r1 i1) (Complex r2 i2) = (r1 + r2) :+ (i1 + i2)