-- | A `Complex` number typeclass.
module Data.Complex
  ( Complex(..)
  , mkComplex
  , (:+)
  , realPart
  , imagPart
  ) where

import Prelude
import Data.Num

-- -----------------------------------------------------------------------------
-- The Complex type

-- | Complex numbers are an algebraic type.
--
-- For a complex number @z@, @'abs' z@ is a number with the magnitude of @z@,
-- but oriented in the positive real direction, whereas @'signum' z@
-- has the phase of @z@, but unit magnitude.
--
data Complex a = Complex {real :: a, imaginary :: a}

mkComplex :: forall a. (Num a) => a -> a -> Complex a
mkComplex r i = Complex {real : r, imaginary: i}

infix 6 mkComplex as :+

-- -----------------------------------------------------------------------------
-- Functions over Complex

-- | Extracts the real part of a complex number.
realPart :: forall a. (Num a) => Complex a -> a
realPart (Complex c) = c.real

-- | Extracts the imaginary part of a complex number.
imagPart :: forall a. (Num a) => Complex a -> a
imagPart (Complex c) = c.imaginary

-- -----------------------------------------------------------------------------
-- Typeclass instances for Complex

instance showComplex :: Show (Complex Int) where
  show (Complex c1) = (show c1.real) <> " + " <> (show c1.imaginary) <> "i"

derive instance eqComplex :: (Num a) => Eq (Complex a)