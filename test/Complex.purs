module Test.Complex where

import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (RunnerEffects, run)

-- Component under test
import Data.Complex

complexSpec :: ∀ e. Spec (RunnerEffects e) Unit
complexSpec =
     describe "Data.Complex" do
       describe "Simple Tests" do
         it "(:+) constructor" $
           x `shouldEqual` y
         where
           x = 1 :+ 2
           y = (Complex {real: 1, imaginary: 2} :: Complex Int)