module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

import Test.Complex


main :: Eff (RunnerEffects ()) Unit
main = run [ consoleReporter ] do
  complexSpec
