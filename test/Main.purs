module Test.Main where

import Prelude hiding ((/))

import Effect (Effect)
import Test.QuickCheck.SumRouteTests as Test.QuickCheck.SumRouteTests
import Test.QuickCheck.VariantRouteTests as Test.QuickCheck.VariantRouteTests
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Unit.CombinatorTests as Test.Unit.CombinatorTests
import Test.Unit.SumOrderingTests as Test.Unit.SumOrderingTests

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Test.Unit.CombinatorTests.tests
  Test.Unit.SumOrderingTests.tests
  Test.QuickCheck.VariantRouteTests.tests
  Test.QuickCheck.SumRouteTests.tests
