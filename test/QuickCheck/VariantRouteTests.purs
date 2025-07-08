module Test.QuickCheck.VariantRouteTests where

import Prelude

import Data.Either (Either(..))
import Data.String.Gen (genAlphaString)
import Data.Variant as V
import Data.Variant.Gen (genVariantUniform)
import Routing.Duplex (RouteDuplex', end, parse, path, print, segment, string, variant, vmatch, (%=))
import Test.QuickCheck.Utils (printThenParseShouldEqualToInput)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Type.Proxy (Proxy(..))

_x = Proxy :: Proxy "x"
_y = Proxy :: Proxy "y"

tests :: Spec Unit
tests = describe "VariantTests" do
  it "quickCheck variant" $ quickCheck $ genVariantUniform { x: genAlphaString, y: pure unit } >>= printThenParseShouldEqualToInput ((_x %= segment) ((_y %= (pure unit)) variant))
  it "quickCheck vmatch" $ quickCheck $ genVariantUniform { x: genAlphaString, y: pure unit } >>= printThenParseShouldEqualToInput (vmatch { x: segment, y: pure unit :: RouteDuplex' Unit })
  it "quickCheck variant -> realworld" do
    let
      -- order of parsing is new -> edit -> list, bc is defined by order of function execution. To learn more read ["About ordering" section in README](#about-ordering)
      routeDuplex = variant
        # (Proxy :: _ "list") %= (pure unit)
        # (Proxy :: _ "edit") %= (string segment)
        # (Proxy :: _ "new") %= (path "new" $ pure unit)
      generator = { new: pure unit, edit: genAlphaString, list: pure unit }
    quickCheck $ genVariantUniform generator >>= printThenParseShouldEqualToInput routeDuplex
  it "quickCheck vmatch -> realworld" do
    let
      routeDuplex = vmatch
        { r1_new: path "new" $ pure unit :: RouteDuplex' Unit
        , r2_edit: string segment
        , r3_list: pure unit :: RouteDuplex' Unit -- r3_list should have such a name to be parsed last, bc order of parsing is defined by names of keys. To learn more read ["About ordering" section in README](#about-ordering)
        }
      generator = { r1_new: pure unit, r2_edit: genAlphaString, r3_list: pure unit }
    quickCheck $ genVariantUniform generator >>= printThenParseShouldEqualToInput routeDuplex
  it "Parsing and printing using variant and vmatch" do
    parse ((_y %= segment) ((_x %= (pure unit)) variant)) "a/b" `shouldEqual` Right (V.inj _y "a")
    parse (vmatch { y: segment, x: pure unit :: RouteDuplex' Unit }) "a/b" `shouldEqual` Right (V.inj _x unit) -- not same output, ordering doesnt matter, only name of keys

    print ((_y %= segment) ((_x %= (pure unit)) variant)) (V.inj _y "a/b") `shouldEqual` "a%2Fb"
    print (vmatch { y: segment, x: pure unit :: RouteDuplex' Unit }) (V.inj _y "a/b") `shouldEqual` "a%2Fb"
    ------
    parse ((_x %= (pure unit)) ((_y %= segment) variant)) "a/b" `shouldEqual` Right (V.inj _x unit)
    parse (vmatch { x: pure unit :: RouteDuplex' Unit, y: segment }) "a/b" `shouldEqual` Right (V.inj _x unit)
    --
    print ((_x %= (pure unit)) ((_y %= segment) variant)) (V.inj _x unit) `shouldEqual` ""
    print (vmatch { x: pure unit :: RouteDuplex' Unit, y: segment }) (V.inj _x unit) `shouldEqual` ""
    ---
    parse ((_y %= segment) ((_x %= (end $ pure unit)) variant)) "a/b" `shouldEqual` Right (V.inj _y "a")
    parse (vmatch { y: segment, x: end $ pure unit :: RouteDuplex' Unit }) "a/b" `shouldEqual` Right (V.inj _y "a")

    print ((_y %= segment) ((_x %= (end $ pure unit)) variant)) (V.inj _y "a/b") `shouldEqual` "a%2Fb"
    print (vmatch { y: segment, x: end $ pure unit :: RouteDuplex' Unit }) (V.inj _y "a/b") `shouldEqual` "a%2Fb"
    ------
    parse ((_x %= (end $ pure unit)) ((_y %= segment) variant)) "a/b" `shouldEqual` Right (V.inj _y "a")
    parse (vmatch { x: end $ pure unit :: RouteDuplex' Unit, y: segment }) "a/b" `shouldEqual` Right (V.inj _y "a")

    print ((_x %= (end $ pure unit)) ((_y %= segment) variant)) (V.inj _x unit) `shouldEqual` ""
    print (vmatch { x: end $ pure unit :: RouteDuplex' Unit, y: segment }) (V.inj _x unit) `shouldEqual` ""
