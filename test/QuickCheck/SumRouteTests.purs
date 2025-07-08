module Test.QuickCheck.SumRouteTests where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.Gen (genAlphaString)
import Routing.Duplex (RouteDuplex', flag, int, param, record, rest, root, segment, string, (:=))
import Routing.Duplex.Generic (noArgs)
import Routing.Duplex.Generic as RDG
import Routing.Duplex.Generic.Syntax ((/), (?))
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf, chooseInt)
import Test.QuickCheck.Utils (printThenParseShouldEqualToInput)
import Test.Spec (Spec, it)
import Test.Spec.QuickCheck (quickCheck)
import Type.Proxy (Proxy(..))

data TestRoute
  = Foo String Int String { a :: String, b :: Boolean } -- Matches /smth/1/smth?a=smth&b=smth
  | Bar { id :: String, search :: String } -- Matches /smth/search
  | Baz String (Array String) -- Matches /smth/smth/smth. Baz should be last, because order of parsing is defined by order of constructors. To learn more read ["About ordering" section in README](#about-ordering)
  | Root -- Matches /

derive instance eqTestRoute :: Eq TestRoute
derive instance genericTestRoute :: Generic TestRoute _

instance showTestRoute :: Show TestRoute where
  show = genericShow

genTestRoute :: Gen TestRoute
genTestRoute = do
  chooseInt 1 4 >>= case _ of
    1 -> pure Root
    2 ->
      Foo
        <$> genAlphaString
        <*> arbitrary
        <*> genAlphaString
        <*> ({ a: _, b: _ } <$> genAlphaString <*> arbitrary)
    3 -> Bar <$> ({ id: _, search: _ } <$> genAlphaString <*> genAlphaString)
    _ -> Baz <$> genAlphaString <*> (arrayOf genAlphaString)

_id = Proxy :: Proxy "id"
_search = Proxy :: Proxy "search"

route :: RouteDuplex' TestRoute
route =
  root $ RDG.sum
    { "Baz": bazRoute
    , "Bar": barRoute
    , "Foo": fooRoute
    , "Root": noArgs
    }
  where
  fooRoute =
    segment / int segment / segment ? { a: string, b: flag }

  barRoute =
    record
      # _id := segment
      # _search := param "search"

  bazRoute =
    segment / rest

tests :: Spec Unit
tests = it "NormalRouteTests" do
  quickCheck $ genTestRoute >>= printThenParseShouldEqualToInput route
