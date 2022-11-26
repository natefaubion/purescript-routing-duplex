module Test.Main where

import Prelude hiding ((/))

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.Gen (genAlphaString)
import Effect (Effect)
import Routing.Duplex (RouteDuplex', flag, int, param, parse, print, record, rest, root, segment, string, (:=))
import Routing.Duplex.Generic (noArgs)
import Routing.Duplex.Generic as RDG
import Routing.Duplex.Generic.Syntax ((/), (?))
import Test.QuickCheck (Result(..), arbitrary, quickCheckGen, (===))
import Test.QuickCheck.Gen (Gen, arrayOf, chooseInt)
import Test.Unit (combinatorUnitTests)
import Type.Proxy (Proxy(..))

data TestRoute
  = Root
  | Foo String Int String { a :: String, b :: Boolean }
  | Bar { id :: String, search :: String }
  | Baz String (Array String)

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
    { "Root": noArgs
    , "Foo": fooRoute
    , "Bar": barRoute
    , "Baz": bazRoute
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

main :: Effect Unit
main = do
  combinatorUnitTests

  quickCheckGen do
    r <- genTestRoute
    let
      url = print route r
      res = parse route url
    pure $ case res of
      Left err ->
        Failed $
          show err <> ":"
            <> "\n  "
            <> show r
            <> "\n  "
            <> show url
      Right r' ->
        r === r'
