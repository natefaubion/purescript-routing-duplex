module Test.Main where

import Prelude hiding ((/))

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.String.Gen (genAlphaString)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Routing.Duplex (RouteDuplex', end', flag, int, param, parse, path, print, record, rest, root, segment, string, (:=))
import Routing.Duplex.Generic (noArgs)
import Routing.Duplex.Generic as RDG
import Routing.Duplex.Generic.Syntax ((/), (?))
import Test.QuickCheck (Result(..), arbitrary, quickCheckGen, (===))
import Test.QuickCheck.Gen (Gen, arrayOf, chooseInt)

data TestRoute
  = Root
  | Foo
  | Bar String Int String { a :: String, b :: Boolean }
  | Baz { id :: String, search :: String }
  | Qux String (Array String)

derive instance eqTestRoute :: Eq TestRoute
derive instance genericTestRoute :: Generic TestRoute _
instance showTestRoute :: Show TestRoute where show = genericShow

genTestRoute :: Gen TestRoute
genTestRoute = do
  chooseInt 1 5 >>= case _ of
    1 -> pure Root
    2 -> pure Foo
    3 ->
      Bar
        <$> genAlphaString
        <*> arbitrary
        <*> genAlphaString
        <*> ({ a: _, b: _ } <$> genAlphaString <*> arbitrary)
    3 -> Baz <$> ({ id: _, search: _ } <$> genAlphaString <*> genAlphaString)
    _ -> Qux <$> genAlphaString <*> (arrayOf genAlphaString)

_id = SProxy :: SProxy "id"
_search = SProxy :: SProxy "search"

route :: RouteDuplex' TestRoute
route =
  root $ RDG.sum
    { "Root": noArgs
    , "Foo": fooRoute
    , "Bar": barRoute
    , "Baz": bazRoute
    , "Qux": quxRoute
    }
  where
  fooRoute =
    path "qux" noArgs # end'

  barRoute =
    segment / int segment / segment ? { a: string, b: flag }

  bazRoute =
    record
      # _id := segment
      # _search := param "search"

  quxRoute =
    segment / rest

main :: Effect Unit
main = do
  quickCheckGen do
    r <- genTestRoute
    let
      url = print route r
      res = parse route url
    pure $ case res of
      Left err ->
        Failed $
          show err <> ":"
            <> "\n  " <> show r
            <> "\n  " <> show url
      Right r' ->
        r === r'
