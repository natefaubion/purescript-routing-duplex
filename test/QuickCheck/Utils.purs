module Test.QuickCheck.Utils where

import Prelude hiding ((/))

import Data.Either (Either(..))
import Routing.Duplex (RouteDuplex', parse, print)
import Test.QuickCheck (Result(..))
import Test.QuickCheck.Gen (Gen)

printThenParseShouldEqualToInput :: forall i. Show i => Eq i => RouteDuplex' i -> i -> Gen Result
printThenParseShouldEqualToInput routeDuplex inputRoute = do
  let inputRoute_url = print routeDuplex inputRoute
  let parsedTestRoute_result = parse routeDuplex inputRoute_url
  pure $ case parsedTestRoute_result of
    Left parsedTestRoute_result_error ->
      Failed $
        show parsedTestRoute_result_error <> ":"
          <> "\n  "
          <> show inputRoute
          <> "\n  URL: "
          <> show inputRoute_url
    Right parsedTestRoute ->
      if inputRoute == parsedTestRoute then Success
      else Failed $
        let
          parsedTestRoute_url = print routeDuplex parsedTestRoute
        in
          "Input route " <> show inputRoute <> " !== parsed route " <> show parsedTestRoute <> ":"
            <> "\n  Input URL: "
            <> show inputRoute_url
            <> "\n  Parsed URL: "
            <> show parsedTestRoute_url
