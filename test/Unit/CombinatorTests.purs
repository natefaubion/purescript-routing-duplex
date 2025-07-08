module Test.Unit.CombinatorTests (tests) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Routing.Duplex (RouteDuplex', as, boolean, default, end, flag, hash, int, many, many1, optional, param, params, parse, path, prefix, print, prop, record, rest, root, segment, string, suffix)
import Routing.Duplex.Parser (RouteError(..), parsePath)
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

tests :: Spec Unit
tests = it "CombinatorTests" do
  -- boolean
  parse (boolean segment) "true" `shouldEqual` Right true
  parse (boolean segment) "false" `shouldEqual` Right false
  parse (boolean segment) "x" `shouldEqual` Left (Expected "Boolean" "x")
  parse (boolean segment) "" `shouldEqual` Left EndOfPath

  -- prefix
  parse (prefix "api" segment) "api/a" `shouldEqual` Right "a"
  parse (prefix "api" segment) "api/a" `shouldEqual` Right "a"
  parse (prefix "/api/v1" segment) "%2Fapi%2Fv1/a" `shouldEqual` Right "a"
  parse (prefix "/api/v1" segment) "/api/v1/a" `shouldEqual` Left (Expected "/api/v1" "")

  -- path
  parse (path "/api/v1" segment) "/api/v1/a" `shouldEqual` Right "a"
  parse (path "/api/v1" segment) "/api/v2/a" `shouldEqual` Left (Expected "v1" "v2")

  -- segment
  parse segment "abc" `shouldEqual` Right "abc"
  parse segment "abc%20def" `shouldEqual` Right "abc def"
  parse segment "abc/def" `shouldEqual` Right "abc"
  parse segment "/abc" `shouldEqual` Right ""
  parse segment "" `shouldEqual` Left EndOfPath

  -- root
  parse (root segment) "/abc" `shouldEqual` Right "abc"
  parse (root segment) "abc" `shouldEqual` Left (Expected "" "abc")
  parse (root segment) "/" `shouldEqual` Left EndOfPath

  -- int
  parse (int segment) "1" `shouldEqual` Right 1
  parse (int segment) "x" `shouldEqual` Left (Expected "Int" "x")

  -- param
  parse (param "search") "?search=keyword" `shouldEqual` Right "keyword"
  parse (param "search") "/" `shouldEqual` Left (MissingParam "search")
  parse (optional (param "search")) "/" `shouldEqual` Right Nothing

  -- hash
  parse hash "abc#def" `shouldEqual` Right "def"

  -- suffix
  parse (suffix segment "latest") "release/latest" `shouldEqual` Right "release"
  parse (suffix segment "latest") "/latest" `shouldEqual` Right ""
  parse (suffix segment "x/y") "a/x%2Fy" `shouldEqual` Right "a"
  parse (suffix segment "latest") "/" `shouldEqual` Left EndOfPath
  parse (suffix segment "x/y") "a/x/y" `shouldEqual` Left (Expected "x/y" "x")

  -- rest
  parse rest "" `shouldEqual` Right []
  parse rest "a/b" `shouldEqual` Right [ "a", "b" ]
  parse (path "a/b" rest) "a/b/c/d" `shouldEqual` Right [ "c", "d" ]
  print rest [ "a", "b" ] `shouldEqual` "a/b"

  -- default
  parse (default 0 $ int segment) "1" `shouldEqual` Right 1
  parse (default 0 $ int segment) "x" `shouldEqual` Right 0

  -- as
  parse (sort segment) "asc" `shouldEqual` Right Asc
  parse (sort segment) "x" `shouldEqual` Left (Expected "asc or desc" "x")

  -- many1
  parse (many1 (int segment)) "1/2/3/x" `shouldEqual` Right [ 1, 2, 3 ]
  parse (many1 (int segment)) "x" `shouldEqual` (Left (Expected "Int" "x") :: Either RouteError (Array Int))

  -- many
  parse (many (int segment)) "1/2/3/x" `shouldEqual` Right [ 1, 2, 3 ]
  parse (many (int segment)) "x" `shouldEqual` Right []

  -- flag
  parse (flag (param "x")) "?x" `shouldEqual` Right true
  parse (flag (param "x")) "?x=true" `shouldEqual` Right true
  parse (flag (param "x")) "?x=false" `shouldEqual` Right true
  parse (flag (param "x")) "?y" `shouldEqual` Right false

  -- string
  parse (string segment) "x" `shouldEqual` Right "x"
  parse (string segment) "%20" `shouldEqual` Right " "

  -- optional
  parse (optional segment) "a" `shouldEqual` Right (Just "a")
  parse (optional segment) "" `shouldEqual` Right Nothing
  print (optional segment) (Just "a") `shouldEqual` "a"
  print (optional segment) Nothing `shouldEqual` ""

  -- record
  parse (path "blog" date) "blog/2019/1/2" `shouldEqual` Right { year: 2019, month: 1, day: 2 }

  -- params
  parse search "?page=3&filter=Galaxy%20Quest" `shouldEqual` Right { page: 3, filter: Just "Galaxy Quest" }

  -- Malformed URI component
  parsePath "https://example.com?keyword=%D0%BF%D0" `shouldEqual` Left (MalformedURIComponent "%D0%BF%D0")
  print (path "foo" segment) "\xdc11" `shouldEqual` "foo"

  -- `pure unit` matches anything
  parse (pure unit) "" `shouldEqual` Right unit
  parse (pure unit) "a" `shouldEqual` Right unit
  parse (pure unit) "/asdf" `shouldEqual` Right unit
  -- `pure unit` prints to ""
  print (pure unit) (Just "asdf/asdf") `shouldEqual` ""
  print (pure unit) Nothing `shouldEqual` ""

  -- `end $ pure unit` matches only ""
  parse (end $ pure unit) "" `shouldEqual` Right unit
  parse (end $ pure unit) "a" `shouldEqual` Left (ExpectedEndOfPath "a")
  parse (end $ pure unit) "a/" `shouldEqual` Left (ExpectedEndOfPath "a")
  parse (end $ pure unit) "a/b" `shouldEqual` Left (ExpectedEndOfPath "a")
  parse (end $ pure unit) "/asdf" `shouldEqual` Left (ExpectedEndOfPath "")
  -- `end $ pure unit` prints to ""
  print (end $ pure unit) (Just "asdf/asdf") `shouldEqual` ""
  print (end $ pure unit) Nothing `shouldEqual` ""

data Sort = Asc | Desc

derive instance eqSort :: Eq Sort
instance showSort :: Show Sort where
  show Asc = "asc"
  show Desc = "desc"

sortToString :: Sort -> String
sortToString = case _ of
  Asc -> "asc"
  Desc -> "desc"

sortFromString :: String -> Either String Sort
sortFromString = case _ of
  "asc" -> Right Asc
  "desc" -> Right Desc
  _ -> Left $ "asc or desc"

sort :: RouteDuplex' String -> RouteDuplex' Sort
sort = as sortToString sortFromString

date :: RouteDuplex' { year :: Int, month :: Int, day :: Int }
date =
  record
    # prop (Proxy :: _ "year") (int segment)
    # prop (Proxy :: _ "month") (int segment)
    # prop (Proxy :: _ "day") (int segment)

search :: RouteDuplex' { page :: Int, filter :: Maybe String }
search =
  params
    { page: int
    , filter: optional <<< string
    }
