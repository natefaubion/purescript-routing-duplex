module Test.Unit (combinatorUnitTests) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Routing.Duplex (RouteDuplex', as, boolean, default, flag, hash, int, many, many1, optional, param, params, parse, path, prefix, print, prop, record, rest, root, segment, string, suffix)
import Routing.Duplex.Parser (RouteError(..), parsePath)
import Test.Assert (assertEqual)
import Type.Proxy (Proxy(..))

combinatorUnitTests :: Effect Unit
combinatorUnitTests = do
  -- boolean
  assertEqual { actual: parse (boolean segment) "true", expected: Right true }
  assertEqual { actual: parse (boolean segment) "false", expected: Right false }
  assertEqual { actual: parse (boolean segment) "x", expected: Left (Expected "Boolean" "x") }
  assertEqual { actual: parse (boolean segment) "", expected: Left EndOfPath }

  -- prefix
  assertEqual { actual: parse (prefix "api" segment) "api/a", expected: Right "a" }
  assertEqual { actual: parse (prefix "api" segment) "api/a", expected: Right "a" }
  assertEqual { actual: parse (prefix "/api/v1" segment) "%2Fapi%2Fv1/a", expected: Right "a" }
  assertEqual { actual: parse (prefix "/api/v1" segment) "/api/v1/a", expected: Left (Expected "/api/v1" "") }

  -- path
  assertEqual { actual: parse (path "/api/v1" segment) "/api/v1/a", expected: Right "a" }
  assertEqual { actual: parse (path "/api/v1" segment) "/api/v2/a", expected: Left (Expected "v1" "v2") }

  -- segment
  assertEqual { actual: parse segment "abc", expected: Right "abc" }
  assertEqual { actual: parse segment "abc%20def", expected: Right "abc def" }
  assertEqual { actual: parse segment "abc/def", expected: Right "abc" }
  assertEqual { actual: parse segment "/abc", expected: Right "" }
  assertEqual { actual: parse segment "", expected: Left EndOfPath }

  -- root
  assertEqual { actual: parse (root segment) "/abc", expected: Right "abc" }
  assertEqual { actual: parse (root segment) "abc", expected: Left (Expected "" "abc") }
  assertEqual { actual: parse (root segment) "/", expected: Left EndOfPath }

  -- int
  assertEqual { actual: parse (int segment) "1", expected: Right 1 }
  assertEqual { actual: parse (int segment) "x", expected: Left (Expected "Int" "x") }

  -- param
  assertEqual { actual: parse (param "search") "?search=keyword", expected: Right "keyword" }
  assertEqual { actual: parse (param "search") "/", expected: Left (MissingParam "search") }
  assertEqual { actual: parse (optional (param "search")) "/", expected: Right Nothing }

  -- hash
  assertEqual { actual: parse hash "abc#def", expected: Right "def" }

  -- suffix
  assertEqual { actual: parse (suffix segment "latest") "release/latest", expected: Right "release" }
  assertEqual { actual: parse (suffix segment "latest") "/latest", expected: Right "" }
  assertEqual { actual: parse (suffix segment "x/y") "a/x%2Fy", expected: Right "a" }
  assertEqual { actual: parse (suffix segment "latest") "/", expected: Left EndOfPath }
  assertEqual { actual: parse (suffix segment "x/y") "a/x/y", expected: Left (Expected "x/y" "x") }

  -- rest
  assertEqual { actual: parse rest "", expected: Right [] }
  assertEqual { actual: parse rest "a/b", expected: Right [ "a", "b" ] }
  assertEqual { actual: parse (path "a/b" rest) "a/b/c/d", expected: Right [ "c", "d" ] }
  assertEqual { actual: print rest [ "a", "b" ], expected: "a/b" }

  -- default
  assertEqual { actual: parse (default 0 $ int segment) "1", expected: Right 1 }
  assertEqual { actual: parse (default 0 $ int segment) "x", expected: Right 0 }

  -- as
  assertEqual { actual: parse (sort segment) "asc", expected: Right Asc }
  assertEqual { actual: parse (sort segment) "x", expected: Left (Expected "asc or desc" "x") }

  -- many1
  assertEqual { actual: parse (many1 (int segment)) "1/2/3/x", expected: Right [ 1, 2, 3 ] }
  assertEqual { actual: parse (many1 (int segment)) "x", expected: Left (Expected "Int" "x") :: Either RouteError (Array Int) }

  -- many
  assertEqual { actual: parse (many (int segment)) "1/2/3/x", expected: Right [ 1, 2, 3 ] }
  assertEqual { actual: parse (many (int segment)) "x", expected: Right [] }

  -- flag
  assertEqual { actual: parse (flag (param "x")) "?x", expected: Right true }
  assertEqual { actual: parse (flag (param "x")) "?x=true", expected: Right true }
  assertEqual { actual: parse (flag (param "x")) "?x=false", expected: Right true }
  assertEqual { actual: parse (flag (param "x")) "?y", expected: Right false }

  -- string
  assertEqual { actual: parse (string segment) "x", expected: Right "x" }
  assertEqual { actual: parse (string segment) "%20", expected: Right " " }

  -- optional
  assertEqual { actual: parse (optional segment) "a", expected: Right (Just "a") }
  assertEqual { actual: parse (optional segment) "", expected: Right Nothing }
  assertEqual { actual: print (optional segment) (Just "a"), expected: "a" }
  assertEqual { actual: print (optional segment) Nothing, expected: "" }

  -- record
  assertEqual { actual: parse (path "blog" date) "blog/2019/1/2", expected: Right { year: 2019, month: 1, day: 2 } }

  -- params
  assertEqual { actual: parse search "?page=3&filter=Galaxy%20Quest", expected: Right { page: 3, filter: Just "Galaxy Quest" } }

  -- Malformed URI component
  assertEqual { actual: parsePath "https://example.com?keyword=%D0%BF%D0", expected: Left (MalformedURIComponent "%D0%BF%D0") }
  assertEqual { actual: print (path "foo" segment) "\xdc11", expected: "foo" }

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
