module Routing.Duplex
  ( RouteDuplex(..)
  , RouteDuplex'
  , parse
  , print
  , prefix
  , suffix
  , path
  , root
  , end
  , segment
  , param
  , flag
  , many1
  , many
  , rest
  , default
  , optional
  , as
  , int
  , boolean
  , string
  , record
  , prop
  , (:=)
  , params
  , buildParams
  , class RouteDuplexParams
  , class RouteDuplexBuildParams
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Data.Either (Either)
import Data.Foldable (class Foldable, foldMap, foldr)
import Data.Maybe (Maybe)
import Data.Profunctor (class Profunctor)
import Data.String (Pattern(..))
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.Row as Row
import Prim.RowList (kind RowList, class RowToList, Cons, Nil)
import Record as Record
import Routing.Duplex.Parser (RouteParser)
import Routing.Duplex.Parser as Parser
import Routing.Duplex.Printer (RoutePrinter)
import Routing.Duplex.Printer as Printer
import Type.Data.RowList (RLProxy(..))

-- | The core abstraction of this library that can be used both for parsing
-- | values of type `o` from `String` as well as printing values of type `i` into `String`.
-- |
-- | For routing purposes you'll likely want to use more type restricted version
-- | `RouterDuplex'` which uses the same type for both input and output type parameters.
data RouteDuplex i o = RouteDuplex (i -> RoutePrinter) (RouteParser o)

-- | A type restricted variant or `RouteDuplex` where input and output are the same type.
-- | This type will typically be your custom `Route` data type representing valid routes within your application.
type RouteDuplex' a = RouteDuplex a a

derive instance functorRouteDuplex :: Functor (RouteDuplex i)

instance applyRouteDuplex :: Apply (RouteDuplex i) where
  apply (RouteDuplex encl decl) (RouteDuplex encr decr) = RouteDuplex (append <$> encl <*> encr) (decl <*> decr)

instance applicativeRouteDuplex :: Applicative (RouteDuplex i) where
  pure = RouteDuplex (const mempty) <<< pure

instance profunctorRouteDuplex :: Profunctor RouteDuplex where
  dimap f g (RouteDuplex enc dec) = RouteDuplex (f >>> enc) (g <$> dec)

-- | Use given codec to parse a value of type `o` out of String
-- | (typically representing part of URL) or produce a `RouteError` if the parsing fails.
parse :: forall i o. RouteDuplex i o -> String -> Either Parser.RouteError o
parse (RouteDuplex _ dec) = Parser.run dec

-- | Use given codec to transform a value of type `i` to a String.
print :: forall i o. RouteDuplex i o -> i -> String
print (RouteDuplex enc _) = Printer.run <<< enc

prefix :: forall a b. String -> RouteDuplex a b -> RouteDuplex a b
prefix s (RouteDuplex enc dec) = RouteDuplex (\a -> Printer.put s <> enc a) (Parser.prefix s dec)

suffix :: forall a b. RouteDuplex a b -> String -> RouteDuplex a b
suffix (RouteDuplex enc dec) s = RouteDuplex (\a -> enc a <> Printer.put s) (dec <* Parser.prefix s (pure unit))

path :: forall a b. String -> RouteDuplex a b -> RouteDuplex a b
path = flip (foldr prefix) <<< String.split (Pattern "/")

-- | Modifies given `codec` by requiring it to be prefixed with '/'.
-- | You can think of it as stripping the '/' at the beginning of route, failing if it's not there.
-- |
-- |```purescript
-- | parse (root segment) "/abc" == Right "abc"
-- | parse (root segment) "abc" == Left (Expected "" "abc")
-- | print (root segment) "abc" == "/abc"
-- |```
root :: forall a b. RouteDuplex a b -> RouteDuplex a b
root = path ""

-- | Modifies given `codec` by requiring it to consume the entire rest of input.
-- | That is `end codec` succeeds if `codec` suceeds AND all the
-- | remaining segments have been consumed in the process.
-- |
-- |```purescript
-- | parse (end segment) "abc" == Right "abc"
-- |
-- | parse (end segment) "abc/def" == Left (ExpectedEndOfPath "def")
-- |```
end :: forall a b. RouteDuplex a b -> RouteDuplex a b
end (RouteDuplex enc dec) = RouteDuplex enc (dec <* Parser.end)

-- | Process path segment as a String.
-- | Note that uri encoding / decoding is done automatically.
-- |
-- | ```purescript
-- | parse segment "abc"       == Right "abc"
-- | parse segment "abc%20def" == Right "abc def" -- automatic decoding of uri components
-- | parse segment "abc/def"   == Right "abc"
-- | parse segment "/abc"      == Right "" -- the empty string before the first '/'
-- |
-- | print segment "hello there" == "hello%20there"
-- | print segment "" == "/"
-- | ```
segment :: RouteDuplex' String
segment = RouteDuplex Printer.put Parser.take

param :: String -> RouteDuplex' String
param p = RouteDuplex (Printer.param p) (Parser.param p)

flag :: RouteDuplex' String -> RouteDuplex' Boolean
flag (RouteDuplex enc dec) = RouteDuplex enc' dec'
  where
  enc' true = enc ""
  enc' _ = mempty
  dec' = Parser.default false (dec $> true)

many1 :: forall f a b.
  Foldable f =>
  Alt f =>
  Applicative f =>
  RouteDuplex a b ->
  RouteDuplex (f a) (f b)
many1 (RouteDuplex enc dec) = RouteDuplex (foldMap enc) (Parser.many1 dec)

many :: forall f a b.
  Foldable f =>
  Alternative f =>
  RouteDuplex a b ->
  RouteDuplex (f a) (f b)
many (RouteDuplex enc dec) = RouteDuplex (foldMap enc) (Parser.many dec)

rest :: RouteDuplex' (Array String)
rest = RouteDuplex (foldMap Printer.put) Parser.rest

default :: forall a b. b -> RouteDuplex a b -> RouteDuplex a b
default d (RouteDuplex enc dec) = RouteDuplex enc (Parser.default d dec)

optional :: forall a b. RouteDuplex a b -> RouteDuplex (Maybe a) (Maybe b)
optional (RouteDuplex enc dec) = RouteDuplex (foldMap enc) (Parser.optional dec)

as :: forall s a b. (a -> s) -> (String -> Either String b) -> RouteDuplex s String -> RouteDuplex a b
as f g (RouteDuplex enc dec) = RouteDuplex (enc <<< f) (Parser.as identity g dec)

int :: RouteDuplex' String -> RouteDuplex' Int
int = as show Parser.int

-- | Make Boolean codec out of a String codec.
-- |
-- | ```purescript
-- | parse (boolean segment) "true"  == Right true
-- | parse (boolean segment) "false" == Right false
-- | parse (boolean segment) "x"     == Left (Expected "Boolean" "x")
-- |
-- | print (boolean segment) true    == "true"
-- | print (boolean segment) false   == "true"
-- | ```
boolean :: RouteDuplex' String -> RouteDuplex' Boolean
boolean = as show Parser.boolean

string :: RouteDuplex' String -> RouteDuplex' String
string = identity

record :: forall r. RouteDuplex r {}
record = RouteDuplex mempty (pure {})

prop :: forall sym a b r1 r2 r3 rx.
  IsSymbol sym =>
  Row.Cons sym a rx r1 =>
  Row.Cons sym b r2 r3 =>
  Row.Lacks sym r2 =>
  SProxy sym ->
  RouteDuplex a b ->
  RouteDuplex { | r1 } { | r2 } ->
  RouteDuplex { | r1 } { | r3 }
prop sym (RouteDuplex f g) (RouteDuplex x y) =
  RouteDuplex (\r -> x r <> f (Record.get sym r)) (flip (Record.insert sym) <$> y <*> g)

infix 2 prop as :=

class RouteDuplexParams (r1 :: # Type) (r2 :: # Type) | r1 -> r2 where
  params :: { | r1 } -> RouteDuplex' { | r2 }

instance routeDuplexParams ::
  ( RowToList r1 rl
  , RouteDuplexBuildParams rl r1 r2 () r2
  ) =>
  RouteDuplexParams r1 r2 where
  params r =
    record
      # buildParams (RLProxy :: RLProxy rl) r

class RouteDuplexBuildParams (rl :: RowList) (r1 :: # Type) (r2 :: # Type) (r3 :: # Type) (r4 :: # Type) | rl -> r1 r2 r3 r4 where
  buildParams ::
    RLProxy rl ->
    { | r1 } ->
    RouteDuplex { | r2 } { | r3 } ->
    RouteDuplex { | r2 } { | r4 }

instance buildParamsCons ::
  ( IsSymbol sym
  , Row.Cons sym (RouteDuplex String String -> RouteDuplex a b) rx1 r1
  , Row.Cons sym a rx2 r2
  , Row.Cons sym b r3 rx3
  , Row.Lacks sym r3
  , RouteDuplexBuildParams rest r1 r2 rx3 r4
  ) =>
  RouteDuplexBuildParams (Cons sym (RouteDuplex String String -> RouteDuplex a b) rest) r1 r2 r3 r4 where
  buildParams _ r prev =
    prev
      # prop sym ((Record.get sym r) (param (reflectSymbol sym)))
      # buildParams (RLProxy :: RLProxy rest) r
    where
    sym = SProxy :: SProxy sym

instance buildParamsNil ::
  RouteDuplexBuildParams Nil r1 r2 r3 r3 where
    buildParams _ r = identity
