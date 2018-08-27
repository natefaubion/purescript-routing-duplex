module Routing.Duplex.Generic where

import Prelude

import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), Sum(..), from, to)
import Data.Profunctor (dimap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row as Row
import Record as Record
import Routing.Duplex (RouteDuplex(..), RouteDuplex', end)

sum :: forall a rep r.
  Generic a rep =>
  GRouteDuplex rep r =>
  { | r } ->
  RouteDuplex' a
sum = dimap from to <<< gRouteDuplex

class GRouteDuplex rep (r :: # Type) | rep -> r where
  gRouteDuplex :: { | r } -> RouteDuplex' rep

instance gRouteSum ::
  ( GRouteDuplex a r
  , GRouteDuplex b r
  ) =>
  GRouteDuplex (Sum a b) r where
  gRouteDuplex r = RouteDuplex enc dec
    where
    RouteDuplex encl decl = gRouteDuplex r
    RouteDuplex encr decr = gRouteDuplex r
    enc = case _ of
      Inl a -> encl a
      Inr b -> encr b
    dec = Inl <$> decl <|> Inr <$> decr

instance gRouteConstructor ::
  ( IsSymbol sym
  , Row.Cons sym (RouteDuplex a a) rx r
  , GRouteDuplexCtr a b
  ) =>
  GRouteDuplex (Constructor sym b) r where
  gRouteDuplex r = RouteDuplex enc dec
    where
    RouteDuplex enc' dec' =
      end
        $ (gRouteDuplexCtr :: RouteDuplex' a -> RouteDuplex' b)
        $ Record.get (SProxy :: SProxy sym) r
    enc (Constructor a) = enc' a
    dec = Constructor <$> dec'

class GRouteDuplexCtr a b | a -> b where
  gRouteDuplexCtr :: RouteDuplex' a -> RouteDuplex' b

instance gRouteProduct ::
  GRouteDuplexCtr (Product a b) (Product a b) where
  gRouteDuplexCtr = identity
else
instance gRouteNoArguments ::
  GRouteDuplexCtr NoArguments NoArguments where
  gRouteDuplexCtr = identity
else
instance gRouteArgument ::
  GRouteDuplexCtr (Argument a) (Argument a) where
  gRouteDuplexCtr = identity
else
instance gRouteAll ::
  GRouteDuplexCtr a (Argument a) where
  gRouteDuplexCtr (RouteDuplex enc dec) =
    RouteDuplex (\(Argument a) -> enc a) (Argument <$> dec)

product :: forall a b c.
  GRouteDuplexCtr b c =>
  RouteDuplex' a ->
  RouteDuplex' b ->
  RouteDuplex' (Product (Argument a) c)
product (RouteDuplex encl decl) l = RouteDuplex enc dec
  where
  RouteDuplex encr decr = gRouteDuplexCtr l
  enc (Product (Argument a) b) = encl a <> encr b
  dec = Product <$> (Argument <$> decl) <*> decr

noArgs:: RouteDuplex' NoArguments
noArgs = pure NoArguments

infixr 0 product as ~
