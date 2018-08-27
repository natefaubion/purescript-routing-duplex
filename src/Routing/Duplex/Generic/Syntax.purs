module Routing.Duplex.Generic.Syntax where

import Prelude

import Data.Generic.Rep (Argument, Product)
import Routing.Duplex (class RouteDuplexParams, RouteDuplex, RouteDuplex', params, path)
import Routing.Duplex.Generic (class GRouteDuplexCtr, gRouteDuplexCtr, product)

class GSep a b c | a b -> c where
  gsep :: a -> b -> RouteDuplex' c

instance gsepStringString ::
  GSep String String Unit where
  gsep a b = path a $ path b $ pure unit
else
instance gsepStringRoute ::
  GRouteDuplexCtr a b =>
  GSep String (RouteDuplex a a) b where
  gsep a = path a <<< gRouteDuplexCtr
else
instance gsepProduct ::
  GRouteDuplexCtr b c =>
  GSep (RouteDuplex a a) (RouteDuplex b b) (Product (Argument a) c) where
  gsep = product

infixr 1 gsep as /

gquery :: forall a r1 r2.
  RouteDuplexParams r1 r2 =>
  RouteDuplex' a ->
  { | r1 } ->
  RouteDuplex' (Product (Argument a) (Argument { | r2 }))
gquery a b = product a (params b)

infix 8 gquery as ?
