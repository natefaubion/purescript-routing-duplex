module Routing.Duplex.Printer
  ( RoutePrinter(..)
  , printer
  , put
  , param
  , flag
  , hash
  , run
  ) where

import Prelude

import Data.Array as Array
import Data.Function (applyFlipped)
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String (joinWith)
import Data.Tuple (Tuple(..), uncurry)
import Global.Unsafe (unsafeEncodeURIComponent)
import Routing.Duplex.Types (RouteState, emptyRouteState)

newtype RoutePrinter = RoutePrinter (Endo (->) RouteState)

derive newtype instance semigroupRoutePrinter :: Semigroup RoutePrinter
derive newtype instance monoidRoutePrinter :: Monoid RoutePrinter
derive instance newtypeRoutePrinter :: Newtype RoutePrinter _

printer :: (RouteState -> RouteState) -> RoutePrinter
printer = RoutePrinter <<< Endo

put :: String -> RoutePrinter
put str = printer \state -> state { segments = Array.snoc state.segments str }

param :: String -> String -> RoutePrinter
param key val = printer \state -> state { params = Array.cons (Tuple key val) state.params }

flag :: String -> Boolean -> RoutePrinter
flag key val
  | val = param key ""
  | otherwise = mempty

hash :: String -> RoutePrinter
hash h = printer _ { hash = h }

run :: RoutePrinter -> String
run = print <<< applyFlipped emptyRouteState <<< unwrap <<< unwrap
  where
  print { segments, params, hash: h } =
    joinWith "/" segments <> printParams params <> printHash h

  printSegments =
    joinWith "/" <<< map unsafeEncodeURIComponent

  printParams [] = ""
  printParams ps = "?" <> joinWith "&" (uncurry printParam <$> ps)

  printParam key ""  = unsafeEncodeURIComponent key
  printParam key val = unsafeEncodeURIComponent key <> "=" <> unsafeEncodeURIComponent val

  printHash "" = ""
  printHash h  = "#" <> h
