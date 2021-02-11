module Routing.Duplex.Printer
  ( RoutePrinter(..)
  , PrintPathError(..)
  , put
  , param
  , flag
  , hash
  , run
  , printPath
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Function (applyFlipped)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), uncurry)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import JSURI (encodeURIComponent)
import Routing.Duplex.Types (RouteState, emptyRouteState)

newtype RoutePrinter = RoutePrinter (RouteState -> RouteState)

derive instance newtypeRoutePrinter :: Newtype RoutePrinter _

instance semigroupRoutePrinter :: Semigroup RoutePrinter where
  append (RoutePrinter f) (RoutePrinter g) = RoutePrinter (f >>> g)

instance monoidRoutePRinter :: Monoid RoutePrinter where
  mempty = RoutePrinter identity

newtype PrintPathError = EncodeURIComponentError String

derive newtype instance eqPrintPathError :: Eq PrintPathError
derive newtype instance ordPrintPathError :: Ord PrintPathError
derive instance newtypePrintPathError :: Newtype PrintPathError _
derive instance genericPrintPathError :: Generic PrintPathError _
instance showPrintPathError :: Show PrintPathError where show = genericShow

put :: String -> RoutePrinter
put str = RoutePrinter \state -> state { segments = Array.snoc state.segments str }

param :: String -> String -> RoutePrinter
param key val = RoutePrinter \state -> state { params = Array.cons (Tuple key val) state.params }

flag :: String -> Boolean -> RoutePrinter
flag key val
  | val = param key ""
  | otherwise = mempty

hash :: String -> RoutePrinter
hash h = RoutePrinter _ { hash = h }

run :: RoutePrinter -> Either PrintPathError String
run = printPath <<< applyFlipped emptyRouteState <<< unwrap

printPath :: RouteState -> Either PrintPathError String
printPath = \{ segments, params, hash: hash' } -> ado
  segments' <- printSegments segments
  params' <- printParams params
  in segments' <> params' <> printHash hash'
  where
  encodeURIComponent' :: String -> Either PrintPathError String
  encodeURIComponent' s = case encodeURIComponent s of
    Just s' -> Right s'
    _ -> Left $ EncodeURIComponentError s

  printSegments :: Array String -> Either PrintPathError String
  printSegments = case _ of
    [""] -> Right "/"
    xs -> joinWith "/" <$> traverse encodeURIComponent' xs

  printParams :: Array (Tuple String String) -> Either PrintPathError String
  printParams [] = Right ""
  printParams ps = (\x -> "?" <> joinWith "&" x) <$> (traverse (uncurry printParam) ps)

  printParam :: String -> String -> Either PrintPathError String
  printParam key ""  = encodeURIComponent' key
  printParam key val = ado
     key' <- encodeURIComponent' key
     val' <- encodeURIComponent' val
     in key' <> "=" <> val'

  printHash :: String -> String
  printHash "" = ""
  printHash h  = "#" <> h
