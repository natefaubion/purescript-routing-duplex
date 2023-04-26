module Routing.Duplex.Types where

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)

type RouteParams = Array (Tuple String String)

type RouteState =
  { segments :: Array String
  , params :: RouteParams
  , hash :: Maybe String
  }

emptyRouteState :: RouteState
emptyRouteState =
  { segments: []
  , params: []
  , hash: Nothing
  }
