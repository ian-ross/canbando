module Canbando.Api.Endpoint
 ( Endpoint(..), DetailParams(..)
 , endpointCodec
 ) where

import Prelude hiding ((/))

import Canbando.Model.Id (Id)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Routing.Duplex (RouteDuplex', optional, root, segment, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))


type DetailParams = { details :: Maybe String }

data Endpoint
  = Boards
  | Board Id DetailParams
  | BoardLabels Id
  | BoardLists Id
  | List Id DetailParams
  | ListLocation Id
  | ListCards Id
  | Card Id
  | CardLocation Id
  | Label Id

derive instance genericEndpoint :: Generic Endpoint _

endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ sum
  { "Boards": "boards" / noArgs
  , "Board": "boards" / segment ? { details: optional <<< string }
  , "BoardLabels": "boards" / segment / "labels"
  , "BoardLists": "boards" / segment / "lists"
  , "List": "lists" / segment ? { details: optional <<< string }
  , "ListLocation": "lists" / segment / "location"
  , "ListCards": "lists" / segment / "cards"
  , "Card": "cards" / segment
  , "CardLocation": "cards" / segment / "location"
  , "Label": "labels" / segment
  }
