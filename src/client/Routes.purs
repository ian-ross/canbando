module Canbando.Routes
  ( Route(..)
  , route
  ) where

import Prelude hiding ((/))

import Canbando.Model.Id (Id)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Routing.Duplex (RouteDuplex', root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))


data Route
  = Home
  | ResetTestData
  | Board Id

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route

instance showRoute :: Show Route where
  show = genericShow

route :: RouteDuplex' Route
route = root $ sum
  { "Home"          : noArgs
  , "ResetTestData" : "reset-test-data" / noArgs
  , "Board"         : "board" / segment
  }
