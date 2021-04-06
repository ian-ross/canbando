module Server.Util
  ( Detail(..), parseDetails
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import HTTPure (Query, (!?), (!@))


data Detail = None | Lists | All

derive instance genericDetail :: Generic Detail _

instance showDetail :: Show Detail where
  show = genericShow


parseDetails :: Query -> Maybe Detail
parseDetails query
  | query !? "details" = case query !@ "details" of
    "all"   -> Just All
    "lists" -> Just Lists
    "none"  -> Just None
    _       -> Nothing
  | otherwise = Just None
