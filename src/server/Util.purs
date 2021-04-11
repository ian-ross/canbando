module Server.Util
  ( Detail(..), withDetails
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import HTTPure (Query, badRequest, (!?), (!@))
import Server.Env (ResponseM)


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

withDetails :: Query -> (Detail -> ResponseM) -> ResponseM
withDetails query f =
  case parseDetails query of
    Nothing -> badRequest "invalid details"
    Just details -> f details
