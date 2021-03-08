module Canbando.Capability.Resource.List where

import Prelude

import Data.Maybe (Maybe)

import Canbando.Model.List (List, Id)
import Canbando.Model.Card (Card)


class Monad m <= ManageList m where
  updateList :: Id -> List -> m (Maybe List)
  deleteList :: Id -> m Unit
  moveCard :: Card -> Id -> Int -> m Unit
  addCard :: Id -> m (Maybe Card)
