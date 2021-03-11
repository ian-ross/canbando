module Canbando.Capability.Resource.List where

import Prelude

import Canbando.Model.List (List, Id)
import Canbando.Model.Card (Card)
import Canbando.Model.Card (Id) as Card


class Monad m <= ManageList m where
  updateList :: Id -> List -> m Unit
  moveCard :: Card.Id -> Id -> Int -> m Unit
  addCard :: Id -> m Card
  updateCard :: Card.Id -> Card -> m Unit
  deleteCard :: Card.Id -> m Unit
