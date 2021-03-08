module Canbando.Capability.Resource.Card where

import Prelude

import Data.Maybe (Maybe)

import Canbando.Model.Card (Card, Id)


class Monad m <= ManageCard m where
  updateCard :: Id -> Card -> m (Maybe Card)
  deleteCard :: Id -> m Unit
