module Canbando.Capability.Resource.Board where

import Prelude

import Data.Maybe (Maybe)

import Canbando.Model.Board (Board, Id)
import Canbando.Model.List (List)
import Canbando.Model.List (Id) as List


class Monad m <= ManageBoard m where
  getBoard :: Id -> m (Maybe Board)
  deleteBoard :: Id -> m Unit
  moveList :: List.Id -> Id -> Int -> m Unit
  addList :: Id -> m List
  deleteList :: List.Id -> m Unit
