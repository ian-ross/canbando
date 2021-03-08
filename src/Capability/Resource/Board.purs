module Canbando.Capability.Resource.Board where

import Prelude

import Data.Maybe (Maybe)

import Canbando.Model.Board (Board, Id)


class Monad m <= ManageBoard m where
  getBoard :: Id -> m (Maybe Board)
  deleteBoard :: Id -> m Unit
  -- moveList :: List.Id ->
