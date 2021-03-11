module Canbando.Model.Board where

import Canbando.Model.List (List)


type Id = String

type BoardRep row =
  ( id :: Id
  , name :: String
  , lists :: Array List
  | row )

type Board = { | BoardRep () }
