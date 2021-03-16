module Canbando.Model.Board
  ( BoardRep, Board, BoardStore
  , toBoard, toBoardStore
  , addListToBoard
  ) where

import Prelude

import Canbando.Model.Id (Id)
import Canbando.Model.List (List)
import Data.Array (snoc)


type BoardRep listrep row =
  ( id :: Id
  , name :: String
  , lists :: Array listrep
  | row )

type Board = { | BoardRep List () }

type BoardStore = { | BoardRep Id () }

toBoard :: forall row. { | BoardRep List row } -> Board
toBoard brd = { id: brd.id, name: brd.name, lists: brd.lists }

toBoardStore :: forall row. { | BoardRep List row } -> BoardStore
toBoardStore brd = { id: brd.id, name: brd.name, lists: map _.id brd.lists }

addListToBoard :: forall list row. list -> { | BoardRep list row } -> { | BoardRep list row }
addListToBoard list brd = brd { lists = brd.lists `snoc` list }
