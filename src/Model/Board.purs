module Canbando.Model.Board
  ( BoardInfo, BoardLists, BoardRep, Board, BoardStore
  , toBoard, toBoardStore
  , addListToBoard
  ) where

import Prelude

import Canbando.Model.Id (Id)
import Canbando.Model.List (List)
import Data.Array (snoc)
import Type.Row (type (+))


type BoardInfo row =
  ( id :: Id
  , name :: String
  , bgColour :: String
  | row )

type BoardLists listrep row =
  ( lists :: Array listrep
  | row )

type BoardRep listrep row =
  { | BoardInfo + BoardLists listrep row }

type Board = BoardRep List ()

type BoardStore = BoardRep Id ()

toBoard :: forall row. BoardRep List row -> Board
toBoard brd =
  { id: brd.id
  , name: brd.name
  , bgColour: brd.bgColour
  , lists: brd.lists }

toBoardStore :: forall row. BoardRep List row -> BoardStore
toBoardStore brd =
  { id: brd.id
  , name: brd.name
  , bgColour: brd.bgColour
  ,lists: map _.id brd.lists }

addListToBoard :: forall list row. list -> BoardRep list row -> BoardRep list row
addListToBoard list brd = brd { lists = brd.lists `snoc` list }
