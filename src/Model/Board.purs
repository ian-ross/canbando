module Canbando.Model.Board
  ( BoardInfo, BoardLists, BoardRep, Board, BoardStore
  , toBoard, toBoardStore
  , addListToBoard
  ) where

import Prelude

import Canbando.Model.Id (Id)
import Canbando.Model.Labels (LabelInfo)
import Canbando.Model.List (List)
import Data.Array (snoc)
import Type.Row (type (+))


type BoardInfo label row =
  ( id :: Id
  , name :: String
  , bgColour :: String
  , labels :: Array label
  | row )

type BoardLists listrep row =
  ( lists :: Array listrep
  | row )

type BoardRep label listrep row =
  { | BoardInfo label + BoardLists listrep row }

type Board = BoardRep LabelInfo List ()

type BoardStore = BoardRep Id Id ()

toBoard :: forall row. BoardRep LabelInfo List row -> Board
toBoard brd =
  { id: brd.id
  , name: brd.name
  , bgColour: brd.bgColour
  , labels: brd.labels
  , lists: brd.lists }

toBoardStore :: forall row. BoardRep LabelInfo List row -> BoardStore
toBoardStore brd =
  { id: brd.id
  , name: brd.name
  , bgColour: brd.bgColour
  , labels: map _.id brd.labels
  ,lists: map _.id brd.lists }

addListToBoard ::
  forall label list row. list -> BoardRep label list row -> BoardRep label list row
addListToBoard list brd = brd { lists = brd.lists `snoc` list }
