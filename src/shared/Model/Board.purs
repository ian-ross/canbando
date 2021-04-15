module Canbando.Model.Board
  ( BoardInfo, BoardCreateInfo
  , BoardNoDetail, BoardListDetail, Board, BoardStore
  , BoardCreateInfoRep, BoardInfoRep, LabelsRep, BoardListsRep
  , FullBoardRep, BoardRep, ListId
  , toBoard, toBoardStore
  , addListToBoard
  , boardInfoCodec, boardCreateInfoCodec
  , boardNoDetailCodec, boardListDetailCodec, boardCodec
  , boardUpdateInfoCodec
  ) where

import Prelude

import Canbando.Model.Id (Id)
import Canbando.Model.Labels (LabelInfo, labelInfoCodec)
import Canbando.Model.List (List, ListNoDetail, listCodec, listNoDetailCodec)
import Data.Array (snoc)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Type.Row (type (+))


-- Basic board information needed for board creation.
type BoardCreateInfoRep row =
  ( name :: String
  , bgColour :: String
  | row )

-- Basic board information.
type BoardInfoRep row =
  BoardCreateInfoRep + (id :: Id | row)

-- Board labels.
type LabelsRep label row =
  ( labels :: Array label
  | row )

-- Lists in board.
type BoardListsRep listrep row =
  ( lists :: Array listrep
  | row )

-- Board with labels and lists.
type FullBoardRep label listrep row =
  { | BoardInfoRep + LabelsRep label + BoardListsRep listrep row }

-- Board with labels and lists and possible extra data.
type BoardRep row = FullBoardRep LabelInfo List row


-- Concrete basic information needed for board creation.
type BoardCreateInfo = { | BoardCreateInfoRep () }

-- Concrete basic information needed for board updates.
type BoardUpdateInfo = { | BoardCreateInfoRep () }


-- Concrete basic board information.
type BoardInfo = { | BoardInfoRep () }

-- Concrete full board with labels and lists.
type Board = BoardRep ()

-- Board representation with only list IDs.
type ListId = { id :: Id }
type BoardNoDetail = FullBoardRep LabelInfo ListId ()

-- Board representation with list data, but cards as IDs only.
type BoardListDetail = FullBoardRep LabelInfo ListNoDetail ()

-- Concrete full board with labels and lists as IDs only.
type BoardStore = FullBoardRep Id Id ()


toBoard :: forall row. FullBoardRep LabelInfo List row -> Board
toBoard brd =
  { id: brd.id
  , name: brd.name
  , bgColour: brd.bgColour
  , labels: brd.labels
  , lists: brd.lists }

toBoardStore :: forall row. BoardRep row -> BoardStore
toBoardStore brd =
  { id: brd.id
  , name: brd.name
  , bgColour: brd.bgColour
  , labels: map _.id brd.labels
  , lists: map _.id brd.lists }

addListToBoard ::
  forall label list row. list -> FullBoardRep label list row -> FullBoardRep label list row
addListToBoard list brd = brd { lists = brd.lists `snoc` list }

boardCreateInfoCodec :: JsonCodec BoardCreateInfo
boardCreateInfoCodec =
  CAR.object "BoardCreateInfo"
    { name: CA.string
    , bgColour: CA.string
    }

boardInfoCodec :: JsonCodec BoardInfo
boardInfoCodec =
  CAR.object "BoardInfo"
    { id: CA.string
    , name: CA.string
    , bgColour: CA.string
    }

listIdCodec :: JsonCodec ListId
listIdCodec =
  CAR.object "ListId" { id: CA.string }

boardNoDetailCodec :: JsonCodec BoardNoDetail
boardNoDetailCodec =
  CAR.object "BoardNoDetail"
    { id: CA.string
    , name: CA.string
    , bgColour: CA.string
    , labels: CA.array labelInfoCodec
    , lists: CA.array listIdCodec
    }

boardListDetailCodec :: JsonCodec BoardListDetail
boardListDetailCodec =
  CAR.object "BoardListDetail"
    { id: CA.string
    , name: CA.string
    , bgColour: CA.string
    , labels: CA.array labelInfoCodec
    , lists: CA.array listNoDetailCodec
    }

boardCodec :: JsonCodec Board
boardCodec =
  CAR.object "Board"
    { id: CA.string
    , name: CA.string
    , bgColour: CA.string
    , labels: CA.array labelInfoCodec
    , lists: CA.array listCodec
    }

boardUpdateInfoCodec :: JsonCodec BoardUpdateInfo
boardUpdateInfoCodec =
  CAR.object "BoardUpdateInfo"
    { name: CA.string
    , bgColour: CA.string
    }
