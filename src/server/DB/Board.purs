module Server.DB.Board
  ( addBoard
  , getBoards
  , getBoardNoDetail, getBoardListDetail, getBoard, getBoardInfo
  , setBoardInfo
  ) where

import Prelude

import Canbando.Model.Board (BoardInfo, BoardListDetail, BoardNoDetail, Board)
import Canbando.Model.Id (Id)
import Canbando.Model.Labels (LabelInfo)
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for)
import MySQL.Connection (execute, query, query_)
import MySQL.QueryValue (toQueryValue)
import Server.DB.List (getList, getListNoDetail)
import Server.DB.Util (DBFun, DBFun1, DBFun3, query1)
import Server.Env (db)


addBoard :: DBFun3 Id String String Unit
addBoard newId name bgColour =
  db $ execute "INSERT INTO boards (id, name, bg_colour) VALUES (?, ?, ?)"
    [toQueryValue newId, toQueryValue name, toQueryValue bgColour]

getBoards :: DBFun (Array BoardInfo)
getBoards =
  db $ query_
    "SELECT id, name, bg_colour as bgColour FROM boards"

getBoardInfo :: DBFun1 Id (Maybe BoardInfo)
getBoardInfo boardId =
    db $ query1 "SELECT id, name, bg_colour as bgColour FROM boards WHERE id = ?"
      [toQueryValue boardId]

setBoardInfo :: DBFun3 Id String String Unit
setBoardInfo boardId name bgColour =
  db $ execute "UPDATE boards SET name = ?, bg_colour = ? WHERE id = ?"
    [toQueryValue name, toQueryValue bgColour, toQueryValue boardId]

getLabelInfo :: DBFun1 Id (Array LabelInfo)
getLabelInfo boardId =
  db $ query
    "SELECT id, name, colour FROM board_labels WHERE board_id = ? ORDER BY name"
      [toQueryValue boardId]

getBoardNoDetail :: DBFun1 Id (Maybe BoardNoDetail)
getBoardNoDetail boardId = getBoardInfo boardId >>= maybe (pure Nothing) ok
  where
    ok bi = do
      labels <- getLabelInfo boardId
      lists :: Array { id :: String } <- db $ query
        "SELECT id FROM lists WHERE board_id = ? ORDER BY idx"
          [toQueryValue boardId]
      pure $ Just { id: bi.id, name: bi.name, bgColour: bi.bgColour
                  , labels, lists }

getBoardListDetail :: DBFun1 Id (Maybe BoardListDetail)
getBoardListDetail boardId = getBoardInfo boardId >>= maybe (pure Nothing) ok
  where
    ok bi = do
      labels <- getLabelInfo boardId
      listIds :: Array { id :: String } <- db $ query
        "SELECT id FROM lists WHERE board_id = ? ORDER BY idx"
          [toQueryValue boardId]
      lists <- catMaybes <$> for (map _.id listIds) getListNoDetail
      pure $ Just { id: bi.id, name: bi.name, bgColour: bi.bgColour
                  , labels, lists }

getBoard :: DBFun1 Id (Maybe Board)
getBoard boardId = getBoardInfo boardId >>= maybe (pure Nothing) ok
  where
    ok bi = do
      labels <- getLabelInfo boardId
      listIds :: Array { id :: String } <- db $ query
        "SELECT id FROM lists WHERE board_id = ? ORDER BY idx"
          [toQueryValue boardId]
      lists <- catMaybes <$> for (map _.id listIds) getList
      pure $ Just { id: bi.id, name: bi.name, bgColour: bi.bgColour
                  , labels, lists }
