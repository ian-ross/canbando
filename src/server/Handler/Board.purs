module Server.Handler.Board
  ( newBoard, listBoards, getBoard, updateBoard, deleteBoard, newLabel
  ) where

import Prelude

import Canbando.Model.Board (BoardInfo, boardCreateInfoCodec, boardInfoCodec, boardListDetailCodec, boardNoDetailCodec)
import Canbando.Model.Labels (LabelInfo)
import Data.Codec.Argonaut as CA
import Data.Traversable (for)
import HTTPure (ok)
import MySQL.Connection (execute, query, query_)
import MySQL.QueryValue (toQueryValue)
import Server.Env (ResponseM)
import Server.Handler (db, deleteEntity, genId, okJson, withJson)
import Server.Util (Detail(..), single)


newBoard :: String -> ResponseM
newBoard body = withJson body boardCreateInfoCodec \info -> do
  newId <- genId "B"
  db $ execute "INSERT INTO boards (id, name, bg_colour) VALUES (?, ?, ?)"
    [toQueryValue newId, toQueryValue info.name, toQueryValue info.bgColour]
  let board = { id: newId, name: info.name, bgColour: info.bgColour }
  okJson boardInfoCodec board

listBoards :: ResponseM
listBoards = do
  boards :: Array BoardInfo <- db $ query_
    "SELECT id, name, bg_colour as bgColour FROM boards"
  okJson (CA.array boardInfoCodec) boards

getBoard :: String -> Detail -> ResponseM
getBoard id detail = do
  boards <- db $ query
    "SELECT id, name, bg_colour as bgColour FROM boards WHERE id = ?"
      [toQueryValue id]
  labels :: Array LabelInfo <- db $ query
    "SELECT id, name, colour FROM board_labels WHERE board_id = ? ORDER BY name"
      [toQueryValue id]
  single boards \(bi :: BoardInfo) ->
      case detail of
        None -> do
          lists :: Array { id :: String } <- db $ query
            "SELECT id FROM lists WHERE board_id = ? ORDER BY idx"
              [toQueryValue id]
          let board = { id: bi.id, name: bi.name, bgColour: bi.bgColour
                      , labels, lists }
          okJson boardNoDetailCodec board
        Lists -> do
          ls :: Array { id :: String, name :: String } <- db $ query
            "SELECT id, name FROM lists WHERE board_id = ? ORDER BY idx"
              [toQueryValue id]
          lists <- for ls \l -> do
            cards :: Array { id :: String } <- db $ query
              "SELECT id FROM cards WHERE list_id = ? ORDER BY idx"
                [toQueryValue l.id]
            pure { id: l.id, name: l.name, cards}
          let board = { id: bi.id, name: bi.name, bgColour: bi.bgColour
                      , labels, lists }
          okJson boardListDetailCodec board
        -- ==> TODO: DO THIS AFTER THE LIST RETRIEVAL STUFF IS DONE.
        -- THIS SHOULD ALL BE REFACTORED TO PULL OUT DB ACCESS
        -- FUNCTIONS.
        All -> ok "NYI"

updateBoard :: String -> String -> ResponseM
updateBoard boardId body =
  ok $ "UPDATE BOARD " <> boardId

deleteBoard :: String -> ResponseM
deleteBoard id = deleteEntity "boards" id

newLabel :: String -> String -> ResponseM
newLabel boardId body =
  ok $ "NEW LABEL FOR " <> boardId
