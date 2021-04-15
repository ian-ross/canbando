module Server.Handler.Board
  ( newBoard, listBoards, getBoard, updateBoard, deleteBoard, newLabel
  ) where

import Prelude

import Canbando.Model.Board (boardCodec, boardCreateInfoCodec, boardInfoCodec, boardListDetailCodec, boardNoDetailCodec, boardUpdateInfoCodec)
import HTTPure (notFound, ok)
import MySQL.Connection (execute)
import MySQL.QueryValue (toQueryValue)
import Server.DB (getBoardInfo)
import Server.DB as DB
import Server.Env (ResponseM, db)
import Server.Handler (deleteEntity, genId, jsonQuery, jsonsQuery, okJson, withJson)
import Server.Util (Detail(..))


newBoard :: String -> ResponseM
newBoard body = withJson body boardCreateInfoCodec \info -> do
  newId <- genId "B"
  db $ execute "INSERT INTO boards (id, name, bg_colour) VALUES (?, ?, ?)"
    [toQueryValue newId, toQueryValue info.name, toQueryValue info.bgColour]
  let board = { id: newId, name: info.name, bgColour: info.bgColour }
  okJson boardInfoCodec board

listBoards :: ResponseM
listBoards = jsonsQuery DB.getBoards boardInfoCodec

getBoard :: String -> Detail -> ResponseM
getBoard boardId detail =
  case detail of
    All   -> jsonQuery (DB.getBoard boardId) boardCodec
    Lists -> jsonQuery (DB.getBoardListDetail boardId) boardListDetailCodec
    None  -> jsonQuery (DB.getBoardNoDetail boardId) boardNoDetailCodec

updateBoard :: String -> String -> ResponseM
updateBoard boardId body = withJson body boardUpdateInfoCodec \update ->
  getBoardInfo boardId >>= case _ of
    [_] -> do
      db $ execute "UPDATE boards SET name = ?, bg_colour = ? WHERE id = ?"
        [toQueryValue update.name, toQueryValue update.bgColour, toQueryValue boardId]
      let board = { id: boardId, name: update.name, bgColour: update.bgColour }
      okJson boardInfoCodec board
    _ -> notFound

deleteBoard :: String -> ResponseM
deleteBoard = deleteEntity "boards"

newLabel :: String -> String -> ResponseM
newLabel boardId body =
  ok $ "NEW LABEL FOR " <> boardId
