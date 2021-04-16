module Server.Handler.Board
  ( newBoard, listBoards, getBoard, updateBoard, deleteBoard
  ) where

import Prelude

import Canbando.Model.Board
  (boardCodec, boardCreateInfoCodec, boardInfoCodec,
   boardListDetailCodec, boardNoDetailCodec, boardUpdateInfoCodec)
import Data.Maybe (Maybe(..))
import HTTPure (notFound)
import Server.DB.Board
  (addBoard, getBoardInfo, getBoard, getBoardListDetail,
   getBoardNoDetail, getBoards, setBoardInfo) as DB
import Server.Env (ResponseM)
import Server.Handler (deleteEntity, genId, jsonQuery, jsonsQuery, okJson, withJson)
import Server.Util (Detail(..))


newBoard :: String -> ResponseM
newBoard body = withJson body boardCreateInfoCodec \info -> do
  newId <- genId "B"
  DB.addBoard newId info.name info.bgColour
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
  DB.getBoardInfo boardId >>= case _ of
    Nothing -> notFound
    Just bi -> do
      DB.setBoardInfo boardId update.name update.bgColour
      let board = { id: boardId, name: update.name, bgColour: update.bgColour }
      okJson boardInfoCodec board

deleteBoard :: String -> ResponseM
deleteBoard = deleteEntity "boards"
