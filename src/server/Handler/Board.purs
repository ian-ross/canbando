module Server.Handler.Board
  ( newBoard, listBoards, getBoard, updateBoard, deleteBoard, newLabel
  ) where

import Prelude

import Canbando.Model.Board (BoardInfo, boardCreateInfoCodec, boardInfoCodec)
import Control.Monad.Reader.Trans (ReaderT)
import Data.Array (head)
import Data.Codec.Argonaut as CA
import Data.Maybe (Maybe, fromMaybe)
import Data.Monoid (power)
import Data.String (length)
import Effect.Aff (Aff)
import HTTPure (noContent, notFound, ok)
import MySQL.Connection (execute, query, query_)
import MySQL.QueryValue (toQueryValue)
import Server.Env (Env, ResponseM)
import Server.Handler (db, okJson, withJson)
import Server.Util (Detail)


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

getBoard :: String -> Maybe Detail -> ResponseM
getBoard boardId detail =
  ok $ "GET BOARD " <> boardId <> " (detail=" <> show detail <> ")"

updateBoard :: String -> String -> ResponseM
updateBoard boardId body =
  ok $ "UPDATE BOARD " <> boardId

deleteBoard :: String -> ResponseM
deleteBoard boardId = do
  deleted :: Array { id :: String } <-
    db $ query "DELETE FROM boards WHERE id = ? RETURNING id"
      [toQueryValue boardId]
  if deleted == [{ id: boardId }]
  then noContent
  else notFound

newLabel :: String -> String -> ResponseM
newLabel boardId body =
  ok $ "NEW LABEL FOR " <> boardId


genId :: String -> ReaderT Env Aff String
genId prefix = do
  res :: Array { next :: Int } <- db (query_ "SELECT NEXT VALUE FOR idgen AS next")
  let nexts = show $ fromMaybe 0 $ _.next <$> head res
      pad = power "0" (8 - length nexts)
  pure $ prefix <> pad <> nexts
