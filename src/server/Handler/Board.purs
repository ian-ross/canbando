module Server.Handler.Board
  ( newBoard, listBoards, getBoard, updateBoard, deleteBoard, newLabel
  ) where

import Prelude

import Data.Maybe (Maybe)
import HTTPure (ResponseM, ok)
import Server.Util (Detail)


newBoard :: String -> ResponseM
newBoard body =
  ok "NEW BOARD"

listBoards :: ResponseM
listBoards =
  ok "LIST BOARDS"

getBoard :: String -> Maybe Detail -> ResponseM
getBoard boardId detail =
  ok $ "GET BOARD " <> boardId <> " (detail=" <> show detail <> ")"

updateBoard :: String -> String -> ResponseM
updateBoard boardId body =
  ok $ "UPDATE BOARD " <> boardId

deleteBoard :: String -> ResponseM
deleteBoard boardId =
  ok $ "DELETE BOARD " <> boardId

newLabel :: String -> String -> ResponseM
newLabel boardId body =
  ok $ "NEW LABEL FOR " <> boardId
