module Server.Handler.List
  ( newList, getList, deleteList, moveList, updateList
  ) where

import Prelude

import Canbando.Model.List (listCodec, listNoDetailCodec)
import HTTPure (ok)
import Server.DB as DB
import Server.Env (ResponseM)
import Server.Handler (deleteEntity, jsonQuery)
import Server.Util (Detail(..))


newList :: String -> String -> ResponseM
newList boardId body =
  ok $ "NEW LIST FOR " <> boardId

getList :: String -> Detail -> ResponseM
getList listId detail =
  case detail of
    All -> jsonQuery (DB.getList listId) listCodec
    _   -> jsonQuery (DB.getListNoDetail listId) listNoDetailCodec

deleteList :: String -> ResponseM
deleteList = deleteEntity "lists"

moveList :: String -> String -> ResponseM
moveList listId body =
  ok $ "MOVE LIST " <> listId

updateList :: String -> String -> ResponseM
updateList listId body =
  ok $ "UPDATE LIST " <> listId
