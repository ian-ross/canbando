module Server.Handler.List
  ( newList, getList, deleteList, moveList, updateList
  ) where

import Prelude

import Canbando.Model.List (listCodec, listCreateInfoCodec, listInfoCodec, listNoDetailCodec)
import HTTPure (notFound, ok)
import MySQL.Connection (execute)
import MySQL.QueryValue (toQueryValue)
import Server.DB (getBoardInfo)
import Server.DB as DB
import Server.Env (ResponseM, db)
import Server.Handler (deleteEntity, genId, jsonQuery, okJson, withJson)
import Server.Util (Detail(..))


newList :: String -> String -> ResponseM
newList boardId body = withJson body listCreateInfoCodec \info ->
  getBoardInfo boardId >>= case _ of
    [_] -> do
      newId <- genId "L"
      db $ execute ("INSERT INTO lists SELECT ?, ?, ?, MAX(idx)+1 " <>
                    "FROM lists WHERE board_id = ?")
        [toQueryValue newId, toQueryValue boardId,
         toQueryValue info.name, toQueryValue boardId]
      let list = { id: newId, name: info.name }
      okJson listInfoCodec list
    _ -> notFound

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
