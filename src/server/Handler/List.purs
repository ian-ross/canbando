module Server.Handler.List
  ( newList, getList, deleteList, moveList, updateList
  ) where

import Prelude

import Canbando.Model.List
  (listCodec, listCreateInfoCodec, listInfoCodec,
   listNoDetailCodec, listUpdateInfoCodec)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe(..))
import HTTPure (noContent, notFound)
import Server.DB.Board (getBoardInfo) as DB
import Server.DB.List
  (addList, getList, getListBoard, getListInfo,
   getListNoDetail, setListIndex, setListName) as DB
import Server.Env (ResponseM, tx)
import Server.Handler (deleteEntity, genId, jsonQuery, okJson, withJson)
import Server.Util (Detail(..))


newList :: String -> String -> ResponseM
newList boardId body = withJson body listCreateInfoCodec \info ->
  DB.getBoardInfo boardId >>= case _ of
    Nothing -> notFound
    Just bi -> do
      newId <- genId "L"
      DB.addList boardId newId info.name
      let list = { id: newId, name: info.name }
      okJson listInfoCodec list

getList :: String -> Detail -> ResponseM
getList listId detail =
  case detail of
    All -> jsonQuery (DB.getList listId) listCodec
    _   -> jsonQuery (DB.getListNoDetail listId) listNoDetailCodec

deleteList :: String -> ResponseM
deleteList = deleteEntity "lists"

moveList :: String -> String -> ResponseM
moveList listId body = withJson body listLocationCodec \loc ->
  DB.getListInfo listId >>= case _ of
    Nothing -> notFound
    Just li -> do
      tx $ \conn -> do
        mBoardId <- DB.getListBoard conn listId
        case mBoardId of
          Nothing -> pure unit
          Just boardId -> DB.setListIndex conn listId loc.location
      noContent

type ListLocationUpdate = { location :: Int }

listLocationCodec :: JsonCodec ListLocationUpdate
listLocationCodec =
  CAR.object "ListLocationUpdate"
    { location: CA.int
    }

updateList :: String -> String -> ResponseM
updateList listId body = withJson body listUpdateInfoCodec \update ->
  DB.getListInfo listId >>= case _ of
    Nothing -> notFound
    Just li -> do
      DB.setListName listId update.name
      let list = { id: listId, name: update.name }
      okJson listInfoCodec list
