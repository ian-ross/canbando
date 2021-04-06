module Server.Handler.List
  ( newList, getList, deleteList, moveList, updateList
  ) where

import Prelude

import Data.Maybe (Maybe)
import HTTPure (ResponseM, ok)
import Server.Util (Detail)


newList :: String -> String -> ResponseM
newList boardId body =
  ok $ "NEW LIST FOR " <> boardId

getList :: String -> Maybe Detail -> ResponseM
getList listId detail =
  ok $ "GET LIST " <> listId <> " (detail=" <> show detail <> ")"

deleteList :: String -> ResponseM
deleteList listId =
  ok $ "DELETE LIST " <> listId

moveList :: String -> String -> ResponseM
moveList listId body =
  ok $ "MOVE LIST " <> listId

updateList :: String -> String -> ResponseM
updateList listId body =
  ok $ "UPDATE LIST " <> listId
