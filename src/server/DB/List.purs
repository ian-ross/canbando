module Server.DB.List
  ( addList
  , getListNoDetail, getList, getListInfo
  , getListBoard, getListOrder, setListName, setListIndex
  ) where

import Prelude

import Canbando.Model.Id (Id)
import Canbando.Model.List (List, ListInfo, ListNoDetail)
import Data.Array (catMaybes, delete, insertAt, mapWithIndex, sortWith)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for, for_)
import Effect.Aff.Class (class MonadAff, liftAff)
import MySQL.Connection (Connection, execute, query)
import MySQL.QueryValue (toQueryValue)
import Server.DB.Card (getCard)
import Server.DB.Util (DBFun1, DBFun2, DBFun3, DBUtil, query1)
import Server.Env (db)


addList :: DBFun3 Id Id String Unit
addList boardId newId name =
  db $ execute ("INSERT INTO lists SELECT ?, ?, ?, MAX(idx)+1 " <>
                "FROM lists WHERE board_id = ?")
    [toQueryValue newId, toQueryValue boardId,
     toQueryValue name, toQueryValue boardId]

getListInfo :: DBFun1 Id (Maybe ListInfo)
getListInfo listId =
    db $ query1 "SELECT id, name FROM lists WHERE id = ?"
      [toQueryValue listId]

getListBoard :: DBUtil (Maybe Id)
getListBoard conn listId = liftAff do
  mid :: Maybe { board_id :: String } <-
    query1 "SELECT board_id FROM lists WHERE id = ?"
      [toQueryValue listId] conn
  pure $ map _.board_id mid

getListOrder :: DBUtil (Array { id :: String, idx :: Int })
getListOrder conn boardId =
  liftAff $ query "SELECT id, idx FROM lists WHERE board_id = ?"
    [toQueryValue boardId] conn

setListIndex :: forall m. MonadAff m =>
                Connection -> Id -> Int -> m Unit
setListIndex conn listId index = do
  rawListData <- getListOrder conn listId
  let newids = rawListData # sortWith _.idx # map _.id
               # delete listId # insertAt index listId
  let mids = mapWithIndex (\idx id -> {id, idx}) <$> newids
  case mids of
    Nothing -> pure unit
    Just ids -> for_ ids \{id, idx} ->
      liftAff $ execute "UPDATE lists SET idx = ? WHERE id = ?"
        [toQueryValue idx, toQueryValue id] conn

getListNoDetail :: DBFun1 Id (Maybe ListNoDetail)
getListNoDetail listId = getListInfo listId >>= maybe (pure Nothing) ok
  where
    ok li = do
      cards :: Array { id :: String } <-
        db $ query "SELECT id FROM cards WHERE list_id = ? ORDER BY idx"
          [toQueryValue listId]
      pure $ Just { id: li.id, name: li.name, cards }

getList :: DBFun1 Id (Maybe List)
getList listId = getListInfo listId >>= maybe (pure Nothing) ok
  where
    ok li = do
      cardIds :: Array { id :: String } <- db $ query
        "SELECT id FROM cards WHERE list_id = ? ORDER BY idx"
          [toQueryValue listId]
      cards <- catMaybes <$> for (map _.id cardIds) getCard
      pure $ Just { id: li.id, name: li.name, cards }

setListName :: DBFun2 Id String Unit
setListName listId name =
  db $ execute "UPDATE lists SET name = ? WHERE id = ?"
  [toQueryValue name, toQueryValue listId]
