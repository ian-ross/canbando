module Server.DB
  ( getBoards
  , getBoardNoDetail, getBoardListDetail, getBoard, getBoardInfo
  , getListNoDetail, getList, getListInfo
  , getCard, getCardInfo
  , deleteEntity
  ) where

import Prelude

import Canbando.Model.Board (BoardInfo, BoardListDetail, BoardNoDetail, Board)
import Canbando.Model.Card (Card, CardInfo)
import Canbando.Model.Id (Id)
import Canbando.Model.Labels (LabelInfo)
import Canbando.Model.List (List, ListInfo, ListNoDetail)
import Control.Monad.Reader.Trans (class MonadAsk)
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Effect.Aff.Class (class MonadAff)
import MySQL.Connection (query, query_)
import MySQL.QueryValue (toQueryValue)
import Server.Env (Env, db)


type DBFun a = forall m. MonadAff m => MonadAsk Env m => m a
type DBFun1 p a = forall m. MonadAff m => MonadAsk Env m => p -> m a
type DBFun2 p q a = forall m. MonadAff m => MonadAsk Env m => p -> q -> m a

getBoards :: DBFun (Array BoardInfo)
getBoards =
  db $ query_
    "SELECT id, name, bg_colour as bgColour FROM boards"

getBoardInfo :: DBFun1 Id (Array BoardInfo)
getBoardInfo boardId =
    db $ query "SELECT id, name, bg_colour as bgColour FROM boards WHERE id = ?"
      [toQueryValue boardId]

getLabelInfo :: DBFun1 Id (Array LabelInfo)
getLabelInfo boardId =
  db $ query
    "SELECT id, name, colour FROM board_labels WHERE board_id = ? ORDER BY name"
      [toQueryValue boardId]

getBoardNoDetail :: DBFun1 Id (Maybe BoardNoDetail)
getBoardNoDetail boardId = getBoardInfo boardId >>= case _ of
  [bi] -> do
    labels <- getLabelInfo boardId
    lists :: Array { id :: String } <- db $ query
      "SELECT id FROM lists WHERE board_id = ? ORDER BY idx"
        [toQueryValue boardId]
    pure $ Just { id: bi.id, name: bi.name, bgColour: bi.bgColour
                , labels, lists }
  _ -> pure Nothing

getBoardListDetail :: DBFun1 Id (Maybe BoardListDetail)
getBoardListDetail boardId = getBoardInfo boardId >>= case _ of
  [bi] -> do
    labels <- getLabelInfo boardId
    listIds :: Array { id :: String } <- db $ query
      "SELECT id FROM lists WHERE board_id = ? ORDER BY idx"
        [toQueryValue boardId]
    lists <- catMaybes <$> for (map _.id listIds) getListNoDetail
    pure $ Just { id: bi.id, name: bi.name, bgColour: bi.bgColour
                , labels, lists }
  _ -> pure Nothing

getBoard :: DBFun1 Id (Maybe Board)
getBoard boardId = getBoardInfo boardId >>= case _ of
  [bi] -> do
    labels <- getLabelInfo boardId
    listIds :: Array { id :: String } <- db $ query
      "SELECT id FROM lists WHERE board_id = ? ORDER BY idx"
        [toQueryValue boardId]
    lists <- catMaybes <$> for (map _.id listIds) getList
    pure $ Just { id: bi.id, name: bi.name, bgColour: bi.bgColour
                , labels, lists }
  _ -> pure Nothing

getListInfo :: DBFun1 Id (Array ListInfo)
getListInfo listId =
    db $ query "SELECT id, name FROM lists WHERE id = ?"
      [toQueryValue listId]

getListNoDetail :: DBFun1 Id (Maybe ListNoDetail)
getListNoDetail listId = getListInfo listId >>= case _ of
  [li] -> do
    cards :: Array { id :: String } <-
      db $ query "SELECT id FROM cards WHERE list_id = ? ORDER BY idx"
        [toQueryValue listId]
    pure $ Just { id: li.id, name: li.name, cards }
  _ -> pure Nothing

getList :: DBFun1 Id (Maybe List)
getList listId = getListInfo listId >>= case _ of
  [li] -> do
    cardIds :: Array { id :: String } <- db $ query
      "SELECT id FROM cards WHERE list_id = ? ORDER BY idx"
        [toQueryValue listId]
    cards <- catMaybes <$> for (map _.id cardIds) getCard
    pure $ Just { id: li.id, name: li.name, cards }
  _ -> pure Nothing

getCardInfo :: DBFun1 Id (Array CardInfo)
getCardInfo cardId =
    db $ query "SELECT id, title FROM cards WHERE id = ?"
      [toQueryValue cardId]

getCard :: DBFun1 Id (Maybe Card)
getCard cardId = getCardInfo cardId >>= case _ of
  [ci] -> do
    let qid = [toQueryValue cardId]
    labels :: Array { id :: String } <-
      db $ query "SELECT label_id AS id FROM card_labels WHERE card_id = ?" qid
    checklist :: Array { name :: String, done :: Int } <- db $ query
        "SELECT name, done FROM checklist_items WHERE card_id = ? ORDER BY idx" qid
    let fixChecklist { name, done } = { name, done: done /= 0 }
    pure $ Just { id: ci.id, title: ci.title
                , labels: map _.id labels, checklist: map fixChecklist checklist }
  _ -> pure Nothing

deleteEntity :: DBFun2 String Id (Array { id :: String })
deleteEntity table id = do
  let sql = "DELETE FROM " <> table <> " WHERE id = ? RETURNING id"
  db $ query sql [toQueryValue id]
