module Server.DB.Card
  ( addCard
  , getCard, getCardInfo, setCardTitle
  , getCardLabels, clearCardLabels, setCardLabels
  , getCardChecklist, clearCardChecklist, setCardChecklist
  , moveCard
  ) where

import Prelude

import Canbando.Model.Card (Card, CardInfo, CheckListItem)
import Canbando.Model.Id (Id)
import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect.Aff.Class (liftAff)
import MySQL.Connection (execute, query)
import MySQL.QueryValue (toQueryValue)
import Server.DB.Util (DBFun1, DBFun3, DBUtil, DBUtil1, DBUtil3, query1)
import Server.Env (db)


addCard :: DBFun3 Id Id String Unit
addCard listId newId title =
  db $ execute ("INSERT INTO cards SELECT ?, ?, ?, MAX(idx)+1 " <>
                "FROM cards WHERE list_id = ?")
    [toQueryValue newId, toQueryValue listId,
     toQueryValue title, toQueryValue listId]

getCardInfo :: DBFun1 Id (Maybe CardInfo)
getCardInfo cardId =
    db $ query1 "SELECT id, title FROM cards WHERE id = ?"
      [toQueryValue cardId]

getCard :: DBFun1 Id (Maybe Card)
getCard cardId = getCardInfo cardId >>= maybe (pure Nothing) ok
  where
    ok ci = do
      let qid = [toQueryValue cardId]
      labels :: Array { id :: String } <-
        db $ query "SELECT label_id AS id FROM card_labels WHERE card_id = ?" qid
      checklist :: Array { name :: String, done :: Int } <- db $ query
          "SELECT name, done FROM checklist_items WHERE card_id = ? ORDER BY idx" qid
      let fixChecklist { name, done } = { name, done: done /= 0 }
      pure $ Just { id: ci.id, title: ci.title
                  , labels: map _.id labels, checklist: map fixChecklist checklist }

setCardTitle :: DBUtil1 String Unit
setCardTitle conn cardId title =
  liftAff $ execute "UPDATE cards SET title = ? WHERE id = ?"
    [toQueryValue title, toQueryValue cardId] conn

getCardLabels :: DBUtil (Array {id :: Id})
getCardLabels conn cardId =
  liftAff $ query "SELECT label_id FROM card_labels WHERE card_id = ?"
    [toQueryValue cardId] conn

getCardChecklist :: DBUtil (Array CheckListItem)
getCardChecklist conn cardId =
  liftAff $ query "SELECT name, done FROM checklist_items WHERE card_id = ? ORDER BY idx"
    [toQueryValue cardId] conn

clearCardChecklist :: DBUtil Unit
clearCardChecklist conn cardId =
  liftAff $ execute "DELETE FROM checklist_items WHERE card_id = ?"
    [toQueryValue cardId] conn

setCardChecklist :: DBUtil1 (Array CheckListItem) Unit
setCardChecklist conn cardId checklist =
  liftAff $ execute "INSERT INTO checklist_items (card_id, idx, name, done) VALUES ?"
    [toQueryValue $ mapWithIndex conv checklist] conn
    where
      conv idx check =
        [toQueryValue cardId, toQueryValue idx,
         toQueryValue check.name, toQueryValue $ if check.done then 1 else 0]

clearCardLabels :: DBUtil Unit
clearCardLabels conn cardId =
  liftAff $ execute "DELETE FROM card_labels WHERE card_id = ?"
    [toQueryValue cardId] conn

setCardLabels :: DBUtil1 (Array Id) Unit
setCardLabels conn cardId labels =
  liftAff $ execute "INSERT INTO card_labels (card_id, label_id) VALUES ?"
    [toQueryValue $ labels <#> \lab -> [cid, toQueryValue lab]] conn
  where
    cid = toQueryValue cardId

moveCard :: DBUtil3 Id Id Int Unit
moveCard conn cardId fromListId toListId index = liftAff $ do
  mOldIndex :: Maybe { index :: Int } <-
    query1 "SELECT index FROM cards WHERE id = ?" [toQueryValue cardId] conn
  let oldIndex = fromMaybe 0 $ _.index <$> mOldIndex
  execute "UPDATE cards SET list_id = ?, idx = ? WHERE id = ?"
    [toQueryValue toListId, toQueryValue index, toQueryValue cardId] conn
  execute "UPDATE cards SET idx = idx - 1 WHERE list_id = ? AND idx > ?"
   [toQueryValue fromListId, toQueryValue oldIndex] conn
  execute ("UPDATE cards SET idx = idx + 1 " <>
           "WHERE list_id = ? AND idx >= ? AND id <> ?")
    [toQueryValue toListId, toQueryValue index, toQueryValue cardId] conn
