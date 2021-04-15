module Server.Handler.Card
  ( newCard, getCard, updateCard, deleteCard, moveCard
  ) where

import Prelude

import Canbando.Model.Card (cardCodec, cardCreateInfoCodec, cardUpdateInfoCodec)
import Data.Array (mapWithIndex, sort)
import HTTPure (notFound, ok)
import MySQL.Connection (execute, query)
import MySQL.QueryValue (toQueryValue)
import Server.DB (getCardInfo, getListInfo)
import Server.DB as DB
import Server.Env (ResponseM, db, tx)
import Server.Handler (deleteEntity, genId, jsonQuery, okJson, withJson)


newCard :: String -> String -> ResponseM
newCard listId body = withJson body cardCreateInfoCodec \info ->
  getListInfo listId >>= case _ of
    [_] -> do
      newId <- genId "C"
      db $ execute ("INSERT INTO cards SELECT ?, ?, ?, MAX(idx)+1 " <>
                    "FROM cards WHERE list_id = ?")
        [toQueryValue newId, toQueryValue listId,
         toQueryValue info.title, toQueryValue listId]
      let card = { id: newId, title: info.title, labels: [], checklist: [] }
      okJson cardCodec card
    _ -> notFound

getCard :: String -> ResponseM
getCard cardId = jsonQuery (DB.getCard cardId) cardCodec

updateCard :: String -> String -> ResponseM
updateCard cardId body = withJson body cardUpdateInfoCodec \update ->
  getCardInfo cardId >>= case _ of
    [_] -> do
      tx \conn -> do
        execute "UPDATE cards SET title = ? WHERE id = ?"
          [toQueryValue update.title, toQueryValue cardId] conn
        labels :: Array { label_id :: String } <-
          query "SELECT label_id FROM card_labels WHERE card_id = ?"
            [toQueryValue cardId] conn
        when (sort (map _.label_id labels) /= sort update.labels) do
          execute "DELETE FROM card_labels WHERE card_id = ?"
            [toQueryValue cardId] conn
          execute "INSERT INTO card_labels (card_id, label_id) VALUES ?"
            [toQueryValue $ update.labels <#>
             \lab -> [toQueryValue cardId, toQueryValue lab]] conn
        checklist :: Array { name :: String, done :: Boolean } <-
          query "SELECT name, done FROM checklist_items WHERE card_id = ? ORDER BY idx"
            [toQueryValue cardId] conn
        when (checklist /= update.checklist) do
          execute "DELETE FROM checklist_items WHERE card_id = ?"
            [toQueryValue cardId] conn
          let conv idx check =
                [toQueryValue cardId, toQueryValue idx,
                 toQueryValue check.name, toQueryValue $ if check.done then 1 else 0]
          execute "INSERT INTO checklist_items (card_id, idx, name, done) VALUES ?"
            [toQueryValue $ mapWithIndex conv update.checklist] conn
      let card = { id: cardId, title: update.title
                 , labels: update.labels, checklist: update.checklist }
      okJson cardCodec card
    _ -> notFound

deleteCard :: String -> ResponseM
deleteCard = deleteEntity "cards"

moveCard :: String -> String -> ResponseM
moveCard cardId body =
  ok $ "MOVE CARD " <> cardId
