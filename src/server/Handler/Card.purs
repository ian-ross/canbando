module Server.Handler.Card
  ( newCard, getCard, updateCard, deleteCard, moveCard
  ) where

import Prelude

import Canbando.Model.Card (cardCodec, cardCreateInfoCodec)
import HTTPure (notFound, ok)
import MySQL.Connection (execute)
import MySQL.QueryValue (toQueryValue)
import Server.DB (getListInfo)
import Server.DB as DB
import Server.Env (ResponseM, db)
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
updateCard cardId body =
  ok $ "UPDATE CARD " <> cardId

deleteCard :: String -> ResponseM
deleteCard = deleteEntity "cards"

moveCard :: String -> String -> ResponseM
moveCard cardId body =
  ok $ "MOVE CARD " <> cardId
