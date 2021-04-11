module Server.Handler.Card
  ( newCard, getCard, updateCard, deleteCard, moveCard
  ) where

import Prelude

import Canbando.Model.Card (cardCodec)
import HTTPure (ok)
import Server.DB as DB
import Server.Env (ResponseM)
import Server.Handler (deleteEntity, jsonQuery)


newCard :: String -> String -> ResponseM
newCard listId body =
  ok $ "NEW CARD FOR " <> listId

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
