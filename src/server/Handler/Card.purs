module Server.Handler.Card
  ( newCard, getCard, updateCard, deleteCard, moveCard
  ) where

import Prelude

import HTTPure (ok)
import Server.Env (ResponseM)


newCard :: String -> String -> ResponseM
newCard listId body =
  ok $ "NEW CARD FOR " <> listId

getCard :: String -> ResponseM
getCard cardId =
  ok $ "GET CARD " <> cardId

updateCard :: String -> String -> ResponseM
updateCard cardId body =
  ok $ "UPDATE CARD " <> cardId

deleteCard :: String -> ResponseM
deleteCard cardId =
  ok $ "DELETE CARD " <> cardId

moveCard :: String -> String -> ResponseM
moveCard cardId body =
  ok $ "MOVE CARD " <> cardId
