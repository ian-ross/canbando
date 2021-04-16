module Server.Handler.Card
  ( newCard, getCard, updateCard, deleteCard, moveCard
  ) where

import Prelude

import Canbando.Model.Card (cardCodec, cardCreateInfoCodec, cardUpdateInfoCodec)
import Canbando.Model.Id (Id)
import Data.Array (sort)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import HTTPure (noContent, notFound)
import Server.DB.Card
  (addCard, clearCardChecklist, clearCardLabels,
   getCard, getCardChecklist, getCardInfo, getCardLabels,
   setCardChecklist, setCardLabels, setCardTitle, moveCard) as DB
import Server.DB.List (getListInfo) as DB
import Server.Env (ResponseM, tx)
import Server.Handler (deleteEntity, genId, jsonQuery, okJson, withJson)


newCard :: String -> String -> ResponseM
newCard listId body = withJson body cardCreateInfoCodec \info ->
  DB.getListInfo listId >>= case _ of
    Nothing -> notFound
    Just li -> do
      newId <- genId "C"
      DB.addCard listId newId info.title
      let card = { id: newId, title: info.title, labels: [], checklist: [] }
      okJson cardCodec card

getCard :: String -> ResponseM
getCard cardId = jsonQuery (DB.getCard cardId) cardCodec

updateCard :: String -> String -> ResponseM
updateCard cardId body = withJson body cardUpdateInfoCodec \update ->
  DB.getCardInfo cardId >>= case _ of
    Nothing -> notFound
    Just ci -> do
      tx \conn -> do
        DB.setCardTitle conn cardId update.title
        labels <- DB.getCardLabels conn cardId
        when (sort (map _.id labels) /= sort update.labels) do
          DB.clearCardLabels conn cardId
          DB.setCardLabels conn cardId update.labels
        checklist <- DB.getCardChecklist conn cardId
        when (checklist /= update.checklist) do
          DB.clearCardChecklist conn cardId
          DB.setCardChecklist conn cardId update.checklist
      let card = { id: cardId, title: update.title
                 , labels: update.labels, checklist: update.checklist }
      okJson cardCodec card

deleteCard :: String -> ResponseM
deleteCard = deleteEntity "cards"

moveCard :: String -> String -> ResponseM
moveCard cardId body = withJson body cardLocationCodec \location -> do
  mci <- DB.getCardInfo cardId
  mli <- DB.getListInfo location.list_id
  case mci /\ mli of
    Just ci /\ Just li -> do
      tx \conn -> do
        DB.moveCard conn cardId li.id location.list_id location.index
      noContent
    _ -> notFound

type CardLocationUpdate = { list_id :: Id, index :: Int }

cardLocationCodec :: JsonCodec CardLocationUpdate
cardLocationCodec =
  CAR.object "CardLocationUpdate"
    { list_id: CA.string
    , index: CA.int
    }
