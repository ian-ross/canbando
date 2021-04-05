module Canbando.Model.Card
       ( Card, CardRep, CardStore, CheckListItem
       , encode, decode, toCard, toCardStore
       ) where

import Canbando.Model.Id (Id)
import Data.Argonaut (Json, JsonDecodeError, decodeJson, encodeJson)
import Data.Either (Either)


type CheckListItem = { name :: String, done :: Boolean }

type CardRep row =
  ( id :: Id
  , title :: String
  , labels :: Array Id
  , checklist :: Array CheckListItem
  | row )

type Card = { | CardRep () }

type CardStore = { | CardRep ( list :: Id ) }

encode :: Card -> Json
encode = encodeJson

decode :: Json -> Either JsonDecodeError Card
decode = decodeJson

toCard :: forall row. { | CardRep row } -> Card
toCard card =
  { id: card.id
  , title: card.title
  , labels: card.labels
  , checklist: card.checklist }

toCardStore :: Card -> Id -> CardStore
toCardStore { id, title, labels, checklist } list =
  { id, title, labels, checklist, list }
