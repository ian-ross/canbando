module Canbando.Model.Card
       ( Card, CardInfo, CardStore, CheckListItem
       , CardInfoRep, CardRep
       , encode, decode, toCard, toCardStore
       , cardCodec
       ) where

import Canbando.Model.Id (Id)
import Data.Argonaut (Json, JsonDecodeError, decodeJson, encodeJson)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either)
import Type.Row (type (+))


type CheckListItem = { name :: String, done :: Boolean }


type CardInfoRep row =
  ( id :: Id
  , title :: String
  | row )

type CardRep row =
  CardInfoRep + ( labels :: Array Id, checklist :: Array CheckListItem | row )

type CardInfo = { | CardInfoRep () }

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

checkListItemCodec :: JsonCodec CheckListItem
checkListItemCodec =
  CAR.object "CheckListItem"
    { name: CA.string
    , done: CA.boolean
    }

cardCodec :: JsonCodec Card
cardCodec =
  CAR.object "Card"
    { id: CA.string
    , title: CA.string
    , labels: CA.array CA.string
    , checklist: CA.array checkListItemCodec
    }
