module Canbando.Model.Card
       ( Card, CardCreateInfo, CardInfo, CardStore, CheckListItem
       , CardCreateInfoRep, CardInfoRep, CardRep
       , encode, decode, toCard, toCardStore
       , cardCreateInfoCodec, cardCodec
       ) where

import Canbando.Model.Id (Id)
import Data.Argonaut (Json, JsonDecodeError, decodeJson, encodeJson)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either)
import Type.Row (type (+))


type CheckListItem = { name :: String, done :: Boolean }


-- Basic card information needed for card creation.
type CardCreateInfoRep row =
  ( title :: String
  | row )

-- Basic card information.
type CardInfoRep row =
  CardCreateInfoRep + ( id :: Id | row )

-- Full card information with possible extra data.
type CardRep row =
  CardInfoRep + ( labels :: Array Id, checklist :: Array CheckListItem | row )

-- Card creation information.
type CardCreateInfo = { | CardCreateInfoRep () }

-- Basic card information.
type CardInfo = { | CardInfoRep () }

-- Full card information.
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

cardCreateInfoCodec :: JsonCodec CardCreateInfo
cardCreateInfoCodec =
  CAR.object "CardCreateInfo"
    { title: CA.string
    }

cardCodec :: JsonCodec Card
cardCodec =
  CAR.object "Card"
    { id: CA.string
    , title: CA.string
    , labels: CA.array CA.string
    , checklist: CA.array checkListItemCodec
    }
