module Canbando.Model.Card
       ( Id, Card, CardRep
       , encode, decode, toCard
       , newCard
       ) where

import Prelude

import Canbando.Capability.IdSupply (class IdSupply, genId)
import Data.Argonaut (Json, JsonDecodeError, decodeJson, encodeJson)
import Data.Either (Either)


type Id = String

type CardRep row =
  ( id :: Id
  , title :: String
  | row )

type Card = { | CardRep () }

encode :: Card -> Json
encode = encodeJson

decode :: Json -> Either JsonDecodeError Card
decode = decodeJson

toCard :: forall row. { | CardRep row } -> Card
toCard card = { id: card.id, title: card.title }

newCard :: forall m. IdSupply m => m Card
newCard = do
  newId <- genId 'C'
  pure { id: newId, title: "New card" }
