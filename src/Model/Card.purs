module Canbando.Model.Card
       ( Card, CardRep, CardStore
       , encode, decode, toCard, toCardStore
       ) where

import Canbando.Model.Id (Id)
import Data.Argonaut (Json, JsonDecodeError, decodeJson, encodeJson)
import Data.Either (Either)


type CardRep row =
  ( id :: Id
  , title :: String
  | row )

type Card = { | CardRep () }

type CardStore = { | CardRep ( list :: Id ) }

encode :: Card -> Json
encode = encodeJson

decode :: Json -> Either JsonDecodeError Card
decode = decodeJson

toCard :: forall row. { | CardRep row } -> Card
toCard card = { id: card.id, title: card.title }

toCardStore :: Card -> Id -> CardStore
toCardStore { id, title } list = { id, title, list }
