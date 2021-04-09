module Canbando.Model.List
       ( List, ListRep, ListStore, ListNoDetail, CardId
       , encode, toList, toListStore
       , listNoDetailCodec
       ) where

import Prelude

import Canbando.Model.Card (Card)
import Canbando.Model.Id (Id)
import Data.Argonaut (Json, encodeJson)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR


type ListRep cardrep row =
  ( id :: Id
  , name :: String
  , cards :: Array cardrep
  | row )

type List = { | ListRep Card () }

type ListStore = { | ListRep Id () }

type CardId = { id :: String }
type ListNoDetail = { | ListRep CardId () }


toList :: forall row. { | ListRep Card row } -> List
toList lst = { id: lst.id, name: lst.name, cards: lst.cards }

toListStore :: forall row. { | ListRep Card row } -> ListStore
toListStore lst = { id: lst.id, name: lst.name, cards: map _.id lst.cards }

encode :: List -> Json
encode { id, name, cards } = encodeJson { id, name, cards: map _.id cards }


cardIdCodec :: JsonCodec CardId
cardIdCodec =
  CAR.object "CardId" { id: CA.string }

listNoDetailCodec :: JsonCodec ListNoDetail
listNoDetailCodec =
  CAR.object "ListNoDetail"
    { id: CA.string
    , name: CA.string
    , cards: CA.array cardIdCodec
    }
