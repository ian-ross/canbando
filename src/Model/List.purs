module Canbando.Model.List
       ( List, ListRep, ListStore
       , encode, toList, toListStore
       ) where

import Prelude

import Canbando.Model.Card (Card)
import Canbando.Model.Id (Id)
import Data.Argonaut (Json, encodeJson)


type ListRep cardrep row =
  ( id :: Id
  , name :: String
  , cards :: Array cardrep
  | row )

type List = { | ListRep Card () }

type ListStore = { | ListRep Id () }

toList :: forall row. { | ListRep Card row } -> List
toList lst = { id: lst.id, name: lst.name, cards: lst.cards }

toListStore :: forall row. { | ListRep Card row } -> ListStore
toListStore lst = { id: lst.id, name: lst.name, cards: map _.id lst.cards }

encode :: List -> Json
encode { id, name, cards } = encodeJson { id, name, cards: map _.id cards }
