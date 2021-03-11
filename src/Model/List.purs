module Canbando.Model.List
       ( Id, List, ListRep
       , encode, toList
       , newList
       ) where

import Prelude

import Canbando.Capability.IdSupply (class IdSupply, genId)
import Canbando.Model.Card (Card)
import Canbando.Model.Card as Card
import Data.Argonaut (Json, encodeJson)


type Id = String

type ListRep cardrep row =
  ( id :: Id
  , name :: String
  , cards :: Array cardrep
  | row )

type List = { | ListRep Card () }

type ListStore = { | ListRep Card.Id () }

toList :: forall row. { | ListRep Card row } -> List
toList lst = { id: lst.id, name: lst.name, cards: lst.cards }

encode :: List -> Json
encode { id, name, cards } = encodeJson { id, name, cards: map _.id cards }

newList :: forall m. IdSupply m => m List
newList = do
  newId <- genId 'L'
  pure { id: newId, name: "New list", cards: [] }
