module Canbando.Model.List where

import Prelude

import Canbando.Model.Card (Card)
import Canbando.Capability.IdSupply (class IdSupply, genId)


type Id = String

type ListRep row =
  ( id :: Id
  , name :: String
  , nextID :: Int
  , cards :: Array Card
  | row )

type List = { | ListRep () }

newList :: forall m. IdSupply m => m List
newList = do
  newId <- genId 'L'
  pure { id: newId, name: "New list", nextID: 1, cards: [] }
