module Canbando.Model.Card where

import Prelude

import Canbando.Capability.IdSupply (class IdSupply, genId)


type Id = String

type CardRep row =
  ( id :: Id
  , title :: String
  | row )

type Card = { | CardRep () }


newCard :: forall m. IdSupply m => m Card
newCard = do
  newId <- genId 'C'
  pure { id: newId, title: "New card" }
