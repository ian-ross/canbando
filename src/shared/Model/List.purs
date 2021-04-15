module Canbando.Model.List
       ( ListCreateInfo, ListInfo, List
       , ListCreateInfoRep, ListInfoRep, FullListRep, ListCardsRep
       , ListRep, ListStore, ListNoDetail, CardId
       , encode, toList, toListStore
       , listCreateInfoCodec, listInfoCodec
       , listNoDetailCodec, listCodec, listUpdateInfoCodec
       ) where

import Prelude

import Canbando.Model.Card (Card, cardCodec)
import Canbando.Model.Id (Id)
import Data.Argonaut (Json, encodeJson)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Type.Row (type (+))


-- Basic list information needed for list creation.
type ListCreateInfoRep row =
  ( name :: String
  | row )

-- Basic list information.
type ListInfoRep row =
  ListCreateInfoRep + (id :: Id | row)

-- Cards in board.
type ListCardsRep cardrep row =
  ( cards :: Array cardrep
  | row )

-- List with cards and possible extra data.
type FullListRep cardrep row =
  { | ListInfoRep + ListCardsRep cardrep row }

-- List with cards and possible extra data.
type ListRep row = FullListRep Card row


-- Concrete basic information needed for list creation.
type ListCreateInfo = { | ListCreateInfoRep () }

-- Concrete basic information needed for list updates.
type ListUpdateInfo = { | ListCreateInfoRep () }

-- Concrete basic list information.
type ListInfo = { | ListInfoRep () }

-- List representation with only card IDs.
type CardId = { id :: Id }
type ListNoDetail = FullListRep CardId ()

-- Concrete full list with cards.
type List = ListRep ()

type ListStore = FullListRep Id ()


toList :: forall row. FullListRep Card row -> List
toList lst = { id: lst.id, name: lst.name, cards: lst.cards }

toListStore :: forall row. FullListRep Card row -> ListStore
toListStore lst = { id: lst.id, name: lst.name, cards: map _.id lst.cards }

encode :: List -> Json
encode { id, name, cards } = encodeJson { id, name, cards: map _.id cards }


listCreateInfoCodec :: JsonCodec ListCreateInfo
listCreateInfoCodec =
  CAR.object "ListCreateInfo"
    { name: CA.string
    }

listUpdateInfoCodec :: JsonCodec ListUpdateInfo
listUpdateInfoCodec =
  CAR.object "ListUpdateInfo"
    { name: CA.string
    }

listInfoCodec :: JsonCodec ListInfo
listInfoCodec =
  CAR.object "ListInfo"
    { id: CA.string
    , name: CA.string
    }

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

listCodec :: JsonCodec List
listCodec =
  CAR.object "List"
    { id: CA.string
    , name: CA.string
    , cards: CA.array cardCodec
    }
