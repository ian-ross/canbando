-- TODO:
--
-- Improve the error handling everywhere. Well, *do* some error
-- handling. Of some sort. There are so many crimes against good
-- practice here, since we're ignoring lots of potential errors,
-- folding return values into "don't care" `Unit` values all over the
-- place, and generally being pretty slapdash about the all "off the
-- happy path" possibilities.
--
-- Since almost all of this stuff so far is concerned with local
-- storage within the browser, it's probably OK to do this, but we'll
-- revisit it with a vengeance when we start using a REST API to talk
-- to a backend on a server. When we do that, the chances of something
-- going wrong for real are much higher and we have to deal with it.
-- (Server down? Database down? Server can't connect to database?
-- Someone pulled the network cable out? Someone pulled out whatever
-- the equivalent of the cable is for wireless? Network goblins?)

module Canbando.Capability.Resource.List where

import Prelude

import Canbando.Capability.IdSupply (class IdSupply, genId)
import Canbando.Capability.Store (class Store, getItem, removeItem, setItem)
import Canbando.Model.Card (Card, CardStore, toCardStore)
import Canbando.Model.Id (Id)
import Canbando.Model.List (List, ListStore, toListStore)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Array (delete, insertAt)
import Data.Maybe (Maybe, fromMaybe)


class Monad m <= ManageList m where
  updateList :: Id {- listId -} -> List -> m Unit
  moveCard :: Id {- cardId -} -> Id {- toListId -} -> Int {- index -} -> m Unit
  addCard :: Id {- listId -} -> m (Maybe Card)
  updateCard :: Id {- cardId -} -> Card -> m Unit
  deleteCard :: Id {- cardId -} -> m Unit


instance manageListM :: (Monad m, IdSupply m, Store m) => ManageList m where
  updateList listId list = setItem listId (toListStore list)

  moveCard cardId toListId idx = const unit <$> runMaybeT do
    card :: CardStore <- MaybeT $ getItem cardId
    oldList :: ListStore <- MaybeT $ getItem card.list
    newList :: ListStore <- MaybeT $ getItem toListId
    lift $ setItem oldList.id $
      oldList { cards = delete cardId oldList.cards }
    lift $ setItem newList.id $
      newList { cards = fromMaybe newList.cards $
                        insertAt idx cardId $
                        delete cardId newList.cards }
    lift $ setItem cardId $ card { list = toListId }

  addCard listId = runMaybeT do
    list <- MaybeT $ getItem listId
    newId <- lift $ genId 'C'
    let card = { id: newId, title: "New card", labels: [] }
    lift $ setItem card.id $ toCardStore card listId
    lift $ setItem listId $ list { cards = list.cards <> [card.id] }
    pure card

  updateCard cardId card = const unit <$> runMaybeT do
    stored :: CardStore <- MaybeT $ getItem cardId
    lift $ setItem cardId $ toCardStore card stored.list

  deleteCard cardId = const unit <$> runMaybeT do
    card :: CardStore <- MaybeT $ getItem cardId
    lift $ removeItem cardId
    list <- MaybeT $ getItem card.list
    lift $ setItem list.id $ list { cards = delete cardId list.cards }
