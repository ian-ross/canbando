module Canbando.Component.List (
  Output(..), Direction(..), Query(..), Slot,
  component
) where

import Prelude hiding (div)

import Canbando.CSS as CSS
import Canbando.Capability.Resource.List (class ManageList, addCard, deleteCard, moveCard, updateCard, updateList)
import Canbando.Component.Card (Output(..), Slot, component) as Card
import Canbando.Component.Card (cardDragData)
import Canbando.Component.Editable (EditAction, editableWith)
import Canbando.Component.Editable as Editable
import Canbando.Component.Icon (icon, iconButton)
import Canbando.Model.Card (Card)
import Canbando.Model.Id (Id)
import Canbando.Model.List (List, ListRep, toList)
import Data.Array (deleteAt, filter, findIndex, insertAt)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Class (class MonadEffect)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, get, gets, lift, liftEffect, mkComponent, mkEval, modify, modify_, raise)
import Halogen as H
import Halogen.HTML (button, div, h1, slot, text)
import Halogen.HTML.Events (onClick, onDragEnter, onDragLeave, onDragOver, onDrop)
import Halogen.HTML.Properties (class_, classes)
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.HTML.Event.DragEvent (DragEvent, toEvent)


type StateInfo =
  ( edit :: String
  , editing :: Boolean
  , lastMoveLocal :: Boolean )

type State = { | ListRep Card StateInfo }

data Output
  = ListDeleted Id
  | ListMoved Direction Id
  | OpenCardModal Card Id

data Direction = Left | Right

data Action
  = AddCard
  | CardAction Card.Output
  | Editing (Maybe EditAction)
  | DragOver DragEvent
  | Drop DragEvent
  | Delete
  | Move Direction

data Query a = DeleteCard Id a

type Slot id = H.Slot Query Output id

type Slots = ( card :: Card.Slot Id )

_card :: Proxy "card"
_card = Proxy


component ::
  forall m.
  MonadEffect m =>
  ManageList m =>
  Component Query List Output m
component = mkComponent
       { initialState
       , render
       , eval : mkEval $ defaultEval { handleAction = handleAction
                                     , handleQuery = handleQuery }
       }


initialState :: List -> State
initialState { name, id, cards } =
  { name: name, id, cards
  , edit: "", editing: false, lastMoveLocal: false }


render ::
  forall m.
  MonadEffect m =>
  State -> ComponentHTML Action Slots m
render s =
  div [classes [ CSS.bgLight, CSS.p3, CSS.rounded, CSS.listWrapper
               , CSS.flexGrow0, CSS.flexShrink0 ]
      , onDragOver DragOver
      , onDragEnter DragOver
      , onDragLeave DragOver
      , onDrop Drop ]
  [div [ class_ CSS.listMenu ]
   [ iconButton "bi-chevron-left" (const $ Move Left)
   , iconButton "bi-chevron-right" (const $ Move Right)
   , text " "
   , iconButton "bi-x-circle" (const Delete)
   ],
   editableWith [CSS.formControlLG] h1 Editing s,
   div [classes [CSS.dFlex, CSS.flexColumn]] cards,
   button [classes [CSS.rounded, CSS.addNewCard], onClick (const AddCard)]
     [ icon "bi-plus-circle", text "Add new card"]
   ]
  where cards = map onecard s.cards
        onecard crd = slot _card crd.id Card.component crd CardAction


handleAction ::
  forall m.
  MonadEffect m =>
  ManageList m =>
  Action -> HalogenM State Action Slots Output m Unit
handleAction action =
  case action of
    AddCard -> do
      (lift <<< addCard =<< gets _.id) >>= case _ of
        Nothing -> pure unit
        Just card -> modify_ \s -> s { cards = s.cards <> [card] }

    Editing Nothing -> pure unit

    Editing (Just editAction) -> do
      Editable.handleAction editAction >>= case _ of
        Just s -> do
          lst <- modify \st -> st { name = s }
          lift $ updateList lst.id (toList lst)
          pure unit
        _ -> pure unit

    Delete -> raise =<< ListDeleted <$> gets _.id

    Move Left -> raise =<< ListMoved Left <$> gets _.id

    Move Right -> raise =<< ListMoved Right <$> gets _.id

    DragOver ev -> liftEffect $ preventDefault (toEvent ev)

    Drop ev -> do
      liftEffect (cardDragData ev) >>= case _ of
        Nothing -> pure unit
        Just from -> do
          st <- get
          case findCard from.id st.cards of
            Nothing -> do
              lift $ moveCard from.id st.id 0
              modify_ \s -> s { cards = insertCard 0 from s.cards }
            Just _ -> modify_ \s -> s { lastMoveLocal = true }

    CardAction (Card.OpenCardModal card) ->
      raise =<< (OpenCardModal card) <$> gets _.id

    CardAction (Card.CardUpdated cardId card) ->
      lift $ updateCard cardId card

    CardAction (Card.CardDeleted cardId) -> do
      st <- get
      if st.lastMoveLocal
      then modify_ \s -> s { lastMoveLocal = false }
      else do
        lift $ deleteCard cardId
        modify_ \s -> s { cards = filter (\c -> c.id /= cardId) s.cards
                        , lastMoveLocal = false }

    CardAction (Card.CardDeletedByMove cardId) -> do
      st <- get
      if st.lastMoveLocal
      then modify_ \s -> s { lastMoveLocal = false }
      else do
        modify_ \s -> s { cards = filter (\c -> c.id /= cardId) s.cards
                        , lastMoveLocal = false }

    CardAction (Card.CardMoved from to) -> do
      st <- get
      case findCard from.id st.cards of
        Nothing -> do
          let toidx = fromMaybe 0 (findCard to st.cards)
          lift $ moveCard from.id st.id toidx
          modify_ \s -> s { cards = insertCard toidx from s.cards }
        Just fromidx -> do
          let cards' = fromMaybe st.cards $ deleteAt fromidx st.cards
          let toidx = fromMaybe 0 (findCard to cards')
          let insidx = if fromidx <= toidx then toidx + 1 else toidx
          lift $ moveCard from.id st.id insidx
          modify_ \s -> s { cards = insertCard insidx from cards', lastMoveLocal = true }

insertCard :: Int -> Card -> Array Card -> Array Card
insertCard idx card cards = fromMaybe cards $ insertAt idx card cards

findCard :: Id -> Array Card -> Maybe Int
findCard id cards = findIndex (\c -> c.id == id) cards


handleQuery ::
  forall m a.
  MonadEffect m =>
  ManageList m =>
  Query a -> HalogenM State Action Slots Output m (Maybe a)
handleQuery (DeleteCard cardId a) = do
  lift $ deleteCard cardId
  modify_ \s -> s { cards = filter (\c -> c.id /= cardId) s.cards }
  pure $ Just a
