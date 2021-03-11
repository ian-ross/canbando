module Canbando.Component.List (
  Output(..), Direction(..), Slot,
  component
) where

import Prelude hiding (div)

import Canbando.CSS as CSS
import Canbando.Capability.IdSupply (class IdSupply)
import Canbando.Capability.Resource.List (class ManageList)
import Canbando.Component.Card (Output(..), Slot, component) as Card
import Canbando.Component.Card (cardDragData)
import Canbando.Component.Editable (EditAction, editableWith)
import Canbando.Component.Editable as Editable
import Canbando.Component.Icon (icon, iconButton)
import Canbando.Model.Card (Card, newCard)
import Canbando.Model.Card (Id) as Card
import Canbando.Model.List (Id, ListRep, List)
import Data.Array (deleteAt, filter, findIndex, insertAt)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Class (class MonadEffect)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, gets, liftEffect, mkComponent, mkEval, modify_, raise)
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

data Direction = Left | Right

data Action
  = AddCard
  | CardAction Card.Output
  | Editing (Maybe EditAction)
  | DragOver DragEvent
  | Drop DragEvent
  | Delete
  | Move Direction

type Slot id = forall query. H.Slot query Output id

type Slots = ( card :: Card.Slot Card.Id )

_card :: Proxy "card"
_card = Proxy


component ::
  forall query m.
  MonadEffect m =>
  ManageList m =>
  IdSupply m =>
  Component query List Output m
component = mkComponent
       { initialState
       , render
       , eval : mkEval $ defaultEval { handleAction = handleAction }
       }


initialState :: List -> State
initialState { name, id, cards } =
  { name: name, id, cards
  , edit: "", editing: false, lastMoveLocal: false }


render :: forall m. MonadEffect m => State -> ComponentHTML Action Slots m
render s =
  div [classes [ CSS.bgLight, CSS.p3, CSS.rounded, CSS.listWrapper
               , CSS.flexGrow0, CSS.flexShrink0 ]
      , onDragOver \e -> DragOver e
      , onDragEnter \e -> DragOver e
      , onDragLeave \e -> DragOver e
      , onDrop \e -> Drop e ]
  [div [ class_ CSS.listMenu ]
   [ iconButton "bi-chevron-left" \_ -> Move Left
   , iconButton "bi-chevron-right" \_ -> Move Right
   , text " "
   , iconButton "bi-x-circle" \_ -> Delete
   ],
   editableWith [CSS.formControlLG] h1 Editing s,
   div [classes [CSS.dFlex, CSS.flexColumn]] cards,
   button [classes [CSS.rounded, CSS.addNewCard], onClick \_ -> AddCard]
     [ icon "bi-plus-circle", text "Add new card"]
   ]
  where cards = map onecard s.cards
        onecard crd = slot _card crd.id Card.component crd CardAction


handleAction ::
  forall m.
  MonadEffect m =>
  ManageList m =>
  IdSupply m =>
  Action -> HalogenM State Action Slots Output m Unit
handleAction action =
  case action of
    AddCard -> do
      card <- newCard
      modify_ \s -> s { cards = s.cards <> [card] }

    Editing Nothing -> pure unit

    Editing (Just editAction) -> do
      Editable.handleAction editAction >>= case _ of
        Just s -> pure unit -- raise $ ListTitleChanged s
        _ -> pure unit

    Delete -> raise =<< ListDeleted <$> gets _.id

    Move Left -> raise =<< ListMoved Left <$> gets _.id

    Move Right -> raise =<< ListMoved Right <$> gets _.id

    DragOver ev -> liftEffect $ preventDefault (toEvent ev)

    Drop ev -> do
      liftEffect (cardDragData ev) >>= case _ of
        Nothing -> pure unit
        Just from ->
          modify_ \s -> case findCard from.id s.cards of
            Nothing -> s { cards = insertCard 0 from s.cards }
            Just _ -> s { lastMoveLocal = true }

    CardAction (Card.CardDeleted id) ->
      modify_ \s -> if s.lastMoveLocal
                    then s { lastMoveLocal = false }
                    else s { cards = filter (\c -> c.id /= id) s.cards
                           , lastMoveLocal = false }

    CardAction (Card.CardMoved from to) ->
      modify_ \s ->
        case findCard from.id s.cards of
          Nothing -> do
            let toidx = fromMaybe 0 (findCard to s.cards)
            s { cards = insertCard toidx from s.cards }
          Just fromidx -> do
            let cards' = deleteCard fromidx s.cards
            let toidx = fromMaybe 0 (findCard to cards')
            let insidx = if fromidx <= toidx then toidx + 1 else toidx
            s { cards = insertCard insidx from cards', lastMoveLocal = true }


insertCard :: Int -> Card -> Array Card -> Array Card
insertCard idx card cards = fromMaybe cards $ insertAt idx card cards

deleteCard :: Int -> Array Card -> Array Card
deleteCard idx cards = fromMaybe cards $ deleteAt idx cards

findCard :: Card.Id -> Array Card -> Maybe Int
findCard id cards = findIndex (\c -> c.id == id) cards
