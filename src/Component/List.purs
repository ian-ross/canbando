module Component.List (Id, Input, Output, State, Slot, component, newList) where

import Prelude hiding (div)

import CSS as CSS
import Component.Card (getDragData)
import Component.Card as Card
import Component.Editable (EditAction, editableWith)
import Component.Editable as Editable
import Component.Icon (icon)
import Data.Array (deleteAt, filter, findIndex, insertAt)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, liftEffect, mkComponent, mkEval, modify_, raise)
import Halogen as H
import Halogen.HTML (HTML, button, div, h1, slot, text)
import Halogen.HTML.Events (onClick, onDragEnter, onDragLeave, onDragOver, onDrop)
import Halogen.HTML.Properties (classes)
import Web.Event.Event (preventDefault)
import Web.HTML.Event.DragEvent (DragEvent, toEvent)


type Id = String

type State = { value :: String
             , edit :: String
             , editing :: Boolean
             , id :: Id
             , nextID :: Int
             , lastMoveLocal :: Boolean
             , cards :: Array Card.Input }

type Input = { name :: String
             , id :: Id
             , nextID :: Int
             , cards :: Array Card.Input }

data Output = ListTitleChanged String

data Action
  = AddCard
  | CardAction Card.Output
  | Editing EditAction
  | DragOver DragEvent
  | Drop DragEvent

type Slot id = forall query. H.Slot query Output id

type Slots = ( card :: Card.Slot Card.Id )

_card :: SProxy "card"
_card = SProxy


component :: forall query m. MonadEffect m => Component HTML query Input Output m
component = mkComponent
       { initialState
       , render
       , eval : mkEval $ defaultEval { handleAction = handleAction }
       }


newList :: Int -> String -> Input
newList id name =
  { id: "L" <> show id, name: name, nextID: 1, cards: [] }


initialState :: Input -> State
initialState { name, id, nextID, cards } =
  { value: name, id, nextID, cards
  , edit: "", editing: false, lastMoveLocal: false }


render :: forall m. MonadEffect m => State -> ComponentHTML Action Slots m
render s =
  div [classes [ CSS.bgLight, CSS.p3, CSS.rounded, CSS.listWrapper
               , CSS.flexGrow0, CSS.flexShrink0 ]
      , onDragOver \e -> Just (DragOver e)
      , onDragEnter \e -> Just (DragOver e)
      , onDragLeave \e -> Just (DragOver e)
      , onDrop \e -> Just (Drop e) ]
  [editableWith [CSS.formControlLG] h1 Editing s,
   div [classes [CSS.dFlex, CSS.flexColumn]] cards,
   button [classes [CSS.rounded, CSS.addNewCard], onClick \_ -> Just AddCard]
     [ icon "bi-plus-circle", text "Add new card"]
   ]
  where cards = map onecard s.cards
        onecard crd = slot _card crd.id Card.component crd (Just <<< CardAction)


handleAction :: forall m. MonadEffect m =>
                Action -> HalogenM State Action Slots Output m Unit
handleAction action =
  case action of
    AddCard ->
      modify_ \s ->
      s { nextID = s.nextID + 1
        , cards = s.cards <> [Card.newCard s.nextID "New card"] }

    Editing editAction -> do
      Editable.handleAction editAction >>= case _ of
        Just s -> raise $ ListTitleChanged s
        _ -> pure unit

    DragOver ev -> liftEffect $ preventDefault (toEvent ev)

    Drop ev ->
      liftEffect (getDragData ev) >>= case _ of
        Nothing -> pure unit
        Just from -> modify_ \s ->
          case findCard from.id s.cards of
            Nothing -> s { cards = insertCard 0 from s.cards }
            Just fromIdx -> s

    CardAction (Card.CardDeleted id) ->
      modify_ \s -> if s.lastMoveLocal
                    then s { lastMoveLocal = false }
                    else s { cards = filter (\c -> c.id /= id) s.cards
                           , lastMoveLocal = false }

    CardAction (Card.TitleChanged id title) ->
      modify_ \s ->
      let edit c = if c.id == id then c { title = title } else c
      in s { cards = map edit s.cards }

    CardAction (Card.CardMoved from to) -> do
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


insertCard :: Int -> Card.Input -> Array Card.Input -> Array Card.Input
insertCard idx card cards = fromMaybe cards $ insertAt idx card cards

deleteCard :: Int -> Array Card.Input -> Array Card.Input
deleteCard idx cards = fromMaybe cards $ deleteAt idx cards

findCard :: String -> Array Card.Input -> Maybe Int
findCard id cards = findIndex (\c -> c.id == id) cards
