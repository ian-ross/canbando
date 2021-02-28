module Component.List (Input, Output, State, Slot, component, newList) where

import Prelude hiding (div)

import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Debug.Trace (traceM)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, get, mkComponent, mkEval, modify_, raise)
import Halogen.HTML.Events (onClick)
import Halogen.HTML (HTML, button, div, h1, slot, text)
import Halogen.HTML.Properties (classes)

import Component.Card as Card
import Component.Editable as Editable
import Component.Editable (EditAction, editableWith)
import Component.Icon (icon)
import CSS as CSS


type State = { value :: String
             , edit :: String
             , editing :: Boolean
             , id :: String
             , nextID :: Int
             , cards :: Array Card.Input }

type Input = { name :: String
             , id :: String
             , nextID :: Int
             , cards :: Array Card.Input }

data Output = ListTitleChanged String

data Action
  = AddCard
  | CardAction Card.Output
  | Editing EditAction

type Slot id = forall query. H.Slot query Output id

type Slots = ( card :: Card.Slot Int )

_card :: SProxy "card"
_card = SProxy


component :: forall query m. MonadEffect m => Component HTML query Input Output m
component = mkComponent
       { initialState
       , render
       , eval : mkEval $ defaultEval { handleAction = handleAction }
       }


newList :: String -> String -> Input
newList id name = { id: id, name: name, nextID: 1, cards: [] }


initialState :: Input -> State
initialState { name, id, nextID, cards } =
  { value: name, id, nextID, cards, edit: "", editing: false }


render :: forall m. MonadEffect m => State -> ComponentHTML Action Slots m
render s =
  div [classes [ CSS.bgLight, CSS.p3, CSS.rounded, CSS.listWrapper
               , CSS.flexGrow0, CSS.flexShrink0 ]]
  (editableWith [CSS.formControlLG] h1 Editing s <>
   [ div [classes [CSS.dFlex, CSS.flexColumn]] cards
   , button [classes [CSS.rounded, CSS.addNewCard], onClick \_ -> Just AddCard]
     [ icon "bi-plus-circle", text "Add new card"]
   ])
  where cards = mapWithIndex onecard s.cards
        onecard idx crd = slot _card idx Card.component crd (Just <<< CardAction)


handleAction :: forall m. MonadEffect m =>
                Action -> HalogenM State Action Slots Output m Unit
handleAction action = do
  st <- get
  case action of
    AddCard ->
      modify_ \state -> state
                        { nextID = st.nextID + 1
                        , cards = st.cards <>
                                  [Card.newCard (st.id <> show st.nextID) "New card"] }

    CardAction o -> traceM o

    Editing editAction -> do
      Editable.handleAction editAction >>= case _ of
        Nothing -> pure unit
        Just s -> raise $ ListTitleChanged s
