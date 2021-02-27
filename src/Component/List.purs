module Component.List (Input, Output, State, Slot, component, newList) where

import Prelude hiding (div)

import CSS as CSS
import Component.Card as Card
import Component.Icon (icon)
import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Debug.Trace (traceM)
import Effect.Class (class MonadEffect)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, get, liftEffect, mkComponent, mkEval, modify_, raise)
import Halogen as H
import Halogen.HTML (HTML, button, div, h1, input, slot, text)
import Halogen.HTML.Events (onBlur, onClick, onFocus, onKeyDown, onKeyUp, onValueChange)
import Halogen.HTML.Properties (classes, id_, tabIndex, value)
import Util (focusElement)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)

type State = { name :: String
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

data EditAction = Edited String | Accept | Reject

data Action
  = AddCard
  | CardAction Card.Output
  | StartEditing
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
  { name, id, nextID, cards, edit: "", editing: false }

catchEnter :: KeyboardEvent -> Maybe Action
catchEnter ev =
  case key ev of
    "Enter" -> Just $ Editing Accept
    _ -> Nothing

catchEscape :: KeyboardEvent -> Maybe Action
catchEscape ev =
  case key ev of
    "Escape" -> Just $ Editing Reject
    _ -> Nothing

render :: forall m. MonadEffect m => State -> ComponentHTML Action Slots m
render s =
  div [classes [ CSS.bgLight, CSS.p3, CSS.rounded, CSS.listWrapper
               , CSS.flexGrow0, CSS.flexShrink0 ]]
  [ input [id_ inputID,
           classes $ [CSS.formControlLG] <> inputClasses,
           onValueChange $ Just <<< Editing <<< Edited,
           onKeyUp catchEnter,
           onKeyDown catchEscape,
           onBlur $ \_ -> if s.editing then Just (Editing Accept) else Nothing,
           value s.edit]
  , h1 [classes h1Classes,
        tabIndex 0,
        onFocus $ \_ -> Just StartEditing,
        onClick $ \_ -> Just StartEditing] [text s.name]
  , div [classes [CSS.dFlex, CSS.flexColumn]] cards
  , button [classes [CSS.rounded, CSS.addNewCard], onClick \_ -> Just AddCard]
    [ icon "bi-plus-circle", text "Add new card"]
  ]
  where cards = mapWithIndex onecard s.cards
        onecard idx crd = slot _card idx Card.component crd (Just <<< CardAction)
        inputClasses = if s.editing then [] else [CSS.hidden]
        h1Classes = if s.editing then [CSS.hidden] else []
        inputID = s.id <> "_in"

handleAction :: forall m. MonadEffect m =>
                Action -> HalogenM State Action Slots Output m Unit
handleAction action = do
  s <- get
  case action of
    AddCard ->
      modify_ \state -> state
                        { nextID = s.nextID + 1
                        , cards = s.cards <>
                                  [Card.newCard (s.id <> show s.nextID) "New card"] }
    CardAction o ->
      traceM o

    Editing (Edited edit) ->
      when s.editing $ modify_ \state -> state { edit = edit }

    Editing Accept -> when s.editing do
      modify_ \state -> state { name = s.edit, editing = false }
      raise $ ListTitleChanged s.edit

    Editing Reject ->
      when s.editing $ modify_ \state -> state { editing = false }

    StartEditing -> when (not s.editing) do
      modify_ \state -> state { edit = s.name, editing = true }
      liftEffect $ focusElement $ s.id <> "_in"
