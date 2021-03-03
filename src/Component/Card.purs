module Component.Card (Id, Input, Output(..), State, Slot, component, newCard) where

import Prelude hiding (div)

import CSS as CSS
import Data.Maybe (Maybe(..))
import Debug.Trace (traceM)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, get, mkComponent, mkEval, modify_, raise)
import Halogen as H
import Halogen.HTML (HTML, a, div, input, text)
import Halogen.HTML.Events (onBlur, onClick, onDragStart, onKeyDown, onKeyUp, onValueChange)
import Halogen.HTML.Properties (class_, classes, draggable, href, id_, value)
import Util (focusElement)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)


type Id = String

type State = { id :: Id
             , value :: String
             , edit :: String
             , editing :: Boolean }

type Input = { id :: Id
             , title :: String }

data Output = TitleChanged Id String
            | CardDeleted Id

data Action = StartEditing | Edited String | Accept | Reject
            | DeleteCard
            | DragStart

type Slot id = forall query. H.Slot query Output id

catchEnter :: KeyboardEvent -> Maybe Action
catchEnter ev =
  if key ev == "Enter" then Just Accept else Nothing

catchEscape :: KeyboardEvent -> Maybe Action
catchEscape ev =
  if key ev == "Escape" then Just Reject else Nothing


component :: forall query m. MonadEffect m => Component HTML query Input Output m
component =
  mkComponent
  { initialState: initialState
  , render
  , eval : mkEval $ defaultEval { handleAction = handleAction }
  }


newCard :: Int -> String -> Input
newCard id title = { id: "C" <> show id, title: title }


initialState :: Input -> State
initialState inp = { id: inp.id, value: inp.title, edit: "", editing: false }


render :: forall cs m. State -> ComponentHTML Action cs m
render s =
  div [ id_ s.id
      , class_ CSS.card
      , draggable (not s.editing)
      , onDragStart \_ -> Just DragStart
      , onClick \_ -> Just StartEditing ]
  [ input [ id_ (s.id <> "_in")
          , classes inputClasses
          , onValueChange (Just <<< Edited)
          , onKeyUp catchEnter
          , onKeyDown catchEscape
          -- KEYBOARD NAVIGATION NOT WORKING YET...
          -- , tabIndex 0
          -- , onFocus \_ -> Just StartEditing
          , onBlur \_ -> if s.editing then Just Accept else Nothing
          , value s.edit ]
  , div [ classes divClasses ] [text s.value]
  , a [ class_ CSS.cardMenu
      , href "#"
      , onClick \_ -> Just DeleteCard ] [text "..."]
  ]
  where inputClasses = if s.editing then [] else [CSS.hidden]
        divClasses = [CSS.cardInner] <> if s.editing then [CSS.hidden] else []


handleAction :: forall m. MonadEffect m => Action -> HalogenM State Action () Output m Unit
handleAction action = do
  s <- get
  case action of
    StartEditing ->
      when (not s.editing) do
        modify_ \state -> state { edit = s.value, editing = true }
        liftEffect $ focusElement $ s.id <> "_in"

    Edited edit ->
      when s.editing do
        modify_ \state -> state { edit = edit }

    Accept ->
      when s.editing do
        modify_ \state -> state { value = s.edit, editing = false }
        raise $ TitleChanged s.id s.edit

    Reject ->
      when s.editing do
        modify_ \state -> state { editing = false }

    DeleteCard -> raise $ CardDeleted s.id

    DragStart ->
      traceM "DRAG-START"
