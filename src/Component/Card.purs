module Component.Card (Input, Output, State, Slot, component, newCard) where

import Prelude hiding (div)

import CSS as CSS
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, get, liftEffect, mkComponent, mkEval, modify_, raise)
import Halogen as H
import Halogen.HTML (HTML, a, div, input, text)
import Halogen.HTML.Events (onBlur, onClick, onFocus, onKeyDown, onKeyUp, onValueChange)
import Halogen.HTML.Properties (class_, classes, href, id_, tabIndex, value)
import Util (focusElement)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)


type State = { id :: String, title :: String, edit :: String, editing :: Boolean }

type Input = { id :: String, title :: String }

data Output = TitleChanged String

data Action = StartEditing | Editing EditAction

data EditAction = Edited String | Accept | Reject

type Slot id = forall query. H.Slot query Output id

component :: forall query m. MonadEffect m => Component HTML query Input Output m
component = mkComponent
       { initialState
       , render
       , eval : mkEval $ defaultEval { handleAction = handleAction }
       }

newCard :: String -> String -> Input
newCard id title = { id: id, title: title }

initialState :: Input -> State
initialState inp = { id: inp.id, title: inp.title, edit: "", editing: false }

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

render :: forall cs m. State -> ComponentHTML Action cs m
render c =
  div [id_ c.id, class_ CSS.card]
  [ input [id_ inputID,
           classes inputClasses,
           onValueChange $ Just <<< Editing <<< Edited,
           onKeyUp catchEnter,
           onKeyDown catchEscape,
           onBlur $ \_ -> if c.editing then Just (Editing Accept) else Nothing,
           value c.edit]
  , div [classes divClasses,
         tabIndex 0,
         onFocus $ \_ -> Just StartEditing,
         onClick $ \_ -> Just StartEditing] [text c.title]
  , a [class_ CSS.cardMenu, href "#"] [text "..."]
  ]
  where inputClasses = if c.editing then [] else [CSS.hidden]
        divClasses = if c.editing then [CSS.hidden] else []
        inputID = c.id <> "_in"


handleAction :: forall m. MonadEffect m => Action -> HalogenM State Action () Output m Unit
handleAction action = do
  s <- get
  if s.editing
    then
    case action of
      Editing (Edited edit) ->
        modify_ \state -> state { edit = edit }

      Editing Accept -> do
        modify_ \state -> state { title = s.edit, editing = false }
        raise $ TitleChanged s.edit

      Editing Reject ->
        modify_ \state -> state { editing = false }

      _ -> pure unit
    else
    case action of
      StartEditing -> do
        modify_ \state -> state { edit = s.title, editing = true }
        liftEffect $ focusElement $ s.id <> "_in"

      _ -> pure unit
