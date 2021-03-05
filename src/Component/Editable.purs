module Component.Editable (EditAction(..), editable, editableWith, handleAction) where

import Prelude hiding (div)

import CSS as CSS
import DOM.HTML.Indexed (Interactive)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen (ClassName, HalogenM, get, liftEffect, modify_)
import Halogen.HTML (Node, HTML, a, div, div_, input, text)
import Halogen.HTML.Events (onBlur, onClick, onKeyDown, onKeyUp, onValueChange)
import Halogen.HTML.Properties (class_, classes, href, id_, tabIndex, value)
import Util (focusElement)
import Web.Event.Event (Event)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)


data EditAction = StartEditing | Edited String | Accept | Reject

type State r = { id :: String, value :: String, edit :: String, editing :: Boolean | r }


catchEnter :: forall action. (EditAction -> action) -> KeyboardEvent -> Maybe action
catchEnter f ev =
  case key ev of
    "Enter" -> Just $ f Accept
    _ -> Nothing

catchEscape :: forall action. (EditAction -> action) -> KeyboardEvent -> Maybe action
catchEscape f ev =
  case key ev of
    "Escape" -> Just $ f Reject
    _ -> Nothing


editable ::
  forall props act r.
  (EditAction -> act) -> State r -> HTML props act
editable f c = editableWith [] div f c

editableWith ::
  forall props act r.
  Array ClassName ->
  (forall i w. Node (Interactive (onScroll :: Event)) w i) ->
  (EditAction -> act) ->
  State r -> HTML props act
editableWith extraInputClasses headElem f c =
  div_
  [ input [id_ inputID,
           classes (extraInputClasses <> inputClasses),
           onValueChange $ Just <<< f <<< Edited,
           onKeyUp (catchEnter f),
           onKeyDown (catchEscape f),
           onBlur $ \_ -> if c.editing then Just (f Accept) else Nothing,
           value c.edit]
  , headElem [classes divClasses,
              tabIndex 0,
              -- onFocus $ \_ -> Just $ f StartEditing,
              onClick $ \_ -> Just $ f StartEditing] [text c.value]
  , a [class_ CSS.cardMenu, href "#"] [text "..."]
  ]
  where inputClasses = if c.editing then [] else [CSS.hidden]
        divClasses = if c.editing then [CSS.hidden] else []
        inputID = c.id <> "_in"


handleAction ::
  forall m r act cs output.
  MonadEffect m =>
  EditAction -> HalogenM (State r) act cs output m (Maybe String)
handleAction action = do
  s <- get
  if s.editing
    then
    case action of
      Edited edit -> do
        modify_ \state -> state { edit = edit }
        pure Nothing

      Accept -> do
        modify_ \state -> state { value = s.edit, editing = false }
        pure $ Just s.edit

      Reject -> do
        modify_ \state -> state { editing = false }
        pure Nothing

      _ -> pure Nothing
    else
    case action of
      StartEditing -> do
        modify_ \state -> state { edit = s.value, editing = true }
        liftEffect $ focusElement $ s.id <> "_in"
        pure Nothing

      _ -> pure Nothing
