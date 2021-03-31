module Canbando.Component.Editable (EditAction(..), editable, editableWith, handleAction) where

import Prelude hiding (div)

import Canbando.CSS as CSS
import Canbando.Util (focusElement)
import DOM.HTML.Indexed (Interactive)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName, HalogenM, get, liftEffect, modify_)
import Halogen.HTML (HTML, Node, div, div_, input, text)
import Halogen.HTML.Events (onBlur, onClick, onKeyDown, onKeyUp, onValueChange)
import Halogen.HTML.Properties (classes, id, tabIndex, value)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)


data EditAction = StartEditing | Edited String | Accept | Reject

type State r = { id :: String, name :: String, edit :: String, editing :: Boolean | r }


catchEnter :: KeyboardEvent -> Maybe EditAction
catchEnter ev =
  case key ev of
    "Enter" -> Just Accept
    _ -> Nothing

catchEscape :: KeyboardEvent -> Maybe EditAction
catchEscape ev =
  case key ev of
    "Escape" -> Just Reject
    _ -> Nothing


editable ::
  forall props act r.
  (Maybe EditAction -> act) -> State r -> HTML props act
editable f c = editableWith [] div f c

editableWith ::
  forall props act r extra.
  Array ClassName ->
  (forall i w. Node (Interactive extra) w i) ->
  (Maybe EditAction -> act) ->
  State r -> HTML props act
editableWith extraInputClasses headElem f c =
  div_
  [ input [id inputID,
           classes (extraInputClasses <> inputClasses),
           onValueChange $ f <<< Just <<< Edited,
           onKeyUp (f <<< catchEnter),
           onKeyDown (f <<< catchEscape),
           onBlur $ \_ -> f $ if c.editing then Just Accept else Nothing,
           value c.edit]
  , headElem [classes divClasses,
              tabIndex 0,
              -- onFocus $ \_ -> Just $ f StartEditing,
              onClick $ \_ -> f (Just StartEditing)] [text c.name]
  ]
  where inputClasses = if c.editing then [] else [CSS.hidden]
        divClasses = if c.editing then [CSS.hidden] else []
        inputID = c.id <> "_in"


handleAction ::
  forall m r act cs output.
  MonadAff m =>
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
        modify_ \state -> state { name = s.edit, editing = false }
        pure $ Just s.edit

      Reject -> do
        modify_ \state -> state { editing = false }
        pure Nothing

      _ -> pure Nothing
    else
    case action of
      StartEditing -> do
        modify_ \state -> state { edit = s.name, editing = true }
        liftEffect $ focusElement $ s.id <> "_in"
        pure Nothing

      _ -> pure Nothing
