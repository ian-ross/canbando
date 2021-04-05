module Canbando.Component.Editable
  ( Output(..), Slot, Action
  , component
  ) where

import Prelude hiding (div)

import Canbando.CSS as CSS
import Canbando.Util (focusElement)
import DOM.HTML.Indexed (Interactive)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName, Component, ComponentHTML, ComponentSlot, HalogenM, defaultEval, gets, liftEffect, mkComponent, mkEval, modify, modify_, put, raise)
import Halogen as H
import Halogen.HTML (Node, div_, input, text)
import Halogen.HTML.Events (onBlur, onClick, onKeyDown, onKeyUp, onValueChange)
import Halogen.HTML.Properties (classes, id, tabIndex, value)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)


data Action m
  = DoNothing
  | Receive (Input m)
  | StartEditing
  | Changed String
  | Accept
  | Reject

type HeadElement m =
  Node (Interactive (onScroll :: Event)) (ComponentSlot () m (Action m)) (Action m)

type Input m =
 { id :: String
 , name :: String
 , extraInputClasses :: Array ClassName
 , headElement :: HeadElement m }

data Output = Edited String

type Slot id = forall query. H.Slot query Output id

type State m =
 { id :: String
 , name :: String
 , edit :: String
 , editing :: Boolean
 , extraInputClasses :: Array ClassName
 , headElement :: HeadElement m }


component ::
  forall query m.
  MonadAff m =>
  Component query (Input m) Output m
component =
  mkComponent
  { initialState
  , render
  , eval: mkEval $ defaultEval { handleAction = handleAction
                               , receive = Just <<< Receive }
  }

initialState :: forall m. Input m -> State m
initialState { id, name, extraInputClasses, headElement } =
  { id, name
  , edit: "", editing: false
  , extraInputClasses
  , headElement: unsafeCoerce headElement }


-- editable ::
--   forall props act r.
--   (Maybe EditAction -> act) -> State r -> HTML props act
-- editable f c = editableWith [] div f c

-- editableWith ::
--   forall props act r extra.
--   Array ClassName ->
--   (forall i w. Node (Interactive extra) w i) ->
--   (Maybe EditAction -> act) ->
--   State r -> HTML props act
-- editableWith extraInputClasses headElem f c =

render :: forall m. State m -> ComponentHTML (Action m) () m
render s =
  div_
  [ input [id inputID,
           classes (s.extraInputClasses <> inputClasses),
           onValueChange Changed,
           onKeyUp catchEnter,
           onKeyDown catchEscape,
           onBlur $ \_ -> if s.editing then Accept else DoNothing,
           value s.edit]
  , s.headElement [ classes divClasses
                  , tabIndex 0
                  -- , onFocus $ \_ -> Just $ f StartEditing
                  , onClick (const StartEditing)] [text s.name]
  ]
  where inputClasses = if s.editing then [] else [CSS.hidden]
        divClasses = if s.editing then [CSS.hidden] else []
        inputID = s.id <> "_in"


handleAction ::
  forall m.
  MonadAff m =>
  Action m -> HalogenM (State m) (Action m) () Output m Unit
handleAction = case _ of
  DoNothing -> pure unit

  Receive input -> put $ initialState input

  Changed edit ->
    whenM (gets _.editing) $ modify_ _ { edit = edit }

  Accept ->
    whenM (gets _.editing) do
      s' <- modify \s -> s { name = s.edit, editing = false }
      raise $ Edited s'.edit

  Reject ->
    whenM (gets _.editing) $ modify_ _ { editing = false }

  StartEditing ->
    whenM (not <$> gets _.editing) do
      s' <- modify \s -> s { edit = s.name, editing = true }
      liftEffect $ focusElement $ s'.id <> "_in"

catchEnter :: forall m. KeyboardEvent -> Action m
catchEnter ev =
  case key ev of
    "Enter" -> Accept
    _ -> DoNothing

catchEscape :: forall m. KeyboardEvent -> Action m
catchEscape ev =
  case key ev of
    "Escape" -> Reject
    _ -> DoNothing
