module Canbando.Component.LabelEdit
  ( Input(..), Action(..), Output(..), Slot, component
  ) where

import Prelude hiding (div)

import Canbando.CSS as CSS
import Canbando.Util (blurElement, focusElement)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, get, gets, liftEffect, mkComponent, mkEval, modify_, raise)
import Halogen as H
import Halogen.HTML (button, div, input, text)
import Halogen.HTML.Events (onBlur, onClick, onKeyDown, onKeyUp, onValueChange)
import Halogen.HTML.Properties (ButtonType(..), InputType(..), classes, id, tabIndex, type_, value)
import Web.Event.Event (stopPropagation)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key, toEvent)


type Input =
  { id :: Int
  , name :: String
  , colour :: String }

type State =
  { id :: Int
  , name :: String
  , colour :: String
  , edit :: String
  , editing :: Boolean
  }

data Action
  = DoNothing
  | StartEditing
  | EditedName String
  | AcceptNameEdit
  | RejectNameEdit (Maybe KeyboardEvent)
  | UpdateColour String
  | Delete

data Output
  = Updated { name :: String, colour :: String }
  | Deleted

type Slot id = forall query. H.Slot query Output id


component ::
  forall query m.
  MonadAff m =>
  Component query Input Output m
component =
  mkComponent
  { initialState: initialState
  , render
  , eval : mkEval $ defaultEval { handleAction = handleAction }
  }

initialState :: Input -> State
initialState { id, name, colour } =
  { id, name, colour, edit: name, editing: false }

catchEnter :: KeyboardEvent -> Action
catchEnter ev =
  case key ev of
    "Enter" -> AcceptNameEdit
    _ -> DoNothing

catchEscape :: KeyboardEvent -> Action
catchEscape ev =
  case key ev of
    "Escape" -> RejectNameEdit (Just ev)
    _ -> DoNothing

render :: forall m. State -> ComponentHTML Action () m
render state =
  div [ classes [CSS.dFlex, CSS.mb3] ]
  [ input [ type_ InputColor, id labelId
          , classes [CSS.formControl, CSS.formControlColor]
          , value state.colour
          , onValueChange UpdateColour ]
  , div [classes [CSS.mt2, CSS.ms2]]
    [ input [ id (labelId <> "_in")
            , classes $ if state.editing then [] else [CSS.hidden]
            , onValueChange EditedName
            , onKeyUp catchEnter
            , onKeyDown catchEscape
            , onBlur $ \_ -> if state.editing then AcceptNameEdit else DoNothing
            , value state.edit ]
    , div [ classes $ if state.editing then [CSS.hidden] else []
          , tabIndex 0
             -- , onFocus (const StartEditing),
          , onClick (const StartEditing)] [text state.name] ]
  , button [ type_ ButtonButton
           , classes [CSS.btnClose, CSS.msAuto, CSS.mt2]
           , onClick (const Delete) ] []
  ]
  where labelId = "label_" <> show state.id

inputId :: forall cs m. HalogenM State Action cs Output m String
inputId =
  gets _.id <#> \id -> "label_" <> show id <> "_in"

focus :: forall cs m. MonadAff m => HalogenM State Action cs Output m Unit
focus = liftEffect <<< focusElement =<< inputId

blur :: forall cs m. MonadAff m => HalogenM State Action cs Output m Unit
blur = liftEffect <<< blurElement =<< inputId

handleAction ::
  forall cs m.
  MonadAff m =>
  Action -> HalogenM State Action cs Output m Unit
handleAction action = case action of
  DoNothing -> pure unit

  StartEditing -> do
    modify_ \s -> s { edit = s.name, editing = true }
    focus

  EditedName name -> modify_ _ { edit = name }

  AcceptNameEdit -> do
    modify_ \s -> s { name = s.edit, editing = false }
    s <- get
    blur
    raise $ Updated { name: s.name, colour: s.colour }

  RejectNameEdit mev -> do
    for_ mev \ev ->
      whenM (gets _.editing) $ liftEffect $ stopPropagation (toEvent ev)
    blur
    modify_ _ { editing = false }

  UpdateColour colour -> do
    modify_ _ { colour = colour }
    s <- get
    raise $ Updated { name: s.name, colour: s.colour }

  Delete -> raise Deleted
