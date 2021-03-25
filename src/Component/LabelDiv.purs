module Canbando.Component.LabelDiv
  ( Output(..), Slot, component
  ) where

import Prelude hiding (div)

import Canbando.CSS as CSS
import Canbando.Component.Editable (EditAction, editableWith)
import Canbando.Component.Editable as Editable
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval)
import Halogen as H
import Halogen.HTML (button, div, input, label)
import Halogen.HTML.Properties (ButtonType(..), InputType(..), classes, id, type_, value)


type InputDataRep row =
  ( id :: String
  , name :: String
  , colour :: String
  | row
  )

type State =
  { | InputDataRep ( edit :: String
                   , editing :: Boolean ) }

type Input = { | InputDataRep () }

data Action
  = Editing (Maybe EditAction)
  | UpdateColour String
  | Delete

data Output
  = Updated { id :: String, name :: String, colour :: String }
  | Deleted String

type Slot = forall query. H.Slot query Output String

initialState :: Input -> State
initialState { id, name, colour } =
  { id, name, colour, edit: name, editing: false }

component ::
  forall query m.
  MonadEffect m =>
  Component query Input Output m
component = mkComponent
       { initialState
       , render
       , eval: mkEval $ defaultEval { handleAction = handleAction }
       }

render :: forall m. State -> ComponentHTML Action () m
render state =
  div [ classes [CSS.dFlex, CSS.mb3] ]
  [ input [ type_ InputColor, id state.id
          , classes [CSS.formControl, CSS.formControlColor]
          , value state.colour ]
  , editableWith [CSS.formLabel, CSS.ms2, CSS.mt2] label Editing state
  , button [ type_ ButtonButton
           , classes [CSS.btnClose, CSS.msAuto, CSS.mt2]] []
 ]


handleAction ::
  forall cs m.
  MonadEffect m =>
  Action -> HalogenM State Action cs Output m Unit
handleAction action = case action of
  Editing Nothing -> pure unit

  Editing (Just editAction) -> do
    Editable.handleAction editAction >>= case _ of
      Just s -> do
        pure unit

      _ -> pure unit

  UpdateColour colour -> pure unit

  Delete -> pure unit
