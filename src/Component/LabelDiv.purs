module Canbando.Component.LabelDiv
  ( Output(..), Slot, component
  ) where

import Prelude hiding (div)

import Canbando.CSS as CSS
import Canbando.Component.Editable (EditAction)
import Data.Maybe (Maybe(..))
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval)
import Halogen as H
import Halogen.HTML (button, div, input, label, text)
import Halogen.HTML.Properties (ButtonType(..), InputType(..), classes, for, id, type_, value)


type InputDataRep row =
  ( id :: String
  , label :: String
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
initialState { id, label, colour } =
  { id, label, colour, edit: label, editing: false }

component ::
  forall query m.
  Component query Input Output m
component = mkComponent
       { initialState
       , render
       , eval: mkEval $ defaultEval { handleAction = handleAction }
       }

render :: forall cs m. State -> ComponentHTML Action cs m
render state =
  div [ classes [CSS.dFlex, CSS.mb3] ]
  [ input [ type_ InputColor, id state.id
          , classes [CSS.formControl, CSS.formControlColor]
          , value state.colour ]
  , label [ for state.id
          , classes [CSS.formLabel, CSS.ms2, CSS.mt2]]
    [text state.label]
  , button [ type_ ButtonButton
           , classes [CSS.btnClose, CSS.msAuto, CSS.mt2]] []
 ]


handleAction ::
  forall cs m.
  Action -> HalogenM State Action cs Output m Unit
handleAction action = case action of
  Editing (Just editAction) -> pure unit

  UpdateColour colour -> pure unit

  Delete -> pure unit

  _ -> pure unit
