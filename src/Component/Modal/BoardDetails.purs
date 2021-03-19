module Canbando.Component.Modal.BoardDetails
  ( BareBoardInfo, Output(..), Slot, component
  ) where

import Prelude hiding (div)

import Canbando.CSS as CSS
import Canbando.Component.Modal (renderModal)
import Canbando.Model.Board (Board, BoardInfo)
import Canbando.Util (dataBsDismiss)
import Data.Maybe (Maybe(..))
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval)
import Halogen as H
import Halogen.HTML (button, div, form_, input, label, p_, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (ButtonType(..), InputType(..), class_, classes, for, id, type_)
import Halogen.HTML.Properties.ARIA as ARIA


type BareBoardInfo = { | BoardInfo () }

type State =
  { visible :: Boolean
  , board :: Maybe BareBoardInfo }

data Action = NoOp

data Query a = Show Board a

data Output = Updated BareBoardInfo

type Slot = forall query. H.Slot query Output Unit

initialState :: State
initialState = { visible: false, board: Nothing }

component ::
  forall query m.
  Component query Unit Output m
component =
  mkComponent
  { initialState: const initialState
  , render
  , eval : mkEval $ defaultEval { handleAction = handleAction }
  }

render :: forall cs m. State -> ComponentHTML Action cs m
render s =
  renderModal
    "boardDetailsModal" "boardDetailsModalLabel" "Edit board details"
    [ form_
      [ div [class_ CSS.mb3]
        [ label [for "boardName", class_ CSS.formLabel]
          [text "Board name"]
        , input [ type_ InputEmail, class_ CSS.formControl, id "boardName"]
        ]
      , div [class_ CSS.mb3]
        [ label [for "boardBgColour", class_ CSS.formLabel]
          [text "Board background colour"]
        , input [ type_ InputColor, id "boardBgColour"
                , classes [CSS.formControl, CSS.formControlColor] ]
        ]
      ]
    ]
    [ button [ type_ ButtonButton, classes [CSS.btn, CSS.btnSecondary]
             , dataBsDismiss "modal" ]
      [text "Close"]
    , button [ type_ ButtonButton, classes [CSS.btn, CSS.btnPrimary]
             , onClick (const NoOp) ]
      [text "Save changes"]
    ]

handleAction ::
  forall cs m.
  Action -> HalogenM State Action cs Output m Unit
handleAction =
  case _ of
    NoOp -> pure unit
