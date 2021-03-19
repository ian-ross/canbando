module Canbando.Component.Modal
  ( renderModal, modalButton
  ) where

import Prelude hiding (div)

import Canbando.CSS as CSS
import Canbando.Util (dataBsDismiss, dataBsTarget, dataBsToggle)
import Halogen (ComponentHTML)
import Halogen.HTML (button, div, h5, text)
import Halogen.HTML.Properties (ButtonType(..), class_, classes, id, tabIndex, type_)
import Halogen.HTML.Properties.ARIA as ARIA

modalButton :: forall act cs m. String -> String -> ComponentHTML act cs m
modalButton target label =
  button [type_ ButtonButton, classes [CSS.btn, CSS.btnSecondary, CSS.btnSm]
         , dataBsToggle "modal", dataBsTarget $ "#" <> target]
  [text label]


renderModal ::
  forall act cs m.
  String -> String -> String ->
  Array (ComponentHTML act cs m) ->
  Array (ComponentHTML act cs m) ->
  ComponentHTML act cs m
renderModal modalId label title body buttons =
  div [ classes [CSS.modal, CSS.fade], id modalId, tabIndex (-1)
      , ARIA.labelledBy label, ARIA.hidden "true" ]
  [ div [ class_ CSS.modalDialog ]
    [ div [ class_ CSS.modalContent ]
      [ div [ class_ CSS.modalHeader ]
        [ h5 [ class_ CSS.modalTitle, id label ]
          [text title]
        , button [ type_ ButtonButton, class_ CSS.btnClose
                 , dataBsDismiss "modal", ARIA.label "Close"]
          []
        ]
      , div [ class_ CSS.modalBody ] body
      , div [ class_ CSS.modalFooter ] buttons
      ]
    ]
  ]
