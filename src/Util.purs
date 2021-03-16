module Canbando.Util
  ( blurTarget, focusElement
  , wrap
  ) where

import Prelude hiding (div)

import Canbando.CSS as CSS
import Effect (Effect)
import Halogen (ComponentHTML)
import Halogen.HTML (div, main)
import Halogen.HTML.Properties (class_, classes)
import Web.Event.Event (Event)


foreign import blurTarget :: Event -> Effect Unit
foreign import focusElement :: String -> Effect Unit


wrap :: forall act cs m. Array (ComponentHTML act cs m) -> ComponentHTML act cs m
wrap content =
  main [class_ CSS.flexShrink0]
  [div [classes [ CSS.containerFluid, CSS.dFlex
                , CSS.flexRow, CSS.alignItemsStart ] ]
    content
  ]
