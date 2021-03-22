module Canbando.Util
  ( blurTarget, focusElement
  , wrap, wrapWith, wrapCol
  , containerRow, containerCol
  , dataBsDismiss, dataBsToggle, dataBsTarget
  ) where

import Prelude hiding (div)

import Canbando.CSS as CSS
import Effect (Effect)
import Halogen (ComponentHTML)
import Halogen.HTML (AttrName(..), div, main)
import Halogen.HTML.Properties (IProp, attr, class_, classes, style)
import Web.Event.Event (Event)


foreign import blurTarget :: Event -> Effect Unit
foreign import focusElement :: String -> Effect Unit


wrap :: forall act cs m. Array (ComponentHTML act cs m) -> ComponentHTML act cs m
wrap content =
  main [class_ CSS.flexShrink0] [containerRow content]

wrapWith :: forall act cs m. String -> Array (ComponentHTML act cs m) -> ComponentHTML act cs m
wrapWith st content =
  main [class_ CSS.flexShrink0, style st] [containerRow content]

containerRow :: forall act cs m. Array (ComponentHTML act cs m) -> ComponentHTML act cs m
containerRow content =
  div [classes [ CSS.containerFluid, CSS.dFlex
                , CSS.flexRow, CSS.alignItemsStart ] ]
  content

wrapCol :: forall act cs m. Array (ComponentHTML act cs m) -> ComponentHTML act cs m
wrapCol content =
  main [class_ CSS.flexShrink0] [containerCol content]

containerCol :: forall act cs m. Array (ComponentHTML act cs m) -> ComponentHTML act cs m
containerCol content =
  div [classes [ CSS.containerFluid, CSS.dFlex
                , CSS.flexColumn, CSS.alignItemsStart ] ]
  content

dataBsDismiss :: forall r i. String -> IProp r i
dataBsDismiss = attr (AttrName "data-bs-dismiss")

dataBsToggle :: forall r i. String -> IProp r i
dataBsToggle = attr (AttrName "data-bs-toggle")

dataBsTarget :: forall r i. String -> IProp r i
dataBsTarget = attr (AttrName "data-bs-target")
