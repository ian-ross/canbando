module Canbando.Component.Icon (icon, iconButton) where

import Halogen (ComponentHTML)
import Halogen.HTML (ClassName(..), i)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes)
import Web.UIEvent.MouseEvent (MouseEvent)

import Canbando.CSS as CSS


icon :: forall action cs m. String -> ComponentHTML action cs m
icon name = i [classes [ClassName name, CSS.icon]] []

iconButton :: forall action cs m.
              String -> (MouseEvent -> action) -> ComponentHTML action cs m
iconButton name f = i [classes [ClassName name, CSS.icon], onClick f] []
