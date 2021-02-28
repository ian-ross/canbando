module Component.Icon (icon) where

import Halogen (ComponentHTML)
import Halogen.HTML (ClassName(..), i)
import Halogen.HTML.Properties (classes)

import CSS as CSS


icon :: forall action cs m. String -> ComponentHTML action cs m
icon name = i [classes [ClassName name, CSS.icon]] []
