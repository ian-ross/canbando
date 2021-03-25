module Canbando.Component.Avatar (avatar) where

import Halogen (ComponentHTML)
import Halogen.HTML (div, text)
import Halogen.HTML.Properties (classes)

import Canbando.CSS as CSS


avatar :: forall action cs m. String -> ComponentHTML action cs m
avatar initials =
  div
  [classes [CSS.roundedCircle, CSS.avatar, CSS.bgSecondary, CSS.textWhite]]
  [text initials]
