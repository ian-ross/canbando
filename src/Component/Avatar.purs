module Component.Avatar (avatar) where

import CSS as CSS
import Halogen (ComponentHTML)
import Halogen.HTML (div, text)
import Halogen.HTML.Properties (classes)

avatar :: forall action slots m. String -> ComponentHTML action slots m
avatar initials =
  div
  [classes [CSS.roundedCircle, CSS.avatar, CSS.bgSecondary, CSS.textWhite]]
  [text initials]
