module Component.Avatar (avatar) where

import Halogen (ComponentHTML)
import Halogen.HTML (ClassName(..), div, text)
import Halogen.HTML.Properties (classes)

avatar :: forall action cs m. String -> ComponentHTML action cs m
avatar name =
  div [classes [ ClassName "rounded-circle"
               , ClassName "avatar"
               , ClassName "bg-secondary"
               , ClassName "text-white" ] ]
  [text name]
