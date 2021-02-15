module Component.Card where

import Prelude hiding (div)

import Halogen (ComponentHTML)
import Halogen.HTML (ClassName(..), a, div, text)
import Halogen.HTML.Properties (class_, href)
import State (Card)

card :: forall action cs m. Card -> ComponentHTML action cs m
card c =
  div [class_ $ ClassName "card"]
  [ text $ c.title
  , a [class_ $ ClassName "card-menu", href "#"]
    [text "..."]
  ]
