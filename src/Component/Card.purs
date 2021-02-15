module Component.Card where

import CSS as CSS
import Halogen (ComponentHTML)
import Halogen.HTML (a, div, text)
import Halogen.HTML.Properties (class_, href)
import State (Card)

card :: forall action cs m. Card -> ComponentHTML action cs m
card c =
  div [class_ CSS.card]
  [ text c.title
  , a [class_ CSS.cardMenu, href "#"] [text "..."]
  ]
