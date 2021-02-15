module Component.List where

import Prelude hiding (div)

import Component.Card (card)
import Component.Icon (icon)
import Halogen (ComponentHTML)
import Halogen.HTML (ClassName(..), button, div, h1_, text)
import Halogen.HTML.Properties (classes)
import State (List)

list :: forall action cs m. List -> ComponentHTML action cs m
list l =
  div [classes $ map ClassName cs]
  [ h1_ [text l.name]
  , div [classes [ ClassName "d-flex", ClassName "flex-column"] ]
    (map card l.cards)
  , button [classes [ClassName "rounded", ClassName "add-new-card"]]
    [ icon "bi-plus-circle", text "Add new card"]
  ]
  where cs = [ "bg-light", "p-3", "rounded", "list-wrapper"
             , "flex-grow-0", "flex-shrink-0" ]
