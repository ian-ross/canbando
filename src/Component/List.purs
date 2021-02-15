module Component.List where

import Prelude hiding (div)

import CSS as CSS
import Component.Card (card)
import Component.Icon (icon)
import Halogen (ComponentHTML)
import Halogen.HTML (button, div, h1_, text)
import Halogen.HTML.Properties (classes)
import State (List)

list :: forall action cs m. List -> ComponentHTML action cs m
list l =
  div [classes [ CSS.bgLight, CSS.p3, CSS.rounded, CSS.listWrapper
               , CSS.flexGrow0, CSS.flexShrink0 ]]
  [ h1_ [text l.name]
  , div [classes [CSS.dFlex, CSS.flexColumn]] (map card l.cards)
  , button [classes [CSS.rounded, CSS.addNewCard]]
    [ icon "bi-plus-circle", text "Add new card"]
  ]
