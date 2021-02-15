module App.Page where

import Prelude hiding (div)

import CSS as CSS
import Component.Header (header)
import Component.Icon (icon)
import Component.List (list)
import Halogen (Component, ComponentHTML, HalogenM, mkComponent, mkEval, defaultEval)
import Halogen.HTML (HTML, button, div, div_, main, text)
import Halogen.HTML.Properties (class_, classes)
import State (State, done, inProgress, todo)

data Action = NoOp

component :: forall q i o m. Component HTML q i o m
component =
  mkComponent
    { initialState: \_ -> { lists: [ todo, inProgress, done ] }
    , render
    , eval: mkEval $ defaultEval { handleAction = handleAction }
    }

wrap :: forall action cs m. Array (ComponentHTML action cs m) -> ComponentHTML action cs m
wrap lists =
  main [class_ CSS.flexShrink0]
  [div [classes [ CSS.containerFluid, CSS.dFlex
                , CSS.flexRow, CSS.alignItemsStart ] ] $
    lists <>
    [button [classes [ CSS.rounded, CSS.addNewList
                     , CSS.flexGrow0, CSS.flexShrink0]]
     [icon "bi-plus-circle", text "Add new list"]]
  ]

render :: forall action cs m. State -> ComponentHTML action cs m
render state = div_ [header, wrap (map list state.lists)]

handleAction :: forall cs o m. Action → HalogenM State Action cs o m Unit
handleAction = const $ pure unit
