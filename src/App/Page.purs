module App.Page where

import Prelude hiding (div)

import Component.Header (header)
import Component.Icon (icon)
import Component.List (list)
import Halogen (Component, ComponentHTML, HalogenM, mkComponent, mkEval, defaultEval)
import Halogen.HTML (ClassName(..), HTML, button, div, div_, main, text)
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
  main [class_ (ClassName "flex-shrink-0")]
  [div [classes $ map ClassName [ "container-fluid", "d-flex"
                                , "flex-row", "align-items-start" ] ] $
    lists <>
    [button [classes $ map ClassName ["rounded", "add-new-list"
                                     , "flex-grow-0", "flex-shrink-0"]]
     [icon "bi-plus-circle", text "Add new list"]]
  ]

render :: forall cs m. State -> ComponentHTML Action cs m
render state = div_ [header, wrap (map list state.lists)]

handleAction :: forall cs o m. Action → HalogenM State Action cs o m Unit
handleAction = const $ pure unit
