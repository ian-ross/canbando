module App.Page where

import Prelude hiding (div)

import CSS as CSS
import Component.Header (header)
import Component.Icon (icon)
import Component.List as List
import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval, modify_)
import Halogen.HTML (HTML, button, div, div_, main, slot, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, classes)
import State (State, done, inProgress, todo)

data Action
  = AddList
  | ListAction List.Output

type Slots = ( list :: List.Slot Int )

_list :: SProxy "list"
_list = SProxy

component :: forall q i o m. MonadEffect m => Component HTML q i o m
component =
  mkComponent
    { initialState: \_ -> { lists: [ todo, inProgress, done ] }
    , render
    , eval: mkEval $ defaultEval { handleAction = handleAction }
    }

wrap :: forall cs m. Array (ComponentHTML Action cs m) -> ComponentHTML Action cs m
wrap lists =
  main [class_ CSS.flexShrink0]
  [div [classes [ CSS.containerFluid, CSS.dFlex
                , CSS.flexRow, CSS.alignItemsStart ] ] $
    lists <>
    [button [classes [ CSS.rounded, CSS.addNewList
                     , CSS.flexGrow0, CSS.flexShrink0],
             onClick \_ -> Just AddList]
     [icon "bi-plus-circle", text "Add new list"]]
  ]

render :: forall m. MonadEffect m => State -> ComponentHTML Action Slots m
render state = div_ [header, wrap lists]
  where lists = mapWithIndex onelist state.lists
        onelist idx lst = slot _list idx List.component lst absurd

handleAction :: forall cs o m. Action -> HalogenM State Action cs o m Unit
handleAction = case _ of
  AddList ->
    modify_ \state -> state { lists = state.lists <>
                                      [{ name: "New list", idPrefix: "new", nextID: 1, cards: [] }]}

  _ ->
    pure unit
