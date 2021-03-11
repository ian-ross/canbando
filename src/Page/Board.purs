module Canbando.Page.Board where

import Prelude hiding (div)

import Canbando.CSS as CSS
import Canbando.Capability.IdSupply (class IdSupply)
import Canbando.Capability.Resource.List (class ManageList)
import Canbando.Component.Header (header)
import Canbando.Component.Icon (icon)
import Canbando.Component.List (Direction(..), Output(..), Slot, component) as List
import Canbando.Model.List (Id) as List
import Canbando.Model.List (List, newList)
import Canbando.State (BoardState, done, inProgress, todo)
import Data.Array (deleteAt, filter, findIndex, insertAt, length, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Class (class MonadEffect)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval, modify_)
import Halogen.HTML (button, div, div_, main, slot, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, classes)
import Type.Proxy (Proxy(..))


data Action
  = AddList
  | ListAction List.Output

type Slots = ( list :: List.Slot List.Id )

_list :: Proxy "list"
_list = Proxy

component ::
  forall q i o m.
  MonadEffect m =>
  ManageList m =>
  IdSupply m =>
  Component q i o m
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
             onClick \_ -> AddList]
     [icon "bi-plus-circle", text "Add new list"]]
  ]

render ::
  forall m.
  MonadEffect m =>
  ManageList m =>
  IdSupply m =>
  BoardState -> ComponentHTML Action Slots m
render state = div_ [header, wrap lists]
  where lists = map onelist state.lists
        onelist lst = slot _list lst.id List.component lst ListAction

handleAction ::
  forall cs o m.
  IdSupply m =>
  Action -> HalogenM BoardState Action cs o m Unit
handleAction = case _ of
  AddList -> do
    list <- newList
    modify_ \s -> s { lists = s.lists <> [list]}

  ListAction (List.ListDeleted id) -> do
    modify_ \s -> s { lists = filter (\lst -> lst.id /= id) s.lists }

  ListAction (List.ListMoved List.Left id) ->
    modify_ \st -> st { lists = moveList id (-1) st.lists }

  ListAction (List.ListMoved List.Right id) ->
    modify_ \st -> st { lists = moveList id 1 st.lists }


moveList :: List.Id -> Int -> Array List -> Array List
moveList id delta lsts =
  case findList id lsts of
    Nothing -> lsts
    Just idx ->
      if delta == -1 && idx == 0 || delta == 1 && idx == length lsts
      then lsts
      else fromMaybe lsts $ join $
           insertAt (idx+delta) <$> (lsts !! idx) <*> deleteAt idx lsts

findList :: List.Id -> Array List -> Maybe Int
findList id lists = findIndex (\lst -> lst.id == id) lists
