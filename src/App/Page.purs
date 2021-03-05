module App.Page where

import Prelude hiding (div)

import CSS as CSS
import Component.Header (header)
import Component.Icon (icon)
import Component.List as List
import Data.Array (deleteAt, filter, findIndex, insertAt, length, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, get, mkComponent, mkEval, modify_)
import Halogen.HTML (HTML, button, div, div_, main, slot, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, classes)
import State (State, done, inProgress, todo)


data Action
  = AddList
  | ListAction List.Output

type Slots = ( list :: List.Slot List.Id )

_list :: SProxy "list"
_list = SProxy

component :: forall q i o m. MonadEffect m => Component HTML q i o m
component =
  mkComponent
    { initialState: \_ -> { nextID: 1, lists: [ todo, inProgress, done ] }
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
  where lists = map onelist state.lists
        onelist lst = slot _list lst.id List.component lst (Just <<< ListAction)

handleAction :: forall cs o m. Action -> HalogenM State Action cs o m Unit
handleAction = case _ of
  AddList ->
    modify_ \s -> s { nextID = s.nextID + 1
                    , lists = s.lists <> [List.newList s.nextID "New list"]}

  ListAction (List.ListDeleted id) -> do
    modify_ \s -> s { lists = filter (\lst -> lst.id /= id) s.lists }

  ListAction (List.ListMoved List.Left id) ->
    modify_ \st -> st { lists = moveList id (-1) st.lists }

  ListAction (List.ListMoved List.Right id) ->
    modify_ \st -> st { lists = moveList id 1 st.lists }

  _ -> pure unit


moveList :: List.Id -> Int -> Array List.Input -> Array List.Input
moveList id delta lsts =
  case findList id lsts of
    Nothing -> lsts
    Just idx ->
      if delta == -1 && idx == 0 || delta == 1 && idx == length lsts
      then lsts
      else fromMaybe lsts $ join $
           insertAt (idx+delta) <$> (lsts !! idx) <*> deleteAt idx lsts

findList :: List.Id -> Array List.Input -> Maybe Int
findList id lists = findIndex (\lst -> lst.id == id) lists
