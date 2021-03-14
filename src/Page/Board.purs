module Canbando.Page.Board where

import Prelude hiding (div)

import Canbando.CSS as CSS
import Canbando.Capability.Resource.Board (class ManageBoard, addList, deleteList, moveList)
import Canbando.Capability.Resource.List (class ManageList)
import Canbando.Component.Header (header)
import Canbando.Component.Icon (icon)
import Canbando.Component.List (Direction(..), Output(..), Slot, component) as List
import Canbando.Model.Board (Board)
import Canbando.Model.Id (Id)
import Canbando.Model.List (List)
import Data.Array (deleteAt, filter, findIndex, insertAt, length, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, get, gets, lift, mkComponent, mkEval, modify_)
import Halogen.HTML (button, div, div_, main, slot, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, classes)
import Type.Proxy (Proxy(..))

type State = Board

data Action
  = AddList
  | ListAction List.Output

type Slots = ( list :: List.Slot Id )

_list :: Proxy "list"
_list = Proxy

component ::
  forall q o m.
  MonadEffect m =>
  ManageList m =>
  ManageBoard m =>
  Component q Board o m
component =
  mkComponent
    { initialState
    , render
    , eval: mkEval $ defaultEval { handleAction = handleAction }
    }

initialState :: Board -> State
initialState = identity

wrap :: forall cs m. Array (ComponentHTML Action cs m) -> ComponentHTML Action cs m
wrap lists =
  main [class_ CSS.flexShrink0]
  [div [classes [ CSS.containerFluid, CSS.dFlex
                , CSS.flexRow, CSS.alignItemsStart ] ] $
    lists <>
    [button [classes [ CSS.rounded, CSS.addNewList
                     , CSS.flexGrow0, CSS.flexShrink0],
             onClick (const AddList)]
     [icon "bi-plus-circle", text "Add new list"]]
  ]

render ::
  forall m.
  MonadEffect m =>
  ManageList m =>
  State -> ComponentHTML Action Slots m
render state = div_ [header state.name, wrap lists]
  where lists = map onelist state.lists
        onelist lst = slot _list lst.id List.component lst ListAction

handleAction ::
  forall cs o m.
  ManageBoard m =>
  Action -> HalogenM State Action cs o m Unit
handleAction = case _ of
  AddList -> do
    (lift <<< addList =<< gets _.id) >>= case _ of
      Nothing -> pure unit
      Just list -> modify_ \s -> s { lists = s.lists <> [list]}

  ListAction (List.ListDeleted listId) -> do
    boardId <- gets _.id
    lift $ deleteList boardId listId
    modify_ \s -> s { lists = filter (\lst -> lst.id /= listId) s.lists }

  ListAction (List.ListMoved List.Left id) -> doMove id List.Left

  ListAction (List.ListMoved List.Right id) -> doMove id List.Right


doMove ::
  forall cs o m.
  ManageBoard m =>
  Id -> List.Direction -> HalogenM State Action cs o m Unit
doMove id dir = do
  st <- get
  case findList id st.lists of
    Nothing -> pure unit
    Just idx -> do
      let len = length st.lists
          newIdx /\ delta =
            case dir of
              List.Left ->
                (idx - 1 `max` 0) /\ (-1)
              List.Right ->
                (idx + 1 `min` len) /\ 1
      if delta == -1 && idx == 0 || delta == 1 && idx == len
        then pure unit
        else do
        lift $ moveList st.id id newIdx
        modify_ \s ->
          s { lists = fromMaybe st.lists $ join $
                      insertAt (idx+delta) <$> (st.lists !! idx) <*>
                      deleteAt idx st.lists }


findList :: Id -> Array List -> Maybe Int
findList id lists = findIndex (\lst -> lst.id == id) lists
