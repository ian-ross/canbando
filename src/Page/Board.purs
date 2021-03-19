module Canbando.Page.Board where

import Prelude hiding (div)

import Canbando.CSS as CSS
import Canbando.Capability.Resource.Board (class ManageBoard, addList, deleteList, getBoard, moveList)
import Canbando.Capability.Resource.List (class ManageList)
import Canbando.Component.Header (header)
import Canbando.Component.Icon (icon)
import Canbando.Component.List (Direction(..), Output(..), Slot, component) as List
import Canbando.Component.Modal.BoardDetails (Output, Slot, component) as BoardDetails
import Canbando.Model.Board (Board, addListToBoard)
import Canbando.Model.Id (Id)
import Canbando.Model.List (List)
import Canbando.Util (wrap)
import Data.Array (deleteAt, filter, findIndex, insertAt, length, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Debug.Trace (traceM)
import Effect.Class (class MonadEffect)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, gets, lift, mkComponent, mkEval, modify_)
import Halogen as H
import Halogen.HTML (button, div_, slot, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes)
import Network.RemoteData (RemoteData(..))
import Type.Proxy (Proxy(..))


type State =
  { id :: Id
  , board :: RemoteData String Board
  }

data Action
  = Initialize
  | AddList
  | ListAction List.Output
  | ModalAction BoardDetails.Output

type Slot id = forall query. H.Slot query Void id

type Slots = ( list :: List.Slot Id
             , modal :: BoardDetails.Slot )


component ::
  forall q o m.
  MonadEffect m =>
  ManageList m =>
  ManageBoard m =>
  Component q Id o m
component =
  mkComponent
    { initialState
    , render
    , eval: mkEval $ defaultEval { handleAction = handleAction
                                 , initialize = Just Initialize }
    }

initialState :: Id -> State
initialState id = { id, board: NotAsked }

render ::
  forall m.
  MonadEffect m =>
  ManageList m =>
  State -> ComponentHTML Action Slots m
render state = div_ [modal, header (Just name), wrap contents]
  where name = case state.board of
          Success board -> board.name
          _ -> "Canbando!"
        contents =
          case state.board of
            NotAsked -> [text "Not asked yet!"]
            Loading -> [text "Loading..."]
            Failure err -> [text $ "FAILED: " <> err]
            Success board ->
              (map onelist board.lists) <>
              [button [classes [ CSS.rounded, CSS.addNewList
                               , CSS.flexGrow0, CSS.flexShrink0],
                       onClick (const AddList)]
               [icon "bi-plus-circle", text "Add new list"]]

        onelist lst = slot (Proxy :: _ "list") lst.id List.component lst ListAction
        modal = slot (Proxy :: _ "modal") unit BoardDetails.component unit ModalAction

handleAction ::
  forall cs o m.
  ManageBoard m =>
  Action -> HalogenM State Action cs o m Unit
handleAction = case _ of
  Initialize -> do
    modify_ \s -> s { board = Loading }
    id <- gets _.id
    gets _.id >>= lift <<< getBoard >>= case _ of
      Nothing -> modify_ \s -> s { board = Failure "Couldn't load board!" }
      Just board -> modify_ \s -> s { board = Success board }

  AddList -> do
    (lift <<< addList =<< gets _.id) >>= case _ of
      Nothing -> pure unit
      Just list -> modify_ \s -> s { board = addListToBoard list <$> s.board }

  ListAction (List.ListDeleted listId) -> do
    boardId <- gets _.id
    lift $ deleteList boardId listId
    modify_ \s -> s { board = deleteListFromBoard listId <$> s.board }

  ListAction (List.ListMoved List.Left id) -> doMove id List.Left

  ListAction (List.ListMoved List.Right id) -> doMove id List.Right

  ModalAction a -> traceM a

deleteListFromBoard :: Id -> Board -> Board
deleteListFromBoard listId brd = brd { lists = filter (\lst -> lst.id /= listId) brd.lists }

doMove ::
  forall cs o m.
  ManageBoard m =>
  Id -> List.Direction -> HalogenM State Action cs o m Unit
doMove id dir = do
  gets _.board >>= case _ of
    Success st -> case findList id st.lists of
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
          let lists = fromMaybe st.lists $ join $
                      insertAt (idx+delta) <$> (st.lists !! idx) <*>
                      deleteAt idx st.lists
          modify_ \s -> s { board = Success st { lists = lists } }
    _ -> pure unit


findList :: Id -> Array List -> Maybe Int
findList id lists = findIndex (\lst -> lst.id == id) lists
