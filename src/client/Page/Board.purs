module Canbando.Page.Board where

import Prelude hiding (div)

import Canbando.CSS as CSS
import Canbando.Capability.IdSupply (class IdSupply)
import Canbando.Capability.Navigate (class Navigate, navigate)
import Canbando.Capability.Resource.Board (class ManageBoard, addList, deleteBoard, deleteList, getBoard, moveList, updateBoard)
import Canbando.Capability.Resource.Labels (class EditLabels, class GetLabels, class SetLabels, setLabels)
import Canbando.Capability.Resource.List (class ManageList)
import Canbando.Component.BoardModal (Output(..), Slot, Query(..), component) as BoardModal
import Canbando.Component.CardModal (Output(..), Slot, Query(..), component) as CardModal
import Canbando.Component.Header (header)
import Canbando.Component.Icon (icon)
import Canbando.Component.List (Direction(..), Output(..), Slot, Query(..), component) as List
import Canbando.Env (Env)
import Canbando.Model.Board (Board, addListToBoard)
import Canbando.Model.Id (Id)
import Canbando.Model.List (List)
import Canbando.Routes (Route(..))
import Canbando.Util (wrap, wrapWith)
import Control.Monad.Reader.Trans (class MonadAsk)
import Data.Array (deleteAt, filter, findIndex, insertAt, length, (!!))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, get, gets, lift, mkComponent, mkEval, modify_, tell)
import Halogen as H
import Halogen.HTML (button, div_, slot, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes)
import Network.RemoteData (RemoteData(..), toMaybe)
import Type.Proxy (Proxy(..))


type State =
  { id :: Id
  , board :: RemoteData String Board
  }

data Action
  = Initialize
  | AddList
  | ListAction List.Output
  | BoardModalAction BoardModal.Output
  | CardModalAction CardModal.Output

type Slot id = forall query. H.Slot query Void id

type Slots = ( list :: List.Slot Id
             , boardModal :: BoardModal.Slot
             , cardModal :: CardModal.Slot )


component ::
  forall q o m.
  MonadAff m =>
  MonadAsk Env m =>
  IdSupply m =>
  GetLabels m =>
  SetLabels m =>
  EditLabels m =>
  ManageList m =>
  ManageBoard m =>
  Navigate m =>
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
  MonadAff m =>
  MonadAsk Env m =>
  IdSupply m =>
  GetLabels m =>
  EditLabels m =>
  ManageList m =>
  State -> ComponentHTML Action Slots m
render state = div_ [boardModal, cardModal, header name, wrapper contents]
  where name = _.name <$> toMaybe state.board
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

        wrapper =
          case state.board of
            Success board ->
              wrapWith ("background-color: " <> board.bgColour <> ";")
            _ -> wrap

        onelist lst = slot (Proxy :: _ "list") lst.id List.component lst ListAction
        boardModal = slot (Proxy :: _ "boardModal") unit BoardModal.component unit BoardModalAction
        cardModal = slot (Proxy :: _ "cardModal") unit CardModal.component unit CardModalAction

handleAction ::
  forall o m.
  ManageBoard m =>
  Navigate m =>
  SetLabels m =>
  Action -> HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    modify_ \s -> s { board = Loading }
    id <- gets _.id
    gets _.id >>= lift <<< getBoard >>= case _ of
      Nothing -> modify_ \s -> s { board = Failure "Couldn't load board!" }
      Just board -> do
        modify_ \s -> s { board = Success board }
        setLabels board.labels
        tell (Proxy :: _ "boardModal") unit (BoardModal.Show board)

  AddList -> do
    (lift <<< addList =<< gets _.id) >>= case _ of
      Nothing -> pure unit
      Just list -> modify_ \s -> s { board = addListToBoard list <$> s.board }

  ListAction (List.OpenCardModal card listId) ->
    tell (Proxy :: _ "cardModal") unit (CardModal.Show card listId)

  ListAction (List.ListDeleted listId) -> do
    boardId <- gets _.id
    lift $ deleteList boardId listId
    modify_ \s -> s { board = deleteListFromBoard listId <$> s.board }

  ListAction (List.ListMoved List.Left id) -> doMove id List.Left

  ListAction (List.ListMoved List.Right id) -> doMove id List.Right

  BoardModalAction (BoardModal.Updated bi) -> do
    st <- get
    let newb = st.board <#>
               _ { name = bi.name, bgColour = bi.bgColour, labels = bi.labels }
    modify_ \s -> s { board = newb }
    for_ newb \b -> lift $ updateBoard b.id b

  BoardModalAction (BoardModal.Deleted boardId) -> do
    lift $ deleteBoard boardId
    navigate Home

  CardModalAction (CardModal.Deleted { cardId, listId }) ->
    tell (Proxy :: _ "list") listId (List.DeleteCard { cardId })

  CardModalAction (CardModal.Updated { cardId, listId, title, labels, checklist }) ->
    tell (Proxy :: _ "list") listId (List.UpdateCard { cardId, title, labels, checklist })


deleteListFromBoard :: Id -> Board -> Board
deleteListFromBoard listId brd =
  brd { lists = filter (\lst -> lst.id /= listId) brd.lists }


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
                List.Left ->  (idx - 1 `max` 0  ) /\ (-1)
                List.Right -> (idx + 1 `min` len) /\ 1
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
