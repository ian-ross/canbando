-- TODO:
--
-- Error handling: see `Canbando.Capability.Resource.List`.

module Canbando.Capability.Resource.Board (
  class ManageBoard,
  addBoard, updateBoard,
  getBoards, getBoardStore, getBoard,
  deleteBoard, moveList, addList, deleteList,
  loadBoards
) where

import Prelude

import Canbando.Capability.IdSupply (class IdSupply, genId)
import Canbando.Capability.Resource.List (deleteCard)
import Canbando.Capability.Store (class Store, getItem, removeItem, setItem)
import Canbando.Model.Board (Board, BoardStore, toBoardStore)
import Canbando.Model.Id (Id)
import Canbando.Model.List (List, ListStore, toListStore)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Array (delete, insertAt)
import Data.Foldable (for_)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Traversable (sequence, traverse, traverse_)


class Monad m <= ManageBoard m where
  addBoard :: m Board
  getBoards :: m (Array Id)
  getBoardStore :: Id {- boardId -} -> m (Maybe BoardStore)
  getBoard :: Id {- boardId -} -> m (Maybe Board)
  updateBoard :: Id {- boardId -} -> Board -> m Unit
  deleteBoard :: Id {- boardId -} -> m Unit
  moveList :: Id {- boardId -} -> Id {- listId -} -> Int {- index -} -> m Unit
  addList :: Id {- boardId -} -> m (Maybe List)
  deleteList :: Id {- boardId -} -> Id {- listId -} -> m Unit


loadBoards :: forall m. ManageBoard m => m (Array BoardStore)
loadBoards =
  getBoards >>= traverse getBoardStore <#> sequence <#> maybe [] identity

instance manageBoardM :: (Monad m, IdSupply m, Store m) => ManageBoard m where
  addBoard = do
    newId <- genId 'B'
    let board = { id: newId
                , name: "New board"
                , bgColour: "#CCCCCC"
                , labels: []
                , lists: [] }
    setItem board.id $ toBoardStore board
    root <- fromMaybe [] <$> getItem "root"
    setItem "root" $ root <> [board.id]
    pure board

  updateBoard id board = setItem id $ toBoardStore board

  getBoards = fromMaybe [] <$> getItem "root"

  getBoardStore boardId = getItem boardId

  getBoard boardId =
    runMaybeT do
      brd <- MaybeT $ getBoardStore boardId
      oklists :: Array ListStore <- MaybeT $ sequence <$> traverse getItem brd.lists
      let getCards list =
            sequence <$> traverse getItem list.cards <#>
            map (\cards -> { id: list.id, name: list.name, cards })
      lists <- MaybeT $ sequence <$> traverse getCards oklists
      pure { id: brd.id, name: brd.name, bgColour: brd.bgColour
           , labels: brd.labels, lists: lists }

  deleteBoard boardId = do
    mroot <- getItem "root"
    for_ mroot \root -> do
      setItem "root" $ delete boardId root
      removeItem boardId

  moveList boardId listId idx = const unit <$> runMaybeT do
    board :: BoardStore <- MaybeT $ getItem boardId
    lift $ setItem boardId $
      board { lists = fromMaybe board.lists $
                        insertAt idx listId $
                        delete listId board.lists }

  addList boardId = runMaybeT do
    board <- MaybeT $ getItem boardId
    newId <- lift $ genId 'L'
    let list = { id: newId, name: "New list", cards: [] }
    lift $ setItem list.id $ toListStore list
    lift $ setItem boardId $ board { lists = board.lists <> [list.id] }
    pure list

  deleteList boardId listId = const unit <$> runMaybeT do
    list :: ListStore <- MaybeT $ getItem listId
    lift $ traverse_ deleteCard list.cards
    lift $ removeItem listId
    board <- MaybeT $ getItem boardId
    lift $ setItem boardId $ board { lists = delete listId board.lists }
