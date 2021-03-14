module Canbando.TestInit (initTestStore) where

import Prelude

import Canbando.AppM (AppM)
import Canbando.Capability.IdSupply (genId)
import Canbando.Capability.Resource.Board (getBoard, getBoards)
import Canbando.Capability.Store (setItem)
import Canbando.Model.Board (Board)
import Canbando.Model.Card (Card)
import Canbando.Model.Id (Id)
import Canbando.Model.List (List)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)


initTestStore :: AppM Board
initTestStore = do
  runMaybeT do
    boardId <- MaybeT $ head <$> getBoards
    MaybeT $ getBoard boardId
  >>= case _ of
    Nothing -> doInitTestStore
    Just b -> pure b

doInitTestStore :: AppM Board
doInitTestStore = do
  id <- genId 'B'
  lists <- sequence [todo, inProgress, done]
  setItem id { id, name: "Test board", lists: map _.id lists }
  setItem "root" [id]
  pure { id, name: "Test board", lists }

initList :: String -> Array String -> AppM List
initList name cardNames = do
  id <- genId 'L'
  cards <- traverse (initCard id) cardNames
  setItem id { id, name, cards: map _.id cards }
  pure { id, name, cards }

initCard :: Id -> String -> AppM Card
initCard list title = do
  id <- genId 'C'
  setItem id { id, title, list }
  pure { id, title }

todo :: AppM List
todo = initList "To Do" ["Task #3", "Task #4", "Task #5", "Task #6", "Task #7"]

inProgress :: AppM List
inProgress = initList "In progress" ["Task #1", "Task #2"]

done :: AppM List
done = initList "Done" []
