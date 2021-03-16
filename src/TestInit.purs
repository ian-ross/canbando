module Canbando.TestInit (initTestStore) where

import Prelude

import Canbando.Capability.IdSupply (class IdSupply, genId)
import Canbando.Capability.Store (class Store, clearStore, setItem)
import Canbando.Model.Card (Card)
import Canbando.Model.Id (Id)
import Canbando.Model.List (List)
import Data.Traversable (sequence, traverse)
import Halogen (HalogenM, lift)


initTestStore ::
  forall s act cs o m.
  IdSupply m => Store m => HalogenM s act cs o m Unit
initTestStore = lift $ do
  clearStore
  doInitTestStore

doInitTestStore :: forall m. IdSupply m => Store m => m Unit
doInitTestStore = do
  id <- genId 'B'
  lists <- sequence [todo, inProgress, done]
  setItem id { id, name: "Test board", lists: map _.id lists }
  setItem "root" [id]

initList ::
  forall m. IdSupply m => Store m => String -> Array String -> m List
initList name cardNames = do
  id <- genId 'L'
  cards <- traverse (initCard id) cardNames
  setItem id { id, name, cards: map _.id cards }
  pure { id, name, cards }

initCard ::
  forall m. IdSupply m => Store m => Id -> String -> m Card
initCard list title = do
  id <- genId 'C'
  setItem id { id, title, list }
  pure { id, title }

todo :: forall m. IdSupply m => Store m => m List
todo = initList "To Do" ["Task #3", "Task #4", "Task #5", "Task #6", "Task #7"]

inProgress :: forall m. IdSupply m => Store m => m List
inProgress = initList "In progress" ["Task #1", "Task #2"]

done :: forall m. IdSupply m => Store m => m List
done = initList "Done" []
