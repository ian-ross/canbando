module Canbando.TestInit (initTestStore) where

import Prelude

import Canbando.Capability.IdSupply (class IdSupply, genId)
import Canbando.Capability.Store (class Store, clearStore, getItem, setItem)
import Canbando.Model.Card (Card)
import Canbando.Model.Id (Id)
import Canbando.Model.List (List)
import Data.Array (take, (!!))
import Data.Maybe (fromJust)
import Data.Traversable (sequence, traverse)
import Halogen (HalogenM, lift)
import Partial.Unsafe (unsafePartial)


initTestStore ::
  forall s act cs o m.
  IdSupply m => Store m => HalogenM s act cs o m Unit
initTestStore = lift $ do
  clearStore
  doInitTestStore

doInitTestStore :: forall m. IdSupply m => Store m => m Unit
doInitTestStore = do
  id <- genId 'B'
  labels <- initLabels
  lists <- sequence [todo labels, inProgress, done]
  setItem id { id
             , name: "Test board"
             , bgColour: "#CCCCCC"
             , labels: labels
             , lists: map _.id lists }
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
  setItem id { id, title, list, labels: [], checklist: [] }
  pure { id, title, labels: [], checklist: [] }

todo :: forall m. IdSupply m => Store m => Array Id -> m List
todo labels = do
  lst <- initList "To Do" ["Task #3", "Task #4", "Task #5", "Task #6", "Task #7"]
  let process :: Int -> Int -> m Unit
      process idx nlabels = do
        let cint = unsafePartial $ fromJust $ lst.cards !! idx
        mc <- getItem cint.id
        let c = unsafePartial $ fromJust mc
        setItem cint.id c { labels = take nlabels labels }
  process 0 1
  process 1 2
  process 2 3
  pure lst

inProgress :: forall m. IdSupply m => Store m => m List
inProgress = initList "In progress" ["Task #1", "Task #2"]

done :: forall m. IdSupply m => Store m => m List
done = initList "Done" []

initLabels :: forall m. IdSupply m => Store m => m (Array Id)
initLabels = do
  id1 <- genId 'T'
  id2 <- genId 'T'
  id3 <- genId 'T'
  let info1 = { id: id1, name: "label-1", colour: "#FF0000" }
  let info2 = { id: id2, name: "label-2", colour: "#00FF00" }
  let info3 = { id: id3, name: "label-3", colour: "#0000FF" }
  setItem id1 info1
  setItem id2 info2
  setItem id3 info3
  pure [id1, id2, id3]
