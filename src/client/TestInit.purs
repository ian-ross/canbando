module Canbando.TestInit (initTestStore) where

import Prelude

import Canbando.Capability.IdSupply (class IdSupply, genId)
import Canbando.Capability.Store (class Store, clearStore, getItem, setItem)
import Canbando.Model.Card (Card, CheckListItem)
import Canbando.Model.Id (Id)
import Canbando.Model.List (List)
import Data.Array (take, zipWithA, (!!))
import Data.Maybe (fromJust)
import Data.Traversable (sequence)
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
  forall m. IdSupply m => Store m =>
  String -> Array String -> Array (Array CheckListItem) -> m List
initList name cardNames checklists = do
  id <- genId 'L'
  cards <- zipWithA (initCard id) cardNames checklists
  setItem id { id, name, cards: map _.id cards }
  pure { id, name, cards }

initCard ::
  forall m. IdSupply m => Store m =>
  Id -> String -> Array CheckListItem -> m Card
initCard list title checklist = do
  id <- genId 'C'
  setItem id { id, title, list, labels: [], checklist: checklist }
  pure { id, title, labels: [], checklist: checklist }

todo :: forall m. IdSupply m => Store m => Array Id -> m List
todo labels = do
  let checklists =
        [ []
        , [ { name: "Todo #4.1", done: true }
          , { name: "Todo #4.2", done: true }
          , { name: "Todo #4.3", done: false }
          , { name: "Todo #4.4", done: false }
          , { name: "Todo #4.5", done: false } ]
        , []
        , [ { name: "Todo #6.1", done: false }
          , { name: "Todo #6.2", done: false }
          , { name: "Todo #6.3", done: false } ]
        , [] ]
  lst <- initList "To Do" ["Task #3", "Task #4", "Task #5", "Task #6", "Task #7"] checklists
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
inProgress = initList "In progress" ["Task #1", "Task #2"] [ [], [] ]

done :: forall m. IdSupply m => Store m => m List
done = initList "Done" [] []

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
