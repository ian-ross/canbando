module Canbando.TestInit (initTestStore) where

import Prelude

import Canbando.AppM (AppM)
import Canbando.Capability.IdSupply (genId)
import Canbando.Capability.Store (setItem)
import Canbando.Model.Board (Board)
import Canbando.Model.Card (Card)
import Canbando.Model.List (List)
import Data.Traversable (traverse)
import Foreign (unsafeToForeign)


initTestStore :: AppM Board
initTestStore = do
  id <- genId 'B'
  list1 <- todo
  list2 <- inProgress
  list3 <- done
  let saveBoard =
        { id
        , name: "Test board"
        , lists: [list1.id, list2.id, list3.id] }
  _ <- setItem id (unsafeToForeign saveBoard)
  pure { id
        , name: "Test board"
        , lists: [list1, list2, list3] }

initList :: String -> Array String -> AppM List
initList name cardNames = do
  id <- genId 'L'
  cards <- traverse initCard cardNames
  let saveList = { id, name, cards: map _.id cards }
      retList = { id, name, cards }
  _ <- setItem id (unsafeToForeign saveList)
  pure retList

initCard :: String -> AppM Card
initCard title = do
  id <- genId 'C'
  let card = { id, title }
  _ <- setItem id (unsafeToForeign card)
  pure card

todo :: AppM List
todo = initList "To Do" [ "Task #3", "Task #4", "Task #5", "Task #6", "Task #7" ]

inProgress :: AppM List
inProgress = initList "In progress" [ "Task #1", "Task #2" ]

done :: AppM List
done = initList "Done" []
