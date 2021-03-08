module Canbando.State where

import Canbando.Model.List (List)


type BoardState = { nextID :: Int
                  , lists :: Array List }

todo :: List
todo =
  { name: "To Do"
  , id: "todo"
  , nextID: 6
  , cards: [ { id: "todo1", title: "Task #3" }
           , { id: "todo2", title: "Task #4" }
           , { id: "todo3", title: "Task #5" }
           , { id: "todo4", title: "Task #6" }
           , { id: "todo5", title: "Task #7" }
           ]
  }

inProgress :: List
inProgress =
  { name: "In progress"
  , id: "prog"
  , nextID: 3
  , cards: [ { id: "prog1", title: "Task #1" }
           , { id: "prog2", title: "Task #2" }
           ]
  }

done :: List
done =
  { name: "Done"
  , id: "done"
  , nextID: 1
  , cards : [] }
