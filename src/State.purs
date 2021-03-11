module Canbando.State (initBoard) where

import Canbando.Model.Board (Board)
import Canbando.Model.List (List)


initBoard :: Board
initBoard = { id: "B0000001"
            , name: "Test board"
            , lists: [todo, inProgress, done] }

todo :: List
todo =
  { name: "To Do"
  , id: "todo"
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
  , cards: [ { id: "prog1", title: "Task #1" }
           , { id: "prog2", title: "Task #2" }
           ]
  }

done :: List
done =
  { name: "Done"
  , id: "done"
  , cards : [] }
