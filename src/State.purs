module State where

import Component.List as List

type State = { lists :: Array List.State }

todo :: List.State
todo =
  { name: "To Do"
  , cards: [ { title: "Task #3" }
           , { title: "Task #4" }
           , { title: "Task #5" }
           , { title: "Task #6" }
           , { title: "Task #7" }
           ]
  }

inProgress :: List.State
inProgress =
  { name: "In progress"
  , cards: [ { title: "Task #1" }
           , { title: "Task #2" }
           ]
  }

done :: List.State
done =
  { name: "Done"
  , cards : [] }
