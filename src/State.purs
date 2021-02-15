module State where

type Card = { title :: String }

type List = { name :: String, cards :: Array Card }

type State = { lists :: Array List }

todo :: List
todo =
  { name: "To Do"
  , cards: [ { title: "Task #3" }
           , { title: "Task #4" }
           , { title: "Task #5" }
           , { title: "Task #6" }
           , { title: "Task #7" }
           ]
  }

inProgress :: List
inProgress =
  { name: "In progress"
  , cards: [ { title: "Task #1" }
           , { title: "Task #2" }
           ]
  }

done :: List
done =
  { name: "Done"
  , cards : [] }
