module Server.Handler.Labels
  ( deleteLabel
  ) where

import Server.Env (ResponseM)
import Server.Handler (deleteEntity)


deleteLabel :: String -> ResponseM
deleteLabel = deleteEntity "board_labels"
