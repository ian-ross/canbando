module Server.DB.Labels
  ( addLabel, getLabelInfo, setLabelInfo
  ) where

import Prelude

import Canbando.Model.Id (Id)
import Canbando.Model.Labels (LabelInfo)
import Data.Maybe (Maybe)
import MySQL.Connection (execute)
import MySQL.QueryValue (toQueryValue)
import Server.DB.Util (DBFun1, DBFun4, DBFun3, query1)
import Server.Env (db)


addLabel :: DBFun4 Id Id String String Unit
addLabel boardId newId name colour =
  db $ execute "INSERT INTO board_labels (id, board_id, name, colour) VALUES (?, ?, ?, ?)"
    [toQueryValue newId, toQueryValue boardId,
     toQueryValue name, toQueryValue colour]

getLabelInfo :: DBFun1 Id (Maybe LabelInfo)
getLabelInfo labelId =
    db $ query1 "SELECT id, name, colour FROM board_labels WHERE id = ?"
      [toQueryValue labelId]

setLabelInfo :: DBFun3 Id String String Unit
setLabelInfo labelId name colour =
  db $ execute "UPDATE board_labelss SET name = ?, colour = ? WHERE id = ?"
    [toQueryValue name, toQueryValue colour, toQueryValue labelId]
