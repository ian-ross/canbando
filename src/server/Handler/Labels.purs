module Server.Handler.Labels
  ( newLabel, updateLabel, deleteLabel
  ) where

import Prelude

import Canbando.Model.Labels (labelCreateInfoCodec, labelInfoCodec)
import Data.Maybe (Maybe(..))
import HTTPure (notFound)
import Server.DB.Board (getBoardInfo) as DB
import Server.DB.Labels (addLabel, getLabelInfo, setLabelInfo) as DB
import Server.Env (ResponseM)
import Server.Handler (deleteEntity, genId, okJson, withJson)


newLabel :: String -> String -> ResponseM
newLabel boardId body = withJson body labelCreateInfoCodec \info ->
  DB.getBoardInfo boardId >>= case _ of
    Nothing -> notFound
    Just bi -> do
      newId <- genId "L"
      DB.addLabel boardId newId info.name info.colour
      let label = { id: newId, name: info.name, colour: info.colour }
      okJson labelInfoCodec label

updateLabel :: String -> String -> ResponseM
updateLabel labelId body = withJson body labelCreateInfoCodec \update ->
  DB.getLabelInfo labelId >>= case _ of
    Nothing -> notFound
    Just li -> do
      DB.setLabelInfo labelId update.name update.colour
      let list = { id: labelId, name: update.name, colour: update.colour }
      okJson labelInfoCodec list

deleteLabel :: String -> ResponseM
deleteLabel = deleteEntity "board_labels"
