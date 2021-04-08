module Server.Handler.Board
  ( newBoard, listBoards, getBoard, updateBoard, deleteBoard, newLabel
  ) where

import Prelude

import Canbando.Model.Board (BareBoardInfo, bareBoardCodec)
import Data.Argonaut (stringify)
import Data.Codec.Argonaut as CA
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import HTTPure (ResponseM, ok)
import MySQL.Connection (Connection, query_)
import MySQL.Pool (withPool)
import Server.Env (Env)
import Server.Util (Detail)


withDB :: forall a. Env -> (Connection -> Aff a) -> Aff a
withDB env = flip withPool env.db

newBoard :: String -> ResponseM
newBoard body =
  ok "NEW BOARD"

listBoards :: Env -> ResponseM
listBoards env = do
  liftEffect $ log "LIST BOARDS"
  boards :: Array BareBoardInfo <- withDB env $ query_
    "SELECT id, name, bg_colour as bgColour FROM boards"
  let res = CA.encode (CA.array bareBoardCodec) boards
  ok $ stringify res

getBoard :: String -> Maybe Detail -> ResponseM
getBoard boardId detail =
  ok $ "GET BOARD " <> boardId <> " (detail=" <> show detail <> ")"

updateBoard :: String -> String -> ResponseM
updateBoard boardId body =
  ok $ "UPDATE BOARD " <> boardId

deleteBoard :: String -> ResponseM
deleteBoard boardId =
  ok $ "DELETE BOARD " <> boardId

newLabel :: String -> String -> ResponseM
newLabel boardId body =
  ok $ "NEW LABEL FOR " <> boardId
