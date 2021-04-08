module Server.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import HTTPure (Method(..), Request, ResponseM, ServerM, notFound, serve)
import HTTPure.Middleware (developmentLogFormat)
import MySQL.Connection (defaultConnectionInfo)
import MySQL.Pool (Pool, createPool, defaultPoolInfo)
import Server.Env (Env, LogLevel(..))
import Server.Handler.Board (deleteBoard, getBoard, listBoards, newBoard, newLabel, updateBoard)
import Server.Handler.Card (deleteCard, getCard, moveCard, newCard, updateCard)
import Server.Handler.List (deleteList, getList, moveList, newList, updateList)
import Server.Util (parseDetails)


-- BLEARGH. UPDATE THIS TO USE A ReaderT FOLLOWING THE "Custom stack"
-- EXAMPLE FROM THE HTTPure DOCUMENTATION.

router :: Env -> Request -> ResponseM

router env {path: ["boards"],                    method: Post,  body} = newBoard body
router env {path: ["boards"],                    method: Get        } = listBoards env
router env {path: ["boards", boardId], query,    method: Get        } = getBoard boardId (parseDetails query)
router env {path: ["boards", boardId],           method: Patch, body} = updateBoard boardId body
router env {path: ["boards", boardId],           method: Delete     } = deleteBoard boardId
router env {path: ["boards", boardId, "labels"], method: Post,  body} = newLabel boardId body
router env {path: ["boards", boardId, "lists"],  method: Post,  body} = newList boardId body

router env {path: ["lists", listId], query,      method: Get        } = getList listId  (parseDetails query)
router env {path: ["lists", listId],             method: Patch, body} = updateList listId body
router env {path: ["lists", listId],             method: Delete     } = deleteList listId
router env {path: ["lists", listId, "location"], method: Post,  body} = moveList listId body
router env {path: ["lists", listId, "cards"],    method: Post,  body} = newCard listId body

router env {path: ["cards", cardId],             method: Get        } = getCard cardId
router env {path: ["cards", cardId],             method: Patch, body} = updateCard cardId body
router env {path: ["cards", cardId],             method: Delete     } = deleteCard cardId
router env {path: ["cards", cardId, "location"], method: Post,  body} = moveCard cardId body

router _ _                                                            = notFound


main :: Effect Unit
main = do
  pool <- createPool'
  let env = { logLevel: Debug, db: pool }
  void $ server env

server :: Env -> ServerM
server env =
  serve 8080 (middleware $ router env) $
  log "Server running on port 8080..."
  where middleware = developmentLogFormat

createPool' :: Effect Pool
createPool' = createPool connInfo defaultPoolInfo
  where
    connInfo = defaultConnectionInfo
               { host = "localhost"
               , user = "canbando"
               , password = "canbando"
               , database = "canbando" }
