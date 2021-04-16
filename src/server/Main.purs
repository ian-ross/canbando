module Server.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import HTTPure (Method(..), Request, ServerM, notFound, serve)
import HTTPure.Middleware (developmentLogFormat)
import MySQL.Connection (defaultConnectionInfo)
import MySQL.Pool (Pool, createPool, defaultPoolInfo)
import Server.Env (Env, LogLevel(..), ResponseM, reader)
import Server.Handler.Board (deleteBoard, getBoard, listBoards, newBoard, updateBoard)
import Server.Handler.Card (deleteCard, getCard, moveCard, newCard, updateCard)
import Server.Handler.Labels (newLabel, deleteLabel, updateLabel)
import Server.Handler.List (deleteList, getList, moveList, newList, updateList)
import Server.Util (withDetails)


router :: Request -> ResponseM

router {path: ["boards"],                    method: Post,  body} = newBoard body
router {path: ["boards"],                    method: Get        } = listBoards
router {path: ["boards", boardId], query,    method: Get        } = withDetails query $ getBoard boardId
router {path: ["boards", boardId],           method: Patch, body} = updateBoard boardId body
router {path: ["boards", boardId],           method: Delete     } = deleteBoard boardId
router {path: ["boards", boardId, "labels"], method: Post,  body} = newLabel boardId body
router {path: ["boards", boardId, "lists"],  method: Post,  body} = newList boardId body

router {path: ["lists", listId], query,      method: Get        } = withDetails query $ getList listId
router {path: ["lists", listId],             method: Patch, body} = updateList listId body
router {path: ["lists", listId],             method: Delete     } = deleteList listId
router {path: ["lists", listId, "location"], method: Post,  body} = moveList listId body
router {path: ["lists", listId, "cards"],    method: Post,  body} = newCard listId body

router {path: ["cards", cardId],             method: Get        } = getCard cardId
router {path: ["cards", cardId],             method: Patch, body} = updateCard cardId body
router {path: ["cards", cardId],             method: Delete     } = deleteCard cardId
router {path: ["cards", cardId, "location"], method: Post,  body} = moveCard cardId body

router {path: ["labels", labelId],           method: Delete     } = deleteLabel labelId
router {path: ["labels", labelId],           method: Patch, body} = updateLabel labelId body

router _                                                          = notFound


main :: Effect Unit
main = do
  pool <- createPool'
  let env = { logLevel: Debug, db: pool }
  void $ server env

server :: Env -> ServerM
server env = serve 8080 (mware router) $ log "Server running on port 8080..."
  where mware = developmentLogFormat <<< reader env

createPool' :: Effect Pool
createPool' = createPool connInfo defaultPoolInfo
  where
    connInfo = defaultConnectionInfo
               { host = "localhost"
               , user = "canbando"
               , password = "canbando"
               , database = "canbando" }
