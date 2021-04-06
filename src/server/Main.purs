module Server.Main where

import Prelude

import Effect.Console (log)
import HTTPure (Method(..), Request, ResponseM, ServerM, notFound, serve)

import Server.Handler.Board (deleteBoard, getBoard, listBoards, newBoard, newLabel, updateBoard)
import Server.Handler.Card (deleteCard, getCard, moveCard, newCard, updateCard)
import Server.Handler.List (deleteList, getList, moveList, newList, updateList)
import Server.Util (parseDetails)


router :: Request -> ResponseM

router {path: ["boards"],                    method: Post,  body} = newBoard body
router {path: ["boards"],                    method: Get        } = listBoards
router {path: ["boards", boardId], query,    method: Get        } = getBoard boardId (parseDetails query)
router {path: ["boards", boardId],           method: Patch, body} = updateBoard boardId body
router {path: ["boards", boardId],           method: Delete     } = deleteBoard boardId
router {path: ["boards", boardId, "labels"], method: Post,  body} = newLabel boardId body
router {path: ["boards", boardId, "lists"],  method: Post,  body} = newList boardId body

router {path: ["lists", listId], query,      method: Get        } = getList listId  (parseDetails query)
router {path: ["lists", listId],             method: Patch, body} = updateList listId body
router {path: ["lists", listId],             method: Delete     } = deleteList listId
router {path: ["lists", listId, "location"], method: Post,  body} = moveList listId body
router {path: ["lists", listId, "cards"],    method: Post,  body} = newCard listId body

router {path: ["cards", cardId],             method: Get        } = getCard cardId
router {path: ["cards", cardId],             method: Patch, body} = updateCard cardId body
router {path: ["cards", cardId],             method: Delete     } = deleteCard cardId
router {path: ["cards", cardId, "location"], method: Post,  body} = moveCard cardId body

router _                                                          = notFound


main :: ServerM
main = serve 8080 router $ log "Server running on port 8080..."
