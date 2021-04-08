module Server.Handler.Board
  ( newBoard, listBoards, getBoard, updateBoard, deleteBoard, newLabel
  ) where

import Prelude

import Canbando.Model.Board (BoardInfo, boardCreateInfoCodec, boardInfoCodec)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, asks, lift)
import Data.Argonaut (jsonParser, stringify)
import Data.Array (head)
import Data.Codec.Argonaut (JsonCodec, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Maybe (Maybe, fromMaybe)
import Data.Monoid (power)
import Data.String (length)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import HTTPure (badRequest, ok)
import MySQL.Connection (Connection, execute, query_)
import MySQL.Pool (withPool)
import MySQL.QueryValue (toQueryValue)
import Server.Env (Env, ResponseM)
import Server.Util (Detail)

log :: forall m. MonadEffect m => String -> m Unit
log = liftEffect <<< Console.log


db :: forall m a. MonadAff m => MonadAsk Env m => (Connection -> Aff a) -> m a
db f = do
  pool <- asks _.db
  liftAff $ flip withPool pool f

okJson :: forall t. JsonCodec t -> t -> ResponseM
okJson codec val =  ok $ stringify $ CA.encode codec val

withJson :: forall t. String -> JsonCodec t -> (t -> ResponseM) -> ResponseM
withJson body codec process =
  case jsonParser body of
    Left err -> badRequest err
    Right json -> case CA.decode codec json of
      Left err -> badRequest $ printJsonDecodeError err
      Right info -> process info

newBoard :: String -> ResponseM
newBoard body = withJson body boardCreateInfoCodec \info -> do
  newId <- genId "B"
  db $ execute "INSERT INTO boards (id, name, bg_colour) VALUES (?, ?, ?)"
    [ toQueryValue newId, toQueryValue info.name, toQueryValue info.bgColour]
  let board = { id: newId, name: info.name, bgColour: info.bgColour }
  okJson boardInfoCodec board

listBoards :: ResponseM
listBoards = do
  boards :: Array BoardInfo <- db $ query_
    "SELECT id, name, bg_colour as bgColour FROM boards"
  okJson (CA.array boardInfoCodec) boards

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


genId :: String -> ReaderT Env Aff String
genId prefix = do
  res :: Array { next :: Int } <- db (query_ "SELECT NEXT VALUE FOR idgen AS next")
  let nexts = show $ fromMaybe 0 $ _.next <$> head res
      pad = power "0" (8 - length nexts)
  pure $ prefix <> pad <> nexts
