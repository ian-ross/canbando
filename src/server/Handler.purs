module Server.Handler
  ( log, db
  , withJson, okJson
  , genId, deleteEntity
  ) where

import Prelude

import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, asks)
import Data.Argonaut (jsonParser, stringify)
import Data.Array (head)
import Data.Codec.Argonaut (JsonCodec, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Data.Monoid (power)
import Data.String (length)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import HTTPure (badRequest, noContent, notFound, ok)
import MySQL.Connection (Connection, query, query_)
import MySQL.Pool (withPool)
import MySQL.QueryValue (toQueryValue)
import Server.Env (Env, ResponseM)

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

genId :: String -> ReaderT Env Aff String
genId prefix = do
  res :: Array { next :: Int } <- db (query_ "SELECT NEXT VALUE FOR idgen AS next")
  let nexts = show $ fromMaybe 0 $ _.next <$> head res
      pad = power "0" (8 - length nexts)
  pure $ prefix <> pad <> nexts

deleteEntity :: String -> String -> ResponseM
deleteEntity table id = do
  let sql = "DELETE FROM " <> table <> " WHERE id = ? RETURNING id"
  deleted :: Array { id :: String } <- db $ query sql [toQueryValue id]
  if deleted == [{ id }] then noContent else notFound
