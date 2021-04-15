module Server.Env
  ( Env(..), LogLevel(..), ResponseM
  , reader, db, tx
  ) where

import Prelude

import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, asks, runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import HTTPure (Response, Request)
import HTTPure as HTTPure
import MySQL.Connection (Connection)
import MySQL.Pool (Pool, withPool)
import MySQL.Transaction (withTransaction)


type ResponseM = ReaderT Env Aff Response

data LogLevel = Debug | Production

type Env =
  { logLevel :: LogLevel
  , db :: Pool
  }


reader :: Env -> (Request -> ResponseM) -> Request -> HTTPure.ResponseM
reader env r req = runReaderT (r req) env

db :: forall m a. MonadAff m => MonadAsk Env m => (Connection -> Aff a) -> m a
db f = do
  pool <- asks _.db
  liftAff $ flip withPool pool f

tx :: forall m a. MonadAff m => MonadAsk Env m => (Connection -> Aff a) -> m a
tx f = do
  pool <- asks _.db
  liftAff $ flip withPool pool (withTransaction f)
