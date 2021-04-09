module Server.Handler
  ( log, db
  , withJson, okJson
  ) where

import Prelude

import Control.Monad.Reader.Trans (class MonadAsk, asks)
import Data.Argonaut (jsonParser, stringify)
import Data.Codec.Argonaut (JsonCodec, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import HTTPure (badRequest, ok)
import MySQL.Connection (Connection)
import MySQL.Pool (withPool)
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
