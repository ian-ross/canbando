module Server.Handler
  ( log
  , withJson, okJson, jsonQuery, jsonsQuery
  , genId, deleteEntity
  ) where

import Prelude

import Control.Monad.Reader.Trans (ReaderT)
import Data.Argonaut (jsonParser, stringify)
import Data.Array (head)
import Data.Codec.Argonaut (JsonCodec, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (power)
import Data.String (length)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import HTTPure (Response, badRequest, noContent, notFound, ok)
import MySQL.Connection (query_)
import Server.DB as DB
import Server.Env (Env, ResponseM, db)

log :: forall m. MonadEffect m => String -> m Unit
log = liftEffect <<< Console.log

okJson :: forall m t. MonadAff m => JsonCodec t -> t -> m Response
okJson codec val =  ok $ stringify $ CA.encode codec val

withJson :: forall t. String -> JsonCodec t -> (t -> ResponseM) -> ResponseM
withJson body codec process =
  case jsonParser body of
    Left err -> badRequest err
    Right json -> case CA.decode codec json of
      Left err -> badRequest $ printJsonDecodeError err
      Right info -> process info

jsonQuery :: forall a. ReaderT Env Aff (Maybe a) -> JsonCodec a -> ResponseM
jsonQuery q codec = q >>= case _ of
  Nothing -> notFound
  Just res -> okJson codec res

jsonsQuery :: forall a. ReaderT Env Aff (Array a) -> JsonCodec a -> ResponseM
jsonsQuery q codec = q >>= okJson (CA.array codec)

genId :: String -> ReaderT Env Aff String
genId prefix = do
  res :: Array { next :: Int } <- db (query_ "SELECT NEXT VALUE FOR idgen AS next")
  let nexts = show $ fromMaybe 0 $ _.next <$> head res
      pad = power "0" (8 - length nexts)
  pure $ prefix <> pad <> nexts

deleteEntity :: String -> String -> ResponseM
deleteEntity table id = do
  deleted <- DB.deleteEntity table id
  if deleted == [{ id }] then noContent else notFound
