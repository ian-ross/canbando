module Server.DB.Util
  ( DBFun, DBFun1, DBFun2, DBFun3, DBFun4, DBUtil, DBUtil1, DBUtil3
  , query1
  , deleteEntity
  ) where

import Prelude

import Canbando.Model.Id (Id)
import Control.Monad.Reader.Trans (class MonadAsk)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import MySQL.Connection (Connection, query)
import MySQL.QueryValue (QueryValue, toQueryValue)
import Server.Env (Env, db)
import Simple.JSON (class ReadForeign)


-- "Normal" database functions.
type DBFun a = forall m. MonadAff m => MonadAsk Env m => m a
type DBFun1 p a = forall m. MonadAff m => MonadAsk Env m => p -> m a
type DBFun2 p q a = forall m. MonadAff m => MonadAsk Env m => p -> q -> m a
type DBFun3 p q r a = forall m. MonadAff m => MonadAsk Env m => p -> q -> r -> m a
type DBFun4 p q r s a = forall m. MonadAff m => MonadAsk Env m => p -> q -> r -> s -> m a

-- "Utility" database function that we use inside transactions.
type DBUtil a = forall m. MonadAff m => Connection -> Id -> m a
type DBUtil1 p a = forall m. MonadAff m => Connection -> Id -> p -> m a
type DBUtil3 p q r a =
  forall m. MonadAff m => Connection -> Id -> p -> q -> r -> m a

query1 :: forall a. ReadForeign a =>
          String -> Array QueryValue -> Connection -> Aff (Maybe a)
query1 sql qs conn = query sql qs conn <#> case _ of
  [v] -> Just v
  _ -> Nothing

deleteEntity :: DBFun2 String Id (Array { id :: String })
deleteEntity table id = do
  let sql = "DELETE FROM " <> table <> " WHERE id = ? RETURNING id"
  db $ query sql [toQueryValue id]
