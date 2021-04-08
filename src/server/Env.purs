module Server.Env
  ( Env(..), LogLevel(..), ResponseM
  , reader
  ) where

import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Effect.Aff (Aff)
import HTTPure (Response, Request)
import HTTPure as HTTPure
import MySQL.Pool (Pool)


type ResponseM = ReaderT Env Aff Response

data LogLevel = Debug | Production

type Env =
  { logLevel :: LogLevel
  , db :: Pool
  }


reader :: Env -> (Request -> ResponseM) -> Request -> HTTPure.ResponseM
reader env r req = runReaderT (r req) env
