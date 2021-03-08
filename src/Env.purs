module Canbando.Env (Env, LogLevel(..)) where

import Effect.Ref (Ref)

data LogLevel = Debug | Production

type Env =
  { logLevel :: LogLevel
  , idSupply :: Ref Int }
