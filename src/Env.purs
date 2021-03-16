module Canbando.Env (Env, LogLevel(..)) where

import Effect.Ref (Ref)
import Localforage (Localforage)
import Routing.PushState (PushStateInterface)

data LogLevel = Debug | Production

type Env =
  { logLevel :: LogLevel
  , idSupply :: Ref Int
  , store :: Localforage
  , nav :: PushStateInterface }
