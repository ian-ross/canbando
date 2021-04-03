module Canbando.Env (Env, LogLevel(..)) where

import Canbando.Capability.Resource.Labels (LabelEvent)
import Canbando.Model.Labels (Labels)
import Effect.Ref (Ref)
import Halogen.Subscription (Emitter, Listener)
import Localforage (Localforage)
import Routing.PushState (PushStateInterface)

data LogLevel = Debug | Production

type Env =
  { logLevel :: LogLevel
  , idSupply :: Ref Int
  , store :: Localforage
  , nav :: PushStateInterface
  , labels :: Ref Labels
  , labelEmitter :: Emitter LabelEvent
  , labelListener :: Listener LabelEvent
  }
