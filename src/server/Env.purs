module Server.Env
  ( Env(..), LogLevel(..)
  ) where

import MySQL.Pool (Pool)


data LogLevel = Debug | Production

type Env =
  { logLevel :: LogLevel
  , db :: Pool
  }
