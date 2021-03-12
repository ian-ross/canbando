module Canbando.Capability.Store where

import Prelude

import Data.Either (Either)
import Effect.Exception (Error)
import Foreign (Foreign)


class (Monad m) <= Store m where
  clearStore :: m (Either Error Unit)
  setItem :: String -> Foreign -> m (Either Error Foreign)
  removeItem :: String -> m (Either Error Unit)
  getItem :: String -> m (Either Error Foreign)


-- Cards
--
-- <card-id> ->
--   { id: "<card-id>", title: "<title>" }
--
--
-- Lists
--
-- <list-id> ->
--   { id: "<list-id>", name: "<name>",
--     cards: [ "<card-id-1>", "<card-id-2>", "<card-id-3>", ... ] }
--
--
-- Boards
--
-- <board-id> ->
--   { id: "<board-id>", name: "<name>",
--     lists: [ "<list-id-1>", "<list-id-2>", "<list-id-3>", ... ] }
