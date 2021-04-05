module Canbando.Capability.Store where

import Prelude

import Data.Maybe (Maybe)


class (Monad m) <= Store m where
  clearStore :: m Unit
  setItem :: forall a. String -> a -> m Unit
  removeItem :: String -> m Unit
  getItem :: forall a. String -> m (Maybe a)


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
