-- This is the module where we implement capabilities for our
-- application by defining an application monad (that we call `AppM`)
-- and defining type class instances for all of our capability
-- classes.
--
-- We also define all the other type class instances for `AppM` that
-- allow it to be used as a general application monad (so we can run
-- effects in `AppM`, basically).
--
-- Some of the application capabilities (e.g. `ManageList`,
-- `ManageBoard`) can be defined for any monad that implements the
-- "core" `IdSupply` and `Store` capabilities, so they're defined in a
-- more general way alongside the definitions of the capabilities.
-- This has the advantage of making it easier to write test code: we
-- define a "test application monad" that implements `IdSupply` and
-- `Store` in ways that make it easy to run tests, then we can write
-- test cases to test generic code implementing the `ManageList` and
-- `ManageBoard` capabilities.

module Canbando.AppM (AppM(..), runApp) where

import Prelude

import Canbando.Capability.IdSupply (class IdSupply, mkId)
import Canbando.Capability.Navigate (class Navigate)
import Canbando.Capability.Store (class Store, getItem, setItem)
import Canbando.Env (Env)
import Canbando.Routes (route)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, asks, runReaderT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Foreign (isNull, unsafeFromForeign, unsafeToForeign)
import Halogen (liftAff, liftEffect)
import Localforage as LF
import Routing.Duplex (print)


-- We're using the standard `ReaderT` pattern here: we have a
-- read-only application environment stored in a value of type `Env`
-- which we can access anywhere in code in our `AppM` monad. The
-- `AppM` monad wraps the `Aff` asynchronous effects monad so that we
-- can do DOM manipulations, HTTP requests and other effects.
newtype AppM a = AppM (ReaderT Env Aff a)

-- To run actiosn in our `AppM` monad, we just unwrap the newtype and
-- use the standard `runReaderT`, passing in the environment.
--
-- The type here is interesting: notice the curly arrow (`~>`). This
-- is a *natural transformation*, and in our case, it's defined so
-- that
--
-- ```
-- (~>) :: forall a. AppM a -> Aff a
-- ```
--
-- That means that we can map actions in `AppM` to actions in `Aff`
-- whatever type we're returning. You can think of this as meaning
-- that `runApp` only knows about and does things to do with the
-- relationship between `AppM` and `Aff`, but can't do anything (or
-- even really look at) the values that our `AppM` monadic actions
-- produce. Remember the "don't care, can't know" rule that goes with
-- universal quantification with `forall`!
runApp :: Env -> AppM ~> Aff
runApp env (AppM m) = runReaderT m env


-- We have to define a bunch of standard type class instances for our
-- `AppM` class. Fortunately, because `AppM` is a newtype, the
-- PureScript compiler will derive most of them for us without any
-- work on our part.
--
-- Notice that to get to `Monad` (which is the thing we really want),
-- we need instances for `Functor`, `Apply`, `Applicative` and `Bind`
-- as well, because of the way that the `Monad` type class is
-- decomposed into its more fundamental algebraic components in the
-- PureScript library type classes.
derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadAskAppM :: MonadAsk Env AppM


-- The first capability we're going to implement is an "ID supply",
-- which is just a way to get unique identifiers to use for labelling
-- cards, lists and boards. When we use a backend on a server, the
-- backend will be responsible for assigning IDs to things, but while
-- we're still working just in the browser, we need to do it
-- ourselves.
--
-- We'll just keep count of the next number to use for an ID in an
-- integer value stored in the browser's local storage.

-- This is the key we use to store the next ID value to use.
nextIdKey :: String
nextIdKey = "next-id"

instance idSupplyAppM :: IdSupply AppM where
  genId pfx =
    -- To generate a new ID, we get the next ID value to use...
    getItem nextIdKey >>= case _ of
      Nothing -> do
        -- If there's no value set up, we make it 1 and return an ID
        -- made from the prefix passed in and the number 1.
        setItem nextIdKey (unsafeToForeign 1)
        pure $ mkId pfx 1
      Just val -> do
        -- If we have a stored ID value, we increment it, store the
        -- new value and return an ID constructed from the prefix and
        -- the ID count.
        let id = val + 1
        setItem nextIdKey (unsafeToForeign id)
        pure $ mkId pfx id


-- Implementation of the `Store` capability: this is mostly about
-- interfacing with the PureScript `localforage` package, converting
-- back and forth between `Foreign` and other types, and throwing away
-- all the error information...
instance storeAppM :: Store AppM where
  clearStore = const unit <$> run LF.clear
  setItem id v =
    const unit <$>
    run \s -> LF.setItem s id (unsafeToForeign v) >>=
              pure <<< (const unit <$> _)
  removeItem id =
    const unit <$> run \s -> LF.removeItem s id
  getItem id =
    run \s -> LF.getItem s id <#> case _ of
      Left _ -> Nothing
      Right v -> if isNull v then Nothing else Just (unsafeFromForeign v)

-- Helper function to run actions that need access to a `Localforage`
-- store.
run :: forall m a.
       Monad m =>
       MonadAsk Env m =>
       MonadAff m =>
       (LF.Localforage -> Aff a) -> m a
run f = asks _.store >>= liftAff <<< f


instance navigateAppM :: Navigate AppM where
  navigate r = do
    nav <- asks _.nav
    liftEffect $ nav.pushState (unsafeToForeign {}) (print route r)
