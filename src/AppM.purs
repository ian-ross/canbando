module Canbando.AppM where

import Prelude

import Canbando.Capability.IdSupply (class IdSupply, hashId)
import Canbando.Capability.Resource.Board (class ManageBoard)
import Canbando.Capability.Resource.List (class ManageList)
import Canbando.Capability.Store (class Store, removeItem, setItem)
import Canbando.Env (Env)
import Canbando.Model.Card (newCard)
import Canbando.Model.List (newList, toListStore)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, asks, runReaderT)
import Data.Maybe (Maybe(..))
import Debug.Trace (traceM)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Foreign (unsafeToForeign)
import Halogen (liftAff)
import Localforage as LF
import Type.Equality (class TypeEquals, from)


newtype AppM a = AppM (ReaderT Env Aff a)


runApp :: Env -> AppM ~> Aff
runApp env (AppM m) = runReaderT m env


derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM


instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from


instance idSupplyAppM :: IdSupply AppM where
  genId = hashId


-- TODO:
--  * ERROR HANDLING
--  * MAKE SURE CARDS GET PUT INTO LISTS WHEN THEY'RE CREATED
--  * MAKE SURE LISTS GET PUT INTO BOARDS WHEN THEY'RE CREATED
--  * CHANGE API FOR moveCard TO HAVE FROM AND TO LISTS SO YOU CAN UPDATE BOTH

run :: forall m a. Monad m => MonadAsk Env m => MonadAff m =>
       (LF.Localforage -> Aff a) -> m a
run f = asks _.store >>= liftAff <<< f

instance storeAppM :: Store AppM where
  clearStore = run LF.clear
  setItem id val = run \s -> LF.setItem s id val
  removeItem id = run \s -> LF.removeItem s id
  getItem id = run \s -> LF.getItem s id


instance manageListAppM :: ManageList AppM where
  updateList id lst = do
    res <- setItem id (unsafeToForeign $ toListStore lst)
    traceM $ "UPDATE-LIST " <> id

  moveCard cardId listId idx =
    traceM $ "MOVE-CARD " <> cardId <> " -> " <> listId <> " @ " <> show idx

  addCard id = do
    card <- newCard
    res <- setItem card.id (unsafeToForeign card)
    traceM $ "ADD-CARD " <> card.id
    pure card

  updateCard id card = do
    res <- setItem id (unsafeToForeign card)
    traceM $ "UPDATE-CARD " <> id

  deleteCard id = do
    res <- removeItem id
    traceM $ "DELETE-CARD " <> id


instance manageBoardAppM :: ManageBoard AppM where
  getBoard id = do
    traceM $ "GET-BOARD " <> id
    pure Nothing

  deleteBoard id = do
    res <- removeItem id
    traceM $ "DELETE-BOARD " <> id

  moveList listId id idx =
    traceM $ "MOVE-LIST " <> listId <> " -> " <> id <> " @ " <> show idx

  addList id = do
    lst <- newList
    res <- setItem lst.id (unsafeToForeign $ toListStore lst)
    traceM $ "ADD-LIST " <> lst.id
    pure lst

  deleteList id = do
    res <- removeItem id
    traceM $ "DELETE-LIST " <> id
