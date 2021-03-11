module Canbando.AppM where

import Prelude

import Canbando.Capability.IdSupply (class IdSupply, hashId)
import Canbando.Capability.Resource.Board (class ManageBoard)
import Canbando.Capability.Resource.List (class ManageList)
import Canbando.Env (Env)
import Canbando.Model.Card (newCard)
import Canbando.Model.List (newList)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, asks, runReaderT)
import Data.Maybe (Maybe(..))
import Debug.Trace (traceM)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
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

instance manageListAppM :: ManageList AppM where
  updateList id lst = traceM $ "UPDATE-LIST " <> id
  moveCard cardId listId idx =
    traceM $ "MOVE-CARD " <> cardId <> " -> " <> listId <> " @ " <> show idx
  addCard id = do
    card <- newCard
    traceM $ "ADD-CARD " <> card.id
    pure card
  updateCard id crd = traceM $ "UPDATE-CARD " <> id
  deleteCard id = traceM $ "DELETE-CARD " <> id

instance manageBoardAppM :: ManageBoard AppM where
  getBoard id = do
    traceM $ "GET-BOARD " <> id
    pure Nothing
  deleteBoard id = traceM $ "DELETE-BOARD " <> id
  moveList listId id idx =
    traceM $ "MOVE-LIST " <> listId <> " -> " <> id <> " @ " <> show idx
  addList id = do
    lst <- newList
    traceM $ "ADD-LIST " <> lst.id
    pure lst
  deleteList id = traceM $ "DELETE-LIST " <> id
