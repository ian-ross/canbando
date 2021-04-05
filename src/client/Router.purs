module Canbando.Router
  ( State, Query(..), component
  ) where

import Prelude

import Canbando.Capability.IdSupply (class IdSupply)
import Canbando.Capability.Navigate (class Navigate)
import Canbando.Capability.Resource.Labels (class EditLabels, class GetLabels, class SetLabels)
import Canbando.Capability.Store (class Store)
import Canbando.Env (Env)
import Canbando.Model.Id (Id)
import Canbando.Page.Board as Board
import Canbando.Page.Home as Home
import Canbando.Page.ResetTestData as ResetTestData
import Canbando.Routes (Route(..))
import Control.Monad.Reader.Trans (class MonadAsk)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval, put)
import Halogen.HTML (HTML, div_, slot, text)
import Type.Proxy (Proxy(..))


type State = Maybe Route

type Input = Unit

data Query a = Nav Route a

data Action = Initialize | Receive

type Slots =
  ( home :: Home.Slot
  , board :: Board.Slot Id
  , resetTestData :: ResetTestData.Slot )


component ::
  forall m.
  MonadAff m =>
  MonadAsk Env m =>
  Navigate m =>
  MonadAsk Env m =>
  IdSupply m => Store m =>
  GetLabels m =>
  SetLabels m =>
  EditLabels m =>
  Navigate m =>
  Component Query Input Void m
component =  mkComponent
             { initialState: const (Just Home)
             , render
             , eval: mkEval $ defaultEval { handleQuery = handleQuery
                                          , handleAction = handleAction
                                          , receive = const (Just Receive)
                                          , initialize = Just Initialize
                                          }
             }

handleAction :: forall m. Action -> HalogenM State Action Slots Void m Unit
handleAction action = pure unit

handleQuery :: forall m a. Query a -> HalogenM State Action Slots Void m (Maybe a)
handleQuery (Nav route a) = do
  put $ Just route
  pure (Just a)

render ::
  forall m.
  MonadAff m =>
  MonadAsk Env m =>
  IdSupply m =>
  Store m =>
  GetLabels m =>
  SetLabels m =>
  EditLabels m =>
  Navigate m =>
  State -> ComponentHTML Action Slots m
render = case _ of
  Just route -> case route of
    Home -> slot (Proxy :: _ "home") unit Home.component unit absurd
    Board id -> slot (Proxy :: _ "board") id Board.component id absurd
    ResetTestData -> slot (Proxy :: _ "resetTestData") unit ResetTestData.component unit absurd
  Nothing -> notFound

notFound :: forall w i. HTML w i
notFound = div_ [ text "Page not found!"]
