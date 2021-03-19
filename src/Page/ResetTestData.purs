module Canbando.Page.ResetTestData where

import Prelude hiding (div)

import Canbando.Capability.IdSupply (class IdSupply)
import Canbando.Capability.Resource.Board (class ManageBoard)
import Canbando.Capability.Store (class Store)
import Canbando.Component.Header (header)
import Canbando.TestInit (initTestStore)
import Canbando.Util (wrap)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval)
import Halogen as H
import Halogen.HTML (button, div_, text)
import Halogen.HTML.Events (onClick)


data Action = DoReset

type State = Unit

type Slot = forall query. H.Slot query Void Unit

component ::
  forall q o m.
  MonadEffect m =>
  ManageBoard m => IdSupply m => Store m =>
  Component q State o m
component =
  mkComponent
    { initialState: const unit
    , render
    , eval: mkEval $ defaultEval { handleAction = handleAction }
    }

render ::
  forall cs m.
  State -> ComponentHTML Action cs m
render state =
  div_ [ header Nothing
       , wrap [ button [onClick (const DoReset)] [text "Reset test data"] ] ]

handleAction ::
  forall cs o m.
  IdSupply m => Store m =>
  Action -> HalogenM State Action cs o m Unit
handleAction = case _ of
  DoReset -> initTestStore
