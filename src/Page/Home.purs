module Canbando.Page.Home where

import Prelude hiding (div)

import Canbando.Capability.IdSupply (class IdSupply)
import Canbando.Capability.Navigate (class Navigate)
import Canbando.Capability.Resource.Board (class ManageBoard, loadBoards)
import Canbando.Capability.Store (class Store)
import Canbando.Component.BoardTile as BoardTile
import Canbando.Component.Header (header)
import Canbando.Model.Board (BoardStore)
import Canbando.Model.Id (Id)
import Canbando.Util (wrap)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval, put)
import Halogen as H
import Halogen.HTML (div_, slot, text)
import Network.RemoteData (RemoteData(..))
import Type.Proxy (Proxy(..))


data Action
  = Initialize
  | TileAction BoardTile.Output

type State = RemoteData String (Array BoardStore)

type Slot = forall query. H.Slot query Void Unit

type Slots = ( tile :: BoardTile.Slot Id )

_tile :: Proxy "tile"
_tile = Proxy

component ::
  forall q o m.
  MonadEffect m =>
  ManageBoard m =>
  IdSupply m =>
  Store m =>
  Navigate m =>
  Component q Unit o m
component =
  mkComponent
    { initialState: const NotAsked
    , render
    , eval: mkEval $ defaultEval { handleAction = handleAction
                                 , initialize = Just Initialize }
    }

render ::
  forall m.
  Navigate m =>
  State -> ComponentHTML Action Slots m
render state = div_ [header "Canbando!", contents]
  where contents =
          case state of
               NotAsked -> text "Not asked yet!"
               Loading -> text "Loading..."
               Failure err -> text $ "FAILED: " <> err
               Success ts -> wrap (map onetile ts)
        onetile brd = slot _tile brd.id BoardTile.component brd TileAction

handleAction ::
  forall cs o m.
  MonadEffect m =>
  ManageBoard m =>
  IdSupply m =>
  Store m =>
  Action -> HalogenM State Action cs o m Unit
handleAction = case _ of
  Initialize -> do
    put Loading
    boards <- lift loadBoards
    put $ Success boards
  TileAction act -> pure unit
