module Canbando.Page.Home where

import Prelude hiding (div)

import Canbando.CSS as CSS
import Canbando.Capability.IdSupply (class IdSupply)
import Canbando.Capability.Navigate (class Navigate)
import Canbando.Capability.Resource.Board (class ManageBoard, addBoard, loadBoards)
import Canbando.Capability.Store (class Store)
import Canbando.Component.BoardTile as BoardTile
import Canbando.Component.Header (header)
import Canbando.Component.Icon (icon)
import Canbando.Model.Board (BoardStore, toBoardStore)
import Canbando.Model.Id (Id)
import Canbando.Util (containerRow, wrapCol)
import Control.Monad.Trans.Class (lift)
import Data.Array (cons, snoc)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval, modify_, put)
import Halogen as H
import Halogen.HTML (button, div_, slot, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes)
import Network.RemoteData (RemoteData(..))
import Type.Proxy (Proxy(..))


data Action
  = Initialize
  | AddBoard

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
render state = div_ [header Nothing, contents]
  where contents =
          case state of
               NotAsked -> text "Not asked yet!"
               Loading -> text "Loading..."
               Failure err -> text $ "FAILED: " <> err
               Success ts -> wrapCol (addBoardButton `cons`
                                      [containerRow (map onetile ts)])
        onetile brd = slot _tile brd.id BoardTile.component brd absurd
        addBoardButton =
          button [classes [ CSS.btn, CSS.btnSecondary
                          , CSS.rounded, CSS.addNewBoard
                          , CSS.flexGrow0, CSS.flexShrink0],
                  onClick (const AddBoard)]
          [icon "bi-plus-circle", text "Add new board"]

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
  AddBoard -> do
    brd <- toBoardStore <$> lift addBoard
    modify_ \s -> (_ `snoc` brd) <$> s
