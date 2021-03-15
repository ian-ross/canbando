module Canbando.Page.Home where

import Prelude hiding (div)

import Canbando.CSS as CSS
import Canbando.Component.BoardTile as BoardTile
import Canbando.Component.Header (header)
import Canbando.Model.Board (BoardStore)
import Canbando.Model.Id (Id)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval)
import Halogen.HTML (div, div_, main, slot)
import Halogen.HTML.Properties (class_, classes)
import Type.Proxy (Proxy(..))


data Action = TileAction BoardTile.Output

type State = Array BoardStore

type Slots = ( tile :: BoardTile.Slot Id )

_tile :: Proxy "tile"
_tile = Proxy

initialState :: Array BoardStore -> State
initialState = identity

component :: forall q o m. Component q State o m
component =
  mkComponent
    { initialState
    , render
    , eval: mkEval $ defaultEval { handleAction = handleAction }
    }

wrap :: forall cs m. Array (ComponentHTML Action cs m) -> ComponentHTML Action cs m
wrap tiles =
  main [class_ CSS.flexShrink0]
  [div [classes [ CSS.containerFluid, CSS.dFlex
                , CSS.flexRow, CSS.alignItemsStart ] ]
    tiles
  ]

render ::
  forall m.
  State -> ComponentHTML Action Slots m
render state = div_ [header "Canbando!", wrap tiles]
  where tiles = map onetile state
        onetile brd = slot _tile brd.id BoardTile.component brd TileAction

handleAction ::
  forall cs o m. Action -> HalogenM State Action cs o m Unit
handleAction _ = pure unit
