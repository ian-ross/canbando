module Canbando.Component.BoardTile where

import Prelude hiding (div)

import Debug.Trace (traceM)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, get, mkComponent, mkEval)
import Halogen as H
import Halogen.HTML (div, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, classes)

import Canbando.CSS as CSS
import Canbando.Model.Board (BoardStore)
import Canbando.Model.Id (Id)


type State = { id :: Id, name :: String }

data Output = BoardTileClicked Id

data Action = Clicked

type Slot id = forall query. H.Slot query Output id

initialState :: BoardStore -> State
initialState bs = { id: bs.id, name: bs.name }

component ::
  forall query m. Component query BoardStore Output m
component =
  mkComponent
  { initialState: initialState
  , render
  , eval : mkEval $ defaultEval { handleAction = handleAction }
  }

render :: forall cs m. State -> ComponentHTML Action cs m
render s =
  div [ classes [CSS.boardTile, CSS.card]
      , onClick (const Clicked) ]
  [ div [ class_ CSS.tileInner ] [ text s.name ] ]

handleAction ::
  forall cs o m. Action -> HalogenM State Action cs o m Unit
handleAction =
  case _ of
    Clicked -> do
      s <- get
      traceM $ "CLICKED " <> s.id
      pure unit