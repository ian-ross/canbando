module Canbando.Component.BoardTile
  ( Slot, component
  ) where

import Prelude hiding (div)

import Canbando.CSS as CSS
import Canbando.Capability.Navigate (class Navigate, navigate)
import Canbando.Model.Board (BoardStore)
import Canbando.Model.Id (Id)
import Canbando.Routes (Route(..))
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, gets, mkComponent, mkEval)
import Halogen as H
import Halogen.HTML (div, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, classes)


type State = { id :: Id, name :: String }

data Action = Clicked

type Slot id = forall query. H.Slot query Void id

initialState :: BoardStore -> State
initialState bs = { id: bs.id, name: bs.name }

component ::
  forall query m.
  Navigate m =>
  Component query BoardStore Void m
component =
  mkComponent
  { initialState: initialState
  , render
  , eval : mkEval $ defaultEval { handleAction = handleAction }
  }

render :: forall m. State -> ComponentHTML Action () m
render s =
  div [ classes [CSS.boardTile, CSS.card]
      , onClick (const Clicked) ]
  [ div [ class_ CSS.tileInner ] [ text s.name ] ]

handleAction ::
  forall cs o m.
  Navigate m =>
  Action -> HalogenM State Action cs o m Unit
handleAction =
  case _ of
    Clicked -> navigate <<< Board =<< gets _.id
