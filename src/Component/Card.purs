module Component.Card (Input, Output, State, Slot, component, newCard) where

import Prelude hiding (div)

import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval, raise)
import Halogen.HTML (HTML, a, div, text)
import Halogen.HTML.Properties (class_, href, id_)

import Component.Editable as Editable
import Component.Editable (EditAction, editable)
import CSS as CSS


type State = { id :: String
             , value :: String
             , edit :: String
             , editing :: Boolean }

type Input = { id :: String
             , title :: String }

data Output = TitleChanged String

data Action = Editing EditAction

type Slot id = forall query. H.Slot query Output id


component :: forall query m. MonadEffect m => Component HTML query Input Output m
component = mkComponent
       { initialState
       , render
       , eval : mkEval $ defaultEval { handleAction = handleAction }
       }


newCard :: String -> String -> Input
newCard id title = { id: id, title: title }


initialState :: Input -> State
initialState inp = { id: inp.id, value: inp.title, edit: "", editing: false }


render :: forall cs m. State -> ComponentHTML Action cs m
render c =
  div [id_ c.id, class_ CSS.card]
  (editable Editing c <> [a [class_ CSS.cardMenu, href "#"] [text "..."]])


handleAction :: forall m. MonadEffect m => Action -> HalogenM State Action () Output m Unit
handleAction action =
  case action of
    Editing editAction -> do
      Editable.handleAction editAction >>= case _ of
        Nothing -> pure unit
        Just s -> raise $ TitleChanged s
