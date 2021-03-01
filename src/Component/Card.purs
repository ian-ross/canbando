module Component.Card (Id, Input, Output(..), State, Slot, component, newCard) where

import Prelude hiding (div)

import CSS as CSS
import Component.Editable (EditAction, editable)
import Component.Editable as Editable
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, gets, mkComponent, mkEval, raise)
import Halogen as H
import Halogen.HTML (HTML, a, div, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, href, id_)


type Id = String

type State = { id :: Id
             , value :: String
             , edit :: String
             , editing :: Boolean }

type Input = { id :: Id
             , title :: String }

data Output = TitleChanged Id String
            | CardDeleted Id

data Action = Editing EditAction
            | DeleteCard

type Slot id = forall query. H.Slot query Output id


component :: forall query m. MonadEffect m => Component HTML query Input Output m
component = mkComponent
       { initialState: initialState
       , render
       , eval : mkEval $ defaultEval { handleAction = handleAction }
       }


newCard :: Int -> String -> Input
newCard id title = { id: "C" <> show id, title: title }


initialState :: Input -> State
initialState inp = { id: inp.id, value: inp.title, edit: "", editing: false }


render :: forall cs m. State -> ComponentHTML Action cs m
render c =
  div [id_ c.id, class_ CSS.card]
  [editable Editing c, a [class_ CSS.cardMenu, href "#", onClick \_ -> Just DeleteCard] [text "..."]]


handleAction :: forall m. MonadEffect m => Action -> HalogenM State Action () Output m Unit
handleAction action = do
  id <- gets _.id
  case action of
    Editing editAction -> do
      Editable.handleAction editAction >>= case _ of
        Nothing -> pure unit
        Just s -> raise $ TitleChanged id s

    DeleteCard -> do
      raise $ CardDeleted id
