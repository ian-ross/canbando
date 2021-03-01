module Component.List (Id, Input, Output, State, Slot, component, newList) where

import Prelude hiding (div)

import CSS as CSS
import Component.Card as Card
import Component.Editable (EditAction, editableWith)
import Component.Editable as Editable
import Component.Icon (icon)
import Data.Array (filter)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval, modify_, raise)
import Halogen as H
import Halogen.HTML (HTML, button, div, h1, slot, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes)


type Id = String

type State = { value :: String
             , edit :: String
             , editing :: Boolean
             , id :: Id
             , nextID :: Int
             , cards :: Array Card.Input }

type Input = { name :: String
             , id :: Id
             , nextID :: Int
             , cards :: Array Card.Input }

data Output = ListTitleChanged String

data Action
  = AddCard
  | CardAction Card.Output
  | Editing EditAction

type Slot id = forall query. H.Slot query Output id

type Slots = ( card :: Card.Slot Card.Id )

_card :: SProxy "card"
_card = SProxy


component :: forall query m. MonadEffect m => Component HTML query Input Output m
component = mkComponent
       { initialState
       , render
       , eval : mkEval $ defaultEval { handleAction = handleAction }
       }


newList :: Int -> String -> Input
newList id name = { id: "L" <> show id, name: name, nextID: 1, cards: [] }


initialState :: Input -> State
initialState { name, id, nextID, cards } =
  { value: name, id, nextID, cards, edit: "", editing: false }


render :: forall m. MonadEffect m => State -> ComponentHTML Action Slots m
render s =
  div [classes [ CSS.bgLight, CSS.p3, CSS.rounded, CSS.listWrapper
               , CSS.flexGrow0, CSS.flexShrink0 ]]
  [editableWith [CSS.formControlLG] h1 Editing s,
   div [classes [CSS.dFlex, CSS.flexColumn]] cards,
   button [classes [CSS.rounded, CSS.addNewCard], onClick \_ -> Just AddCard]
     [ icon "bi-plus-circle", text "Add new card"]
   ]
  where cards = map onecard s.cards
        onecard crd = slot _card crd.id Card.component crd (Just <<< CardAction)


handleAction :: forall m. MonadEffect m =>
                Action -> HalogenM State Action Slots Output m Unit
handleAction action =
  case action of
    AddCard ->
      modify_ \s ->
      s { nextID = s.nextID + 1
        , cards = s.cards <> [Card.newCard s.nextID "New card"] }

    CardAction (Card.CardDeleted id) ->
      modify_ \s ->
      s { cards = filter (\c -> c.id /= id) s.cards }

    CardAction (Card.TitleChanged id title) ->
      modify_ \s ->
      let edit c = if c.id == id then c { title = title } else c
      in s { cards = map edit s.cards }

    Editing editAction -> do
      Editable.handleAction editAction >>= case _ of
        Just s -> raise $ ListTitleChanged s
        _ -> pure unit
