module Component.List (Input, Output, State, Slot, component) where

import Prelude hiding (div)

import CSS as CSS
import Component.Card as Card
import Component.Icon (icon)
import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Debug.Trace (traceM)
import Effect.Class (class MonadEffect)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval, modify_)
import Halogen as H
import Halogen.HTML (HTML, button, div, h1_, slot, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes)

type State = { name :: String
             , id :: String
             , nextID :: Int
             , cards :: Array Card.Input }

type Input = State

type Output = Void

data Action
  = AddCard
  | CardAction Card.Output

type Slot id = forall query. H.Slot query Void id

type Slots = ( card :: Card.Slot Int )

_card :: SProxy "card"
_card = SProxy

component :: forall query m. MonadEffect m => Component HTML query Input Output m
component = mkComponent
       { initialState
       , render
       , eval : mkEval $ defaultEval { handleAction = handleAction }
       }

initialState :: Input -> State
initialState = identity

render :: forall m. MonadEffect m => State -> ComponentHTML Action Slots m
render state =
  div [classes [ CSS.bgLight, CSS.p3, CSS.rounded, CSS.listWrapper
               , CSS.flexGrow0, CSS.flexShrink0 ]]
  [ h1_ [text state.name]
  , div [classes [CSS.dFlex, CSS.flexColumn]] cards
  , button [classes [CSS.rounded, CSS.addNewCard], onClick \_ -> Just AddCard]
    [ icon "bi-plus-circle", text "Add new card"]
  ]
  where cards = mapWithIndex onecard state.cards
        onecard idx crd = slot _card idx Card.component crd (Just <<< CardAction)

handleAction :: forall output m. MonadEffect m =>
                Action -> HalogenM State Action Slots output m Unit
handleAction = case _ of
  AddCard ->
    modify_ \state -> state
                      { nextID = state.nextID + 1
                      , cards = state.cards <>
                                [Card.newCard (state.id <>
                                               show state.nextID) "New card"] }
  CardAction o ->
    traceM o
