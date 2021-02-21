module Component.Card (Input, Output, State, Slot, component) where

import Prelude hiding (div)

import CSS as CSS
import DOM.HTML.Indexed (Interactive)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, liftEffect, mkComponent, mkEval, modify_, raise)
import Halogen as H
import Halogen.HTML (AttrName(..), ElemName(..), HTML, Node, a, div, element, text)
import Halogen.HTML.Events (onBlur, onKeyUp)
import Halogen.HTML.Properties (attr, class_, href)
import Web.Event.Event (Event, preventDefault)
import Web.UIEvent.FocusEvent as F
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)
import Web.UIEvent.KeyboardEvent as K

foreign import editDivContent :: Event -> String

foreign import blurTarget :: Event -> Effect Unit

type State = { title :: String }

type Input = String

data Output = TitleChanged String

data Action = HandleInput Event
            | EnterPressed Event

type Slot id = forall query. H.Slot query Output id

component :: forall query m. MonadEffect m => Component HTML query Input Output m
component = mkComponent
       { initialState
       , render
       , eval : mkEval $ defaultEval { handleAction = handleAction }
       }

initialState :: Input -> State
initialState title = { title }

catchEnter :: KeyboardEvent -> Maybe Action
catchEnter ev =
  case key ev of
    "Enter" -> Just $ EnterPressed $ K.toEvent ev
    _ -> Nothing

render :: forall cs m. State -> ComponentHTML Action cs m
render c =
  div [class_ CSS.card]
  [ editDiv [attr (AttrName "contenteditable") "true",
             onBlur $ Just <<< HandleInput <<< F.toEvent,
             onKeyUp catchEnter] [text c.title]
  , a [class_ CSS.cardMenu, href "#"] [text "..."]
  ]


type HTMLeditDiv = Interactive (onScroll :: Event, onInput :: Event, onBlur :: Event)

editDiv :: forall w i. Node HTMLeditDiv w i
editDiv = element (ElemName "div")

handleAction :: forall m. MonadEffect m => Action -> HalogenM State Action () Output m Unit
handleAction = case _ of
  HandleInput event -> do
    let title = editDivContent event
    liftEffect $ log $ "EVENT: " <> title
    modify_ \state -> state { title = title }
    raise $ TitleChanged title
  EnterPressed event -> liftEffect do
    log "CALLING preventDefault"
    preventDefault event
    log "CALLING blurTarget"
    blurTarget event
    log "ENTER!"
