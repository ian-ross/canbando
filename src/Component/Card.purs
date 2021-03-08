module Canbando.Component.Card (
  Output(..), Slot,
  component, cardDragData
) where

import Prelude hiding (div)

import Data.Argonaut (decodeJson, encodeJson, jsonParser, stringify)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, get, mkComponent, mkEval, modify_, raise)
import Halogen as H
import Halogen.HTML (HTML, a, div, input, text)
import Halogen.HTML.Events (onBlur, onClick, onDragEnd, onDragEnter, onDragLeave, onDragOver, onDragStart, onDrop, onKeyDown, onKeyUp, onValueChange)
import Halogen.HTML.Properties (class_, classes, draggable, href, id_, value)
import Web.Event.Event (preventDefault)
import Web.HTML.Event.DataTransfer (DropEffect(..), dropEffect, getData, setData)
import Web.HTML.Event.DragEvent (DragEvent, dataTransfer, toEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)

import Canbando.CSS as CSS
import Canbando.Model.Card (Id, CardRep, Card)
import Canbando.Util (focusElement)


type StateInfo =
  ( edit :: String
  , editing :: Boolean
  , dragging :: Boolean
  , dragIndicate :: Boolean )

type State =  { | CardRep StateInfo }

data Output
  = CardDeleted Id
  | CardMoved Card Id

data Action = StartEditing | Edited String | Accept | Reject
            | DeleteCard
            | DragStart DragEvent | DragEnd DragEvent | Drop DragEvent
            | DragIndicate DragEvent Boolean

type Slot id = forall query. H.Slot query Output id

catchEnter :: KeyboardEvent -> Maybe Action
catchEnter ev =
  if key ev == "Enter" then Just Accept else Nothing

catchEscape :: KeyboardEvent -> Maybe Action
catchEscape ev =
  if key ev == "Escape" then Just Reject else Nothing


component :: forall query m. MonadEffect m => Component HTML query Card Output m
component =
  mkComponent
  { initialState: initialState
  , render
  , eval : mkEval $ defaultEval { handleAction = handleAction }
  }


initialState :: Card -> State
initialState inp =
  { id: inp.id, title: inp.title, edit: ""
  , editing: false, dragging: false, dragIndicate: false }


render :: forall cs m. State -> ComponentHTML Action cs m
render s =
  div [ id_ s.id
      , classes $
        [CSS.card] <>
        if s.dragging then [CSS.cardDragging] else [] <>
        if s.dragIndicate then [CSS.cardDragIndicate] else []
      , draggable (not s.editing)
        -- ALL the drag events...
      , onDragStart \e -> Just (DragStart e)
      , onDragEnd \e -> Just (DragEnd e)
      , onDragOver \e -> Just (DragIndicate e true)
      , onDragEnter \e -> Just (DragIndicate e true)
      , onDragLeave \e -> Just (DragIndicate e false)
      , onDrop \e -> Just (Drop e)
      , onClick \_ -> Just StartEditing ]
  [ input [ id_ (s.id <> "_in")
          , classes inputClasses
          , onValueChange (Just <<< Edited)
          , onKeyUp catchEnter
          , onKeyDown catchEscape
          -- KEYBOARD NAVIGATION NOT WORKING YET...
          -- , tabIndex 0
          -- , onFocus \_ -> Just StartEditing
          , onBlur \_ -> if s.editing then Just Accept else Nothing
          , value s.edit ]
  , div [ classes divClasses ] [text s.title]
  , a [ class_ CSS.cardMenu
      , href "#"
      , onClick \_ -> Just DeleteCard ] [text "..."]
  ]
  where inputClasses = if s.editing then [] else [CSS.hidden]
        divClasses = [CSS.cardInner] <> if s.editing then [CSS.hidden] else []


mtype :: MediaType
mtype = MediaType "application/json"

handleAction :: forall m. MonadEffect m => Action -> HalogenM State Action () Output m Unit
handleAction action = do
  s <- get
  case action of
    StartEditing ->
      when (not s.editing) do
        modify_ \st -> st { edit = s.title, editing = true }
        liftEffect $ focusElement $ s.id <> "_in"

    Edited edit ->
      when s.editing do
        modify_ \st -> st { edit = edit }

    Accept ->
      when s.editing do
        modify_ \st -> st { title = s.edit, editing = false }

    Reject ->
      when s.editing do
        modify_ \st -> st { editing = false }

    DeleteCard -> raise (CardDeleted s.id)

    DragStart ev -> do
      liftEffect $ setData mtype (encodeDragData s) (dataTransfer ev)
      modify_ \st -> st { dragging = true }

    DragEnd ev -> do
      effect <- liftEffect $ dropEffect (dataTransfer ev)
      modify_ \st -> st { dragging = false, dragIndicate = false }
      when (effect == Move) do
        raise (CardDeleted s.id)

    Drop ev -> do
      modify_ \st -> st { dragging = false, dragIndicate = false }
      liftEffect (cardDragData ev) >>= case _ of
        Nothing -> pure unit
        Just f -> raise (CardMoved f s.id)

    DragIndicate ev drag -> do
      liftEffect $ preventDefault (toEvent ev)
      modify_ \st -> st { dragIndicate = drag }


encodeDragData :: State -> String
encodeDragData s = stringify (encodeJson { id: s.id, title: s.title })

decodeDragData :: String -> Maybe Card
decodeDragData = hush <<< (jsonParser >=> decodeJson)

cardDragData :: DragEvent -> Effect (Maybe Card)
cardDragData ev = decodeDragData <$> getData mtype (dataTransfer ev)
