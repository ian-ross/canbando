module Component.Card (
  Id, Input(..), Output(..), State, Slot,
  component, newCard, decodeDragData, mtype
) where

import Prelude hiding (div)

import CSS as CSS
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonParser, stringify)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, get, mkComponent, mkEval, modify_, raise)
import Halogen as H
import Halogen.HTML (HTML, a, div, input, text)
import Halogen.HTML.Events (onBlur, onClick, onDragEnd, onDragEnter, onDragLeave, onDragOver, onDragStart, onDrop, onKeyDown, onKeyUp, onValueChange)
import Halogen.HTML.Properties (class_, classes, draggable, href, id_, value)
import Util (focusElement)
import Web.Event.Event (preventDefault)
import Web.HTML.Event.DataTransfer (DropEffect(..), dropEffect, getData, setData)
import Web.HTML.Event.DragEvent (DragEvent, dataTransfer, toEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)


type Id = String

type State = { id :: Id
             , value :: String
             , edit :: String
             , editing :: Boolean
             , dragging :: Boolean
             , dragIndicate :: Boolean }

type Input = { id :: Id, title :: String }

stateToInput :: State -> Input
stateToInput s = { id: s.id, title: s.value }

data Output = TitleChanged Id String
            | CardDeleted Id
            | CardMoved Input Id

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


component :: forall query m. MonadEffect m => Component HTML query Input Output m
component =
  mkComponent
  { initialState: initialState
  , render
  , eval : mkEval $ defaultEval { handleAction = handleAction }
  }


newCard :: Int -> String -> Input
newCard id title = { id: "C" <> show id, title: title }


initialState :: Input -> State
initialState inp =
  { id: inp.id, value: inp.title, edit: ""
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
  , div [ classes divClasses ] [text s.value]
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
        modify_ \st -> st { edit = s.value, editing = true }
        liftEffect $ focusElement $ s.id <> "_in"

    Edited edit ->
      when s.editing do
        modify_ \st -> st { edit = edit }

    Accept ->
      when s.editing do
        modify_ \st -> st { value = s.edit, editing = false }
        raise $ TitleChanged s.id s.edit

    Reject ->
      when s.editing do
        modify_ \st -> st { editing = false }

    DeleteCard -> raise $ CardDeleted s.id

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
      from <- decodeDragData <$> (liftEffect $ getData mtype (dataTransfer ev))
      case from of
        Nothing -> pure unit
        Just f -> raise $ CardMoved f s.id

    DragIndicate ev drag -> do
      liftEffect $ preventDefault (toEvent ev)
      modify_ \st -> st { dragIndicate = drag }


encodeDragData :: State -> String
encodeDragData = stringify <<< encodeJson <<< stateToInput

decodeDragData :: String -> Maybe Input
decodeDragData s =
  case jsonParser s of
    Right json ->
      case decodeJson json of
        Left err -> Nothing
        Right ok -> Just ok
    Left _ -> Nothing
