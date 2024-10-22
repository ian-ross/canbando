module Canbando.Component.Card (
  Output(..), Slot, Query(..),
  component, cardDragData
) where

import Prelude hiding (div)

import Canbando.CSS as CSS
import Canbando.Capability.Resource.Labels (class GetLabels, getLabels)
import Canbando.Component.Icon (icon)
import Canbando.Env (Env)
import Canbando.Model.Card (Card, CardRep, CheckListItem, toCard)
import Canbando.Model.Id (Id)
import Canbando.Model.Labels (LabelEvent(..), Labels)
import Canbando.Util (dataBsTarget, dataBsToggle, focusElement, textColourStyles)
import Control.Monad.Reader.Trans (class MonadAsk, asks)
import Data.Argonaut (decodeJson, encodeJson, parseJson, stringify)
import Data.Array (filter, find, intersect, length, null)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, get, gets, liftEffect, mkComponent, mkEval, modify, modify_, raise, subscribe)
import Halogen as H
import Halogen.HTML (button, div, input, span, text)
import Halogen.HTML.Events (onBlur, onClick, onDragEnd, onDragEnter, onDragLeave, onDragOver, onDragStart, onDrop, onKeyDown, onKeyUp, onValueChange)
import Halogen.HTML.Properties (ButtonType(..), class_, classes, draggable, id, style, tabIndex, type_, value)
import Web.Event.Event (preventDefault, stopPropagation)
import Web.HTML.Event.DataTransfer (DropEffect(..), dropEffect, getData, setData)
import Web.HTML.Event.DragEvent (DragEvent, dataTransfer, toEvent)
import Web.HTML.Event.DragEvent as DE
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME


type StateInfo =
  ( edit :: String
  , editing :: Boolean
  , dragging :: Boolean
  , dragIndicate :: Boolean
  , boardLabels :: Labels )

type State =  { | CardRep StateInfo }

data Output
  = CardDeleted Id
  | CardDeletedByMove Id
  | CardMoved Card Id
  | CardUpdated Id Card
  | OpenCardModal Card

data Action
  = Initialize | LabelAction LabelEvent
  | StartEditing | Edited String | Accept | Reject
  | DeleteCard MouseEvent
  | DragStart DragEvent | DragEnd DragEvent | Drop DragEvent
  | DragIndicate Boolean DragEvent
  | ModalOpenClicked | DoNothing

data Query a =
  UpdateCard { title :: String
             , labels :: Array Id
             , checklist :: Array CheckListItem } a

type Slot id = H.Slot Query Output id

catchEnter :: KeyboardEvent -> Action
catchEnter ev =
  if key ev == "Enter" then Accept else DoNothing

catchEscape :: KeyboardEvent -> Action
catchEscape ev =
  if key ev == "Escape" then Reject else DoNothing


component ::
  forall m.
  MonadAff m =>
  MonadAsk Env m =>
  GetLabels m =>
  Component Query Card Output m
component =
  mkComponent
  { initialState
  , render
  , eval : mkEval $ defaultEval { handleAction = handleAction
                                , handleQuery = handleQuery
                                , initialize = Just Initialize }
  }


initialState :: Card -> State
initialState inp =
  { id: inp.id
  , title: inp.title
  , labels: inp.labels
  , checklist: inp.checklist
  , edit: ""
  , editing: false
  , dragging: false
  , dragIndicate: false
  , boardLabels: [] }


render :: forall m. State -> ComponentHTML Action () m
render s =
  div [ id s.id
      , classes $
        [CSS.card, CSS.listCard] <>
        if s.dragging then [CSS.cardDragging] else [] <>
        if s.dragIndicate then [CSS.cardDragIndicate] else []
      , draggable (not s.editing)
        -- ALL the drag events...
      , onDragStart DragStart
      , onDragEnd DragEnd
      , onDragOver (DragIndicate true)
      , onDragEnter (DragIndicate true)
      , onDragLeave (DragIndicate false)
      , onDrop Drop
      , onClick (const StartEditing) ]
  [ input [ id (s.id <> "_in")
          , classes inputClasses
          , onValueChange Edited
          , onKeyUp catchEnter
          , onKeyDown catchEscape
          -- KEYBOARD NAVIGATION NOT WORKING YET...
          -- , tabIndex 0
          -- , onFocus (const StartEditing)
          , onBlur $ const (if s.editing then Accept else DoNothing)
          , value s.edit ]
  , div [ classes divClasses ] $ [text s.title] <> checklist
  , button [ type_ ButtonButton, tabIndex (-1)
           , classes [CSS.cardMenu, CSS.btnLink, CSS.btnSm]
           , dataBsToggle "modal", dataBsTarget "#cardDetailsModal"
           , onClick (const ModalOpenClicked)]
    [text "..."]
  , div [ class_ CSS.labelChips ] (map onelabel s.labels)
  ]
  where inputClasses = if s.editing then [] else [CSS.hidden]
        divClasses = [CSS.cardInner] <> if s.editing then [CSS.hidden] else []
        onelabel id =
          case find (\l -> l.id == id) s.boardLabels of
            Nothing -> text ""
            Just lab ->
              div [ class_ CSS.labelChip, style (textColourStyles lab.colour) ] [ ]
        checklist =
          if null s.checklist
          then []
          else [span [class_ CSS.ms2]
                [ icon "bi-check2-square"
                , text $ show (length (filter _.done s.checklist)) <>
                  "/" <> show (length s.checklist)]]


mtype :: MediaType
mtype = MediaType "application/json"

handleAction ::
  forall m.
  MonadAff m =>
  MonadAsk Env m =>
  GetLabels m =>
  Action -> HalogenM State Action () Output m Unit
handleAction action = do
  s <- get
  case action of
    DoNothing -> pure unit

    Initialize -> do
      emitter <- asks _.labelEmitter
      _ <- subscribe $ LabelAction <$> emitter
      blabs <- getLabels
      modify_ _ { boardLabels = blabs }

    LabelAction (LabelsChanged labels) -> do
      modify_ _ { boardLabels = labels }
      newLabels <- flip intersect (map _.id labels) <$> gets _.labels
      modify_ _ { labels = newLabels }

    ModalOpenClicked -> raise $ OpenCardModal $ toCard s

    StartEditing ->
      when (not s.editing) do
        modify_ \st -> st { edit = s.title, editing = true }
        liftEffect $ focusElement $ s.id <> "_in"

    Edited edit -> when s.editing (modify_ \st -> st { edit = edit })

    Accept -> when s.editing do
      new <- modify \st -> st { title = s.edit, editing = false }
      raise $ CardUpdated new.id (toCard new)

    Reject -> when s.editing (modify_ \st -> st { editing = false })

    DeleteCard ev -> do
      liftEffect $ stopPropagation (ME.toEvent ev)
      raise (CardDeleted s.id)

    DragStart ev -> do
      liftEffect $ setData mtype (encodeDragData s) (dataTransfer ev)
      modify_ \st -> st { dragging = true }

    DragEnd ev -> do
      effect <- liftEffect $ dropEffect (dataTransfer ev)
      modify_ \st -> st { dragging = false, dragIndicate = false }
      when (effect /= None) $ raise (CardDeletedByMove s.id)

    Drop ev -> do
      modify_ \st -> st { dragging = false, dragIndicate = false }
      liftEffect $ stopPropagation $ toEvent ev
      liftEffect (cardDragData ev) >>= case _ of
        Nothing -> pure unit
        Just f -> raise (CardMoved f s.id)

    DragIndicate drag ev -> do
      liftEffect $ preventDefault (DE.toEvent ev)
      modify_ \st -> st { dragIndicate = drag }


encodeDragData :: State -> String
encodeDragData s =
  stringify (encodeJson { id: s.id, title: s.title
                        , labels: s.labels, checklist: s.checklist })

decodeDragData :: String -> Maybe Card
decodeDragData = hush <<< (parseJson >=> decodeJson)

cardDragData :: DragEvent -> Effect (Maybe Card)
cardDragData ev = decodeDragData <$> getData mtype (dataTransfer ev)


handleQuery ::
  forall m a.
  MonadAff m =>
  Query a -> HalogenM State Action () Output m (Maybe a)
handleQuery (UpdateCard { title, labels, checklist } a) = do
  modify_ _ { title = title, labels = labels, checklist = checklist }
  pure $ Just a
