module Canbando.Component.Modal.BoardDetails
  ( BareBoardInfo, Output(..), Slot, Query(..), component
  ) where

import Prelude hiding (div)

import Canbando.CSS as CSS
import Canbando.Component.Modal (renderModal)
import Canbando.Component.Modal.BoardDetails.Label as Label
import Canbando.Model.Board (Board, BoardInfo)
import Canbando.Model.Id (Id)
import Canbando.Util (dataBsDismiss)
import Data.Array (filter, mapWithIndex, snoc)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Foldable (for_, maximum)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Class (class MonadEffect)
import Halogen (Component, ComponentHTML, HalogenM, RefLabel(..), defaultEval, get, getHTMLElementRef, gets, liftEffect, mkComponent, mkEval, modify_, put, raise)
import Halogen as H
import Halogen.HTML (button, details, div, form_, input, label, p, slot, summary, text)
import Halogen.HTML.Events (onClick, onKeyUp, onValueInput)
import Halogen.HTML.Properties (ButtonType(..), InputType(..), class_, classes, for, id, ref, type_, value)
import Type.Proxy (Proxy(..))
import Web.HTML.HTMLInputElement (fromHTMLElement, setValue)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)


type BareBoardInfo = { | BoardInfo () }

toBareBoardInfo :: forall row. { | BoardInfo row } -> BareBoardInfo
toBareBoardInfo { id, name, bgColour, labels } = { id, name, bgColour, labels }

type State =
  { visible :: Boolean
  , board :: Maybe BareBoardInfo
  , name :: String
  , bgColour :: String
  , deleting :: Boolean
  , labels :: Array Label.Input }

data Action = DoNothing
            | NameUpdate String
            | BgColourUpdate String
            | Dismiss
            | SaveChanges
            | StartDelete
            | CancelDelete
            | ConfirmDelete
            | AddLabel
            | LabelAction Int Label.Output

data Query a = Show Board a

data Output = Updated BareBoardInfo
            | Deleted Id

type Slots = ( label :: Label.Slot Int )

_label :: Proxy "label"
_label = Proxy

type Slot = H.Slot Query Output Unit


initialState :: State
initialState =
  { visible: false
  , board: Nothing
  , name: ""
  , bgColour: "#CCCCCC"
  , deleting: false
  , labels: []
  }

component ::
  forall m.
  MonadEffect m =>
  Component Query Unit Output m
component =
  mkComponent
  { initialState: const initialState
  , render
  , eval : mkEval $ defaultEval { handleQuery = handleQuery
                                , handleAction = handleAction }
  }

boardNameId :: String
boardNameId = "boardName"

boardBgColourId :: String
boardBgColourId = "boardBgColour"

catchEnter :: KeyboardEvent -> Action
catchEnter ev =
  if key ev == "Enter" then SaveChanges else DoNothing

render ::
  forall m.
  MonadEffect m =>
  State -> ComponentHTML Action Slots m
render model =
  renderModal
    "boardDetailsModal" "boardDetailsModalLabel" "Edit board details"
    Dismiss
    [ form_
      [ div [class_ CSS.mb3]
        [ label [for boardNameId, class_ CSS.formLabel]
          [text "Board name"]
        , input [ type_ InputText, class_ CSS.formControl, id boardNameId
                -- TODO: THIS ISN'T WORKING
                , onKeyUp catchEnter
                , onValueInput NameUpdate
                , ref (RefLabel boardNameId), value model.name]
        ]
      , div [class_ CSS.mb3]
        [ label [for boardBgColourId, class_ CSS.formLabel]
          [text "Board background colour"]
        , input [ type_ InputColor, id boardBgColourId
                , classes [CSS.formControl, CSS.formControlColor]
                , onValueInput BgColourUpdate
                , ref (RefLabel boardBgColourId) , value model.bgColour]
        ]
      ]
    , div [class_ CSS.mb3]
      [ details [] $
        [ summary [class_ CSS.formLabel] [text "Labels"] ]
        <> map onelabel model.labels <>
        [ button [ classes [CSS.btn, CSS.btnSecondary, CSS.btnSm]
                 , onClick (const AddLabel) ]
          [text "Create label..."]
        ]
      ]
    , div [ classes [CSS.card, CSS.textDanger, CSS.mb3] ]
      [ div [classes [CSS.cardHeader]] [text "DANGER!"]
      , div [class_ CSS.cardBody]
        [ p [class_ CSS.cardText]
          [text "You can delete this board here. This action is irreversible!"]
        , div [classes [CSS.dFlex, CSS.justifyContentBetween]] $
          if model.deleting
          then [ button [ classes [CSS.btn, CSS.btnSecondary]
                        , onClick (const CancelDelete)]
                 [text "Cancel"]
               , button [ classes [CSS.btn, CSS.btnDanger, CSS.msAuto]
                        , dataBsDismiss "modal", onClick (const ConfirmDelete)]
                 [text "Confirm deletion"] ]
          else [ button [ classes [CSS.btn, CSS.btnDanger]
                        , onClick (const StartDelete) ]
                 [text "Delete board..."] ]
        ]
      ]
    ]
    [ button [ type_ ButtonButton, classes [CSS.btn, CSS.btnSecondary]
             , dataBsDismiss "modal", onClick (const Dismiss) ]
      [text "Close"]
    , button [ type_ ButtonButton, classes [CSS.btn, CSS.btnPrimary]
             , dataBsDismiss "modal", onClick (const SaveChanges) ]
      [text "Save changes"]
    ]
    where
      onelabel dat  = slot _label dat.id Label.component dat (LabelAction dat.id)

handleAction ::
  forall m.
  MonadEffect m =>
  Action -> HalogenM State Action Slots Output m Unit
handleAction = case _ of
  LabelAction id action ->
    case action of
      Label.Deleted -> do
        labs <- filter (\l -> l.id /= id) <$> gets _.labels
        modify_ _ { labels = labs }

      Label.Updated dat -> do
        labs <- gets _.labels
        let newlabs = map (\l -> if l.id /= id then l
                                 else l { name = dat.name, colour = dat.colour}) labs
        modify_ _ { labels = newlabs }

  AddLabel -> do
    labs <- gets _.labels
    let newId = 1 + (fromMaybe 0 $ maximum $ map _.id labs)
        newLabel = { id: newId, name: "New label", colour: "#FFFFFF"}
    modify_ _ { labels = labs `snoc` newLabel }

  DoNothing -> pure unit

  NameUpdate name -> modify_ _ { name = name }

  BgColourUpdate bgColour -> modify_ _ { bgColour = bgColour }

  Dismiss -> do
    mb <- gets _.board
    for_ mb \b -> do
      nm <- getHTMLElementRef (RefLabel boardNameId)
      for_ (nm >>= fromHTMLElement) \nameInput ->
        liftEffect $ setValue b.name nameInput
      bgCol <- getHTMLElementRef (RefLabel boardBgColourId)
      for_ (bgCol >>= fromHTMLElement) \bgColourInput ->
        liftEffect $ setValue b.bgColour bgColourInput
      modify_ _ { name = b.name, bgColour = b.bgColour
                , deleting = false }

  SaveChanges -> do
    st <- get
    for_ st.board \b -> do
      let mklab { id, name, colour } = { label: name, colour }
          newb = b { name = st.name, bgColour = st.bgColour
                   , labels = map mklab st.labels }
      put $ st { board = Just newb }
      raise (Updated newb)

  StartDelete -> modify_ _ { deleting = true }

  CancelDelete -> modify_ _ { deleting = false }

  ConfirmDelete -> do
    st <- get
    for_ st.board \b -> raise $ Deleted b.id


handleQuery ::
  forall m a.
  Query a -> HalogenM State Action Slots Output m (Maybe a)
handleQuery (Show board a) = do
  let mklab id lab = { id, name: lab.label, colour: lab.colour }
  modify_ \s -> s { board = Just $ toBareBoardInfo board
                  , name = board.name, bgColour = board.bgColour
                  , labels = mapWithIndex mklab board.labels }
  pure $ Just a
