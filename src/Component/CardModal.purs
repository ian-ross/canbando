module Canbando.Component.CardModal
  ( Output(..), Slot, Query(..), component
  ) where

import Prelude hiding (div)

import Canbando.CSS as CSS
import Canbando.Capability.Resource.Labels (class GetLabels, getLabels)
import Canbando.Component.Modal (renderModal)
import Canbando.Env (Env)
import Canbando.Model.Card (Card)
import Canbando.Model.Id (Id)
import Canbando.Model.Labels (LabelEvent(..), Labels)
import Canbando.Util (dataBsDismiss, textColourStyles)
import Control.Monad.Reader.Trans (class MonadAsk, asks)
import Data.Array (delete, elem, nub, snoc)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, get, gets, mkComponent, mkEval, modify_, raise, subscribe)
import Halogen as H
import Halogen.HTML (button, details, div, input, p, summary, text)
import Halogen.HTML as HH
import Halogen.HTML.Events (onChecked, onClick)
import Halogen.HTML.Properties (ButtonType(..), InputType(..), checked, class_, classes, for, id, style, type_, value)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)

type Input = Unit

type State =
  { visible :: Boolean
  , deleting :: Boolean
  , id :: Id
  , listId :: Id
  , name :: String
  , labels :: Array Id
  , boardLabels :: Labels }

data Action = DoNothing
            | Initialize
            | Dismiss
            | SaveChanges
            | StartDelete
            | CancelDelete
            | ConfirmDelete
            | LabelChecked Id Boolean
            | LabelAction LabelEvent

data Query a = Show Card Id a

data Output
  = Updated { cardId :: Id, listId :: Id, title :: String, labels :: Array Id }
  | Deleted { cardId :: Id, listId :: Id }

type Slot = H.Slot Query Output Unit


initialState :: State
initialState =
  { visible: false
  , id: ""
  , listId : ""
  , name: ""
  , labels: []
  , boardLabels: []
  , deleting: false }

component ::
  forall m.
  MonadAff m =>
  MonadAsk Env m =>
  GetLabels m =>
  Component Query Input Output m
component =
  mkComponent
  { initialState: const initialState
  , render
  , eval : mkEval $ defaultEval { handleQuery = handleQuery
                                , handleAction = handleAction
                                , initialize = Just Initialize }
  }

catchEnter :: KeyboardEvent -> Action
catchEnter ev =
  if key ev == "Enter" then SaveChanges else DoNothing

render ::
  forall m.
  MonadAff m =>
  State -> ComponentHTML Action () m
render state =
  renderModal
    "cardDetailsModal" "cardDetailsModalLabel" state.name
    Dismiss
    [ div [class_ CSS.mb3]
      [ details [] $
        [ summary [class_ CSS.formLabel] [text "Labels"] ]
        <> map (onelabel state.labels) state.boardLabels
      ]
    , div [ classes [CSS.card, CSS.textDanger, CSS.mb3] ]
      [ div [classes [CSS.cardHeader]] [text "DANGER!"]
      , div [class_ CSS.cardBody]
        [ p [class_ CSS.cardText]
          [text "You can delete this card here. This action is irreversible!"]
        , div [classes [CSS.dFlex, CSS.justifyContentBetween]] $
          if state.deleting
          then [ button [ classes [CSS.btn, CSS.btnSecondary]
                        , onClick (const CancelDelete)]
                 [text "Cancel"]
               , button [ classes [CSS.btn, CSS.btnDanger, CSS.msAuto]
                        , dataBsDismiss "modal", onClick (const ConfirmDelete)]
                 [text "Confirm deletion"] ]
          else [ button [ classes [CSS.btn, CSS.btnDanger]
                        , onClick (const StartDelete) ]
                 [text "Delete card..."] ]
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
      onelabel cardLabels label =
        div [class_ CSS.formCheck]
        [ input [ class_ CSS.formCheckInput
                , type_ InputCheckbox
                , value ""
                , id ("label-check-" <> label.id)
                , checked (label.id `elem` cardLabels)
                , onChecked (LabelChecked label.id)]
        , HH.label [ classes [CSS.formCheckLabel, CSS.cardModalLabel]
                   , for ("label-check-" <> label.id)
                   , style (textColourStyles label.colour)]
          [text label.name]
        ]

modifyLabels :: forall m. Array Id -> HalogenM State Action () Output m Unit
modifyLabels labs = modify_ _ { labels = labs }

handleAction ::
  forall m.
  MonadAff m =>
  MonadAsk Env m =>
  GetLabels m =>
  Action -> HalogenM State Action () Output m Unit
handleAction = case _ of
  DoNothing -> pure unit

  Initialize -> do
    emitter <- asks _.labelEmitter
    _ <- subscribe $ LabelAction <$> emitter
    blabs <- getLabels
    modify_ _ { boardLabels = blabs }

  LabelAction (LabelsChanged labels) ->
    modify_ _ { boardLabels = labels }

  LabelChecked id checked -> do
    labels <- gets _.labels
    let newLabels =
          if checked then nub (labels `snoc` id) else delete id labels
    modifyLabels newLabels

  Dismiss -> modify_ _ { deleting = false }

  SaveChanges -> do
    s <- get
    raise $ Updated { cardId: s.id, listId: s.listId
                    , title: s.name, labels: s.labels }

  StartDelete -> modify_ _ { deleting = true }

  CancelDelete -> modify_ _ { deleting = false }

  ConfirmDelete -> do
    s <- get
    modify_ _ { deleting = false }
    raise $ Deleted { cardId: s.id, listId: s.listId }


handleQuery ::
  forall m a.
  Query a -> HalogenM State Action () Output m (Maybe a)
handleQuery (Show card listId a) = do
  modify_ \s -> s { id = card.id, listId = listId,
                    name = card.title, labels = card.labels }
  pure $ Just a
