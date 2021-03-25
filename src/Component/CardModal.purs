module Canbando.Component.CardModal
  ( Output(..), Slot, Query(..), component
  ) where

import Prelude hiding (div)

import Canbando.CSS as CSS
import Canbando.Component.Modal (renderModal)
import Canbando.Model.Card (Card)
import Canbando.Model.Id (Id)
import Canbando.Util (dataBsDismiss)
import Data.Maybe (Maybe(..))
import Data.Set (Set, delete, empty, insert)
import Effect.Class (class MonadEffect)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, get, gets, mkComponent, mkEval, modify_, raise)
import Halogen as H
import Halogen.HTML (button, div, p, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (ButtonType(..), class_, classes, type_)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)


type State =
  { visible :: Boolean
  , deleting :: Boolean
  , id :: Id
  , listId :: Id
  , name :: String
  , labels :: Set String }

data Action = DoNothing
            | Dismiss
            | SaveChanges
            | StartDelete
            | CancelDelete
            | ConfirmDelete
            | AddLabel String
            | RemoveLabel String

data Query a = Show Card Id a

data Output = Updated { cardId :: Id, labels :: Set String }
            | Deleted { cardId :: Id, listId :: Id }

type Slot = H.Slot Query Output Unit


initialState :: State
initialState =
  { visible: false
  , id: ""
  , listId : ""
  , name: ""
  , labels: empty
  , deleting: false }

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

catchEnter :: KeyboardEvent -> Action
catchEnter ev =
  if key ev == "Enter" then SaveChanges else DoNothing

render ::
  forall m.
  MonadEffect m =>
  State -> ComponentHTML Action () m
render state =
  renderModal
    "cardDetailsModal" "cardDetailsModalLabel" state.name
    Dismiss
    [ div [ classes [CSS.card, CSS.textDanger, CSS.mb3] ]
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

modifyLabels :: forall m. Set String -> HalogenM State Action () Output m Unit
modifyLabels labs = modify_ _ { labels = labs }

handleAction ::
  forall m.
  MonadEffect m =>
  Action -> HalogenM State Action () Output m Unit
handleAction = case _ of
  DoNothing -> pure unit

  AddLabel lab ->
    modifyLabels =<< insert lab <$> gets _.labels

  RemoveLabel lab ->
    modifyLabels =<< delete lab <$> gets _.labels

  Dismiss -> modify_ _ { deleting = false }

  SaveChanges -> do
    pure unit
    -- st <- get
    -- for_ st.board \b -> do
    --   let mklab { id, name, colour } = { label: name, colour }
    --       newb = b { name = st.name, bgColour = st.bgColour
    --                , labels = map mklab st.labels }
    --   put $ st { board = Just newb }
      -- raise (Updated newb)

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
