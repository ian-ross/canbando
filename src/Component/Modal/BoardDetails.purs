module Canbando.Component.Modal.BoardDetails
  ( BareBoardInfo, Output(..), Slot, Query(..), component
  ) where

import Prelude hiding (div)

import Canbando.CSS as CSS
import Canbando.Component.Modal (renderModal)
import Canbando.Model.Board (Board, BoardInfo)
import Canbando.Model.Id (Id)
import Canbando.Util (dataBsDismiss)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Debug.Trace (traceM)
import Effect.Class (class MonadEffect)
import Halogen (Component, ComponentHTML, HalogenM, RefLabel(..), defaultEval, get, getHTMLElementRef, gets, liftEffect, mkComponent, mkEval, modify_, put, raise)
import Halogen as H
import Halogen.HTML (button, div, form_, h5, input, label, p, text)
import Halogen.HTML.Events (onClick, onKeyUp, onValueInput)
import Halogen.HTML.Properties (ButtonType(..), InputType(..), class_, classes, for, id, ref, type_, value)
import Web.HTML.HTMLInputElement (fromHTMLElement, setValue)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)


type BareBoardInfo = { | BoardInfo () }

toBareBoardInfo :: forall row. { | BoardInfo row } -> BareBoardInfo
toBareBoardInfo { id, name, bgColour } = { id, name, bgColour }

type State =
  { visible :: Boolean
  , board :: Maybe BareBoardInfo
  , name :: String
  , bgColour :: String
  , deleting :: Boolean }

data Action = DoNothing
            | NameUpdate String
            | BgColourUpdate String
            | Dismiss
            | SaveChanges
            | StartDelete
            | CancelDelete
            | ConfirmDelete

data Query a = Show Board a

data Output = Updated BareBoardInfo
            | Deleted Id

type Slot = H.Slot Query Output Unit

initialState :: State
initialState =
  { visible: false
  , board: Nothing
  , name: ""
  , bgColour: "#CCCCCC"
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

boardNameId :: String
boardNameId = "boardName"

boardBgColourId :: String
boardBgColourId = "boardBgColour"

catchEnter :: KeyboardEvent -> Action
catchEnter ev =
  if key ev == "Enter" then SaveChanges else DoNothing

render :: forall cs m. State -> ComponentHTML Action cs m
render s =
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
                , ref (RefLabel boardNameId), value s.name]
        ]
      , div [class_ CSS.mb3]
        [ label [for boardBgColourId, class_ CSS.formLabel]
          [text "Board background colour"]
        , input [ type_ InputColor, id boardBgColourId
                , classes [CSS.formControl, CSS.formControlColor]
                , onValueInput BgColourUpdate
                , ref (RefLabel boardBgColourId) , value s.bgColour]
        ]
      ]
    , div [ classes [CSS.card, CSS.textDanger, CSS.mb3] ]
      [ div [classes [CSS.cardHeader]] [text "DANGER!"]
      , div [class_ CSS.cardBody]
        [ h5 [class_ CSS.cardTitle] [text "Delete board"]
        , p [class_ CSS.cardText]
          [text "You can delete this board here. This action is irreversible!"]
        , div [classes [CSS.dFlex, CSS.justifyContentBetween]] $
          if s.deleting
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

handleAction ::
  forall cs m.
  MonadEffect m =>
  Action -> HalogenM State Action cs Output m Unit
handleAction =
  case _ of
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
        modify_ _ { name = b.name, bgColour = b.bgColour }

    SaveChanges -> do
      st <- get
      for_ st.board \b -> do
        let newb = b { name = st.name, bgColour = st.bgColour }
        put $ st { board = Just newb }
        raise (Updated newb)

    StartDelete -> modify_ _ { deleting = true }

    CancelDelete -> modify_ _ { deleting = false }

    ConfirmDelete -> do
      st <- get
      for_ st.board \b -> raise $ Deleted b.id


handleQuery :: forall m a. Query a -> HalogenM State Action () Output m (Maybe a)
handleQuery (Show board a) = do
  modify_ \s -> s { board = Just $ toBareBoardInfo board
                  , name = board.name, bgColour = board.bgColour }
  pure $ Just a