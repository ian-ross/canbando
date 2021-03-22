module Canbando.Component.Modal.BoardDetails
  ( BareBoardInfo, Output(..), Slot, Query(..), component
  ) where

import Prelude hiding (div)

import Canbando.CSS as CSS
import Canbando.Component.Modal (renderModal)
import Canbando.Model.Board (Board, BoardInfo)
import Canbando.Util (dataBsDismiss)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Debug.Trace (traceM)
import Effect.Class (class MonadEffect)
import Halogen (Component, ComponentHTML, HalogenM, RefLabel(..), defaultEval, get, getHTMLElementRef, gets, liftEffect, mkComponent, mkEval, modify_, put, raise)
import Halogen as H
import Halogen.HTML (button, div, form_, input, label, text)
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

data Query a = Show Board a

data Output = Updated BareBoardInfo

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

    NameUpdate name -> modify_ \s -> s { name = name }

    BgColourUpdate bgColour -> modify_ \s -> s { bgColour = bgColour }

    Dismiss -> do
      mb <- gets _.board
      for_ mb \b -> do
        nm <- getHTMLElementRef (RefLabel boardNameId)
        for_ (nm >>= fromHTMLElement) \nameInput ->
          liftEffect $ setValue b.name nameInput
        bgCol <- getHTMLElementRef (RefLabel boardBgColourId)
        for_ (bgCol >>= fromHTMLElement) \bgColourInput ->
          liftEffect $ setValue b.bgColour bgColourInput
        modify_ \s -> s { name = b.name, bgColour = b.bgColour }

    SaveChanges -> do
      traceM "SAVE CHANGES"
      st <- get
      for_ st.board \b -> do
        let newb = b { name = st.name, bgColour = st.bgColour }
        put $ st { board = Just newb }
        raise (Updated newb)

handleQuery :: forall m a. Query a -> HalogenM State Action () Output m (Maybe a)
handleQuery (Show board a) = do
  modify_ \s -> s { board = Just $ toBareBoardInfo board
                  , name = board.name, bgColour = board.bgColour }
  pure $ Just a
