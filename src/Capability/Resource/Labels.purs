-- There are three places where access to card labels is needed:
--
--  1. The set of labels that can be used in a board is edited in the
--     "Board details..." modal.
--  2. The labels assigned to a card is edited in the card modal.
--  3. The labels assigned to a card are rendered when the card is
--     rendered, requiring access to the label names and colours.
--
-- This means that cards may need to re-render when a label is
-- renamed, has its colour changed, or is deleted.

module Canbando.Capability.Resource.Labels
  ( class GetLabels
  , getLabels
  , class SetLabels
  , setLabels
  , class EditLabels
  , addLabel, deleteLabel, updateLabel
  ) where

import Prelude

import Canbando.Model.Id (Id)
import Canbando.Model.Labels (Labels, LabelInfo)
import Halogen (HalogenM, lift)


class Monad m <= GetLabels m where
  getLabels :: m Labels

instance getLabelsHalogenM ::
  GetLabels m => GetLabels (HalogenM state action slots output m) where
  getLabels = lift getLabels


class Monad m <= SetLabels m where
  setLabels :: Labels -> m Unit

instance setLabelsHalogenM ::
  SetLabels m => SetLabels (HalogenM state action slots output m) where
  setLabels = lift <<< setLabels


class Monad m <= EditLabels m where
  addLabel :: m Id
  deleteLabel :: Id {- labelId -} -> m Unit
  updateLabel :: LabelInfo -> m Unit

instance editLabelsHalogenM ::
  EditLabels m => EditLabels (HalogenM state action slots output m) where
  addLabel = lift addLabel
  deleteLabel = lift <<< deleteLabel
  updateLabel = lift <<< updateLabel
