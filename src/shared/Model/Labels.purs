module Canbando.Model.Labels
  ( LabelInfo, Labels, LabelEvent(..)
  ) where

import Canbando.Model.Id (Id)


type LabelInfo = { id :: Id, name :: String, colour :: String }

type Labels = Array LabelInfo

data LabelEvent = LabelsChanged Labels
