module Canbando.Model.Labels
  ( LabelInfo, Labels, LabelEvent(..)
  , labelInfoCodec
  ) where

import Canbando.Model.Id (Id)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR


type LabelInfo = { id :: Id, name :: String, colour :: String }

type Labels = Array LabelInfo

data LabelEvent = LabelsChanged Labels


labelInfoCodec :: JsonCodec LabelInfo
labelInfoCodec =
  CAR.object "LabelInfo"
  { id: CA.string
  , name: CA.string
  , colour: CA.string
  }
