module Canbando.Model.Labels
  ( LabelCreateInfoRep, LabelInfo, LabelCreateInfo, Labels, LabelEvent(..)
  , labelCreateInfoCodec, labelInfoCodec
  ) where

import Canbando.Model.Id (Id)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR


type LabelCreateInfoRep row = ( name :: String, colour :: String | row)

type LabelCreateInfo = { | LabelCreateInfoRep () }

type LabelInfo = { | LabelCreateInfoRep ( id :: Id ) }

type Labels = Array LabelInfo

data LabelEvent = LabelsChanged Labels


labelCreateInfoCodec :: JsonCodec LabelCreateInfo
labelCreateInfoCodec =
  CAR.object "LabelCreateInfo"
  { name: CA.string
  , colour: CA.string
  }

labelInfoCodec :: JsonCodec LabelInfo
labelInfoCodec =
  CAR.object "LabelInfo"
  { id: CA.string
  , name: CA.string
  , colour: CA.string
  }
