module Util (blurTarget, focusElement) where

import Prelude

import Effect (Effect)
import Web.Event.Event (Event)


foreign import blurTarget :: Event -> Effect Unit
foreign import focusElement :: String -> Effect Unit
