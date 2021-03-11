module Main where

import Prelude

import Canbando.AppM (runApp)
import Canbando.Env (LogLevel(..))
import Canbando.Page.Board as Page
import Effect (Effect)
import Effect.Ref (new)
import Halogen (hoist, liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Localforage (createInstance, defaultLocalforageConfig)


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  idSupply <- liftEffect $ new 0
  localForage <- liftEffect $ createInstance defaultLocalforageConfig
  let env = { logLevel: Debug, idSupply, store: localForage }
      root = hoist (runApp env) Page.component
  runUI root unit body
