module Main where

import Prelude

import Canbando.AppM (runApp)
import Canbando.Capability.Resource.Board (initialBoards)
import Canbando.Env (LogLevel(..))
import Canbando.Page.Board as BoardPage
import Canbando.Page.Home as HomePage
import Canbando.TestInit (initTestStore)
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
  -- initBoard <- runApp env initTestStore
--  let root = hoist (runApp env) BoardPage.component
--  runUI root initBoard body
  initData <- runApp env initialBoards
  let root = hoist (runApp env) HomePage.component
  runUI root initData body
