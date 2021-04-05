module Main where

import Prelude

import Canbando.AppM (runApp)
import Canbando.Env (LogLevel(..))
import Canbando.Router (Query(..))
import Canbando.Router as Router
import Canbando.Routes (route)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Ref (new)
import Halogen (hoist, liftEffect, mkTell)
import Halogen.Aff as HA
import Halogen.Subscription (create)
import Halogen.VDom.Driver (runUI)
import Localforage (createInstance, defaultLocalforageConfig)
import Routing.Duplex (parse)
import Routing.PushState (makeInterface, matchesWith)


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody

  idSupply <- liftEffect $ new 0
  localForage <- liftEffect $ createInstance defaultLocalforageConfig
  nav <- liftEffect makeInterface
  labels <- liftEffect $ new []
  { emitter, listener } <- liftEffect create

  let env = { logLevel: Debug
            , idSupply
            , store: localForage
            , nav
            , labels, labelEmitter: emitter, labelListener: listener }
  let root = hoist (runApp env) Router.component
  halogenIO <- runUI root unit body

  void $ liftEffect $
    nav # matchesWith (parse route) \old new ->
      launchAff_ $ halogenIO.query $ mkTell $ Nav new
