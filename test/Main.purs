module Test.Main where

import Prelude

import Canbando.Routes (Route(..), route)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Routing.Duplex (parse)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Canbando" do
    describe "Routing" do
      it "recognises / route" $
        parse route "/" `shouldEqual` Right Home
      it "recognises /board/B00000001 route" $
        parse route "/board/B00000001" `shouldEqual` Right (Board "B00000001")
      it "recognises /reset-test-data route" $
        parse route "/reset-test-data" `shouldEqual` Right ResetTestData
