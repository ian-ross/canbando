{ name = "halogen-project"
, dependencies =
  [ "argonaut"
  , "bigints"
  , "colors"
  , "console"
  , "debug"
  , "effect"
  , "halogen"
  , "halogen-subscriptions"
  , "localforage"
  , "psci-support"
  , "remotedata"
  , "routing"
  , "routing-duplex"
  , "spec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
