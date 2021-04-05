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
  , "httpure"
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
