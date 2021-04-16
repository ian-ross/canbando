{ name = "halogen-project"
, dependencies =
  [ "affjax"
  , "argonaut"
  , "bigints"
  , "codec-argonaut"
  , "colors"
  , "console"
  , "debug"
  , "effect"
  , "halogen"
  , "halogen-subscriptions"
  , "httpure"
  , "httpure-middleware"
  , "localforage"
  , "mysql"
  , "psci-support"
  , "remotedata"
  , "routing"
  , "routing-duplex"
  , "spec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
