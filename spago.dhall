{ name = "halogen-project"
, dependencies =
  [ "argonaut"
  , "bigints"
  , "console"
  , "debug"
  , "effect"
  , "halogen"
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
