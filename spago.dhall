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
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
