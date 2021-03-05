{ name = "halogen-project"
, dependencies =
  [ "argonaut"
  , "argonaut-generic"
  , "console"
  , "debug"
  , "effect"
  , "generics-rep"
  , "halogen"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
