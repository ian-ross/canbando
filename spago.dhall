{ name = "halogen-project"
, dependencies =
  [ "argonaut"
  , "argonaut-generic"
  , "bigints"
  , "console"
  , "debug"
  , "effect"
  , "generics-rep"
  , "halogen"
  , "halogen-hooks"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
