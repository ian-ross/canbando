{ name = "halogen-project"
, dependencies =
  [ "console", "debug", "effect", "generics-rep", "halogen", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
