{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purs-noc"
, dependencies = [ "console", "effect", "halogen", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
