{ name = "routing-duplex"
, dependencies =
  [ "arrays"
  , "assert"
  , "bifunctors"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "js-uri"
  , "lazy"
  , "maybe"
  , "newtype"
  , "partial"
  , "prelude"
  , "profunctor"
  , "quickcheck"
  , "record"
  , "strings"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
