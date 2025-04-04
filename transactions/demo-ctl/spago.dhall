{ name = "demo-ctl"
, dependencies =
  [ "aeson"
  , "aff"
  , "arraybuffer"
  , "arraybuffer-types"
  , "arrays"
  , "b64"
  , "cardano-transaction-lib"
  , "cardano-types"
  , "control"
  , "effect"
  , "either"
  , "encoding"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "gen"
  , "js-bigints"
  , "maybe"
  , "mote"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "ordered-collections"
  , "partial"
  , "posix-types"
  , "plutus-types"
  , "prelude"
  , "rationals"
  , "quickcheck"
  , "quickcheck-utf8"
  , "spec"
  , "strings"
  , "tuples"
  , "uint"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs", ".extras/**/*.purs" ]
}
