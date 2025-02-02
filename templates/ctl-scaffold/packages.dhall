let upstream =
    -- https://github.com/mlabs-haskell/purescript-cardano-package-set
      https://raw.githubusercontent.com/mlabs-haskell/purescript-cardano-package-set/df6380a005e5fdb9f1642ab9f1e86d22a00194cc/packages.dhall sha256:af8b447d071f5404716fef7d3d2e45d42f838c8858bc83dc5d98e77dbd167259

let additions =
      { cardano-transaction-lib =
        { dependencies =
          [ "aeson"
          , "aff"
          , "aff-promise"
          , "aff-retry"
          , "affjax"
          , "ansi"
          , "argonaut"
          , "argonaut-codecs"
          , "arrays"
          , "avar"
          , "bifunctors"
          , "bignumber"
          , "bytearrays"
          , "cardano-hd-wallet"
          , "cardano-key-wallet"
          , "cardano-message-signing"
          , "cardano-plutus-data-schema"
          , "cardano-serialization-lib"
          , "cardano-transaction-builder"
          , "cardano-types"
          , "cardano-provider"
          , "checked-exceptions"
          , "cip30"
          , "cip30-mock"
          , "cip30-typesafe"
          , "cip95"
          , "cip95-typesafe"
          , "console"
          , "control"
          , "crypto"
          , "datetime"
          , "debug"
          , "effect"
          , "either"
          , "enums"
          , "exceptions"
          , "foldable-traversable"
          , "foreign"
          , "foreign-object"
          , "formatters"
          , "functions"
          , "heterogeneous"
          , "http-methods"
          , "identity"
          , "integers"
          , "js-bigints"
          , "js-date"
          , "lattice"
          , "lists"
          , "literals"
          , "maybe"
          , "media-types"
          , "monad-logger"
          , "mote"
          , "mote-testplan"
          , "newtype"
          , "noble-secp256k1"
          , "node-buffer"
          , "node-child-process"
          , "node-fs"
          , "node-fs-aff"
          , "node-path"
          , "node-process"
          , "node-readline"
          , "node-streams"
          , "node-streams-aff"
          , "nonempty"
          , "now"
          , "nullable"
          , "numbers"
          , "optparse"
          , "ordered-collections"
          , "orders"
          , "parallel"
          , "parsing"
          , "partial"
          , "plutus-types"
          , "posix-types"
          , "prelude"
          , "profunctor"
          , "profunctor-lenses"
          , "quickcheck"
          , "quickcheck-combinators"
          , "random"
          , "rationals"
          , "record"
          , "refs"
          , "safe-coerce"
          , "safely"
          , "spec"
          , "spec-quickcheck"
          , "strings"
          , "stringutils"
          , "tailrec"
          , "these"
          , "toppokki"
          , "transformers"
          , "tuples"
          , "typelevel-prelude"
          , "uint"
          , "unfoldable"
          , "unsafe-coerce"
          , "untagged-union"
          , "uplc-apply-args"
          , "variant"
          , "web-html"
          , "web-storage"
          ]
        , repo = "https://github.com/Plutonomicon/cardano-transaction-lib.git"
        , version = "38912b635cd72cc00fee1387717afe1178476f23"
        }
      }

in  upstream // additions
