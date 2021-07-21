let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.1-20210613/packages.dhall sha256:5f10380b3ca7d3a32ea5c2b7535e4814a5e3f3590c70692f76e596d6ab0687b3

let overrides =
      { test-unit =
        { dependencies =
          [ "aff"
          , "either"
          , "prelude"
          , "effect"
          , "quickcheck"
          , "free"
          , "strings"
          , "lists"
          , "js-timers"
          , "avar"
          ]
        , repo = "https://github.com/milesfrain/purescript-test-unit.git"
        , version = "stackless-default"
        }
      }

let additions =
      { react-basic =
        { dependencies = [ "prelude", "effect", "record" ]
        , repo = "https://github.com/lumihq/purescript-react-basic.git"
        , version = "main"
        }
      , react-basic-hooks =
        { dependencies =
          [ "prelude"
          , "aff-promise"
          , "aff"
          , "console"
          , "datetime"
          , "effect"
          , "either"
          , "indexed-monad"
          , "maybe"
          , "newtype"
          , "numbers"
          , "react-basic"
          , "type-equality"
          , "unsafe-coerce"
          , "unsafe-reference"
          , "web-html"
          ]
        , repo =
            "https://github.com/milesfrain/purescript-react-basic-hooks.git"
        , version = "v6.3.0-ps-0.14"
        }
      , react-basic-dom =
        { dependencies =
          [ "prelude"
          , "effect"
          , "foreign-object"
          , "react-basic"
          , "unsafe-coerce"
          , "web-dom"
          , "web-events"
          , "web-file"
          , "web-html"
          ]
        , repo = "https://github.com/lumihq/purescript-react-basic-dom.git"
        , version = "v3.2.0"
        }
      , indexed-monad =
        { dependencies = [ "control", "newtype" ]
        , repo = "https://github.com/garyb/purescript-indexed-monad.git"
        , version = "master"
        }
      }

in  upstream // overrides // additions
