{-
See README.txt

TOOLS
=====
1. Cabal
--------
- build system and package manager

    start a new project

    cabal init
    cabal configure

    cabal build
    cabal run
    cabal repl
    cabal haddock

- Sandboxes (in Cabal > 1.18)
    self contained environments of Haskell packages separate from
    the global package index stored in the ./.cabal-sandbox of our
    project's root.

    cabal sandbox init      -- new sandbox
    cabal sandbox delete    -- tear down a sandbox

    Invoking the cabal commands when in the working directory of a
    project with a sandbox configuration set up alters the behavior
    of cabal itself. For example the cabal install command will only
    alter the install to the local package index and will not touch
    the global configuration.

    cabal install --only-dependencies

    cabal sandbox add-source /path/to/project

-}
