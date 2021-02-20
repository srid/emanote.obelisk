Emanote has two high-level components:

1. [Obelisk](https://wiki.srid.ca/-/Obelisk) full-stack app:
  - `./frontend` - Frontend Haskell code that compiles to JS
  - `./backend` - Backend Haskell code (the main process)
  - `./common` - Haskell code shared between the above two
2. Core libraries
  - `./lib/emanote`: The core build engine for Emanote. It *incrementally* transforms a directory of Markdown notes to an in-memory Haskell value (TVar + TChan), that in turn is used by the Obelisk app.
  - `./lib/emanote-core`: Portion of the above library extracted for use in GHCJS frontend.
  - `./lib/algebraic-graphs-patch`: Diff and patching for the algebraic-graphs library.
