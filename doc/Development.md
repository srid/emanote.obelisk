To run locally,

1. Install https://github.com/obsidiansystems/obelisk
1. Run `bin/css` by side (to build CSS)
1. Run `ob run`

Go to <http://localhost:8000/> ([Don't use Firefox](https://github.com/reflex-frp/reflex-examples/issues/30#issuecomment-462827693))

Edit `config/backend/notesDir` to run on your own notebook.

To do a **full build** (uses GHCJS to produce JS that runs entirely on browser):

```
nix-build -A exe
```
