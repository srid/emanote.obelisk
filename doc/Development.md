To run locally,

1. Install https://github.com/obsidiansystems/obelisk
1. Run `bin/css` by side (to build CSS)
1. Run `ob run`

Go to <http://localhost:8000/> ([Don't use Firefox](https://github.com/reflex-frp/reflex-examples/issues/30#issuecomment-462827693))

Note:

- Edit `config/backend/notesDir` to run on your own notebook.
- The development server may be glitchy, and slow at times. But the full build should be reliable and performant.

To do a **full build**, refer to Obelisk's deployment guide.