To run locally,

1. Install https://github.com/obsidiansystems/obelisk
1. Build CSS by side: `bin/css` (optional; run if you are modifying css)
1. Run the development server: `ob run`

Go to <http://localhost:8000/> ([Don't use Firefox](https://github.com/reflex-frp/reflex-examples/issues/30#issuecomment-462827693))

Note:

- Edit `config/backend/notesDir` to run on your own notebook instead of on `./doc`.
- The development server may be glitchy, and slow at times (due to jsaddle being unreliable). 
  - You can try disabling websockets; set `config/frontend/requestType` to `xhr`.
  - The full build doesn't suffer from this issue; so you should use websockets there.

To do a **full build**, refer to Obelisk's deployment guide.