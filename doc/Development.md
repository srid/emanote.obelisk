To run emanote locally,

1. Install https://github.com/obsidiansystems/obelisk
1. Build CSS by side: `bin/css` 
   - This is optional; it should be run if you are modifying CSS
2. Run the development server: 
   ```
   ob run
   ```

Go to <http://localhost:8000/> ([Don't use Firefox](https://github.com/reflex-frp/reflex-examples/issues/30#issuecomment-462827693))

Note:

- Edit `config/backend/notesDir` to run on your own notebook instead of on `./doc`.
- The development server may be glitchy, and slow at times (due to jsaddle being unreliable). 
  - When that happens, simply restart `ob run`
  - The full build doesn't suffer from this issue.

To do a **full build**, refer to Obelisk's deployment guide, and to run in production, try [obelisk-systemd](https://github.com/obsidiansystems/obelisk-systemd).