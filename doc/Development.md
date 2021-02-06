To run locally,

1. Install https://github.com/obsidiansystems/obelisk
1. Build CSS (see below)
2. Run `ob run`

To **build**:

```
nix-build -A exe
```

Go to <http://localhost:3000/> (This will soon get replaced with Obelisk's backend running at 8000)

## Building CSS

TODO: Nixify this and put in Procfile

```
cd static/style
npm install
node_modules/.bin/postcss  main.css -o main-compiled.css
```