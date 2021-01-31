While hacking, run this:

```
bin/run ./doc
```

To **build**:

```
nix-build
```

Then **run** it against your folder of Markdown files (but first, copy `index.html` and `style.css` from ./doc directory):

```
./result/bin/emanote /path/to/your/notebook 
```

Go to <http://localhost:3000/>
