# Development

While hacking, run this:

```
nix-shell --run 'ghcid -T ":main ./doc"'
```

To **build**:

```
nix-build
```

Then **run** it against your folder of Markdown files:

```
./result/bin/g /path/to/your/notebook /path/to/output
```
