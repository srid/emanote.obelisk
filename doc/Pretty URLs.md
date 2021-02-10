Notes named `Foo Bar-Qux.md` will get the URL slug `Foo_Bar-Qux`. Essentially we replace whitespace with underscores; and this is done only to avoid the unseamly "%20" in the URL bar.

- [ ] Support disambiguating filenames with underscore. For eg., `Foo bar_qux.md` should be supported; currently they don't work.