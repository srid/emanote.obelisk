This filter maps every folder to a note with the name of that folder. If a `.md` with that file already exists, that note is reused. Finally, it appends a tag-link `#[[Parent]]` on every file belonging to the `Parent/` folder. 

In effect, it maps the filesystem directory hierarchy to your notebook graph.

## TODO

- [ ] File deletions don't propagate to the directory hierarchy, yet. As a workaround, you can simply restart emanote after file deletions or moves.