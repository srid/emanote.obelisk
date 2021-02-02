**Be warned** that this filter is not complete (see the TODO below).

This filter maps every folder to a note with the name of that folder. If a `${folderName}.md` file already exists, it is reused. Finally, it appends a tag-link `#[[Parent]]` on every file (or folder) belonging to every `Parent/` folder. 

In effect, this filter maps the filesystem directory hierarchy to your notebook graph.


## TODO

- [ ] File deletions don't propagate to the directory hierarchy, yet. As a workaround, you can simply restart emanote after file deletions or moves.