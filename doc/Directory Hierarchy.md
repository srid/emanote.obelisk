**Be warned** that this filter is not complete (see the TODO below), and is disabled by default, because the author feels that it will easily violate Zettelkasten principles, and that users are better off creating structure links manually (which forces them to make them with *context).

This filter maps every folder to a note with the name of that folder. If a `${folderName}.md` file already exists, it is reused. Finally, it appends a tag-link `#[[Parent]]` on every file (or folder) belonging to every `Parent/` folder. 

In effect, this filter maps the filesystem directory hierarchy to your notebook graph.


## TODO

- [ ] File deletions don't propagate to the directory hierarchy, yet. As a workaround, you can simply restart emanote after file deletions or moves.
