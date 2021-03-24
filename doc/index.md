# Emanote Docs

## What's [emanote](https://github.com/srid/emanote)?

Srid's playground for uninhibitedly exploring the problem-space of [Unix-pipeline](https://en.wikipedia.org/wiki/Pipeline_(Unix))-based transformation of a directory of Markdown files (or anything!) to sophisticated structures like directed graphs, all the while supporting end-to-end *incremental* updates (as your files change, so will the graph---instantly). Also, using a fully dynamic frontend rather than being limited to what a statically generated site can ofter.

Interested in trying out, or hacking? 

- Start from [[Development]]#
- Familiarize yourself with,
  - Haskell (of course) 
  - Haskell's [STM](http://book.realworldhaskell.org/read/software-transactional-memory.html) (incremental patching of in-memory database).
  - Frontend programming in Haskell: [Reflex]'s `Incremental` type, as well as [Obelisk].
  - [[Architecture]]#

## [WIP] User Guide

- Philosophy: [[Keep It Simple, Stupid]]#
- [[Features]]#

[Reflex]: https://www.srid.ca/reflex-frp
[Obelisk]: https://www.srid.ca/obelisk