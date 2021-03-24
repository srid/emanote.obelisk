# emanote
[![Built with Nix](https://img.shields.io/static/v1?logo=nixos&logoColor=white&label=&message=Built%20with%20Nix&color=41439a)](https://nixos.org) [![Obelisk](https://img.shields.io/badge/Powered%20By-Obelisk-black?style=flat&logo=data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI3NjgiIGhlaWdodD0iNzY4Ij48ZyBmaWxsLXJ1bGU9ImV2ZW5vZGQiPjxwYXRoIGQ9Ik0zMDUuODggNjIyLjY3M2MtMzcuOTI0LTEyLjM4Ni03MS44NzMtMzMuNTU2LTk5LjQzNS02MS4xMThDMTYxLjAyIDUxNi4xMjkgMTMyLjk1MiA0NTMuMzQ0IDEzMi45NTIgMzg0YzAtNjkuMjU3IDI4LjA2Ny0xMzIuMTMgNzMuNDkzLTE3Ny41NTVDMjUxLjg3MSAxNjEuMDIgMzE0LjY1NiAxMzIuOTUyIDM4NCAxMzIuOTUyYzY5LjM0NCAwIDEzMi4xMyAyOC4wNjcgMTc3LjU1NSA3My40OTNDNjA2Ljk4IDI1MS44NzEgNjM1LjA0OCAzMTQuNzQzIDYzNS4wNDggMzg0YzAgNjkuMzQ0LTI4LjA2NyAxMzIuMTMtNzMuNDkzIDE3Ny41NTVDNTE2LjEyOSA2MDYuOTggNDUzLjM0NCA2MzUuMDQ4IDM4NCA2MzUuMDQ4VjE2MS4zNWwtMzkuNjEgMzIuMDU2LTM4LjUxIDQyOS4yNjYiIGZpbGw9IiMyZDMyM2IiLz48cGF0aCBkPSJNMzg0IDYwNi42NDdjNjEuNDk5IDAgMTE3LjE3OS0yNC44OTUgMTU3LjQ2NS02NS4xODJDNTgxLjc1MiA1MDEuMTggNjA2LjY0NyA0NDUuNSA2MDYuNjQ3IDM4NGMwLTYxLjQyNS0yNC44OTUtMTE3LjE3OS02NS4xODItMTU3LjQ2NUM1MDEuMTggMTg2LjI0OCA0NDUuNSAxNjEuMzUzIDM4NCAxNjEuMzUzdjQ0NS4yOTQiIGZpbGw9IiM3MDllYjUiLz48cGF0aCBkPSJNMzg0IDYzNS4wNDhjMjYuOTkgMCA1My41NjQtNC4yMzYgNzkuMjI1LTEyLjc5TDQyMy42MTMgMTkzLjQxIDM4NCAxNjEuMzUzdjQ3My42OTUiIGZpbGw9IiMyZDMyM2IiLz48L2c+PC9zdmc+)](https://github.com/obsidiansystems/obelisk)
[![Matrix](https://img.shields.io/matrix/neuron:matrix.org)](https://app.element.io/#/room/#neuron:matrix.org)

Write plain-text notes, but do complex things with it - such as to eman**a**te a smart notebook.

> *emanate*: (of something abstract but perceptible) issue or spread out from (a source).

## Purpose

Goals: large **private** Zettelkastens, **dynamic** navigation, simplicity, performance.

Non-goals: static site publishing, theming.

Maybe-goals: use `emanote-core` as a core library in neuron.

## Give it a test-drive

Clone the source, and read `./doc/Development.md`

## Self-host it in production

If you are NixOS, [obelisk-systemd](https://github.com/obsidiansystems/obelisk-systemd) can be used to automate self-hosting. Otherwise, on other Linux, follow the instructions below.

1. Install Nix
1. Set up nix cache by following [instructions here](https://github.com/obsidiansystems/obelisk#installing-obelisk)
2. Build it (might take a while):
    ```
    nix-build -A exe -j auto -o ./result
    ```
3. Prepare runtime files:
    ```
    mkdir ~/my-emanote
    cp -r ./result/* ~/my-emanote
    cp -r config ~/my-emanote/
    ```
4. Tell emanote where your Zettelkasten (directory of Markdown files) lives:
    ```
    vim ~/emanote/config/backend/notesDir  
    ```
5. Run it! (on http://localhost:8000)
    ```
    cd ~/my-emanote
    ./backend -p 8000  #
    ```
6. NOTE: When self-hosting and exposing under a different address, say www.example.com, you must edit `~/my-emanote/config/common/route` to contain the corresponding URL, i.e., `https://www.example.com`.

## Talk about it

Join us in Matrix: https://app.element.io/#/room/#neuron:matrix.org
