# Torrskel

## Setup

`stack setup`

## Commands

Build project:

`stack build`

Run tests:

```bash
stack test
# or, for hot reloading:
npm install -g nodemon
nodemon -w ./src,./test -e hs -x 'stack test'
```

## Formatting

This project uses [hindent](https://github.com/commercialhaskell/hindent). In
Vim, run hindent on file save with
[vim-hindent](https://github.com/alx741/vim-hindent).

## Documents

All code written is based on my interpretation of

[http://www.bittorrent.org/beps/bep_0003.html](http://www.bittorrent.org/beps/bep_0003.html)

and

[https://wiki.theory.org/BitTorrentSpecification](https://wiki.theory.org/BitTorrentSpecification)
