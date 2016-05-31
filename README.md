[![Build Status](https://travis-ci.org/ony/exherbo-cabal.svg?branch=master)](https://travis-ci.org/ony/exherbo-cabal)

Description
===========
Generate package description from .cabal files in format of exheres-0 for
Exherbo Linux.

Usage
-----
As a source you can use one of the following reference:
- ./LOCAL-FILE.cabal and /LOCAL-FILE.cabal: read from LOCAL-FILE.cabal
- PACKAGE-VERSION: fetch appropriate package from Hackage

You can either pass sources as a list of arguments or as lines of standard
input if no arguments provided.

Examples
--------
```
> exherbo-cabal mtl-2.2.1
> exherbo-cabal mtl transformers
> echo yesod-core | exherbo-cabal
> exherbo-cabal ./exherbo-cabal.cabal
> find /tmp/index -name \*.cabal | exherbo-cabal
```
