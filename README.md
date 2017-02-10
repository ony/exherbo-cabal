[![Build Status](https://travis-ci.org/ony/exherbo-cabal.svg?branch=master)](https://travis-ci.org/ony/exherbo-cabal)

Description
===========
Generate package description from `*.cabal` files in format of exheres-0 for
Exherbo Linux.

Usage
-----
As a source you can use one of the following reference:
- `./LOCAL-FILE.cabal` and `/LOCAL-FILE.cabal`: read from `LOCAL-FILE.cabal`
- `PACKAGE-VERSION`: fetch appropriate package from the Hackage

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

Development notes
-----------------

Whenever code being changed and version bump required according to [Haskell
package versioning policy](https://pvp.haskell.org/) you should ensure that
version in cabal file is higher than the on from last release tag.

No need to bump version if one of the higher-rank version parts were bumped
since last release.

During bump of high-rank version part all the minors should be reset to zero.

For example image that the latest release tag is `v0.2.1.1`.
- Patch code changes requires version bump to `0.2.1.2` if current cabal file
  still refers to `0.2.1.1`
- Patch code changes shouldn't bump the version if current cabal file already
  specifies version similar to `0.2.1.2` or `0.2.2.0` and so on.
- First commit that extends API should bump version to `0.2.2.0` if version in
  cabal file is still `0.2.2.1`.

I.e. the only possible values for version in cabal file in case if the last
release was `v0.2.1.1`:
- `0.2.1.1`: only changes unrelated with code behavior. I.e. development
  infrastructure changes (`.gitignore`, `.travis.yml`, formatting, etc).
- `0.2.1.2`: there were some change in the code. Like bug fix, performance
  improvement, re-factoring, etc.
- `0.2.2.0`: current head contains extends API in a non-breaking way since last
  release.
- `0.3.0.0`: API is different from the one provided in last release. Changes
  may result in breaking some non-obscure clients.
- `1.0.0.0`: first API intended for usage outside of this package.
