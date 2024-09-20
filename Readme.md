# happy-rad

This repository is a version of [happy](https://github.com/haskell/happy) which uses a recursive ascent-descent backend as described in [this bachelors thesis](https://pp.ipd.kit.edu/uploads/publikationen/knothe20bachelorarbeit.pdf). It uses happy-lib to do .y-file parsing and processing and only uses a different backend for RAD code generation.

## Building

Build via:

```
$ cabal build
$ cabal build -f -bootstrap
```

(depending on whether you want a bootstrapped happy (by default) or not), and perform the tests via:

```
$ cabal test
$ cabal test -f -bootstrap
```

