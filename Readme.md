# happy-rad

This repository exposes two cabal packages which extend `happy` by a recursive ascent-descent backend:

- `rad-backend` contains the rad-backend implementation;
- `happy-rad` is the executable where the baseline happy packages and `rad-backend` are put together.

This means `happy-rad` exposes all standard `happy` functionality and additionally provides a recursive ascent descent backend. To use it, call `happy-rad` with the `-r / --rad` flag.

If the `--rad` flag is set, `happy-rad` will produce recursive ascent-descent code. Otherwise, it will produce normal LALR/GLR `happy` code.

## Building

Build via:

```
$ cabal build happy-rad
$ cabal build happy-rad -f -bootstrap
```

(depending on whether you want a bootstrapped happy (by default) or not), and perform the tests via:

```
$ cabal test happy-rad
$ cabal test happy-rad -f -bootstrap
```

