# glmnet

Racket bindings for [glmnet](https://glmnet.stanford.edu/) — lasso and
elastic-net regularized models — via the original Friedman/Hastie/Tibshirani
coordinate-descent **Fortran**, wrapped behind a clean C ABI.

This is the Fortran member of a family of Racket FFI bindings (`scs`,
`xgboost-rkt`, `rkt-polars`). The numerics come from the self-contained glmnet
Fortran (vendored, no BLAS/LAPACK dependency); a small `iso_c_binding` shim
exports a C ABI that the Racket FFI binds to.

## Status

Example-driven, in progress. Phase 0 (toolchain + FFI spine) is complete; the
four core models — OLS, ridge, lasso, elastic net — land one at a time, each as
a runnable literate example that doubles as the user guide. See `AGENTS.md` for
the development workflow and `plans/` notes.

## Install

```racket
(require glmnet)
```

A prebuilt native library is staged at install time; no Fortran toolchain is
needed to use the package.

## Quick check

```racket
(require glmnet)
(glmnet-hello 2.5 4.0)        ; => 6.5
(glmnet-default-real-bytes)  ; => 8   (the -fdefault-real-8 precision contract)
```

## Build from source

```bash
nix build .#native      # build libglmnetcompat + run the Fortran ctest suite
nix flake check         # build everything, run raco test, render docs
```

or with a local toolchain (gfortran + cmake + Racket): see `AGENTS.md`.

## License

**GPL-2.0-or-later.** This package vendors and links the GPL-2.0 glmnet Fortran
(`fortran/vendor/glmnet5.f90`); see `LICENSE` and `fortran/vendor/NOTICE.md`.
