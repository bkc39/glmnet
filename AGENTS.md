# AGENTS.md — how to work in this repository

`glmnet` is a Racket FFI binding to the classic **glmnet Fortran** coordinate-
descent solver (lasso / ridge / elastic net). It is the Fortran sibling of our
`scs` (C/Fortran), `xgboost-rkt` (C++), and `rkt-polars` (Rust) bindings and
shares their architecture. Read this file before adding a model or touching the
native layer.

## Architecture at a glance

```
fortran/                       native C-ABI shim (built as a Nix subderivation)
  vendor/glmnet5.f90           vendored GPL-2 glmnet Fortran (FIXED-FORM; pinned)
  glmnet_capi.f90              iso_c_binding wrappers -> clean bind(C) symbols
  CMakeLists.txt               -fdefault-real-8; per-file FIXED/FREE form; ctest
  tests/test_*.f90             standalone Fortran test drivers (ctest)
glmnet/                        Racket collection
  foreign/raw/library.rkt      ffi-lib loader + define-glmnet definer
  foreign/raw/*.rkt            raw FFI bindings (capi.rkt, elnet.rkt, ...)
  foreign.rkt                  contracted wrappers + load-time precision guard
  main.rkt                     public API (require glmnet)
  examples/NN-*.rkt            #lang scribble/lp2 literate examples (run-example)
  examples/test/NN-*.rkt       companion runners + rackunit harnesses
  scribblings/*.scrbl          user guide (guide/examples/reference)
  tests/*.rkt                  rackunit unit tests
  private/install-glmnet-native.rkt   pre-install hook (env -> staged -> candidate)
  native-libs/candidates/<plat>/      committed prebuilt shared objects
scripts/                       build-so.sh, glibc-shim.c, ... (portable candidates)
flake.nix                      native + racket derivations, devShell, checks
```

## Non-negotiable invariants

1. **`-fdefault-real-8`.** `glmnet5.f90` declares single-precision `real`; the
   whole elnet ABI is double *only* because we promote default `real` to 8
   bytes. Every binding (R, `glmnet_jll`) does the same. `glmnet_capi.f90`'s
   `glmnet_default_real_bytes` probe + the load-time guard in `foreign.rkt`
   exist to enforce this. Never drop the flag.
2. **`vendor/glmnet5.f90` is FIXED-FORM** (col-1 `c` comments, `*` continuation
   in col 6, sequence numbers in cols 73–80) despite the `.f90` name. The build
   sets `Fortran_FORMAT FIXED` on it and `FREE` on our shim. Do not reformat it;
   if you re-vendor, update `vendor/NOTICE.md` with the new pinned commit.
3. **Clean C ABI only.** Each shim entry point is `bind(C, name="…")` so it
   exports an unmangled symbol; the Racket side uses
   `convention:hyphen->underscore`. The internal `elnet_`/`spelnet_` symbols are
   never bound directly.
4. **GPL-2.0-or-later.** We vendor and link GPL-2 Fortran. Keep the license field
   and the root `LICENSE`/`vendor/NOTICE.md` consistent; do not relicense.
5. **The elnet wrapper densifies output.** `elnet` returns *compressed*
   coefficients (`ca`/`ia`/`nin`); the Fortran wrapper must uncompress them into
   a dense `beta(ni)` so the Racket side reads a plain vector.

## The per-feature workflow (follow in order for every new model/capability)

The four core models (OLS, ridge, lasso, elastic net) are one `elnet` call with
different `α` (`parm`) and `λ`. Each new capability is shipped as one unit:

1. **Add the example.** `glmnet/examples/NN-name.rkt`, `#lang scribble/lp2`,
   exporting `run-example`. Write the prose first (the model, its math, the
   expected result), then `@chunk` code. It won't run yet — it *is* the spec.
2. **Add the Fortran C-API.** Extend `fortran/glmnet_capi.f90` with a
   `bind(C)` entry mapping clean C args -> the internal `elnet` call (set
   `ka`/`jd`/`vp`/`cl`/`flmin`/`ulam`, uncompress `ca`/`ia` -> dense `beta`).
3. **Test the Fortran.** Add `fortran/tests/test_*.f90`: a tiny known dataset,
   assert outputs within tolerance, `error stop 1` on failure; register with
   `add_test` in `CMakeLists.txt`. Run `ctest` (`nix build .#native` or local
   `cmake`).
4. **Add the Racket raw binding.** Extend `foreign/raw/elnet.rkt` via
   `define-glmnet` (`_f64vector`/`_s32vector` buffers, `(_ptr o …)` scalar outs;
   no allocator/finalizer — these are pure calls). Add a contracted wrapper in
   `foreign.rkt` (row->column-major marshal, `jerr` check, result struct).
5. **Test the Racket binding.** `glmnet/tests/*-test.rkt` rackunit: round-trip vs
   closed-form / known values, `jerr` error surfacing, shape-mismatch contract
   errors.
6. **Verify the example end to end.** Wire `glmnet/examples/test/NN-name.rkt`
   (`module+ main` runner + `module+ test` asserting the documented result);
   `raco test` passes; `racket glmnet/examples/test/NN-name.rkt` prints it.
7. **Add the user-guide page.** Extend `scribblings/guide.scrbl` (concept) and
   `@lp-include` the example in `scribblings/examples.scrbl`.
8. **Add the reference entry.** Document the new public proc in
   `scribblings/reference.scrbl` (contract + short example).

**Gate (all green before the next feature):**
`raco test ./glmnet/` · `raco scribble --htmls …/glmnet.scrbl` renders ·
`nix flake check` · resyntax clean.

## Local dev loop

```bash
# native library
cmake -S fortran -B fortran/build -DBUILD_TESTING=ON && cmake --build fortran/build
ctest --test-dir fortran/build --output-on-failure
cp fortran/build/libglmnetcompat.* glmnet/native-libs/      # stage for the loader

# racket (link mode, once)
raco pkg install --batch --auto --link --name glmnet ./glmnet
raco test ./glmnet/
bash scripts/run-examples.sh                                 # run every example
```

Or `nix build .#native` (runs the Fortran ctest suite) and `nix flake check`
(builds the native lib + Racket package, runs `raco test`, renders the docs).

## Shipping native libraries (catalog candidates)

Using the package needs no toolchain because a prebuilt `libglmnetcompat` (plus
its gfortran/quadmath runtime) is committed per platform under
`glmnet/native-libs/candidates/<platform>/` and staged at install time by
`private/install-glmnet-native.rkt` (env var → already-staged → candidate).

Build a candidate with `scripts/build-so.sh <darwin|linux|linux-aarch64>`. It
builds the `.#native` derivation, bundles the runtime closure, sets portable
rpaths (`@rpath`/`@loader_path` on macOS; `RUNPATH=$ORIGIN` on Linux), and on
Linux runs `polyfill-glibc` + `libglmnetshim.so` (scripts/glibc-shim.c) to drop
the glibc floor to 2.17 so it loads on Ubuntu 22.04 (the catalog build server)
and older. `scripts/test-local.sh` then installs from the candidate with a plain
Racket (no Nix) and runs the suite — exactly what `pkgs.racket-lang.org` does.

The script needs Nix and must run **on each target platform**: build the
`darwin` candidate on macOS and the `linux`/`linux-aarch64` candidates on a Linux
host (`.github/workflows/raco-catalog.yml` builds and validates them in CI, with
`ubuntu-22.04` exercised explicitly). Commit the resulting `candidates/<platform>/`
files; only the loose copies directly under `native-libs/` are git-ignored.

The **Linux candidate has a known blocker** (polyfill-glibc vs libmvec SIMD math)
and its CI legs are currently allowed-to-fail — see **`LINUX_CANDIDATE.md`** for
the build steps, the diagnosis, and the fix to apply on the Linux host.

## Roadmap

v1 = Phase 0 hello + OLS / ridge / lasso / elastic net (single-λ "solo" fits).
Future arc (additive, no rework — the wrapper is already path-capable): full
regularization path (`nlam>1`, `flmin<1`), cross-validation, sparse `spelnet`,
and other GLM families (`lognet` logistic, `coxnet`, `fishnet`) — all already
present in `vendor/glmnet5.f90`.
