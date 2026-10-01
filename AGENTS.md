# AGENTS.md — how to work in this repository

`glmnet` is a Racket FFI binding to the classic **glmnet Fortran** coordinate-
descent solver (lasso / ridge / elastic net). It is the Fortran sibling of our
`scs` (C/Fortran), `xgboost-rkt` (C++), and `rkt-polars` (Rust) bindings and
shares their architecture. Read this file before adding a model or touching the
native layer.

## Architecture at a glance

```
fortran/                       native C-ABI shim (built as a Nix subderivation)
  vendor/glmnet5dpclean.f      R glmnet 4.1 Fortran, byte-for-byte (FIXED-FORM; pinned)
  glmnet_capi.f90              iso_c_binding wrappers -> clean bind(C) symbols
  r_stubs.f90                  no-op setpb (R's progress callback)
  CMakeLists.txt               -fdefault-real-8 -fdefault-double-8; FIXED/FREE form; ctest
  tests/test_*.f90             standalone Fortran test drivers (ctest)
glmnet/                        Racket collection
  foreign/raw/library.rkt      ffi-lib loader + define-glmnet definer
  foreign/raw/*.rkt            raw FFI bindings (capi.rkt, elnet.rkt, ...)
  foreign.rkt                  contracted wrappers + load-time precision guard
  data.rkt                     design-matrix layer (glmnet/data): the one input layout,
                               and tables (named columns) that convert into it
  data/nested.rkt              glmnet/data/nested: the four nestings of lists and
                               vectors to and from a design matrix (not re-exported)
  data/csv.rkt                 glmnet/data/csv: CSV files to and from tables (RFC 4180,
                               each cell typed as R's type.convert types a column of
                               that one cell); each data format is a module in data/ (#41)
  data/math.rkt                glmnet/data/math: math/matrix matrices <-> design matrices,
                               math arrays -> responses; its element loops are a Typed
                               Racket submodule; main.rkt does not load it (math-lib)
  data/polars.rkt              glmnet/data/polars: rkt-polars dataframes to and from
                               design matrices, responses and tables; not re-exported
  data/tabular-asa.rkt         glmnet/data/tabular-asa: tabular-asa tables to and from
                               design matrices, responses and tables; not re-exported
  datasets.rkt                 glmnet/datasets: R glmnet's example datasets (loaders
                               returning the family fitter's arguments) and R's
                               mtcars and iris, from datasets/*.csv
  datasets/*.csv               R's data, written by scripts/export-datasets.R
  core/*.rkt                   one module per family; marshal.rkt, path.rkt shared
  core/model.rkt               gen:glmnet-model: predict / coef / deviance-ratio on any result
  core/cv.rkt                  cross-validation (R's cv.glmnet) behind every family's *-cv
  core/formula.rkt             formula front end: (~ y all) on a table -> any family,
                               a formula-model with name-keyed coef and predict
  core/terms.rkt               formula terms: R's terms() expansion (+ - * : ^, 0/1),
                               transforms (log x), (I expr), factors (strings,
                               booleans, (factor x)) with their levels, and
                               model.matrix()'s coding; terms are sorted index lists
  main.rkt                     public API (require glmnet)
  plot.rkt                     glmnet/plot: R's plot.glmnet / plot.cv.glmnet on plot-lib
                               (picts); not re-exported by main.rkt
  examples/NN-*.rkt            #lang scribble/lp2 literate examples (run-example)
  examples/test/NN-*.rkt       companion runners + rackunit harnesses
  scribblings/glmnet.scrbl     manual root: guide.scrbl (guide/*.scrbl) + reference.scrbl;
                               the plots are guide/plots.scrbl and reference's ref-plot
  scribblings/utils.rkt        for-label imports + make-glmnet-eval for live examples
  tests/*.rkt                  rackunit unit tests
  private/install-glmnet-native.rkt   pre-install hook (env -> staged -> candidate)
  native-libs/candidates/<plat>/      committed prebuilt shared objects
scripts/                       build-so.sh, test-local.sh (portable candidates);
                               export-datasets.R (glmnet/datasets/*.csv from R)
flake.nix                      native + racket derivations, devShell, checks
```

There is one package, one collection and one manual (arc #44, decision 4, as
the owner revised it). The plots, `glmnet/plot`, are part of the `glmnet`
package, which therefore depends on plot-lib, but `main.rkt` does not re-export
them: plot-lib is Typed Racket and pulls in the drawing stack, and
`(require glmnet)` must not load it, just as Racket's own `plot` stays out of
`racket`. Their documentation is a guide chapter (`guide/plots.scrbl`, tag
`plots`) and a reference section (`ref-plot`, `@defmodule[glmnet/plot]`).

Data sources follow the same rule (#41, #61): a conversion from a format
into a design matrix or a table is a module `glmnet/data/<format>.rkt`, such
as `glmnet/data/csv`, and the example datasets are `glmnet/datasets`;
`main.rkt` re-exports neither. The documentation tags follow the module
names:

- the guide's Data chapter, `guide/data.scrbl` (tag `data`), has a section
  `data-<format>` for each format and `data-datasets` for the datasets;
- the reference has a top-level section `ref-data-<format>` for each, with
  its `@defmodule`, after `ref-data` (the design-matrix layer) and before
  `ref-datasets`;
- the Concepts section `concepts-data` holds only what every fitter accepts:
  the design matrix, the response forms and named data.

A format builds its design matrix, and reports bad data, with the one support
set in `data.rkt`'s `support` submodule, which documents each procedure:
`flat->design-matrix` (a flat flvector or f64vector, column- or row-major,
copied or adopted, its length checked by contract), `element-error` and
`missing-error` (the two error shapes, "<what> has an element that is not a
real number / not finite" and "<what> has a missing value", with the fields
column, row and element, or position), `->finite-flonum` and
`default-column-names` (R's `V1` ... `Vn`). Column names are strings in every
design matrix. `tests/docs-coverage-test.rkt` lists `glmnet/data/` itself, so
a new format's exports are checked without editing the test. The datasets are CSV files that
`scripts/export-datasets.R` writes from the pinned R; the `dataset-*` parity
goldens check every number of every file against R's, bit for bit, the
`vignette-*` goldens the vignette's calls on each dataset, and the
`csv-cells` golden how R's `read.csv` types each spelling of a cell, in a
UTF-8 `LC_CTYPE` that `gen-reference.R` sets, since white space is
locale-dependent in R and the Nix sandbox runs R in the C locale.

The library a format adapts is a real dependency in `info.rkt`, which
`main.rkt` does not load. `glmnet/data/polars` depends on the catalog package
`polars` (rkt-polars, Apache-2.0 OR MIT):

- **Its version is not pinned.** The adapter needs `dataframe->f64vector`, but
  rkt-polars has not bumped its version since adding it
  (bkc39/rkt-polars#144), so `info.rkt` cannot ask for a new enough polars;
  `tests/polars-version-test.rkt` fails, saying so, on an older one. Add
  `#:version` to the dependency once #144 lands.
- **Platforms.** polars ships native libraries for Linux x86-64 and macOS
  arm64 only, so glmnet installs only there.
- **Nix.** The sandbox cannot reach the catalog, so `flake.nix` installs
  rkt-polars from its flake input (`nix flake update rkt-polars` moves it),
  with its Racket dependencies from that flake's fixed-output `racket-deps`,
  installed first. The flake stages the system's own native library itself
  and throws for a system with none, because polars' pre-install hook picks
  the library by OS family and, under Nix on macOS, left it missing
  (bkc39/rkt-polars#146).
- **When `racket-deps` stops matching its hash.** It runs `raco pkg install`
  against the live catalog, unpinned (#146), so an update there to gregor,
  cldr, tzinfo, tzdata, memoize or threading changes its output, and `nix
  flake check` fails with `hash mismatch in fixed-output derivation` for
  `racket-deps`, on a machine that does not have the old output cached (CI
  first). To recover:
  1. if rkt-polars' `flake.nix` already has the new `outputHash`, run `nix
     flake update rkt-polars` and commit `flake.lock`;
  2. otherwise set it there to the `got:` hash from the error, then do 1;
  3. to unblock glmnet before that lands, use
     `rkt-polars.packages.${system}.racket-deps.overrideAttrs (_: {
     outputHash = "<got>"; })` in `installPolars`, and drop the override at 1.

`glmnet/data/tabular-asa` depends on the catalog package `tabular-asa`, which
depends on csv-reading, mcfly and overeasy (#63 is on hold for their licences).
In the Nix sandbox, `installCatalogDeps` installs the four, before polars, from
the sources in `catalogSources`, each pinned to what the catalog names: the git
commit of tabular-asa, and the SHA-1 checksum of the others' zip files.
`raco pkg catalog-show <name>` prints both when a pin moves.

- **When the csv-reading zip changes.** Its pin fetches an unversioned URL,
  `https://www.neilvandyke.org/racket/csv-reading.zip`, the catalog's own
  source, where mcfly's and overeasy's name their versions
  (`mcfly--2-2.zip`). A new upload there replaces the pinned file, and `nix
  flake check` fails with a hash mismatch for it, on a machine that does not
  have the old file cached (CI first). To recover, either:
  1. move the pin: once `raco pkg catalog-show csv-reading` shows the new
     checksum, review the new release and set `sha1` in `catalogSources` to
     it; or
  2. keep the pinned content: mirror the old zip somewhere stable, such as a
     release asset of this repository, and point `url` at the mirror, with
     the same `sha1`.

The formula language (#53) is R's, checked against R's `terms()` and
`model.matrix()` by the parity goldens. A new kind of formula term, such as
another R call with a meaning of its own, adds:

- its data form to `term?` and `parse-term` in `core/terms.rkt`, and a
  clause of the `term` syntax class of `~` in `core/formula.rkt`, before the
  transform clause, since any other group whose head is bound is a transform;
- a variable struct and a clause in each of `leaf-variable`,
  `variable-label`, `variable-kind`, `variable-inputs`, `variable-values`
  and `variable-flonums`;
- if its values can make a factor, the rule in `factor-levels`. Levels are
  found by `resolve-levels` when the model is fitted and kept in the terms;
  `design-codings` applies R's coding, `model-terms-column-names` and
  `terms->design-matrix` name and build the columns;
- R parity fixtures that map R's name of the term to its Racket source.

## Non-negotiable invariants

1. **Both reals are 8 bytes.** The whole ABI is double precision. The
   vendored R Fortran is explicitly `double precision`, and the shim and tests
   use `real(c_double)`. The build passes `-fdefault-real-8`, which the
   `glmnet_default_real_bytes` probe and the load-time guard in `foreign.rkt`
   check, together with `-fdefault-double-8`, without which `-fdefault-real-8`
   would widen `double precision` to 16 bytes. `tests/test_precision.f90`
   asserts both. Never drop either flag.
2. **The vendored Fortran is pristine upstream, never hand-edited.**
   `vendor/glmnet5dpclean.f` is R glmnet 4.1's file byte-for-byte; its commit,
   SHA-256 and re-fetch command are in `vendor/NOTICE.md`. A numerical fix
   belongs upstream. Behaviour that R adds around a Fortran call belongs in
   `glmnet_capi.f90` and is listed in `vendor/NOTICE.md` (for example, the Cox
   ties nudge from R's `coxnet.R`). The file is FIXED-FORM (col-1 `c` comments,
   `*` continuation in col 6, sequence numbers in cols 73–80); the build sets
   `Fortran_FORMAT FIXED` on it and `FREE` on our shim.
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
   An entry point with more than eight integer and pointer arguments takes its
   integer scalars **by reference** (no `value`), and the raw binding passes
   them as `(_ptr i _int)`: the extra arguments go on the stack, where Apple's
   arm64 ABI packs 4-byte ints but gfortran reads 8-byte slots, which corrupted
   arguments on macOS (PR #45; see the path section of `glmnet_capi.f90`). A new
   or changed entry point bumps `glmnet_capi_abi_version`, the guard in
   `glmnet/foreign.rkt` and `fortran/tests/test_precision.f90`, and the
   committed candidates are refreshed from CI's catalog job.
3. **Test the Fortran.** Add `fortran/tests/test_*.f90`: a tiny known dataset,
   assert outputs within tolerance, `error stop 1` on failure; register with
   `add_test` in `CMakeLists.txt`. Run `ctest` (`nix build .#native` or local
   `cmake`).
4. **Add the Racket raw binding.** Extend `foreign/raw/elnet.rkt` via
   `define-glmnet` (`_f64vector`/`_s32vector` buffers, `(_ptr o …)` scalar outs;
   no allocator/finalizer — these are pure calls). Add a contracted wrapper in
   `core/` (inputs through the design-matrix layer, `data.rkt`, via
   `core/marshal.rkt`; `jerr` check; a result struct that implements
   `gen:glmnet-model` from `core/model.rkt`, so `predict`, `coef` and printing
   work on it; prediction helpers are `predict` at a fixed `#:type`).
5. **Test the Racket binding.** `glmnet/tests/*-test.rkt` rackunit: round-trip vs
   closed-form / known values, `jerr` error surfacing, shape-mismatch contract
   errors.
6. **Verify the example end to end.** Wire `glmnet/examples/test/NN-name.rkt`
   (`module+ main` runner + `module+ test` asserting the documented result);
   `raco test` passes; `racket glmnet/examples/test/NN-name.rkt` prints it.
7. **Add the user-guide pages.** Add the family to `scribblings/guide/concepts.scrbl`
   (its `@deftech`, response shape, table rows) and convert the example into
   `scribblings/guide/examples/name.scrbl`, included from
   `guide/examples.scrbl`: the lp2 prose and chunks as live `@examples`, plus a
   λ/α sweep and the prediction helpers on new data.
8. **Add the reference entry.** Document the new public procs and result struct
   in `scribblings/reference.scrbl` (contract matching `contract-out` + a live
   example each).
9. **Register a new family everywhere the families are listed.** A family is
   more than its single fit:
   - `*-path`, its regularization path: a `glmnet_<family>_path` shim entry
     (integer scalars by reference, step 2), its raw binding in
     `foreign/raw/path.rkt`, and the fitter in its `core/` module, built on the
     `support` submodule of `core/path.rkt` (`path-lambdas`, `finish-lambdas`,
     `unpack-*`).
   - `*-cv`, its cross-validation, through `cross-validate` in `core/cv.rkt`,
     and the family's cases in `cv.rkt`: `measure-name`, `observation-loss`,
     and `check-training-folds` when every fold's training data needs a class
     or an event.
   - `core/formula.rkt`: the `families` table (the fit, path and cv
     procedures, the type measures with the default first, and whether
     `#:intercept?` applies), `family/c`, and the response checks
     (`response-form-problem` and `model-frame`). A family whose response
     has classes takes a response of strings, symbols or booleans through
     `response-classes` in `model-frame`, and `coef` and `predict` name the
     classes through `prop:class-labels` (`core/model.rkt`).
   - `core/model.rkt`: `row-transform` and `check-type` (what `#:type`
     makes of the linear predictor), and for a family with one coefficient
     vector per class or response, the grouped cases of `single-fit-path`,
     `predict-rows` and `coefficient-namer` (and `path-num-predictors` in
     `core/path.rkt`).
   - `glmnet/plot.rkt`: `path-panels` for a family drawn as several panels.
   - `main.rkt` re-exports the new `core/` module, and
     `tests/docs-coverage-test.rkt` then fails until each export has a
     reference entry.
   - R parity: fixtures in `scripts/r-parity/gen-reference.R` and checks in
     `glmnet/tests/parity-test.rkt`, and anything R does around the Fortran in
     `fortran/vendor/NOTICE.md`.

**Gate (all green before the next feature):**
`raco test ./glmnet/` · `raco scribble --htmls …/glmnet.scrbl` renders with no
`collected information for key multiple times` warnings · `nix flake check` ·
resyntax clean.

## Documentation

The manual follows racket-doc's own guide and reference, as rkt-polars does.
Snippets are `@examples[#:eval ev #:label #f ...]` evaluated at build time with
the evaluator from `scribblings/utils.rkt`; never paste output by hand. Use
`eval:error` for expected failures. Each `@deftech` is defined once, in
`guide/concepts.scrbl`. Every `@section` gets an explicit `#:tag`; the `ex-*`
tags name the example pages and stay stable. An example section changes when
its `glmnet/examples/NN-*.rkt` changes. `tests/docs-coverage-test.rkt` fails,
naming the bindings, if anything `(require glmnet)` exports has no `defproc`,
`defstruct*`, `defthing` or `defform` entry in the manual: `scribblings/glmnet.scrbl`
and the files it reaches through `include-section`, outside code blocks and
examples, and so does anything `(require glmnet/plot)`, `(require
glmnet/datasets)` or a module under `glmnet/data/` exports. A definition with
`#:link-target? #f` does not count, nor does a `defstruct*` with
`#:omit-constructor` for the constructor. A new `.scrbl` file counts once
something includes it.

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
(builds the native lib + Racket package, checks its declared dependencies with
`raco setup --check-pkg-deps`, runs `raco test`, the plot tests included, and
renders the manual, plots included).

## Shipping native libraries (catalog candidates)

Using the package needs no toolchain because a prebuilt `libglmnetcompat` (plus
its gfortran/quadmath runtime) is committed per platform under
`glmnet/native-libs/candidates/<platform>/` and staged at install time by
`private/install-glmnet-native.rkt` (env var → already-staged → candidate).

Build a candidate with `scripts/build-so.sh <darwin|linux|linux-aarch64>`, which
bundles the gfortran/quadmath runtime alongside `libglmnetcompat` and sets
portable rpaths (`@rpath`/`@loader_path` on macOS; `RUNPATH=$ORIGIN` on Linux):

- **darwin** builds the `.#native` flake derivation (needs Nix) and rewrites the
  dylib closure to `@rpath`.
- **linux / linux-aarch64** build *inside* the `manylinux2014` container (needs
  Docker), so the `.so` links natively against **glibc 2.17** — it requires only
  `GLIBC <= 2.17` and loads on Ubuntu 22.04 (the catalog build server) and every
  Linux from the last decade, with no ELF post-processing (no polyfill / symbol
  shim). This mirrors the sibling `rkt-polars` build. See **`LINUX_CANDIDATE.md`**.

`scripts/test-local.sh` then installs from the candidate with a plain Racket (no
Nix) and runs the suite — exactly what `pkgs.racket-lang.org` does.

The script must run **on each target platform**: build the `darwin` candidate on
macOS and the `linux`/`linux-aarch64` candidates on the matching Linux host
(`.github/workflows/raco-catalog.yml` builds and validates them in CI, with
`ubuntu-22.04` exercised explicitly). Commit the resulting `candidates/<platform>/`
files; only the loose copies directly under `native-libs/` are git-ignored.

## Roadmap

Done: Phase 0 (toolchain + FFI spine); the six families of R glmnet 4.1
(Gaussian, with OLS / ridge / lasso / elastic net, binomial, multinomial, Cox,
Poisson and multi-response Gaussian) as single fits; and the R-style modelling
arc #44: regularization paths (#10), the design-matrix layer (#35), the
generic model interface (#25), cross-validation (#27), plots (#28) and the
formula front end (#26); R's formula language, its algebra, transforms
and factors (#53); and R glmnet's example datasets, CSV files for tables and
parity fixtures that follow the vignette on each dataset (#61).

Next: the per-fit knobs, weights,
`penalty.factor`, coefficient limits, offsets and `exclude` (#12); sparse
input through `spelnet` / `splognet` / `spfishnet` (#11), already present in
`vendor/`; and the data-source adapters of the input-formats arc (#41), each a
module under `glmnet/data/` beside `glmnet/data/csv`.
