# Building the Linux native-lib candidate

> Read this on an **x86_64 Linux host** with **Docker**. The macOS (`darwin`)
> candidate is built separately on macOS. Background: `AGENTS.md` → "Shipping
> native libraries".

## Goal

Produce and commit `glmnet/native-libs/candidates/linux-cpu/` — `libglmnetcompat.so`
plus its bundled gfortran/quadmath runtime — so `raco pkg install glmnet` works on
Linux (notably the glibc-2.17 `pkgs.racket-lang.org` build host) without Nix.

## Approach: build inside manylinux2014

The candidate is compiled **inside the `quay.io/pypa/manylinux2014_x86_64`
container** (glibc 2.17, gfortran 10 via devtoolset-10). Linking natively against
that old glibc means the `.so` requires only `GLIBC <= 2.17` from the start — no
`polyfill-glibc`, no symbol shim, no ELF post-processing. This mirrors the sibling
`rkt-polars` Linux build.

`scripts/build-so.sh linux` (see the `bundle_linux` function):

1. Runs the container, mounting the repo read-only and the candidate dir at `/out`.
2. `cmake ../fortran -DBUILD_TESTING=ON` + `cmake --build` + `ctest` (expect **5/5**).
3. Copies `libglmnetcompat.so` out, plus its only non-system runtime deps —
   `libgfortran.so.5` and `libquadmath.so.0` (libc/libm/libgcc_s come from the
   host) — and sets `RUNPATH=$ORIGIN` on all three with `patchelf`.

> **libmvec note.** glibc 2.17 predates libmvec (glibc 2.22), so gfortran cannot
> auto-vectorize `exp`/`log` into the `_ZGVbN2v_*` SIMD-math calls that previously
> broke the Nix + `polyfill-glibc` build. Arithmetic SIMD is unaffected; glmnet's
> solves are tiny, so vectorized transcendentals would buy nothing anyway.

## Prerequisites

- An **x86_64-linux** host with **Docker**. (The `linux-aarch64` candidate builds
  the same way from `manylinux2014_aarch64` on an aarch64 host.)
- Racket + `raco` on PATH for the local install test.

## Build and verify

```bash
scripts/build-so.sh linux                      # builds in manylinux2014; ctest 5/5

dir=glmnet/native-libs/candidates/linux-cpu
readelf -d "$dir/libglmnetcompat.so" | grep -E 'RUNPATH|RPATH'      # => $ORIGIN
for f in "$dir"/*.so*; do readelf -V "$f" 2>/dev/null \
  | grep -oE 'GLIBC_[0-9]+\.[0-9]+' | sort -V | tail -1; done       # <= GLIBC_2.17
ldd "$dir"/*.so* | grep /nix/store && echo "LEAK" || echo "no nix-store leaks"
ls "$dir"/libgfortran*.so* "$dir"/libquadmath*.so*                  # runtime bundled

scripts/test-local.sh                                               # install + tests pass
```

Then commit `glmnet/native-libs/candidates/linux-cpu/`. CI
(`.github/workflows/raco-catalog.yml`) rebuilds and validates it on every push —
portability checks on ubuntu-22.04 / ubuntu-latest plus a manylinux2014 (glibc
2.17) `dlopen` probe.
