# Building the Linux native-lib candidate (instructions + known blocker)

> Read this on the **x86_64 Linux host**. The macOS (`darwin`) candidate is
> already built, committed, and verified end-to-end; only the Linux candidate is
> outstanding. Background: `AGENTS.md` → "Shipping native libraries".

## Goal

Produce and commit `glmnet/native-libs/candidates/linux-cpu/` — `libglmnetcompat.so`
plus its bundled gfortran/quadmath runtime — so `raco pkg install glmnet` works on
Linux (notably Ubuntu 22.04, the pkgs.racket-lang.org build server).

## Prerequisites

- An **x86_64-linux** host with **Nix** (flakes enabled) and Docker (only for the
  optional old-glibc probe). `scripts/build-so.sh` uses `nix build .#native`,
  `patchelf`, the `.#polyfill-glibc` derivation, and `gcc` for the shim.

## The command

```bash
scripts/build-so.sh linux
# then, after it succeeds:
scripts/test-local.sh                       # install from the candidate + run tests
git add glmnet/native-libs/candidates/linux-cpu && git commit
```

## ⚠️ Known blocker (this is why CI's linux leg is currently red)

`scripts/build-so.sh linux` currently fails in the `polyfill-glibc` step:

```
Cannot change target version of .../libglmnetcompat.so to 2.17 (x86_64)
due to missing knowledge about how to handle:
  _ZGVbN2v_exp@GLIBC_2.22
  _ZGVbN2v_log@GLIBC_2.22
```

**Cause.** Those are **libmvec** symbols — GCC's SSE2 vectorized `exp`/`log`
(`_ZGVbN2v_*` = vector-of-2-doubles). gfortran auto-vectorized the `exp`/`log`
loops in `vendor/glmnet5.f90` at `-O2` into libmvec calls, which are versioned
`@GLIBC_2.22` (libmvec was introduced in glibc 2.22, with no older equivalent).
`polyfill-glibc` can downgrade everything else to the 2.17 floor but does not know
how to lower these, so it aborts. The library itself is correct and runs fine on
any glibc ≥ 2.22 — this only blocks the 2.17 (CentOS 7 / manylinux2014) target.

## Fixes (pick one)

**Option A — stop emitting libmvec calls (recommended; keeps the 2.17 floor).**
This library's solves are tiny, so vectorized transcendental math buys nothing.
Disable it in `fortran/CMakeLists.txt`:

```cmake
add_compile_options(-fdefault-real-8 -O2 -fPIC -fno-tree-vectorize)
```

(`-fno-tree-vectorize` removes the `_ZGVbN2v_*` imports; `polyfill-glibc
--target-glibc=2.17` then succeeds.) Rebuild and re-run `scripts/build-so.sh linux`.
This is harmless on macOS too (darwin doesn't use libmvec/polyfill), so it keeps
both candidates building from identical flags.

**Option B — raise the floor to glibc 2.22.** Keep vectorization; in
`scripts/build-so.sh`'s `polyfill_glibc_linux`, change `--target-glibc=2.17` to
`--target-glibc=2.22`, and relax the matching checks in
`.github/workflows/raco-catalog.yml` (the `GLIBC_2.17` assertion → `2.22`, and the
manylinux2014/2.17 `old-glibc-load` probe → a glibc-2.27 image such as
`ubuntu:18.04`). glibc 2.22 (2015) still covers Ubuntu 22.04 and essentially every
supported distro; it only drops ancient CentOS 7. The catalog server (2.35) is
unaffected.

**Option C (not recommended).** Redirecting `_ZGVbN2v_exp/_log` to abort() stubs
via `scripts/glibc-renames.txt` (as we do for the f128 symbols) would crash at
runtime *if* those vector paths are reached — unlike the f128 stubs, which never
are. Only viable combined with Option A, which makes it moot.

## Verify after fixing

```bash
scripts/build-so.sh linux                      # must print "Done." with no polyfill error
dir=glmnet/native-libs/candidates/linux-cpu
readelf -d "$dir/libglmnetcompat.so" | grep -E 'RUNPATH|RPATH'      # => $ORIGIN
for f in "$dir"/*.so*; do readelf -V "$f" 2>/dev/null \
  | grep -oE 'GLIBC_[0-9]+\.[0-9]+' | sort -V | tail -1; done       # <= your target
ldd "$dir"/*.so* | grep /nix/store && echo "LEAK" || echo "no nix-store leaks"
ls "$dir"/libgfortran*.so*                                          # runtime bundled
scripts/test-local.sh                                               # install + tests pass
```

Then commit `glmnet/native-libs/candidates/linux-cpu/`. Once the candidate builds,
remove the `continue-on-error` / `experimental` marker on the linux legs in
`.github/workflows/raco-catalog.yml` so the Linux path is required again.
