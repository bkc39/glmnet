#!/usr/bin/env bash
set -euo pipefail

# Stage the glmnet native library (libglmnetcompat) and its gfortran/quadmath
# runtime under glmnet/native-libs/candidates/<platform>/ with portable rpaths,
# so `raco pkg install` works on pkgs.racket-lang.org without Nix.
#
# Linux: built inside the manylinux2014 container (glibc 2.17) so the .so and its
# bundled gfortran/quadmath runtime require only GLIBC <= 2.17 and load on every
# Linux from the last decade, including pkg-build.racket-lang.org's test host.
# Building natively against an old glibc avoids any ELF post-processing (no
# polyfill / symbol shim). Mirrors the sibling rkt-polars build.
#
# Darwin: built from the flake's `native` derivation (nix build .#native), with
# its dylib closure bundled and rewritten to @rpath/@loader_path.
#
# Usage: scripts/build-so.sh <target>
#   targets: darwin | linux | linux-aarch64

TARGET="${1:-}"

usage() {
  echo "Usage: $0 <target>"
  echo "  targets: darwin | linux | linux-aarch64"
  exit 1
}

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

# Build the native derivation and echo its store path (darwin).
native_out() {
  nix build --no-link --print-out-paths .#native 2>/dev/null
}

# Print the lib/ directories of every store path in the native runtime closure.
native_closure_libdirs() {
  nix path-info -r .#native 2>/dev/null | while IFS= read -r p; do
    [ -d "$p/lib" ] && echo "$p/lib"
  done
}

# Find a library by basename anywhere in the native closure; echo its full path.
find_in_closure() {
  local base="$1" d
  while IFS= read -r d; do
    if [ -f "$d/$base" ]; then
      echo "$d/$base"
      return 0
    fi
  done < <(native_closure_libdirs)
  return 1
}

# ---------------------------------------------------------------------------
# macOS
# ---------------------------------------------------------------------------

# BFS the dependency graph of libglmnetcompat.dylib, copying every non-system
# dylib into $dest, then rewrite ids/deps/rpaths so they all resolve via @rpath
# + @loader_path.
bundle_darwin() {
  local dest="$1" native
  native=$(native_out)

  cp -v "$native/lib/libglmnetcompat.dylib" "$dest/"

  local -a worklist=(libglmnetcompat.dylib)
  local i=0
  while [ "$i" -lt "${#worklist[@]}" ]; do
    local lib="${worklist[$i]}"
    i=$((i + 1))
    local dep base src
    while IFS= read -r dep; do
      base="$(basename "$dep")"
      [ "$base" = "$lib" ] && continue
      case "$dep" in
        /usr/lib/*|/System/*) continue ;;
      esac
      if [ ! -f "$dest/$base" ]; then
        if src=$(find_in_closure "$base"); then
          cp -v "$src" "$dest/$base"
          worklist+=("$base")
        else
          echo "Warning: could not locate $base in native closure" >&2
        fi
      fi
    done < <(otool -L "$dest/$lib" | tail -n +2 | awk '{print $1}')
  done

  local f base dep dbase rp
  for f in "$dest"/*.dylib; do
    base="$(basename "$f")"
    chmod +w "$f"
    install_name_tool -id "@rpath/$base" "$f"
    while IFS= read -r dep; do
      dbase="$(basename "$dep")"
      case "$dep" in
        /usr/lib/*|/System/*) continue ;;
      esac
      [ "$dep" = "@rpath/$dbase" ] && continue
      if [ -f "$dest/$dbase" ]; then
        install_name_tool -change "$dep" "@rpath/$dbase" "$f"
      fi
    done < <(otool -L "$f" | tail -n +2 | awk '{print $1}')
    for rp in $(otool -l "$f" | awk '/^ *path /{print $2}' | grep -v '^@' || true); do
      install_name_tool -delete_rpath "$rp" "$f" 2>/dev/null || true
    done
    if ! otool -l "$f" | awk '/^ *path /{print $2}' | grep -qx '@loader_path/.'; then
      install_name_tool -add_rpath '@loader_path/.' "$f"
    fi
    # Re-sign ad-hoc: Apple Silicon kills dylibs whose signature no longer
    # matches after install_name_tool edits.
    if command -v codesign >/dev/null 2>&1; then
      codesign -f -s - "$f" 2>/dev/null || true
    fi
  done
  echo "Bundled darwin candidate:"
  ls -la "$dest"
}

# ---------------------------------------------------------------------------
# Linux (manylinux)
# ---------------------------------------------------------------------------

# Build libglmnetcompat.so + run ctest inside the given manylinux container
# (glibc baseline), then bundle the gfortran/quadmath runtime alongside it with
# RUNPATH=$ORIGIN. Everything links natively against the container's old glibc,
# so no polyfill / symbol-version rewriting is needed.
bundle_linux() {
  local dest="$1" image="$2"
  command -v docker >/dev/null 2>&1 || { echo "linux target requires docker" >&2; exit 1; }

  # Run as the invoking user so the committed candidate files are not root-owned.
  docker run --rm \
    -u "$(id -u):$(id -g)" -e HOME=/tmp \
    -v "$ROOT:/src:ro" \
    -v "$dest:/out" \
    "$image" bash -ec '
      # devtoolset enable references unbound vars; source it before set -u.
      source /opt/rh/devtoolset-10/enable
      set -euo pipefail

      # Source is mounted read-only; build out-of-tree in a writable copy.
      cp -r /src /tmp/glmnet-build
      cd /tmp/glmnet-build
      rm -rf build && mkdir build && cd build
      cmake ../fortran -DCMAKE_BUILD_TYPE=Release -DBUILD_TESTING=ON
      cmake --build . -j"$(nproc)"
      ctest --output-on-failure

      # Stage the seed lib and bundle its non-system runtime deps. libc/libm/
      # libgcc_s/ld-linux come from the host; only the gfortran runtime travels.
      cp -v libglmnetcompat.so /out/
      for needed in libgfortran.so.5 libquadmath.so.0; do
        path=$(ldd libglmnetcompat.so | awk -v n="$needed" "\$1==n {print \$3}")
        [ -n "$path" ] || { echo "could not resolve $needed in container" >&2; exit 1; }
        cp -v "$path" /out/
      done

      # Portable rpath: each bundled lib resolves its siblings via its own dir.
      for f in /out/libglmnetcompat.so /out/libgfortran.so.5 /out/libquadmath.so.0; do
        chmod u+w "$f"
        patchelf --set-rpath "\$ORIGIN" "$f"
      done
    '

  echo "Bundled linux candidate:"
  ls -la "$dest"
}

case "$TARGET" in
  darwin)
    [ "$(uname)" = "Darwin" ] || { echo "darwin target requires macOS" >&2; exit 1; }
    dest=glmnet/native-libs/candidates/darwin
    mkdir -p "$dest"
    rm -f "$dest"/*.dylib
    bundle_darwin "$dest"
    ;;
  linux)
    SYSTEM="$(uname -m)-linux"
    [ "$SYSTEM" = "x86_64-linux" ] || { echo "linux target requires x86_64-linux (got $SYSTEM)" >&2; exit 1; }
    dest="$ROOT/glmnet/native-libs/candidates/linux-cpu"
    mkdir -p "$dest"
    rm -f "$dest"/*.so "$dest"/*.so.*
    bundle_linux "$dest" quay.io/pypa/manylinux2014_x86_64
    ;;
  linux-aarch64)
    SYSTEM="$(uname -m)-linux"
    [ "$SYSTEM" = "aarch64-linux" ] || { echo "linux-aarch64 target requires aarch64-linux (got $SYSTEM)" >&2; exit 1; }
    dest="$ROOT/glmnet/native-libs/candidates/linux-aarch64"
    mkdir -p "$dest"
    rm -f "$dest"/*.so "$dest"/*.so.*
    bundle_linux "$dest" quay.io/pypa/manylinux2014_aarch64
    ;;
  *)
    usage
    ;;
esac

echo "Done."
