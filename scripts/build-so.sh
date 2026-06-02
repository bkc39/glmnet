#!/usr/bin/env bash
set -euo pipefail

# Stage the glmnet native library (libglmnetcompat) and its gfortran/quadmath
# runtime closure under glmnet/native-libs/candidates/<platform>/ with portable
# rpaths, so `raco pkg install` works on pkgs.racket-lang.org without Nix.
#
# The seed library is built from source by the flake's `native` derivation
# (nix build .#native); we then bundle the parts of its closure the loader needs
# and rewrite the binaries to a portable, old-glibc baseline.
#
# Usage: scripts/build-so.sh <target>
#   targets: darwin | linux | linux-aarch64

TARGET="${1:-}"

usage() {
  echo "Usage: $0 <target>"
  echo "  targets: darwin | linux | linux-aarch64"
  exit 1
}

cd "$(dirname "$0")/.."

# Build the native derivation and echo its store path.
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
# Linux
# ---------------------------------------------------------------------------

# BFS the NEEDED graph of libglmnetcompat.so, copy every non-glibc dependency
# from the native closure, set RPATH=$ORIGIN, build the companion glibc shim,
# then rewrite all bundled libs down to the glibc 2.17 baseline.
bundle_linux() {
  local dest="$1" target_arch="${2:-x86_64}" native patchelf
  native=$(native_out)
  patchelf=$(nix build --no-link --print-out-paths nixpkgs#patchelf 2>/dev/null)/bin/patchelf

  cp -v --no-preserve=mode "$native/lib/libglmnetcompat.so" "$dest/"

  # glibc / loader libs we must NOT bundle (they come from the host). libgcc_s
  # is an ABI-stable system library; the toolchain's copy pulls
  # _dl_find_object@GLIBC_2.35 which polyfill cannot lower, so let the host
  # provide it (libgfortran needs only ancient GCC_* symbols from it).
  local skip='^(libc|libm|libpthread|libdl|librt|ld-linux.*|libresolv|libutil|libgcc_s)\.'

  local -a worklist=(libglmnetcompat.so)
  local i=0
  while [ "$i" -lt "${#worklist[@]}" ]; do
    local lib="${worklist[$i]}"
    i=$((i + 1))
    local base src
    while IFS= read -r base; do
      [ -z "$base" ] && continue
      echo "$base" | grep -qE "$skip" && continue
      if [ ! -f "$dest/$base" ]; then
        if src=$(find_in_closure "$base"); then
          cp -v --no-preserve=mode "$src" "$dest/$base"
          worklist+=("$base")
        else
          echo "Warning: could not locate $base in native closure" >&2
        fi
      fi
    done < <("$patchelf" --print-needed "$dest/$lib")
  done

  local f
  for f in "$dest"/*.so "$dest"/*.so.*; do
    [ -f "$f" ] || continue
    "$patchelf" --set-rpath '$ORIGIN' "$f"
  done
  echo "Set RPATH=\$ORIGIN on bundled libraries"

  if [ "$target_arch" = "x86_64" ]; then
    build_glibc_shim_linux "$dest"
    polyfill_glibc_linux "$dest"
  else
    echo "Skipping glibc shim + polyfill on $target_arch (keeps native glibc dep)"
  fi
  echo "Bundled linux candidate:"
  ls -la "$dest"
}

# Build libglmnetshim.so from scripts/glibc-shim.c (see that file + glibc-renames.txt).
build_glibc_shim_linux() {
  local dest="$1" cc_pkg cc patchelf
  cc_pkg=$(nix build --no-link --print-out-paths 'nixpkgs#gcc^out' 2>/dev/null)
  cc="$cc_pkg/bin/gcc"
  if [ ! -x "$cc" ]; then
    echo "Warning: gcc not available; skipping libglmnetshim.so build" >&2
    return
  fi
  "$cc" -shared -fPIC -O2 -fno-builtin \
        -Wl,-soname,libglmnetshim.so \
        -o "$dest/libglmnetshim.so" \
        scripts/glibc-shim.c
  patchelf=$(nix build --no-link --print-out-paths nixpkgs#patchelf 2>/dev/null)/bin/patchelf
  "$patchelf" --set-rpath '$ORIGIN' "$dest/libglmnetshim.so"
  echo "Built libglmnetshim.so (glibc deps: $(readelf -V "$dest/libglmnetshim.so" 2>/dev/null | grep -oE 'GLIBC_[0-9]+\.[0-9]+' | sort -V -u | tr '\n' ' '))"
}

# Rewrite the bundled ELF binaries to require only glibc <= 2.17 (manylinux2014).
polyfill_glibc_linux() {
  local dest="$1" polyfill f
  polyfill=$(nix build --no-link --print-out-paths .#polyfill-glibc 2>/dev/null)/bin/polyfill-glibc
  if [ ! -x "$polyfill" ]; then
    echo "Warning: polyfill-glibc unavailable; skipping glibc downgrade" >&2
    return
  fi
  echo "Polyfilling bundled libs to require only glibc <= 2.17..."
  for f in "$dest"/*.so "$dest"/*.so.*; do
    [ -f "$f" ] || continue
    [ "$(basename "$f")" = "libglmnetshim.so" ] && continue
    "$polyfill" --rename-dynamic-symbols=scripts/glibc-renames.txt \
                --target-glibc=2.17 "$f"
    echo "  $(basename "$f") -> max glibc dep now: $(readelf -V "$f" 2>/dev/null | grep -oE 'GLIBC_[0-9]+\.[0-9]+' | sort -V -u | tail -1)"
  done
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
    dest=glmnet/native-libs/candidates/linux-cpu
    mkdir -p "$dest"
    rm -f "$dest"/*.so "$dest"/*.so.*
    bundle_linux "$dest" x86_64
    ;;
  linux-aarch64)
    SYSTEM="$(uname -m)-linux"
    [ "$SYSTEM" = "aarch64-linux" ] || { echo "linux-aarch64 target requires aarch64-linux (got $SYSTEM)" >&2; exit 1; }
    dest=glmnet/native-libs/candidates/linux-aarch64
    mkdir -p "$dest"
    rm -f "$dest"/*.so "$dest"/*.so.*
    bundle_linux "$dest" aarch64
    ;;
  *)
    usage
    ;;
esac

echo "Done."
