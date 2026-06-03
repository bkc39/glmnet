#!/usr/bin/env bash
set -euo pipefail

# Run every example end to end and print its output. Each example's companion
# harness has a `main` submodule that runs the example and prints the fit, so
# `racket <harness>` executes it. Requires the glmnet package to be installed
# (link mode is fine) and the native library reachable -- inside `nix develop`
# both are already set up (GLMNET_NATIVE_LIB_PATH is exported); otherwise stage
# the library first (see AGENTS.md).

cd "$(dirname "$0")/.."

echo "Running all glmnet examples..."
for f in glmnet/examples/test/*.rkt; do
  echo
  echo "=== $f ==="
  racket "$f"
done
echo
echo "All examples ran."
