#!/usr/bin/env bash
set -euo pipefail

# End-to-end test using the system Racket install (no Nix, no
# GLMNET_NATIVE_LIB_PATH). Installs the glmnet package from
# native-libs/candidates/ via the pre-install hook, then runs the unit tests and
# all example companions. This reproduces what pkg-build.racket-lang.org does.

RACKET=$(command -v racket 2>/dev/null || true)
RACO=$(command -v raco 2>/dev/null || true)

if [ -z "$RACKET" ] || [ -z "$RACO" ]; then
  echo "Error: racket/raco not found on PATH" >&2
  exit 1
fi
echo "Using $("$RACKET" --version)"

cd "$(dirname "$0")/.."

# Ensure the loader cannot fall back to a Nix store path.
unset GLMNET_NATIVE_LIB_PATH

echo "--- cleaning compiled bytecode ---"
find glmnet -name "compiled" -type d -exec rm -rf {} + 2>/dev/null || true

echo "--- removing previous glmnet install ---"
"$RACO" pkg remove glmnet 2>/dev/null || true

echo "--- clearing staged native libs (keep candidates/) ---"
find glmnet/native-libs -maxdepth 1 -type f -name 'lib*' -delete 2>/dev/null || true

echo "--- installing from candidates ---"
"$RACO" pkg install --batch --auto --name glmnet ./glmnet

echo "--- raco test glmnet/ (package + example companions) ---"
"$RACO" test glmnet/

echo "--- all done ---"
