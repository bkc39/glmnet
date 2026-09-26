#!/usr/bin/env bash
set -euo pipefail

# End-to-end test using the system Racket install (no Nix, no
# GLMNET_NATIVE_LIB_PATH). Installs the glmnet package from
# native-libs/candidates/ via the pre-install hook, and the glmnet-plot package
# on top of it, then runs the unit tests of both and all example companions.
# This reproduces what pkg-build.racket-lang.org does.

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
find glmnet glmnet-plot -name "compiled" -type d -exec rm -rf {} + 2>/dev/null || true

echo "--- removing previous glmnet-plot and glmnet installs ---"
"$RACO" pkg remove glmnet-plot 2>/dev/null || true
"$RACO" pkg remove glmnet 2>/dev/null || true

echo "--- clearing staged native libs (keep candidates/) ---"
find glmnet/native-libs -maxdepth 1 -type f -name 'lib*' -delete 2>/dev/null || true

echo "--- installing from candidates ---"
"$RACO" pkg install --batch --auto --name glmnet ./glmnet

echo "--- installing glmnet-plot ---"
"$RACO" pkg install --batch --auto --name glmnet-plot ./glmnet-plot

echo "--- raco setup --check-pkg-deps (mirrors catalog dependency check) ---"
"$RACO" setup --check-pkg-deps --pkgs glmnet glmnet-plot

echo "--- raco test glmnet/ glmnet-plot/ (packages + example companions) ---"
"$RACO" test glmnet/ glmnet-plot/

echo "--- all done ---"
