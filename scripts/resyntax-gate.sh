#!/usr/bin/env bash
# Resyntax with lint/'s suite over the diff from BASE_REF, red on a suggestion,
# a file it could not analyze, or a run that did not complete. Run inside a
# glmnet dev shell; CI's resyntax job and .racket-dev.rktd both call it.
#
#   scripts/resyntax-gate.sh [BASE_REF [BASE_NAME]]
#
# Resyntax exits 0 with suggestions, so they are matched in its output. Every
# other failure also prints a line in the suggestion format, so a runner that
# reads only those lines turns red on it too.
set -euo pipefail

base_ref=${1:-origin/master}
base_name=${2:-master}

fail() {
  echo "::error::$1"
  echo "resyntax: resyntax-gate.rkt:0:0 [gate] $1"
  exit 1
}

if [ ! -f "${PLTUSERHOME:-}/.glmnet-linked" ]; then
  if racket -l racket/base -l pkg/lib \
       -e "(exit (if (hash-ref (installed-pkg-table #:scope (quote user)) \"glmnet\" #f) 0 1))"; then
    fail "glmnet installed, but raco setup failed on it (a compile error?); see the shell's log."
  fi
  fail "The shell could not install glmnet or its dependencies; see its log."
fi
# nix bash has no compgen; an unmatched glob stays literal
stamps=("${PLTUSERHOME:-}"/.linters-*)
if [ ! -e "${stamps[0]}" ]; then
  fail "The shell could not install the pinned Resyntax or lint/; see the shell's log."
fi

rc=0
out=$(resyntax analyze --refactoring-suite glmnet-lint/style project-style \
        --local-git-repository . "$base_ref" --analyzer-timeout 30000 2>&1) || rc=$?
echo "$out"
if [ "$rc" -ne 0 ]; then
  fail "Resyntax exited $rc without completing the analysis."
fi

status=0
broken=$(grep -B1 -E "^ encountered an error during (initial analysis|macro expansion)" <<<"$out" \
           | sed -n "s/^resyntax: skipping //p" | sort -u | paste -sd " " || true)
if [ -n "$broken" ]; then
  echo "::error::Resyntax could not analyze $broken; see each error above."
  echo "resyntax: resyntax-gate.rkt:0:0 [gate] could not analyze $broken"
  status=1
fi
if grep -qE "resyntax: .*\.rkt:[0-9]+:[0-9]+ \[" <<<"$out"; then
  echo "::error::Resyntax found refactoring suggestions. Run: resyntax fix --refactoring-suite glmnet-lint/style project-style --local-git-repository . origin/$base_name"
  status=1
fi
timeouts=$(grep -c "timed out" <<<"$out" || true)
if [ "$timeouts" -gt 0 ]; then
  echo "::warning::Resyntax analyzer timed out on $timeouts file(s); that part of the analysis was partial."
fi
if [ "$status" -eq 0 ]; then
  echo "Resyntax: no suggestions."
fi
exit "$status"
