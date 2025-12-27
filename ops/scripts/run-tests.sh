#!/bin/bash
set -euo pipefail

FOLD_ROOT="${FOLD_ROOT:-$(cd "$(dirname "$0")/../.." && pwd)}"
cd "$FOLD_ROOT"

SCHEME_CMD="${FOLD_SCHEME_CMD:-}"
if [ -z "$SCHEME_CMD" ]; then
  for cmd in scheme chez-scheme chezscheme petite; do
    if command -v "$cmd" >/dev/null 2>&1; then
      SCHEME_CMD="$cmd"
      break
    fi
  done
fi

if [ -z "$SCHEME_CMD" ]; then
  echo "Error: Chez Scheme not found. Run ./start.sh first to install." >&2
  exit 1
fi

exec "$SCHEME_CMD" --script test-all.ss
