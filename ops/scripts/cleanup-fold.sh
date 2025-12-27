#!/bin/bash
set -euo pipefail

FOLD_ROOT="${FOLD_ROOT:-$(cd "$(dirname "$0")/../.." && pwd)}"
CLEANUP_DAYS="${FOLD_CLEANUP_DAYS:-7}"
SESSION_DAYS="${FOLD_CLEANUP_SESSION_DAYS:-30}"
DRY_RUN="${FOLD_CLEANUP_DRY_RUN:-0}"

say() {
  printf '%s\n' "$*"
}

run_find() {
  local dir="$1"
  local pattern="$2"
  local days="$3"

  if [ ! -d "$dir" ]; then
    return 0
  fi

  if [ "$DRY_RUN" = "1" ]; then
    find "$dir" -type f -name "$pattern" -mtime "+$days" -print
  else
    find "$dir" -type f -name "$pattern" -mtime "+$days" -print -delete
  fi
}

say "Cleanup root: $FOLD_ROOT"

run_find "$FOLD_ROOT/.fold-repl/responses" "*.txt" "$CLEANUP_DAYS"
run_find "$FOLD_ROOT/.fold-repl/responses" "*.error.txt" "$CLEANUP_DAYS"
run_find "$FOLD_ROOT/.fold-repl/requests" "*.ss" "$CLEANUP_DAYS"
run_find "$FOLD_ROOT/.fold-repl" "request.ss" "$CLEANUP_DAYS"
run_find "$FOLD_ROOT/.fold-repl" "response.txt" "$CLEANUP_DAYS"
run_find "$FOLD_ROOT/.fold-sessions" "*.session" "$SESSION_DAYS"

if [ "$DRY_RUN" = "1" ]; then
  say "Dry run complete."
fi
