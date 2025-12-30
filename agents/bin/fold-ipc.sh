#!/usr/bin/env bash
# Interface to The Fold's IPC system
set -euo pipefail

FOLD_DIR="${FOLD_DIR:-$HOME/the-fold}"
REPL_DIR="$FOLD_DIR/.fold-repl"
SESSION_ID="${1:?usage: fold-ipc.sh <session-id> <expr>}"
EXPR="${2:?}"

REQUEST_FILE="$REPL_DIR/requests/${SESSION_ID}.ss"
RESPONSE_FILE="$REPL_DIR/responses/${SESSION_ID}.txt"

# Clear any stale response
rm -f "$RESPONSE_FILE"

# Write the request
echo "$EXPR" > "$REQUEST_FILE"

# Wait for response (with timeout)
# Check for both success and error responses
TIMEOUT=30
ELAPSED=0
ERROR_FILE="${RESPONSE_FILE%.txt}.error.txt"
while [[ ! -f "$RESPONSE_FILE" ]] && [[ ! -f "$ERROR_FILE" ]] && (( ELAPSED < TIMEOUT )); do
    sleep 1
    ELAPSED=$((ELAPSED + 1))
done

# Check for error response first
if [[ -f "$ERROR_FILE" ]]; then
    echo "ERROR: $(cat "$ERROR_FILE")" >&2
    exit 1
fi

if [[ ! -f "$RESPONSE_FILE" ]]; then
    echo "ERROR: Timeout waiting for Fold response" >&2
    exit 1
fi

cat "$RESPONSE_FILE"
