#!/bin/bash
# fold.sh — Send expressions to the REPL daemon (Unix)
#
# Usage:
#   ./fold.sh "(+ 1 2)"         — Evaluate expression
#   ./fold.sh script.ss         — Run a script file
#   echo "(+ 1 2)" | ./fold.sh  — Pipe expression
#   SESSION=my-session ./fold.sh "(+ 1 2)"  — Use specific session
#
# Environment:
#   FOLD_USE_SCHEME=1  — Force Scheme backend (skip Rust)
#
# When daemon is not running, prefers Rust backend (fold-rs/target/release/fold-repl)
# if available, otherwise falls back to Chez Scheme.

set -e
cd "$(dirname "$0")"

# Session-based IPC paths
READY_FILE=".fold-repl/ready"
REQUESTS_DIR=".fold-repl/requests"
RESPONSES_DIR=".fold-repl/responses"

# Generate session ID if not provided
SESSION_ID="${SESSION:-cli-$$}"
REQUEST_FILE="$REQUESTS_DIR/$SESSION_ID.ss"
RESPONSE_FILE="$RESPONSES_DIR/$SESSION_ID.txt"
ERROR_FILE="$RESPONSES_DIR/$SESSION_ID.error.txt"
TIMEOUT=30

# Find Rust binary (preferred) or Scheme for fallback mode
SCRIPT_DIR="$(dirname "$0")"
RUST_REPL="$SCRIPT_DIR/fold-rs/target/release/fold-repl"

find_scheme() {
    for cmd in scheme chez-scheme chezscheme petite; do
        if command -v "$cmd" &> /dev/null; then
            echo "$cmd"
            return 0
        fi
    done
    return 1
}

# Check if daemon is running
if [ ! -f "$READY_FILE" ]; then
    echo "Daemon not running, falling back to direct execution..."

    # Prefer Rust binary if available (unless FOLD_USE_SCHEME is set)
    if [ -x "$RUST_REPL" ] && [ -z "$FOLD_USE_SCHEME" ]; then
        if [ -n "$1" ]; then
            # Argument provided
            if [ -f "$1" ]; then
                "$RUST_REPL" --file "$1"
            else
                "$RUST_REPL" --expr "$*"
            fi
        elif [ ! -t 0 ]; then
            # Read from stdin (piped input)
            "$RUST_REPL"
        else
            echo "Usage: ./fold.sh \"(expression)\" or echo \"(expr)\" | ./fold.sh"
            exit 1
        fi
        exit 0
    fi

    # Fall back to Scheme
    SCHEME_CMD=$(find_scheme) || {
        echo "Error: Neither Rust binary nor Chez Scheme found."
        echo "Run 'cargo build --release' in fold-rs/ to build the Rust REPL."
        exit 1
    }

    # Create temp script
    TMP_SCRIPT=$(mktemp)
    echo '(load "thimble/repl.ss")' > "$TMP_SCRIPT"

    if [ -t 0 ] && [ -n "$1" ]; then
        # Argument provided
        if [ -f "$1" ]; then
            echo "(load \"$1\")" >> "$TMP_SCRIPT"
        else
            echo "$*" >> "$TMP_SCRIPT"
        fi
    else
        # Read from stdin
        cat >> "$TMP_SCRIPT"
    fi

    "$SCHEME_CMD" --script "$TMP_SCRIPT"
    rm -f "$TMP_SCRIPT"
    exit 0
fi

# Ensure directories exist
mkdir -p "$REQUESTS_DIR" "$RESPONSES_DIR"

# Clear previous response for this session
rm -f "$RESPONSE_FILE" "$ERROR_FILE"

# Write request
if [ -n "$1" ]; then
    # Argument provided
    if [ -f "$1" ]; then
        cp "$1" "$REQUEST_FILE"
    else
        echo "$*" > "$REQUEST_FILE"
    fi
elif [ ! -t 0 ]; then
    # Read from stdin (piped input)
    cat > "$REQUEST_FILE"
else
    echo "Usage: ./fold.sh \"(expression)\" or echo \"(expr)\" | ./fold.sh"
    echo "       SESSION=my-session ./fold.sh \"(expr)\"  — Use specific session"
    exit 1
fi

# Wait for response
for i in $(seq 1 $TIMEOUT); do
    if [ -f "$RESPONSE_FILE" ]; then
        cat "$RESPONSE_FILE"
        # Show error details if present
        if [ -f "$ERROR_FILE" ]; then
            echo ""
            echo "[Error: $(cat "$ERROR_FILE")]"
        fi
        exit 0
    fi
    # Check for error-only response
    if [ -f "$ERROR_FILE" ]; then
        echo "ERROR: $(cat "$ERROR_FILE")"
        exit 1
    fi
    sleep 0.5
done

echo "ERROR: Timeout waiting for response (session: $SESSION_ID)"
exit 1
