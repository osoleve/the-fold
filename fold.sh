#!/bin/bash
# fold.sh — Send expressions to the REPL daemon (Unix)
#
# Usage:
#   ./fold.sh "(+ 1 2)"         — Evaluate expression
#   ./fold.sh script.ss         — Run a script file
#   echo "(+ 1 2)" | ./fold.sh  — Pipe expression
#   SESSION=my-session ./fold.sh "(+ 1 2)"  — Use specific session
#
# When daemon is not running, falls back to Chez Scheme.

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
    echo "Daemon not running, falling back to Chez Scheme..."

    # Use Scheme for direct execution
    SCHEME_CMD=$(find_scheme) || {
        echo "Error: Chez Scheme not found."
        echo "Please install Chez Scheme to run The Fold."
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
        # Use cat with heredoc to avoid bash escaping special characters (like !)
        cat > "$REQUEST_FILE" << FOLD_END
$*
FOLD_END
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
