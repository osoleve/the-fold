#!/bin/bash
# fold.sh — Send expressions to the REPL daemon (Unix)
#
# Usage:
#   ./fold.sh "(+ 1 2)"         — Evaluate expression
#   ./fold.sh script.ss         — Run a script file
#   echo "(+ 1 2)" | ./fold.sh  — Pipe expression
#
# The daemon must be running (use ./daemon.sh start first).

set -e
cd "$(dirname "$0")"

READY_FILE=".fold-repl/ready"
REQUEST_FILE=".fold-repl/request.ss"
RESPONSE_FILE=".fold-repl/response.txt"
ERROR_FILE=".fold-repl/error.txt"
TIMEOUT=30

# Find Scheme for fallback mode
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
    SCHEME_CMD=$(find_scheme) || {
        echo "Error: Chez Scheme not found."
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

# Clear previous response
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
    exit 1
fi

# Wait for response
for i in $(seq 1 $TIMEOUT); do
    if [ -f "$RESPONSE_FILE" ]; then
        cat "$RESPONSE_FILE"
        if [ -f "$ERROR_FILE" ]; then
            echo ""
            echo "[Error details in $ERROR_FILE]"
        fi
        exit 0
    fi
    sleep 1
done

echo "ERROR: Timeout waiting for response"
exit 1
