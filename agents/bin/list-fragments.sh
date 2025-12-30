#!/bin/bash
# List available DSL fragments for personas
# Usage: ./agents/bin/list-fragments.sh

FRAGMENTS_DIR="${FOLD_FRAGMENTS:-agents/personas/fragments}"

if [ ! -d "$FRAGMENTS_DIR" ]; then
    # Try finding it relative to project root if we are in bin
    PROJECT_ROOT="$(dirname "$(dirname "$(dirname "$(readlink -f "$0")")")")"
    FRAGMENTS_DIR="$PROJECT_ROOT/agents/personas/fragments"
fi

if [ ! -d "$FRAGMENTS_DIR" ]; then
    echo "Error: Fragments directory not found."
    exit 1
fi

echo "Available Fragments:"
echo "-------------------"

for f in "$FRAGMENTS_DIR"/*.ss; do
    if [ -f "$f" ]; then
        basename "$f" .ss | sed 's/^/  (load-fragment '\''/' | sed 's/$/)/'
    fi
done
