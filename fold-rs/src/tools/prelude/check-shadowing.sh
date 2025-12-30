#!/bin/bash
# Detect shadowing conflicts in prelude modules

set -euo pipefail

PRELUDE_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$PRELUDE_DIR"

# Colors for output
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

echo "=== Prelude Shadowing Detector ==="
echo

# Function to extract function definitions from a file
extract_functions() {
    local file="$1"
    grep -n "^([a-z][a-z0-9?!-]* (fn\|^([a-z][a-z0-9?!-]* (fix" "$file" 2>/dev/null | \
        awk -F: '{print $2}' | \
        sed 's/^(//' | \
        sed 's/ .*//' || true
}

# Track all definitions and their locations
declare -A all_defs
declare -A def_counts

# Modules to check (in load order from mod.rs)
MODULES=(
    "core.ss"
    "function.ss"
    "list.ss"
    "equality.ss"
    "maybe.ss"
    "either.ss"
    "numeric.ss"
    "comparison.ss"
    "string.ss"
    "collection.ss"
    "control.ss"
    "validation.ss"
    "stream.ss"
    "datastructures.ss"
    "graph.ss"
    "tree.ss"
    "missing.ss"
    "path.ss"
    "encoding.ss"
    "compat.ss"
)

echo "Scanning modules in load order..."
echo

total_defs=0
total_shadowed=0

for module in "${MODULES[@]}"; do
    if [[ ! -f "$module" ]]; then
        echo -e "${YELLOW}⚠ Skipping missing module: $module${NC}"
        continue
    fi

    while IFS= read -r funcname; do
        [[ -z "$funcname" ]] && continue

        ((total_defs++))

        if [[ -n "${all_defs[$funcname]:-}" ]]; then
            # Shadowing detected!
            ((total_shadowed++))
            echo -e "${RED}✗ SHADOW:${NC} $funcname"
            echo "    First:  ${all_defs[$funcname]}"
            echo "    Shadow: $module"
            ((def_counts[$funcname]++))
        else
            all_defs[$funcname]="$module"
            def_counts[$funcname]=1
        fi
    done < <(extract_functions "$module")
done

echo
echo "=== Summary ==="
echo "Total definitions: $total_defs"
echo "Shadowed definitions: $total_shadowed"
echo

if [[ $total_shadowed -gt 0 ]]; then
    echo -e "${RED}Functions defined multiple times:${NC}"
    for func in "${!def_counts[@]}"; do
        if [[ ${def_counts[$func]} -gt 1 ]]; then
            echo "  $func: ${def_counts[$func]} times"
        fi
    done | sort -t: -k2 -rn | head -20
    echo
    exit 1
else
    echo -e "${GREEN}✓ No shadowing conflicts detected!${NC}"
    exit 0
fi
