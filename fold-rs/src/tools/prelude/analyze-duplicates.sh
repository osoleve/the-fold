#!/bin/bash
# Analyze duplicate function definitions in missing.ss

echo "=== Duplicate Function Analysis ==="
echo

# Extract all function definitions (pattern: (funcname (fn ...))
grep -n "^([a-z][a-z0-9-]*\? (fn\|^([a-z][a-z0-9-]*\? (fix" missing.ss | \
    sed 's/:([^(]*(.*//' | \
    awk -F: '{print $2}' | \
    sed 's/^(//' | \
    sed 's/ .*//' | \
    sort | \
    uniq -c | \
    sort -rn | \
    head -50

echo
echo "=== Functions defined 3+ times ==="
grep -n "^([a-z][a-z0-9-]*\? (fn\|^([a-z][a-z0-9-]*\? (fix" missing.ss | \
    sed 's/:([^(]*(.*//' | \
    awk -F: '{print $2}' | \
    sed 's/^(//' | \
    sed 's/ .*//' | \
    sort | \
    uniq -c | \
    sort -rn | \
    awk '$1 >= 3 {print $2 " (" $1 " times)"}'

echo
echo "=== Line count by section ==="
awk '/^; [A-Z].*====/{
    if (section) print section ": " count
    section = $0
    count = 0
    next
}
{count++}
END {if (section) print section ": " count}' missing.ss | \
    sort -t: -k2 -rn | \
    head -20
