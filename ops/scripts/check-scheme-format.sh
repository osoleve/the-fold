#!/bin/bash
# Check if Scheme files are properly indented using scmindent
#
# Usage: ./check-scheme-format.sh [--fix]
#   --fix  : Auto-fix indentation issues

set -e

FIX_MODE=false
if [ "$1" = "--fix" ]; then
    FIX_MODE=true
fi

# Check if scmindent is available
if ! command -v scmindent >/dev/null 2>&1; then
    echo "Error: scmindent not found" >&2
    echo "Install with: sudo npm install -g scmindent" >&2
    exit 1
fi

# Find all Scheme files (excluding certain directories)
SCHEME_FILES=$(find . -name "*.ss" -o -name "*.scm" | \
    grep -v "/\\.git/" | \
    grep -v "/node_modules/" | \
    grep -v "/\\.bbs/" | \
    grep -v "/\\.store/" | \
    sort)

if [ -z "$SCHEME_FILES" ]; then
    echo "No Scheme files found"
    exit 0
fi

ERRORS=0
FILES_CHECKED=0

for file in $SCHEME_FILES; do
    FILES_CHECKED=$((FILES_CHECKED + 1))

    # Get formatted version
    formatted=$(scmindent < "$file" 2>/dev/null || echo "SCMINDENT_ERROR")

    if [ "$formatted" = "SCMINDENT_ERROR" ]; then
        echo "⚠️  Skipping $file (scmindent parse error)"
        continue
    fi

    # Compare with original using temp file
    echo "$formatted" > /tmp/scmindent-check.tmp
    if ! diff -q "$file" /tmp/scmindent-check.tmp >/dev/null 2>&1; then
        if [ "$FIX_MODE" = true ]; then
            echo "🔧 Fixing $file"
            echo "$formatted" > "$file"
        else
            echo "❌ $file needs indentation"
            ERRORS=$((ERRORS + 1))
        fi
    fi
    rm -f /tmp/scmindent-check.tmp
done

if [ "$FIX_MODE" = false ]; then
    if [ $ERRORS -eq 0 ]; then
        echo "✓ All $FILES_CHECKED Scheme files properly indented"
        exit 0
    else
        echo ""
        echo "❌ $ERRORS file(s) need indentation fixes"
        echo "Run: ./ops/scripts/check-scheme-format.sh --fix"
        exit 1
    fi
else
    echo "✓ Fixed indentation in all Scheme files"
    exit 0
fi
