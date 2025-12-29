#!/bin/bash
# Run mutation testing with cargo-mutants
#
# Usage:
#   ./mutants.sh           # Run mutation testing (SLOW!)
#   ./mutants.sh --list    # List potential mutants without running
#   ./mutants.sh --file <path>  # Test specific file

set -e

cd "$(dirname "$0")/../.." || exit 1

if [ ! -d "fold-rs" ]; then
    echo "Error: fold-rs directory not found" >&2
    exit 1
fi

cd fold-rs || exit 1

# Check if cargo-mutants is installed
if ! command -v cargo-mutants >/dev/null 2>&1; then
    echo "Error: cargo-mutants not found" >&2
    echo "Install with: cargo install cargo-mutants" >&2
    exit 1
fi

echo "Running mutation testing..."
echo "⚠️  WARNING: This is VERY SLOW and runs tests many times!"
echo ""

cargo mutants "$@"
