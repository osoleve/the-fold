#!/bin/bash
# Check for API breaking changes with cargo-semver-checks
#
# Usage:
#   ./semver-checks.sh           # Check against latest published version
#   ./semver-checks.sh --baseline-rev <commit>  # Check against specific commit

set -e

cd "$(dirname "$0")/../.." || exit 1

if [ ! -d "fold-rs" ]; then
    echo "Error: fold-rs directory not found" >&2
    exit 1
fi

cd fold-rs || exit 1

# Check if cargo-semver-checks is installed
if ! command -v cargo-semver-checks >/dev/null 2>&1; then
    echo "Error: cargo-semver-checks not found" >&2
    echo "Install with: cargo install cargo-semver-checks" >&2
    exit 1
fi

echo "Running semver compatibility checks..."
echo "Note: This checks for API breaking changes"
echo ""

cargo semver-checks "$@"
