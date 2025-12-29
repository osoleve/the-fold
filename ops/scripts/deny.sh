#!/bin/bash
# Run cargo-deny checks on fold-rs dependencies
#
# Usage:
#   ./deny.sh           # Check everything
#   ./deny.sh advisories # Check security advisories
#   ./deny.sh licenses  # Check licenses
#   ./deny.sh bans      # Check banned/duplicate dependencies
#   ./deny.sh sources   # Check dependency sources

set -e

cd "$(dirname "$0")/../.." || exit 1

if [ ! -d "fold-rs" ]; then
    echo "Error: fold-rs directory not found" >&2
    exit 1
fi

cd fold-rs || exit 1

# Check if cargo-deny is installed
if ! command -v cargo-deny >/dev/null 2>&1; then
    echo "Error: cargo-deny not found" >&2
    echo "Install with: cargo install cargo-deny" >&2
    exit 1
fi

CHECK="${1:-check}"

case "$CHECK" in
    advisories|licenses|bans|sources)
        echo "Running cargo deny check $CHECK..."
        cargo deny check "$CHECK"
        ;;
    check|*)
        echo "Running all cargo deny checks..."
        cargo deny check
        ;;
esac
