#!/bin/bash
# Check for security vulnerabilities in Rust dependencies
#
# Usage:
#   ./audit.sh              # Run security audit
#   ./audit.sh --fix        # Attempt to fix vulnerabilities

set -e

cd "$(dirname "$0")/../.." || exit 1

if [ ! -d "fold-rs" ]; then
    echo "Error: fold-rs directory not found" >&2
    exit 1
fi

cd fold-rs || exit 1

# Check if cargo-audit is installed
if ! command -v cargo-audit >/dev/null 2>&1; then
    echo "Error: cargo-audit not found" >&2
    echo "Install with: cargo install cargo-audit" >&2
    exit 1
fi

echo "Running security audit..."
cargo audit "$@"
