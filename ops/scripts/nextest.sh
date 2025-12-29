#!/bin/bash
# Run tests using cargo-nextest (faster, better test runner)
#
# Usage:
#   ./nextest.sh           # Run all tests
#   ./nextest.sh --no-capture  # Run with output visible
#   ./nextest.sh <filter>  # Run specific test

set -e

cd "$(dirname "$0")/../.." || exit 1

if [ ! -d "fold-rs" ]; then
    echo "Error: fold-rs directory not found" >&2
    exit 1
fi

cd fold-rs || exit 1

# Check if cargo-nextest is installed
if ! command -v cargo-nextest >/dev/null 2>&1; then
    echo "Error: cargo-nextest not found" >&2
    echo "Install with: cargo install cargo-nextest" >&2
    exit 1
fi

echo "Running tests with nextest..."
cargo nextest run --all-features "$@"
