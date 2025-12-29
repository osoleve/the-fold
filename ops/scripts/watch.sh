#!/bin/bash
# Auto-rebuild and run tests on file changes
#
# Usage:
#   ./watch.sh              # Watch for changes and run tests
#   ./watch.sh --exec run   # Watch and run the binary
#   ./watch.sh --help       # Show cargo-watch help

set -e

cd "$(dirname "$0")/../..") || exit 1

if [ ! -d "fold-rs" ]; then
    echo "Error: fold-rs directory not found" >&2
    exit 1
fi

cd fold-rs || exit 1

# Check if cargo-watch is installed
if ! command -v cargo-watch >/dev/null 2>&1; then
    echo "Error: cargo-watch not found" >&2
    echo "Install with: cargo install cargo-watch" >&2
    exit 1
fi

# Default: watch and run tests
if [ $# -eq 0 ]; then
    echo "Watching for changes... (Ctrl+C to stop)"
    cargo watch -x "test --all-features"
else
    cargo watch "$@"
fi
