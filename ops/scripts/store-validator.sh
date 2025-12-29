#!/bin/bash
# Validate .store/ directory integrity
#
# Usage:
#   ./store-validator.sh [store-path]      # Validate specific store
#   ./store-validator.sh                   # Validate .store in project root

set -e

cd "$(dirname "$0")/../.." || exit 1

STORE_PATH="${1:-.store}"

if [ ! -d "$STORE_PATH" ]; then
    echo "Error: Store directory not found: $STORE_PATH" >&2
    echo "Usage: $0 [store-path]" >&2
    exit 1
fi

echo "Validating block store: $STORE_PATH"
echo ""

cd fold-rs || exit 1
cargo run --bin fold-store-validator -- "../$STORE_PATH"
