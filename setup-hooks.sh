#!/usr/bin/env bash
# Setup git hooks for The Fold
#
# This script configures git to use the project's hooks.
# Run once after cloning the repository.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo "Setting up git hooks for The Fold..."

# Option 1: Use pre-commit framework (recommended)
if command -v pre-commit &> /dev/null; then
    echo "Found pre-commit framework, installing hooks..."
    pre-commit install
    echo "✓ Pre-commit hooks installed"
    echo ""
    echo "Run 'pre-commit run --all-files' to check all files"
    exit 0
fi

# Option 2: Use native git hooks
echo "Pre-commit framework not found, using native git hooks..."
git config core.hooksPath .githooks
echo "✓ Git hooks configured to use .githooks/"
echo ""
echo "To install pre-commit framework instead:"
echo "  pip install pre-commit && pre-commit install"
