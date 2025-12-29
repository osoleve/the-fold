#!/bin/bash
# Install custom git hooks for The Fold
#
# This script installs git hooks that enforce code quality:
# - bd sync (beads integration)
# - cargo fmt --check (Rust formatting)
# - cargo clippy (Rust linting)

set -e

HOOKS_DIR=".git/hooks"
HOOK_FILE="$HOOKS_DIR/pre-commit"

if [ ! -d ".git" ]; then
    echo "Error: Must run from repository root" >&2
    exit 1
fi

echo "Installing git pre-commit hook..."

cat > "$HOOK_FILE" << 'EOF'
#!/bin/sh
# bd-shim v1 + Rust formatting + clippy linting
# bd-hooks-version: 0.39.1
#
# bd (beads) pre-commit hook + Rust quality checks
#
# This hook:
# 1. Delegates to 'bd hooks run pre-commit' for bd sync
# 2. Runs cargo fmt --check to ensure consistent formatting
# 3. Runs clippy to ensure code quality

# Check if bd is available and run bd hooks
if command -v bd >/dev/null 2>&1; then
    bd hooks run pre-commit "$@"
    BD_EXIT=$?
    if [ $BD_EXIT -ne 0 ]; then
        echo "bd pre-commit hook failed" >&2
        exit $BD_EXIT
    fi
else
    echo "Warning: bd command not found in PATH, skipping bd sync" >&2
fi

# Run Rust checks on fold-rs if it exists
if [ -d "fold-rs" ]; then
    cd fold-rs || exit 1

    if ! command -v cargo >/dev/null 2>&1; then
        echo "Warning: cargo not found, skipping Rust checks" >&2
        exit 0
    fi

    # Check formatting first
    echo "Checking Rust formatting..."
    cargo fmt --check
    FMT_EXIT=$?

    if [ $FMT_EXIT -ne 0 ]; then
        echo "" >&2
        echo "❌ Formatting check failed! Please format your code before committing." >&2
        echo "Run: cd fold-rs && cargo fmt" >&2
        exit 1
    fi

    echo "✓ Formatting check passed"

    # Run clippy
    echo "Running clippy..."
    cargo clippy --all-targets --all-features -- -D warnings
    CLIPPY_EXIT=$?

    if [ $CLIPPY_EXIT -ne 0 ]; then
        echo "" >&2
        echo "❌ Clippy check failed! Please fix linting errors before committing." >&2
        echo "Run: cd fold-rs && cargo clippy --all-targets --all-features -- -D warnings" >&2
        exit 1
    fi

    echo "✓ Clippy check passed"
fi

exit 0
EOF

chmod +x "$HOOK_FILE"

echo "✓ Git hooks installed successfully"
echo ""
echo "The pre-commit hook will now:"
echo "  1. Sync beads (bd) changes"
echo "  2. Check Rust formatting (cargo fmt --check)"
echo "  3. Run Rust linting (cargo clippy)"
