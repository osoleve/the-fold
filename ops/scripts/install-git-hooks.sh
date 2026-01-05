#!/bin/bash
# Install custom git hooks for The Fold
#
# This script installs git hooks that enforce code quality:
# - bd sync (beads integration)
# - scmindent (Scheme indentation, auto-fixing)
# - kind-check (kind system validation)
# - type-annotation-check (type signature validation)
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
# bd-shim v1 + Scheme + Rust formatting + clippy linting + kind checking
# bd-hooks-version: 0.39.2
#
# bd (beads) pre-commit hook + code quality checks
#
# This hook:
# 1. Delegates to 'bd hooks run pre-commit' for bd sync
# 2. Checks Scheme indentation (scmindent)
# 3. Validates kind system (kind-check.ss)
# 4. Runs cargo fmt --check (Rust formatting)
# 5. Runs clippy (Rust linting)

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

# Check Scheme formatting on staged .ss and .scm files
STAGED_SCHEME=$(git diff --cached --name-only --diff-filter=ACM | grep -E '\.(ss|scm)$' || true)

if [ -n "$STAGED_SCHEME" ]; then
    if ! command -v scmindent >/dev/null 2>&1; then
        echo "Warning: scmindent not found, skipping Scheme formatting check" >&2
        echo "  Install with: sudo npm install -g scmindent" >&2
    else
        echo "Checking Scheme indentation..."
        SCHEME_FIXED=0

        for file in $STAGED_SCHEME; do
            # Get formatted version directly to temp file
            # (Using printf or echo corrupts backslash escapes like #\b)
            if ! scmindent < "$file" > /tmp/scmindent-check.tmp 2>/dev/null; then
                continue  # Skip files that scmindent can't parse
            fi
            if ! diff -q "$file" /tmp/scmindent-check.tmp >/dev/null 2>&1; then
                # Auto-fix: apply the formatted version
                cp /tmp/scmindent-check.tmp "$file"
                git add "$file"
                echo "  ✓ Fixed indentation: $file"
                SCHEME_FIXED=$((SCHEME_FIXED + 1))
            fi
            rm -f /tmp/scmindent-check.tmp
        done

        if [ $SCHEME_FIXED -gt 0 ]; then
            echo "✓ Fixed indentation in $SCHEME_FIXED file(s)"
        else
            echo "✓ Scheme indentation check passed"
        fi
    fi
fi

# Run kind system validation if core/types files changed
STAGED_TYPES=$(git diff --cached --name-only --diff-filter=ACM | grep -E '^core/types/.*\.ss$' || true)

if [ -n "$STAGED_TYPES" ]; then
    if command -v scheme >/dev/null 2>&1; then
        echo "Running kind system validation..."
        if scheme --script core/types/kind-check.ss; then
            echo "✓ Kind check passed"
        else
            echo "" >&2
            echo "❌ Kind check failed! Please fix kind errors before committing." >&2
            echo "Run: scheme --script core/types/kind-check.ss" >&2
            exit 1
        fi
    else
        echo "Warning: scheme not found, skipping kind check" >&2
    fi
fi

# Run type annotation validation if core files changed
STAGED_CORE=$(git diff --cached --name-only --diff-filter=ACM | grep -E '^core/.*\.ss$' || true)

if [ -n "$STAGED_CORE" ]; then
    if command -v scheme >/dev/null 2>&1; then
        echo "Running type annotation validation..."
        if scheme --script core/types/type-annotation-check.ss $STAGED_CORE; then
            echo "✓ Type annotation check passed"
        else
            echo "" >&2
            echo "❌ Type annotation check failed!" >&2
            echo "Run: scheme --script core/types/type-annotation-check.ss --verbose" >&2
            exit 1
        fi
    else
        echo "Warning: scheme not found, skipping type annotation check" >&2
    fi
fi

# Run Rust checks on fold-rs if it exists and has Cargo.toml
if [ -f "fold-rs/Cargo.toml" ]; then
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
echo "  2. Check/fix Scheme indentation (scmindent)"
echo "  3. Validate kind system (kind-check.ss) for core/types changes"
echo "  4. Validate type annotations (type-annotation-check.ss) for core changes"
echo "  5. Check Rust formatting (cargo fmt --check)"
echo "  6. Run Rust linting (cargo clippy)"
echo ""
echo "Note: Install additional tools if needed:"
echo "  scmindent: sudo npm install -g scmindent"
