#!/bin/bash
# Install custom git hooks for The Fold
#
# This script installs git hooks that enforce code quality:
# - bd sync (beads integration)
# - typos (spell checking)
# - scmindent (Scheme indentation)
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
# bd-shim v1 + Scheme + Rust formatting + clippy linting
# bd-hooks-version: 0.39.1
#
# bd (beads) pre-commit hook + code quality checks
#
# This hook:
# 1. Delegates to 'bd hooks run pre-commit' for bd sync
# 2. Checks spelling with typos
# 3. Checks Scheme indentation (scmindent)
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

# Check spelling with typos
if command -v typos >/dev/null 2>&1; then
    echo "Checking spelling with typos..."
    if ! typos; then
        echo "" >&2
        echo "❌ Spelling check failed!" >&2
        echo "Run: typos (to see errors) or typos --write-changes (to fix)" >&2
        exit 1
    fi
    echo "✓ Spelling check passed"
else
    echo "Warning: typos not found, skipping spell check" >&2
    echo "  Install with: cargo install typos-cli" >&2
fi

# Check Scheme formatting on staged .ss and .scm files
STAGED_SCHEME=$(git diff --cached --name-only --diff-filter=ACM | grep -E '\.(ss|scm)$' || true)

if [ -n "$STAGED_SCHEME" ]; then
    if ! command -v scmindent >/dev/null 2>&1; then
        echo "Warning: scmindent not found, skipping Scheme formatting check" >&2
        echo "  Install with: sudo npm install -g scmindent" >&2
    else
        echo "Checking Scheme indentation..."
        SCHEME_ERRORS=0

        for file in $STAGED_SCHEME; do
            # Get formatted version
            formatted=$(scmindent < "$file" 2>/dev/null || echo "SCMINDENT_ERROR")

            if [ "$formatted" = "SCMINDENT_ERROR" ]; then
                continue  # Skip files that scmindent can't parse
            fi

            # Compare with original using temp file
            echo "$formatted" > /tmp/scmindent-check.tmp
            if ! diff -q "$file" /tmp/scmindent-check.tmp >/dev/null 2>&1; then
                if [ $SCHEME_ERRORS -eq 0 ]; then
                    echo "" >&2
                    echo "❌ Scheme indentation check failed!" >&2
                fi
                echo "  - $file" >&2
                SCHEME_ERRORS=$((SCHEME_ERRORS + 1))
            fi
            rm -f /tmp/scmindent-check.tmp
        done

        if [ $SCHEME_ERRORS -gt 0 ]; then
            echo "" >&2
            echo "Run: ./ops/scripts/check-scheme-format.sh --fix" >&2
            echo "Or fix individual files: scmindent < file.ss > file.ss.tmp && mv file.ss.tmp file.ss" >&2
            exit 1
        fi

        echo "✓ Scheme indentation check passed"
    fi
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
echo "  2. Check spelling (typos)"
echo "  3. Check Scheme indentation (scmindent)"
echo "  4. Check Rust formatting (cargo fmt --check)"
echo "  5. Run Rust linting (cargo clippy)"
echo ""
echo "Note: Install additional tools if needed:"
echo "  typos:     cargo install typos-cli"
echo "  scmindent: sudo npm install -g scmindent"
