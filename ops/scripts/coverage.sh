#!/bin/bash
# Generate test coverage reports for fold-rs
#
# Usage:
#   ./coverage.sh           # Show summary
#   ./coverage.sh --html    # Generate HTML report
#   ./coverage.sh --lcov    # Generate LCOV file
#   ./coverage.sh --open    # Generate and open HTML report

set -e

cd "$(dirname "$0")/../.." || exit 1

if [ ! -d "fold-rs" ]; then
    echo "Error: fold-rs directory not found" >&2
    exit 1
fi

cd fold-rs || exit 1

# Check if cargo-llvm-cov is installed
if ! command -v cargo-llvm-cov >/dev/null 2>&1; then
    echo "Error: cargo-llvm-cov not found" >&2
    echo "Install with: cargo install cargo-llvm-cov" >&2
    exit 1
fi

MODE="${1:---summary}"

case "$MODE" in
    --html)
        echo "Generating HTML coverage report..."
        cargo llvm-cov --all-features --workspace --html
        echo ""
        echo "✓ Report generated: fold-rs/target/llvm-cov/html/index.html"
        ;;
    --open)
        echo "Generating and opening HTML coverage report..."
        cargo llvm-cov --all-features --workspace --open
        ;;
    --lcov)
        echo "Generating LCOV coverage report..."
        cargo llvm-cov --all-features --workspace --lcov --output-path coverage.lcov
        echo ""
        echo "✓ Report generated: fold-rs/coverage.lcov"
        ;;
    --summary|*)
        echo "Running tests with coverage..."
        echo ""
        cargo llvm-cov --all-features --workspace --summary-only
        echo ""
        echo "For detailed reports:"
        echo "  HTML: ./ops/scripts/coverage.sh --html"
        echo "  Open: ./ops/scripts/coverage.sh --open"
        echo "  LCOV: ./ops/scripts/coverage.sh --lcov"
        ;;
esac
