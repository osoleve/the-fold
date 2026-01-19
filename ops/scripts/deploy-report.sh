#!/bin/bash
# deploy-report.sh — Deploy technical report to website
#
# Builds the technical report (markdown + HTML) and deploys to /var/www/oso.rocks/
#
# Usage:
#   ./deploy-report.sh              # Build and deploy
#   ./deploy-report.sh --build-only # Build without deploying
#   ./deploy-report.sh --deploy-only # Deploy existing files (skip build)

set -euo pipefail

FOLD_ROOT="${FOLD_ROOT:-$(cd "$(dirname "$0")/../.." && pwd)}"
cd "$FOLD_ROOT"

WEBSITE_DIR="/var/www/oso.rocks"
BUILD_ONLY=0
DEPLOY_ONLY=0

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --build-only)
            BUILD_ONLY=1
            shift
            ;;
        --deploy-only)
            DEPLOY_ONLY=1
            shift
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 [--build-only|--deploy-only]"
            exit 1
            ;;
    esac
done

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log() {
    echo -e "${GREEN}[DEPLOY]${NC} $*"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

# Build step
build_report() {
    log "Building technical report..."

    # Assemble markdown
    info "  Assembling markdown from chapters..."
    scheme --script docs/technical-report/assemble.ss

    # Build HTML versions
    info "  Building HTML (simple)..."
    scheme --script boundary/tools/build-report.ss

    info "  Building HTML (navigable)..."
    scheme --script boundary/tools/build-nav-report.ss

    log "Build complete."
    echo ""
    info "Generated files:"
    ls -lh docs/technical-report.md docs/technical-report.html docs/technical-report-nav.html 2>/dev/null || true
}

# Deploy step
deploy_report() {
    log "Deploying to $WEBSITE_DIR..."

    # Check if website directory exists
    if [[ ! -d "$WEBSITE_DIR" ]]; then
        warn "Website directory $WEBSITE_DIR does not exist"
        exit 1
    fi

    # Check if we have the files to deploy
    if [[ ! -f "docs/technical-report.html" ]] || [[ ! -f "docs/technical-report-nav.html" ]]; then
        warn "HTML files not found. Run with --build-only first or without flags."
        exit 1
    fi

    # Deploy files
    info "  Copying technical-report.html -> report-fold.html"
    sudo cp docs/technical-report.html "$WEBSITE_DIR/report-fold.html"

    info "  Copying technical-report-nav.html -> report.html"
    sudo cp docs/technical-report-nav.html "$WEBSITE_DIR/report.html"

    log "Deploy complete."
    echo ""
    info "Website files:"
    ls -lh "$WEBSITE_DIR"/report*.html
    echo ""
    info "View at:"
    echo "  https://oso.rocks/report-fold.html (simple)"
    echo "  https://oso.rocks/report.html (navigable)"
}

# Main
echo ""
echo "═══════════════════════════════════════════════════════════"
echo "  Technical Report Deploy Pipeline"
echo "═══════════════════════════════════════════════════════════"
echo ""

if [[ $DEPLOY_ONLY -eq 0 ]]; then
    build_report
    echo ""
fi

if [[ $BUILD_ONLY -eq 0 ]]; then
    deploy_report
fi

echo ""
log "Done."
