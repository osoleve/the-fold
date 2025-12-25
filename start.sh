#!/bin/bash
# start.sh — The Fold REPL Entry Point (Unix)
#
# This is the unified entry point for The Fold. It:
#   1. Checks if Chez Scheme is installed
#   2. Downloads and installs it if needed (Ubuntu/Debian)
#   3. Starts the REPL daemon in background
#   4. Provides an interactive REPL for humans
#
# For Claude Code:
#   - Use: ./daemon.sh start
#   - Write expressions to: .fold-repl/request.ss
#   - Read responses from: .fold-repl/response.txt
#
# For humans:
#   - Just run ./start.sh for an interactive session

set -e
cd "$(dirname "$0")"

# ============================================================
# Configuration
# ============================================================

SCHEME_CMD=""
SCHEME_VERSION="10.1.0"
SCHEME_URL="https://github.com/cisco/ChezScheme/releases/download/v${SCHEME_VERSION}/csv${SCHEME_VERSION}-1_amd64.deb"

# ============================================================
# Find Chez Scheme
# ============================================================

find_scheme() {
    # Try common locations
    for cmd in scheme chez-scheme chezscheme petite; do
        if command -v "$cmd" &> /dev/null; then
            SCHEME_CMD="$cmd"
            return 0
        fi
    done

    # Check common install paths
    if [ -x "/usr/bin/scheme" ]; then
        SCHEME_CMD="/usr/bin/scheme"
        return 0
    fi

    if [ -x "/usr/local/bin/scheme" ]; then
        SCHEME_CMD="/usr/local/bin/scheme"
        return 0
    fi

    return 1
}

# ============================================================
# Install Chez Scheme (Debian/Ubuntu)
# ============================================================

install_scheme() {
    echo "Chez Scheme not found. Attempting to install..."

    if [ -f /etc/debian_version ]; then
        echo "Detected Debian/Ubuntu system"

        # Try apt first
        if command -v apt-get &> /dev/null; then
            echo "Installing via apt..."
            sudo apt-get update
            sudo apt-get install -y chezscheme || {
                echo "apt install failed, trying direct download..."
                install_scheme_direct
            }
        else
            install_scheme_direct
        fi
    else
        echo "Unsupported system. Please install Chez Scheme manually."
        echo "Visit: https://cisco.github.io/ChezScheme/"
        exit 1
    fi

    # Verify installation
    if ! find_scheme; then
        echo "Installation failed. Please install Chez Scheme manually."
        exit 1
    fi

    echo "Chez Scheme installed successfully: $SCHEME_CMD"
}

install_scheme_direct() {
    echo "Downloading Chez Scheme ${SCHEME_VERSION}..."
    local DEB_FILE="/tmp/chez-scheme.deb"
    curl -L -o "$DEB_FILE" "$SCHEME_URL"
    sudo dpkg -i "$DEB_FILE" || sudo apt-get install -f -y
    rm -f "$DEB_FILE"
}

# ============================================================
# Main
# ============================================================

echo ""
echo "========================================"
echo "       THE FOLD - GENESIS"
echo "========================================"
echo ""

# Find or install Scheme
if ! find_scheme; then
    install_scheme
fi

echo "Using Scheme: $SCHEME_CMD"
echo ""

# Check for daemon mode
if [ "$1" = "daemon" ]; then
    echo "Starting REPL daemon..."
    exec "$SCHEME_CMD" --script start-daemon.ss
fi

# Interactive mode
echo "Starting interactive REPL..."
echo "After loading, login with:"
echo "  (hi 'opus 'your-name \"message\")"
echo ""

exec "$SCHEME_CMD" --script start-repl.ss
