#!/bin/bash
# session-start.sh — Install Chez Scheme and start The Fold REPL daemon
#
# This hook runs at session start in Claude Code web environments.
# It installs Chez Scheme from source (if needed) and starts the daemon.

set -euo pipefail

# Only run in remote/cloud environments
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
    exit 0
fi

cd "$CLAUDE_PROJECT_DIR"

echo "=== The Fold Session Start Hook ==="

# Check if Chez Scheme is already installed
SCHEME_CMD=""
for cmd in scheme chez-scheme chezscheme petite; do
    if command -v "$cmd" &> /dev/null; then
        SCHEME_CMD="$cmd"
        break
    fi
done

if [ -z "$SCHEME_CMD" ]; then
    echo "Installing Chez Scheme..."

    # Install build dependencies
    apt-get update -qq
    apt-get install -y -qq build-essential libncurses-dev libx11-dev uuid-dev git

    # Clone and build Chez Scheme
    cd /tmp
    if [ -d "ChezScheme" ]; then
        rm -rf ChezScheme
    fi
    git clone --depth 1 https://github.com/cisco/ChezScheme.git
    cd ChezScheme
    ./configure --installprefix=/usr/local
    make -j$(nproc)
    make install

    cd "$CLAUDE_PROJECT_DIR"

    # Verify installation
    if command -v scheme &> /dev/null; then
        echo "Chez Scheme installed successfully: $(scheme --version 2>&1 | head -1)"
        SCHEME_CMD="scheme"
    else
        echo "ERROR: Chez Scheme installation failed"
        exit 1
    fi
else
    echo "Chez Scheme already installed: $($SCHEME_CMD --version 2>&1 | head -1)"
fi

# Create REPL directory
mkdir -p .fold-repl

# Stop any existing daemon
if [ -f ".fold-repl/ready" ]; then
    echo "Stopping existing daemon..."
    rm -f .fold-repl/ready
    if [ -f ".fold-repl/daemon.pid" ]; then
        kill "$(cat .fold-repl/daemon.pid)" 2>/dev/null || true
        rm -f .fold-repl/daemon.pid
    fi
    sleep 1
fi

# Start the REPL daemon
echo "Starting The Fold REPL daemon..."
nohup "$SCHEME_CMD" --script start-daemon.ss > .fold-repl/daemon.log 2>&1 &
echo $! > .fold-repl/daemon.pid

# Wait for daemon to be ready
for i in {1..30}; do
    if [ -f ".fold-repl/ready" ]; then
        echo "Daemon started (PID: $(cat .fold-repl/daemon.pid))"
        break
    fi
    sleep 0.5
done

if [ ! -f ".fold-repl/ready" ]; then
    echo "WARNING: Daemon may not have started. Check .fold-repl/daemon.log"
fi

# Export scheme command for the session
echo "export SCHEME_CMD=$SCHEME_CMD" >> "$CLAUDE_ENV_FILE"
echo "export PATH=/usr/local/bin:\$PATH" >> "$CLAUDE_ENV_FILE"

echo "=== Session Start Hook Complete ==="
