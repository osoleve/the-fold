#!/bin/bash
# daemon.sh — REPL Daemon Management (Unix)
#
# Usage:
#   ./daemon.sh start    — Start the daemon in background
#   ./daemon.sh stop     — Stop the daemon
#   ./daemon.sh status   — Check if daemon is running
#   ./daemon.sh fg       — Run daemon in foreground (for debugging)
#   ./daemon.sh cleanup  — Cleanup stale workers by heartbeat age
#
# Environment:
#   FOLD_USE_SCHEME=1  — Force Scheme daemon (skip Rust)
#
# Prefers Rust daemon if available, otherwise falls back to Chez Scheme.

set -e
cd "$(dirname "$0")"

READY_FILE=".fold-repl/ready"
PID_FILE=".fold-repl/daemon.pid"
RUST_DAEMON="./fold-rs/target/release/fold-daemon"

# Find Scheme (for fallback)
find_scheme() {
    for cmd in scheme chez-scheme chezscheme petite; do
        if command -v "$cmd" &> /dev/null; then
            echo "$cmd"
            return 0
        fi
    done
    return 1
}

case "$1" in
    start)
        if [ -f "$READY_FILE" ]; then
            echo "Daemon is already running."
            echo "Use './daemon.sh stop' to stop it first."
            exit 1
        fi

        echo "Starting REPL daemon in background..."
        mkdir -p .fold-repl

        # Prefer Rust daemon if available (unless FOLD_USE_SCHEME is set)
        if [ -x "$RUST_DAEMON" ] && [ -z "$FOLD_USE_SCHEME" ]; then
            echo "Using Rust daemon..."
            nohup "$RUST_DAEMON" > .fold-repl/daemon.log 2>&1 &
        else
            SCHEME_CMD=$(find_scheme) || {
                echo "Error: Neither Rust daemon nor Chez Scheme found."
                echo "Run 'cargo build --release' in fold-rs/ to build the Rust daemon."
                exit 1
            }
            echo "Using Scheme daemon..."
            export FOLD_SCHEME_CMD="$SCHEME_CMD"
            nohup "$SCHEME_CMD" --script start-daemon.ss > .fold-repl/daemon.log 2>&1 &
        fi
        echo $! > "$PID_FILE"

        # Wait for ready file
        for i in {1..30}; do
            if [ -f "$READY_FILE" ]; then
                echo "Daemon started (PID: $(cat $PID_FILE))"
                echo "Write expressions to: .fold-repl/requests/<session-id>.ss"
                echo "Read responses from:  .fold-repl/responses/<session-id>.txt"
                exit 0
            fi
            sleep 0.5
        done

        echo "Timeout waiting for daemon to start."
        exit 1
        ;;

    stop)
        if [ ! -f "$READY_FILE" ]; then
            echo "Daemon is not running."
            exit 0
        fi

        echo "Stopping daemon..."
        rm -f "$READY_FILE"

        if [ -f "$PID_FILE" ]; then
            PID=$(cat "$PID_FILE")
            kill "$PID" 2>/dev/null || true
            rm -f "$PID_FILE"
        fi

        if [ -d ".fold-repl/workers" ]; then
            for pidfile in .fold-repl/workers/*.pid; do
                [ -e "$pidfile" ] || continue
                WPID=$(cat "$pidfile")
                kill "$WPID" 2>/dev/null || true
            done
        fi

        echo "Daemon stopped."
        ;;

    cleanup)
        echo "Cleaning up stale workers..."
        SCHEME_CMD=$(find_scheme) || {
            echo "Error: Chez Scheme not found (cleanup only applies to Scheme daemon)."
            exit 1
        }
        export FOLD_SCHEME_CMD="$SCHEME_CMD"
        "$SCHEME_CMD" --script thimble/cleanup-workers.ss
        ;;

    status)
        if [ -f "$READY_FILE" ]; then
            echo "Daemon is running."
            if [ -f "$PID_FILE" ]; then
                echo "PID: $(cat $PID_FILE)"
            fi
        else
            echo "Daemon is not running."
        fi
        ;;

    fg)
        echo "Running daemon in foreground (Ctrl+C to stop)..."
        mkdir -p .fold-repl

        # Prefer Rust daemon if available (unless FOLD_USE_SCHEME is set)
        if [ -x "$RUST_DAEMON" ] && [ -z "$FOLD_USE_SCHEME" ]; then
            exec "$RUST_DAEMON"
        else
            SCHEME_CMD=$(find_scheme) || {
                echo "Error: Neither Rust daemon nor Chez Scheme found."
                exit 1
            }
            export FOLD_SCHEME_CMD="$SCHEME_CMD"
            exec "$SCHEME_CMD" --script start-daemon.ss
        fi
        ;;

    *)
        echo ""
        echo "Usage: ./daemon.sh [start|stop|status|fg|cleanup]"
        echo ""
        echo "  start    — Start the daemon in background"
        echo "  stop     — Stop the daemon"
        echo "  status   — Check if daemon is running"
        echo "  fg       — Run daemon in foreground"
        echo "  cleanup  — Cleanup stale workers by heartbeat age"
        echo ""
        exit 1
        ;;
esac
