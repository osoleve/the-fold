#!/bin/bash
# daemon.sh — REPL Daemon Management (Unix)
#
# Usage:
#   ./daemon.sh start    — Start the daemon in background
#   ./daemon.sh stop     — Stop the daemon
#   ./daemon.sh status   — Check if daemon is running
#   ./daemon.sh fg       — Run daemon in foreground (for debugging)
#   ./daemon.sh cleanup  — Cleanup stale workers by heartbeat age

set -e
cd "$(dirname "$0")"

READY_FILE=".fold-repl/ready"
PID_FILE=".fold-repl/daemon.pid"

# Find Scheme
SCHEME_CMD=""
for cmd in scheme chez-scheme chezscheme petite; do
    if command -v "$cmd" &> /dev/null; then
        SCHEME_CMD="$cmd"
        break
    fi
done

if [ -z "$SCHEME_CMD" ]; then
    echo "Error: Chez Scheme not found. Run ./start.sh first to install."
    exit 1
fi

case "$1" in
    start)
        if [ -f "$READY_FILE" ]; then
            echo "Daemon is already running."
            echo "Use './daemon.sh stop' to stop it first."
            exit 1
        fi

        echo "Starting REPL daemon in background..."
        export FOLD_SCHEME_CMD="$SCHEME_CMD"
        nohup "$SCHEME_CMD" --script start-daemon.ss > .fold-repl/daemon.log 2>&1 &
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
        export FOLD_SCHEME_CMD="$SCHEME_CMD"
        exec "$SCHEME_CMD" --script start-daemon.ss
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
