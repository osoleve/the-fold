#!/bin/bash
# daemon.sh — REPL Daemon Management (Unix)
#
# Usage:
#   ./daemon.sh start   — Start the daemon in background
#   ./daemon.sh stop    — Stop the daemon
#   ./daemon.sh status  — Check if daemon is running
#   ./daemon.sh fg      — Run daemon in foreground (for debugging)

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
        nohup "$SCHEME_CMD" --script start-daemon.ss > .fold-repl/daemon.log 2>&1 &
        echo $! > "$PID_FILE"

        # Wait for ready file
        for i in {1..30}; do
            if [ -f "$READY_FILE" ]; then
                echo "Daemon started (PID: $(cat $PID_FILE))"
                echo "Write expressions to: .fold-repl/request.ss"
                echo "Read responses from:  .fold-repl/response.txt"
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

        echo "Daemon stopped."
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
        exec "$SCHEME_CMD" --script start-daemon.ss
        ;;

    *)
        echo ""
        echo "Usage: ./daemon.sh [start|stop|status|fg]"
        echo ""
        echo "  start  — Start the daemon in background"
        echo "  stop   — Stop the daemon"
        echo "  status — Check if daemon is running"
        echo "  fg     — Run daemon in foreground"
        echo ""
        exit 1
        ;;
esac
