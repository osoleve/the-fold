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

# Find Chez Scheme
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
        # Parse sub-options
        USE_LEGACY=false
        if [ "$2" = "--legacy" ]; then
            USE_LEGACY=true
        fi

        if [ -f "$READY_FILE" ]; then
            # Check if daemon is actually running or in zombie state
            if [ -f "$PID_FILE" ]; then
                PID=$(cat "$PID_FILE" 2>/dev/null)
                # Validate PID is numeric
                if ! [[ "$PID" =~ ^[0-9]+$ ]]; then
                    echo "Cleaning up corrupt PID file..."
                    rm -f "$READY_FILE" "$PID_FILE"
                elif kill -0 "$PID" 2>/dev/null; then
                    echo "Daemon is already running (PID: $PID)."
                    echo "Use './daemon.sh stop' to stop it first."
                    exit 1
                else
                    echo "Cleaning up zombie state (PID $PID was not running)..."
                    rm -f "$READY_FILE" "$PID_FILE"
                fi
            else
                echo "Cleaning up stale ready file..."
                rm -f "$READY_FILE"
            fi
        fi

        mkdir -p .fold-repl

        SCHEME_CMD=$(find_scheme) || {
            echo "Error: Chez Scheme not found."
            echo "Install with: apt install chezscheme"
            exit 1
        }
        export FOLD_SCHEME_CMD="$SCHEME_CMD"

        if [ "$USE_LEGACY" = true ]; then
            echo "Starting REPL daemon (file-based) in background..."
            nohup "$SCHEME_CMD" --script start-daemon.ss > .fold-repl/daemon.log 2>&1 &
        else
            echo "Starting REPL daemon (socket) in background..."
            nohup "$SCHEME_CMD" --script start-daemon-socket.ss > .fold-repl/daemon.log 2>&1 &
        fi
        echo $! > "$PID_FILE"

        # Wait for ready file
        for i in {1..30}; do
            # Check if daemon process died
            if ! kill -0 "$(cat "$PID_FILE")" 2>/dev/null; then
                echo "Daemon process died during startup."
                echo "Check .fold-repl/daemon.log for details."
                rm -f "$PID_FILE"
                exit 1
            fi
            if [ -f "$READY_FILE" ]; then
                echo "Daemon started (PID: $(cat $PID_FILE))"
                # Check transport type
                if grep -q "^socket:" "$READY_FILE" 2>/dev/null; then
                    SOCK_PATH=$(sed 's/^socket://' "$READY_FILE")
                    echo "Transport: Unix domain socket ($SOCK_PATH)"
                else
                    echo "Transport: File-based IPC"
                    echo "  Requests:  .fold-repl/requests/<session-id>.ss"
                    echo "  Responses: .fold-repl/responses/<session-id>.txt"
                fi
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

        # Clean up socket file
        rm -f .fold-repl/fold.sock

        if [ -d ".fold-repl/workers" ]; then
            for pidfile in .fold-repl/workers/*.pid; do
                [ -e "$pidfile" ] || continue
                WPID=$(cat "$pidfile")
                # Kill the process group to catch child scheme processes
                kill -- -"$WPID" 2>/dev/null || kill "$WPID" 2>/dev/null || true
            done
        fi

        # Fallback: kill any orphaned worker processes (no pidfile)
        pkill -f "repl-worker-socket.ss" 2>/dev/null || true

        # Clean up stale BBS lock files left by killed workers
        rm -f .bbs/counter.lock .bbs/counter.lock.* 2>/dev/null

        echo "Daemon stopped."
        ;;

    cleanup)
        echo "Cleaning up stale workers..."
        SCHEME_CMD=$(find_scheme) || {
            echo "Error: Chez Scheme not found."
            exit 1
        }
        export FOLD_SCHEME_CMD="$SCHEME_CMD"
        "$SCHEME_CMD" --script boundary/cleanup-workers.ss
        ;;

    status)
        if [ -f "$READY_FILE" ]; then
            # Verify the process is actually running (not a zombie state)
            if [ -f "$PID_FILE" ]; then
                PID=$(cat "$PID_FILE" 2>/dev/null)
                # Validate PID is numeric
                if ! [[ "$PID" =~ ^[0-9]+$ ]]; then
                    echo "WARNING: Corrupt PID file (non-numeric content)"
                    echo "Use './daemon.sh stop' to clean up"
                elif kill -0 "$PID" 2>/dev/null; then
                    echo "Daemon is running (PID: $PID)"
                    if grep -q "^socket:" "$READY_FILE" 2>/dev/null; then
                        SOCK_PATH=$(sed 's/^socket://' "$READY_FILE")
                        echo "Transport: Unix domain socket ($SOCK_PATH)"
                    else
                        echo "Transport: File-based IPC"
                    fi
                else
                    echo "WARNING: Daemon is in zombie state (PID $PID not running)"
                    echo "Ready file exists but process is dead."
                    echo "Use './daemon.sh stop' to clean up, then './daemon.sh start'"
                fi
            else
                echo "WARNING: Ready file exists but no PID file"
                echo "Use './daemon.sh stop' to clean up"
            fi
        else
            echo "Daemon is not running."
        fi
        ;;

    fg)
        echo "Running daemon in foreground (Ctrl+C to stop)..."
        mkdir -p .fold-repl

        SCHEME_CMD=$(find_scheme) || {
            echo "Error: Chez Scheme not found."
            exit 1
        }
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
