#!/bin/bash
# health-check.sh — Health check for The Fold daemon
#
# Verifies:
#   - Daemon is running
#   - REPL is responsive
#   - Resources are within limits
#
# Exit codes:
#   0 - Healthy
#   1 - Unhealthy (should alert)
#   2 - Warning (degraded but functional)

set -euo pipefail

FOLD_ROOT="${FOLD_ROOT:-$(cd "$(dirname "$0")/../.." && pwd)}"
cd "$FOLD_ROOT"

TIMEOUT="${HEALTH_CHECK_TIMEOUT:-10}"
ALERT_SCRIPT="${FOLD_ROOT}/ops/scripts/alert.sh"

# Thresholds
MEMORY_WARN_PERCENT=70
MEMORY_CRIT_PERCENT=90
DISK_WARN_PERCENT=80
DISK_CRIT_PERCENT=90
READY_FILE_MAX_AGE=300  # 5 minutes

RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
NC='\033[0m'

EXIT_CODE=0
FAILURES=()
WARNINGS=()

check_fail() {
    FAILURES+=("$1")
    EXIT_CODE=1
}

check_warn() {
    WARNINGS+=("$1")
    if [[ $EXIT_CODE -eq 0 ]]; then
        EXIT_CODE=2
    fi
}

# Check 1: Ready file exists and is fresh
if [[ ! -f .fold-repl/ready ]]; then
    check_fail "Daemon not ready (no ready file)"
else
    READY_AGE=$(($(date +%s) - $(stat -c %Y .fold-repl/ready)))
    if [[ $READY_AGE -gt $READY_FILE_MAX_AGE ]]; then
        check_fail "Ready file is stale (${READY_AGE}s old)"
    fi
fi

# Check 2: Daemon process is running
if ! pgrep -f "scheme.*start-daemon" > /dev/null; then
    check_fail "Daemon process not found"
fi

# Check 3: REPL responds to simple expression
if [[ -f .fold-repl/ready ]]; then
    TEST_SESSION="health-check-$$"
    TEST_REQUEST=".fold-repl/requests/${TEST_SESSION}.ss"
    TEST_RESPONSE=".fold-repl/responses/${TEST_SESSION}.txt"

    # Clean up any old test files
    rm -f "$TEST_REQUEST" "$TEST_RESPONSE"

    # Send test expression
    echo "(+ 1 1)" > "$TEST_REQUEST"

    # Wait for response
    ELAPSED=0
    while [[ ! -f "$TEST_RESPONSE" ]] && [[ $ELAPSED -lt $TIMEOUT ]]; do
        sleep 0.5
        ELAPSED=$((ELAPSED + 1))
    done

    if [[ ! -f "$TEST_RESPONSE" ]]; then
        check_fail "REPL did not respond within ${TIMEOUT}s"
    else
        RESPONSE=$(cat "$TEST_RESPONSE" 2>/dev/null || echo "")
        if [[ "$RESPONSE" != "2" ]]; then
            check_fail "REPL response incorrect (expected '2', got '$RESPONSE')"
        fi
    fi

    # Cleanup
    rm -f "$TEST_REQUEST" "$TEST_RESPONSE"
fi

# Check 4: Memory usage
if pgrep -f "scheme.*start-daemon" > /dev/null; then
    DAEMON_PID=$(pgrep -f "scheme.*start-daemon")
    MEMORY_KB=$(ps -o rss= -p "$DAEMON_PID" 2>/dev/null || echo "0")
    MEMORY_MB=$((MEMORY_KB / 1024))
    TOTAL_MEMORY_MB=$(free -m | awk '/^Mem:/ {print $2}')
    MEMORY_PERCENT=$((MEMORY_MB * 100 / TOTAL_MEMORY_MB))

    if [[ $MEMORY_PERCENT -gt $MEMORY_CRIT_PERCENT ]]; then
        check_fail "Memory usage critical: ${MEMORY_PERCENT}% (${MEMORY_MB}MB)"
    elif [[ $MEMORY_PERCENT -gt $MEMORY_WARN_PERCENT ]]; then
        check_warn "Memory usage high: ${MEMORY_PERCENT}% (${MEMORY_MB}MB)"
    fi
fi

# Check 5: Disk usage
DISK_USAGE=$(df -h /home/oso | awk 'NR==2 {print $(NF-1)}' | sed 's/%//')
if [[ $DISK_USAGE -gt $DISK_CRIT_PERCENT ]]; then
    check_fail "Disk usage critical: ${DISK_USAGE}%"
elif [[ $DISK_USAGE -gt $DISK_WARN_PERCENT ]]; then
    check_warn "Disk usage high: ${DISK_USAGE}%"
fi

# Check 6: Store integrity (basic)
if [[ -d .store/objects ]]; then
    OBJECT_COUNT=$(find .store/objects -type f 2>/dev/null | wc -l)
    if [[ $OBJECT_COUNT -eq 0 ]]; then
        check_warn "Store has no objects (might be freshly initialized)"
    fi
fi

# Report results
echo ""
echo "╔════════════════════════════════════════════════════╗"
echo "║          THE FOLD HEALTH CHECK                     ║"
echo "╚════════════════════════════════════════════════════╝"
echo ""

if [[ ${#FAILURES[@]} -eq 0 ]] && [[ ${#WARNINGS[@]} -eq 0 ]]; then
    echo -e "${GREEN}✓ All checks passed${NC}"
    echo ""
    echo "Status: HEALTHY"
    echo "Timestamp: $(date -u +"%Y-%m-%d %H:%M:%S UTC")"
elif [[ ${#FAILURES[@]} -eq 0 ]]; then
    echo -e "${YELLOW}⚠ Warnings detected${NC}"
    echo ""
    for warn in "${WARNINGS[@]}"; do
        echo -e "${YELLOW}  • $warn${NC}"
    done
    echo ""
    echo "Status: DEGRADED"
else
    echo -e "${RED}✗ Health check failed${NC}"
    echo ""
    for fail in "${FAILURES[@]}"; do
        echo -e "${RED}  • $fail${NC}"
    done
    if [[ ${#WARNINGS[@]} -gt 0 ]]; then
        echo ""
        echo -e "${YELLOW}Additional warnings:${NC}"
        for warn in "${WARNINGS[@]}"; do
            echo -e "${YELLOW}  • $warn${NC}"
        done
    fi
    echo ""
    echo "Status: UNHEALTHY"

    # Send alert if script exists
    if [[ -x "$ALERT_SCRIPT" ]]; then
        MESSAGE="The Fold health check failed:\n"
        for fail in "${FAILURES[@]}"; do
            MESSAGE+="- $fail\n"
        done
        "$ALERT_SCRIPT" "Health Check Failed" "$MESSAGE" || true
    fi
fi

echo ""

exit $EXIT_CODE
