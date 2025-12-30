#!/usr/bin/env bash
#
# scheduler.sh - Agent Scheduler
#
# Manages scheduled execution of agents. Can be run as:
#   - One-shot: scheduler.sh run-due     (run all agents due now)
#   - Daemon:   scheduler.sh daemon      (continuous loop)
#   - List:     scheduler.sh list        (show all agents and schedules)
#   - Test:     scheduler.sh test <name> (run specific agent)

set -euo pipefail

AGENTS_DIR="$(cd "$(dirname "$0")/.." && pwd)"
source "$AGENTS_DIR/lib/common.sh"

REGISTRY="$AGENTS_DIR/agents.yaml"
STATE_DIR="$AGENTS_DIR/state"
LOCKFILE="$STATE_DIR/.scheduler.lock"

# Check if cron expression matches current time
# Simplified: only handles "M H * * *" and "M */N * * *" patterns
cron_matches_now() {
    local cron="$1"
    local now_min=$(date +%M | sed 's/^0//')
    local now_hour=$(date +%H | sed 's/^0//')

    local cron_min=$(echo "$cron" | awk '{print $1}')
    local cron_hour=$(echo "$cron" | awk '{print $2}')

    # Check minute
    if [[ "$cron_min" != "$now_min" && "$cron_min" != "*" ]]; then
        return 1
    fi

    # Check hour
    if [[ "$cron_hour" == "*" ]]; then
        return 0
    elif [[ "$cron_hour" == *//* ]]; then
        local interval="${cron_hour#*/}"
        (( now_hour % interval == 0 ))
    else
        [[ "$cron_hour" == "$now_hour" ]]
    fi
}

# Get list of enabled agents
list_enabled_agents() {
    yq -r '.agents | to_entries[] | select(.value.enabled == true) | .key' "$REGISTRY"
}

# Get agent schedule
get_schedule() {
    local agent="$1"
    yq -r ".agents.$agent.schedule" "$REGISTRY"
}

# Check if agent is due to run
is_due() {
    local agent="$1"
    local schedule=$(get_schedule "$agent")
    cron_matches_now "$schedule"
}

# Run a single agent
run_agent() {
    local agent="$1"
    log "Running agent: $agent"
    "$AGENTS_DIR/bin/run-agent.sh" "$agent" 2>&1 | while read -r line; do
        log "  [$agent] $line"
    done
}

# Commands
cmd_list() {
    echo "Registered Agents:"
    echo "=================="
    yq -r '.agents | to_entries[] | "\(.key): type=\(.value.type) enabled=\(.value.enabled) schedule=\"\(.value.schedule)\""' "$REGISTRY"
}

cmd_run_due() {
    log "Checking for due agents..."
    local ran=0

    for agent in $(list_enabled_agents); do
        if is_due "$agent"; then
            run_agent "$agent" &
            ((ran++))
        fi
    done

    wait
    log "Ran $ran agent(s)"
}

cmd_test() {
    local agent="${1:?Usage: scheduler.sh test <agent-name>}"
    run_agent "$agent"
}

cmd_daemon() {
    log "Starting scheduler daemon..."

    # Create lock
    if [[ -f "$LOCKFILE" ]]; then
        local old_pid=$(cat "$LOCKFILE")
        if kill -0 "$old_pid" 2>/dev/null; then
            die "Scheduler already running (PID $old_pid)"
        fi
    fi
    echo $$ > "$LOCKFILE"
    trap "rm -f $LOCKFILE" EXIT

    while true; do
        cmd_run_due
        # Sleep until next minute
        sleep $((60 - $(date +%S)))
    done
}

cmd_status() {
    if [[ -f "$LOCKFILE" ]]; then
        local pid=$(cat "$LOCKFILE")
        if kill -0 "$pid" 2>/dev/null; then
            echo "Scheduler running (PID $pid)"
        else
            echo "Scheduler not running (stale lockfile)"
        fi
    else
        echo "Scheduler not running"
    fi
}

# Main
case "${1:-help}" in
    list)      cmd_list ;;
    run-due)   cmd_run_due ;;
    test)      cmd_test "${2:-}" ;;
    daemon)    cmd_daemon ;;
    status)    cmd_status ;;
    help|*)
        cat <<EOF
Agent Scheduler

Usage: scheduler.sh <command>

Commands:
    list        List all registered agents and their schedules
    run-due     Run all agents that are due now (for cron)
    test NAME   Run a specific agent (ignores schedule)
    daemon      Run as continuous daemon
    status      Check if daemon is running

Cron setup:
    * * * * * /path/to/agents/bin/scheduler.sh run-due

EOF
        ;;
esac
