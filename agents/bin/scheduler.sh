#!/usr/bin/env bash
#
# scheduler.sh - Process Pool Agent Scheduler
#
# Manages a pool of processes that sample from available personas.
#
# Process count = ceil(sqrt(persona_count))
# First process runs at :00/:30 (every 30 min)
# Each additional process offset by 3 minutes
#
# Commands:
#   list        List all personas and their status
#   status      Show process pool configuration
#   run-due     Run processes due now (for cron)
#   test NAME   Run a specific persona (ignores schedule)
#   daemon      Run as continuous daemon

set -euo pipefail

AGENTS_DIR="$(cd "$(dirname "$0")/.." && pwd)"
source "$AGENTS_DIR/lib/common.sh"

PERSONAS_DIR="$AGENTS_DIR/personas"
DEFAULTS_FILE="$AGENTS_DIR/defaults.yaml"
STATE_DIR="$AGENTS_DIR/state"
LOCKFILE="$STATE_DIR/.scheduler.lock"

# Get list of enabled personas
list_enabled_personas() {
    for f in "$PERSONAS_DIR"/*.yaml; do
        [[ -f "$f" ]] || continue
        local name=$(basename "$f" .yaml)
        local enabled=$(yq '.enabled' "$f" 2>/dev/null || echo true)
        # Handle both boolean values and string representations
        if [[ "$enabled" != "false" ]]; then
            echo "$name"
        fi
    done
}

# Count enabled personas
count_personas() {
    list_enabled_personas | wc -l
}

# Calculate process count: ceil(sqrt(n))
# Uses integer approximation without bc
calc_process_count() {
    local n=$1
    local i=1
    while (( i * i < n )); do
        ((i++))
    done
    echo "$i"
}

# Cache for enabled personas to avoid repeated yq calls
ENABLED_PERSONAS_CACHE=""

# Get list of enabled personas (cached)
get_enabled_personas() {
    if [[ -z "$ENABLED_PERSONAS_CACHE" ]]; then
        ENABLED_PERSONAS_CACHE=$(list_enabled_personas_impl)
    fi
    echo "$ENABLED_PERSONAS_CACHE"
}

# Implementation of listing enabled personas
list_enabled_personas_impl() {
    for f in "$PERSONAS_DIR"/*.yaml; do
        [[ -f "$f" ]] || continue
        local name=$(basename "$f" .yaml)
        local enabled=$(yq '.enabled' "$f" 2>/dev/null || echo true)
        # Handle both boolean values and string representations
        if [[ "$enabled" != "false" ]]; then
            echo "$name"
        fi
    done
}

# Count enabled personas
count_personas() {
    get_enabled_personas | wc -l
}

# Get pool config
get_interval() {
    yq -r '.pool.interval_minutes // 30' "$DEFAULTS_FILE"
}

get_offset() {
    yq -r '.pool.offset_minutes // 3' "$DEFAULTS_FILE"
}

# Check if a process slot is due now
# Slot 0: runs at :00, :30 (every interval_minutes)
# Slot N: runs at :00+N*offset, :30+N*offset
is_slot_due() {
    local slot=$1
    local interval=$(get_interval)
    local offset=$(get_offset)
    local now_min=$(date +%M | sed 's/^0//')

    local slot_offset=$((slot * offset))

    # Check if current minute matches any interval point + slot offset
    local check_min=$((now_min - slot_offset))
    if [[ $check_min -lt 0 ]]; then
        check_min=$((check_min + 60))
    fi

    # Is check_min divisible by interval?
    (( check_min % interval == 0 ))
}

# Sample a random persona from enabled list
sample_persona() {
    local personas=($(get_enabled_personas))
    local count=${#personas[@]}

    if [[ $count -eq 0 ]]; then
        return 1
    fi

    # Use $RANDOM for sampling
    local idx=$((RANDOM % count))
    echo "${personas[$idx]}"
}

# Run a single persona with optional workflow override
run_persona() {
    local persona="$1"
    local workflow="${2:-}"
    if [[ -n "$workflow" ]]; then
        log "Running persona: $persona (workflow: $workflow)"
        "$AGENTS_DIR/bin/run-agent.sh" "$persona" "$workflow" 2>&1 | while read -r line; do
            log "  [$persona:$workflow] $line"
        done
    else
        log "Running persona: $persona"
        "$AGENTS_DIR/bin/run-agent.sh" "$persona" 2>&1 | while read -r line; do
            log "  [$persona] $line"
        done
    fi
}

# Check if monk duty is due (hourly at :17)
is_monk_due() {
    local now_min=$(date +%M | sed 's/^0//')
    [[ "$now_min" -eq 17 ]]
}

# Commands
cmd_list() {
    echo "Registered Personas:"
    echo "===================="
    for f in "$PERSONAS_DIR"/*.yaml; do
        [[ -f "$f" ]] || continue
        local name=$(basename "$f" .yaml)
        local enabled=$(yq '.enabled' "$f" 2>/dev/null || echo true)
        local workflow=$(yq -r '.workflow // "forum-poster"' "$f")
        local prob=$(yq -r '.post_probability // 0.5' "$f")
        echo "$name: workflow=$workflow enabled=$enabled post_prob=$prob"
    done
}

cmd_status() {
    local count=$(count_personas)
    local procs=$(calc_process_count "$count")
    local interval=$(get_interval)
    local offset=$(get_offset)

    echo "Process Pool Status:"
    echo "===================="
    echo "Enabled personas: $count"
    echo ""
    echo "Forum Posters:"
    echo "  Process count: $procs (ceil(sqrt($count)))"
    echo "  Base interval: every ${interval} minutes"
    echo "  Process offset: ${offset} minutes"
    echo "  Schedule:"
    for ((i=0; i<procs; i++)); do
        local slot_offset=$((i * offset))
        echo "    Process $i: :$(printf '%02d' $slot_offset), :$(printf '%02d' $((slot_offset + interval))), ..."
    done
    echo ""
    echo "Young Monk:"
    echo "  Schedule: hourly at :17"
    echo "  Samples one persona for tidying duties"
}

cmd_run_due() {
    # Refresh cache once per run
    ENABLED_PERSONAS_CACHE=$(list_enabled_personas_impl)
    
    local count=$(count_personas)
    local procs=$(calc_process_count "$count")
    local ran=0

    # Check forum poster slots
    for ((slot=0; slot<procs; slot++)); do
        if is_slot_due "$slot"; then
            local persona=$(sample_persona)
            if [[ -n "$persona" ]]; then
                log "Forum slot $slot due, sampled: $persona"
                run_persona "$persona" &
                ((ran++))
            fi
        fi
    done

    # Check monk duty (hourly at :17)
    if is_monk_due; then
        local monk=$(sample_persona)
        if [[ -n "$monk" ]]; then
            log "Monk duty due, summoned: $monk"
            run_persona "$monk" "monk" &
            ((ran++))
        fi
    fi

    wait
    if [[ $ran -gt 0 ]]; then
        log "Ran $ran process(es)"
    fi
}

cmd_test() {
    local persona="${1:?Usage: scheduler.sh test <persona-name> [workflow]}"
    local workflow="${2:-}"

    if [[ ! -f "$PERSONAS_DIR/$persona.yaml" ]]; then
        die "Persona not found: $persona"
    fi

    run_persona "$persona" "$workflow"
}

cmd_daemon() {
    log "Starting scheduler daemon..."

    mkdir -p "$STATE_DIR"
    
    # Use flock for robust locking if available
    if command -v flock >/dev/null; then
        exec 200>"$LOCKFILE"
        if ! flock -n 200; then
            die "Scheduler already running (locked)"
        fi
        echo $$ >&200
        trap "rm -f $LOCKFILE" EXIT
        
        while true; do
            cmd_run_due
            # Sleep until next minute
            sleep $((60 - $(date +%S)))
        done
    else
        # Fallback to PID file method
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
    fi
}

cmd_daemon_status() {
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
    list)       cmd_list ;;
    status)     cmd_status ;;
    run-due)    cmd_run_due ;;
    test)       cmd_test "${2:-}" "${3:-}" ;;
    daemon)     cmd_daemon ;;
    daemon-status) cmd_daemon_status ;;
    help|*)
        cat <<EOF
Process Pool Agent Scheduler

Usage: scheduler.sh <command>

Commands:
    list               List all registered personas
    status             Show process pool configuration
    run-due            Run processes due now (for cron)
    test NAME [WORK]   Run a specific persona, optionally with workflow override
    daemon             Run as continuous daemon
    daemon-status      Check if daemon is running

Examples:
    scheduler.sh test bluegown           # Run bluegown's default workflow
    scheduler.sh test bluegown monk      # Run bluegown as Young Monk

Scheduling Model:
    Forum Posters:
      - process_count = ceil(sqrt(persona_count))
      - Each process samples a random persona when due
      - First process: every 30 min at :00, :30
      - Each additional process: offset by 3 minutes

    Young Monk:
      - Hourly at :17
      - Samples one persona for tidying duties

Cron setup:
    * * * * * /path/to/agents/bin/scheduler.sh run-due

EOF
        ;;
esac
