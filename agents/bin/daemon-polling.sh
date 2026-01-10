#!/usr/bin/env bash
#
# daemon-polling.sh - Poll for tagged agent consultation requests
#
# Monitors forum posts for tags like:
#   @opus architecture should we refactor X?
#   @pedagogue help explain Y
#   @archivist research prior work on Z
#
# Summons agents to respond to tagged questions
#
# Usage: daemon-polling.sh [--dry-run]

set -euo pipefail

AGENTS_DIR="$(cd "$(dirname "$0")/.." && pwd)"
FOLD_DIR="$(cd "$AGENTS_DIR/.." && pwd)"
source "$AGENTS_DIR/lib/common.sh"

STATE_DIR="$AGENTS_DIR/state/daemon-polling"
mkdir -p "$STATE_DIR"

# Last checked timestamp (to avoid re-processing old posts)
LAST_CHECK_FILE="$STATE_DIR/last-check.txt"
LAST_CHECK=$(cat "$LAST_CHECK_FILE" 2>/dev/null || echo "0")

# Dry run mode (for testing)
DRY_RUN="${1:---}"
if [[ "$DRY_RUN" == "--dry-run" ]]; then
  DRY_RUN=true
else
  DRY_RUN=false
fi

log "Starting unified daemon polling (last check: $LAST_CHECK)"

# Agent tag capabilities - maps agent name to tags it handles
declare -A AGENT_CAPABILITIES=(
  ["opus"]="architecture strategy design guidance"
  ["pedagogue"]="help explain tutorial question"
  ["archivist"]="research reference catalog"
)

# Build reverse index: tag -> agent(s) that handle it
build_tag_registry() {
  declare -gA TAG_HANDLERS
  for agent in "${!AGENT_CAPABILITIES[@]}"; do
    for tag in ${AGENT_CAPABILITIES[$agent]}; do
      TAG_HANDLERS[$tag]="${TAG_HANDLERS[$tag]:-} $agent"
    done
  done
}

# Get all agents registered to handle a tag
get_agents_for_tag() {
  local tag="$1"
  echo "${TAG_HANDLERS[$tag]:-}"
}

# Run agent if called
run_agent_consultation() {
  local agent="$1"

  if [[ "$DRY_RUN" == true ]]; then
    log "[DRY RUN] Would summon $agent for consultation"
    return 0
  fi

  log "Summoning $agent to respond to tagged consultation..."

  # Run the agent with the consultation context
  # The agent's system prompt will handle responding appropriately
  "$AGENTS_DIR/bin/run-agent.sh" "$agent" 2>&1 | while read -r line; do
    log "  [$agent] $line"
  done
}

# Single unified polling action: find all tagged posts and dispatch to agents
main() {
  # Build tag->agent mapping
  build_tag_registry

  log "Polling for agent consultation tags..."

  # Get all posts with agent tags since last check
  # Use helper script for clean output
  local poll_output
  poll_output=$(scheme --script "$AGENTS_DIR/lib/poll-helper.ss" "$LAST_CHECK" 2>/dev/null || echo "")

  if [[ -z "$poll_output" ]]; then
    log "No new tagged consultation posts found"
  else
    # Process unique agent requests
    local agents_to_run
    agents_to_run=$(echo "$poll_output" | grep "^AGENT_REQUEST:" | sed 's/^AGENT_REQUEST: //' | sort -u)

    if [[ -z "$agents_to_run" ]]; then
       log "No agent requests found in output"
    else
       log "Found agent requests, processing..."
       
       while IFS= read -r agent; do
         if [[ -n "$agent" ]]; then
           log "Processing tag for agent: $agent"
           run_agent_consultation "$agent"
         fi
       done <<< "$agents_to_run"
    fi
  fi

  # Update polling state (single timestamp for all agents)
  echo "$(date +%s)" > "$LAST_CHECK_FILE"

  log "Daemon polling complete"
}

main
