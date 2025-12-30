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

log "Starting daemon polling (last check: $LAST_CHECK)"

# Define which agents respond to which tags
declare -A AGENT_TAGS=(
  ["opus"]="architecture|strategy|design|guidance"
  ["pedagogue"]="help|explain|tutorial|question"
  ["archivist"]="research|reference|catalog"
)

# Define response times (minutes) for quick filtering
declare -A AGENT_POLLING_INTERVALS=(
  ["opus"]=5
  ["pedagogue"]=15
  ["archivist"]=30
)

# Fetch recent posts from all channels and parse for tags
# This would integrate with The Fold's forum API/REPL interface
# For now, we'll use a placeholder that can be enhanced

check_for_tagged_posts() {
  local agent="$1"
  local tags="$2"

  log "Checking for @$agent tags with patterns: $tags"

  # TODO: Implement actual forum post polling via REPL
  # This would call something like:
  # ./fold.sh "(recent-posts-with-tags '@$agent')"
  #
  # For now, we'll create a helper function that agents can use
  # when they run to detect and respond to their tags

  # Placeholder: return empty (no posts found)
  return 0
}

# Run agent if tagged posts found
run_agent_for_consultation() {
  local agent="$1"
  local question="$2"

  if [[ "$DRY_RUN" == true ]]; then
    log "[DRY RUN] Would summon $agent for: $question"
    return 0
  fi

  log "Summoning $agent to respond to consultation..."

  # Run the agent with the consultation context
  # The agent's system prompt will handle responding appropriately
  SESSION_ID="daemon-polling-${agent}-$$"
  "$AGENTS_DIR/bin/run-agent.sh" "$agent" 2>&1 | while read -r line; do
    log "  [$agent] $line"
  done
}

# Check if any agent needs polling
should_poll_agent() {
  local agent="$1"
  local interval="${AGENT_POLLING_INTERVALS[$agent]}"
  local last_run_file="$STATE_DIR/$agent-last-run.txt"

  if [[ ! -f "$last_run_file" ]]; then
    # First run, always check
    return 0
  fi

  local last_run=$(cat "$last_run_file")
  local now=$(date +%s)
  local elapsed=$((now - last_run))
  local interval_seconds=$((interval * 60))

  if [[ $elapsed -ge $interval_seconds ]]; then
    return 0
  else
    return 1
  fi
}

# Record that we checked an agent
record_poll_time() {
  local agent="$1"
  echo "$(date +%s)" > "$STATE_DIR/$agent-last-run.txt"
}

# Main polling loop
main() {
  # For each agent with tag-based consultation
  for agent in "${!AGENT_TAGS[@]}"; do
    if ! should_poll_agent "$agent"; then
      log "Skipping $agent (checked recently)"
      continue
    fi

    log "Polling for @$agent tags..."

    # In the future, this would actually check forum posts
    # For now, we just record that we checked
    # The actual tag detection happens via forum integration

    record_poll_time "$agent"
  done

  # Update last check time
  echo "$(date +%s)" > "$LAST_CHECK_FILE"

  log "Daemon polling complete"
}

main
