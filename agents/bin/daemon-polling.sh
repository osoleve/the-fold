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
check_for_tagged_posts() {
  local agent="$1"
  local tags="$2"

  log "Checking for @$agent tags with patterns: $tags"

  # Get last polling timestamp for this agent
  local last_check=$(
    "$FOLD_DIR/fold.sh" "(get-polling-state \"$agent\")" 2>/dev/null || echo "0"
  )

  log "  Last check: $last_check, searching for newer posts..."

  # Find posts with agent tags since last check
  local tagged_posts=$(
    "$FOLD_DIR/fold.sh" "(find-posts-with-agent-tags $last_check)" 2>/dev/null || echo "()"
  )

  # If we got posts with tags, process them
  if [[ "$tagged_posts" != "()" && -n "$tagged_posts" ]]; then
    log "  Found tagged posts: $tagged_posts"
    # This will be processed by run_agent_for_consultation
    echo "$tagged_posts"
  else
    log "  No new tagged posts found"
    return 0
  fi
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

    # Check for posts with agent tags
    local tags="${AGENT_TAGS[$agent]}"
    local tagged_posts=$(check_for_tagged_posts "$agent" "$tags")

    # If we found tagged posts, invoke the agent to respond
    if [[ -n "$tagged_posts" && "$tagged_posts" != "()" ]]; then
      log "Found tagged posts for $agent, invoking agent..."
      run_agent_for_consultation "$agent" "$tagged_posts"
    fi

    # Record that we checked this agent
    record_poll_time "$agent"
  done

  # Update last check time
  echo "$(date +%s)" > "$LAST_CHECK_FILE"

  log "Daemon polling complete"
}

main
