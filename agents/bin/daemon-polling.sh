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
  # First load the forum modules, then query
  local fold_output=$(
    "$FOLD_DIR/fold.sh" "
(begin
  (load \"forum/polling-queries.ss\")
  (find-posts-with-agent-tags $LAST_CHECK))" 2>/dev/null || echo "()"
  )

  # Extract just the Scheme result (last line after "=> ")
  local tagged_posts=$(echo "$fold_output" | grep "^=>" | sed 's/^=> //' | tail -1)

  # Default to empty if no result found
  tagged_posts="${tagged_posts:-()}"

  if [[ "$tagged_posts" == "()" || -z "$tagged_posts" ]]; then
    log "No new tagged consultation posts found"
  else
    log "Found tagged posts, processing..."

    # Parse through the posts and call appropriate agents
    # For each post, extract tags and determine which agents to call
    while IFS= read -r line; do
      # Extract agent names from tags in each post
      # This is handled by the extract-agent-tags function in tag-parser
      if [[ "$line" =~ agent[[:space:]]+.[[:space:]]*([a-z_-]+) ]]; then
        local agent="${BASH_REMATCH[1]}"
        if [[ -n "$agent" ]]; then
          log "Processing tag for agent: $agent"
          run_agent_consultation "$agent"
        fi
      fi
    done <<< "$(echo "$tagged_posts" | grep -o '@[a-z_-]*' | sort -u)"
  fi

  # Update polling state (single timestamp for all agents)
  echo "$(date +%s)" > "$LAST_CHECK_FILE"

  log "Daemon polling complete"
}

main
