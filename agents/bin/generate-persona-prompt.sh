#!/usr/bin/env bash
#
# generate-persona-prompt.sh - Generate persona prompt from DSL or fall back to YAML
#
# Usage: generate-persona-prompt.sh <persona-name> [--debug]
#
# Attempts to load and evaluate a persona DSL file (agents/personas/<name>-dsl.ss).
# If not found or evaluation fails, falls back to static prompt from YAML.
# Returns the generated prompt string to stdout.
#
# Options:
#   --debug   Print detailed error messages to stderr instead of falling back silently

set -euo pipefail

AGENTS_DIR="$(cd "$(dirname "$0")/.." && pwd)"
source "$AGENTS_DIR/lib/common.sh"

PERSONA_NAME="${1:?Usage: generate-persona-prompt.sh <persona-name> [--debug]}"
DEBUG="${2:-}"
PERSONA_DIR="$AGENTS_DIR/personas"
DSL_FILE="$PERSONA_DIR/${PERSONA_NAME}-dsl.ss"
YAML_FILE="$PERSONA_DIR/${PERSONA_NAME}.yaml"

# Try DSL first
if [[ -f "$DSL_FILE" ]]; then
    # Use Fold daemon to evaluate DSL
    SESSION_ID="gen-prompt-${PERSONA_NAME}-$$"

    # Load the DSL helper module, fragments, and the persona DSL
    # The persona DSL file defines persona-prompt variable
    EXPR="(begin
      (load \"agents/lib/persona-prompt-gen.ss\")
      (load \"agents/personas/${PERSONA_NAME}-dsl.ss\")
      persona-prompt)"

    # Write request to daemon (use fold.sh for better handling)
    RESULT=$("$AGENTS_DIR"/../fold.sh "$EXPR" 2>&1 || echo "")

    if [[ -n "$RESULT" ]] && [[ ! "$RESULT" =~ ^Error ]]; then
        # Success - return the generated prompt
        echo "$RESULT"
        exit 0
    fi

    # If DSL evaluation fails, log error if debug mode
    if [[ "$DEBUG" == "--debug" ]]; then
        log "ERROR: DSL evaluation failed for $PERSONA_NAME"
        log "DSL file: $DSL_FILE"
        log "Scheme output:"
        echo "$RESULT" | sed 's/^/  /' >&2
        log "Falling back to YAML prompt"
    else
        log "Warning: DSL evaluation failed for $PERSONA_NAME, falling back to YAML" >&2
    fi
fi

# Fall back to YAML static prompt
if [[ -f "$YAML_FILE" ]]; then
    PROMPT=$(yq -r '.system // ""' "$YAML_FILE")
    if [[ -n "$PROMPT" ]]; then
        if [[ "$DEBUG" == "--debug" ]]; then
            log "Using YAML prompt from $YAML_FILE" >&2
        fi
        echo "$PROMPT"
        exit 0
    fi
fi

# If we get here, no prompt found
die "No prompt found for persona: $PERSONA_NAME (checked DSL: $DSL_FILE, YAML: $YAML_FILE)"
