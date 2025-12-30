#!/usr/bin/env bash
#
# generate-persona-prompt.sh - Generate persona prompt from DSL or fall back to YAML
#
# Usage: generate-persona-prompt.sh <persona-name>
#
# Attempts to load and evaluate a persona DSL file (agents/personas/<name>-dsl.ss).
# If not found, falls back to static prompt from agents/personas/<name>.yaml.
# Returns the generated prompt string to stdout.

set -euo pipefail

AGENTS_DIR="$(cd "$(dirname "$0")/.." && pwd)"
source "$AGENTS_DIR/lib/common.sh"

PERSONA_NAME="${1:?Usage: generate-persona-prompt.sh <persona-name>}"
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

    # If DSL evaluation fails, fall through to YAML fallback
    log "Warning: DSL evaluation failed for $PERSONA_NAME, falling back to YAML"
fi

# Fall back to YAML static prompt
if [[ -f "$YAML_FILE" ]]; then
    PROMPT=$(yq -r '.system // ""' "$YAML_FILE")
    if [[ -n "$PROMPT" ]]; then
        echo "$PROMPT"
        exit 0
    fi
fi

# If we get here, no prompt found
die "No prompt found for persona: $PERSONA_NAME (checked DSL and YAML)"
