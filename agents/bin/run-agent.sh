#!/usr/bin/env bash
set -euo pipefail

AGENTS_DIR="$(cd "$(dirname "$0")/.." && pwd)"
source "$AGENTS_DIR/lib/common.sh"

# Source user's bash environment for opencode helpers (ocpj, etc.)
[[ -f ~/.bashrc ]] && source ~/.bashrc 2>/dev/null || true

WORKFLOW="${1:?usage: run-agent.sh <workflow-name>}"
WORKFLOW_FILE="$AGENTS_DIR/config/workflows/${WORKFLOW}.yaml"

if [[ ! -f "$WORKFLOW_FILE" ]]; then
    log "ERROR: Workflow not found: $WORKFLOW_FILE"
    exit 1
fi

# Parse workflow (simplified - in practice you'd use yq properly)
PERSONA=$(yq -r '.persona' "$WORKFLOW_FILE")
PERSONA_FILE="$AGENTS_DIR/config/personas/${PERSONA}.yaml"

MODEL=$(yq -r '.model' "$PERSONA_FILE")
TIER=$(yq -r '.tier' "$PERSONA_FILE")
LOGIN_NAME=$(yq -r '.session.login_name' "$PERSONA_FILE")

SESSION_ID=$(session_id "$PERSONA")
STATE_DIR="$AGENTS_DIR/state/$PERSONA"
mkdir -p "$STATE_DIR"

log "Starting workflow: $WORKFLOW (persona: $PERSONA, session: $SESSION_ID)"

# Process steps
STEP_COUNT=$(yq -r '.steps | length' "$WORKFLOW_FILE")
declare -A STEP_OUTPUTS

for ((i=0; i<STEP_COUNT; i++)); do
    STEP_NAME=$(yq -r ".steps[$i].name" "$WORKFLOW_FILE")
    STEP_TYPE=$(yq -r ".steps[$i].type" "$WORKFLOW_FILE")
    STEP_WHEN=$(yq -r ".steps[$i].when // \"true\"" "$WORKFLOW_FILE")
    STEP_ALWAYS=$(yq -r ".steps[$i].always // false" "$WORKFLOW_FILE")

    log "Step $((i+1))/$STEP_COUNT: $STEP_NAME ($STEP_TYPE)"

    # Evaluate when condition with template substitution
    EVAL_WHEN="$STEP_WHEN"
    for key in "${!STEP_OUTPUTS[@]}"; do
        # Parse JSON from step outputs for condition checking
        if echo "${STEP_OUTPUTS[$key]}" | jq -e . >/dev/null 2>&1; then
            # It's JSON - extract fields for substitution
            ACTION=$(echo "${STEP_OUTPUTS[$key]}" | jq -r '.action // empty')
            EVAL_WHEN="${EVAL_WHEN//\{\{ steps.$key.output.action \}\}/$ACTION}"
        fi
        EVAL_WHEN="${EVAL_WHEN//\{\{ steps.$key.output \}\}/${STEP_OUTPUTS[$key]}}"
    done

    # Check when condition
    if [[ "$EVAL_WHEN" == *"=="* ]]; then
        # Simple equality check: {{ steps.X.output.action == 'post' }}
        LHS=$(echo "$EVAL_WHEN" | sed "s/\s*==.*//")
        RHS=$(echo "$EVAL_WHEN" | sed "s/.*==\s*//" | tr -d "'" | tr -d '"')
        if [[ "$LHS" != "$RHS" ]]; then
            log "  -> skipped (condition not met)"
            continue
        fi
    elif [[ "$EVAL_WHEN" != "true" ]]; then
        log "  -> skipped (unsupported condition: $EVAL_WHEN)"
        continue
    fi

    case "$STEP_TYPE" in
        fold)
            EXPR=$(yq -r ".steps[$i].expr" "$WORKFLOW_FILE")
            # TODO: template substitution for {{ steps.X.output }}
            OUTPUT=$("$AGENTS_DIR/bin/fold-ipc.sh" "$SESSION_ID" "$EXPR")
            STEP_OUTPUTS[$STEP_NAME]="$OUTPUT"
            log "  -> ${OUTPUT:0:100}..."
            ;;
        llm)
            TEMPLATE=$(yq -r ".steps[$i].template" "$WORKFLOW_FILE")
            TEMPLATE_FILE="$AGENTS_DIR/$TEMPLATE"
            # Build prompt with substitutions
            PROMPT=$(cat "$TEMPLATE_FILE")
            # Substitute {{ steps.get-digest.output }} etc
            for key in "${!STEP_OUTPUTS[@]}"; do
                PROMPT="${PROMPT//\{\{ steps.$key.output \}\}/${STEP_OUTPUTS[$key]}}"
            done

            SYSTEM_PROMPT_FILE="$AGENTS_DIR/prompts/$PERSONA/system.md"

            # Build full prompt with system instructions
            if [[ -f "$SYSTEM_PROMPT_FILE" ]]; then
                FULL_PROMPT="<system>
$(cat "$SYSTEM_PROMPT_FILE")
</system>

$PROMPT"
            else
                FULL_PROMPT="$PROMPT"
            fi

            # Call the model using opencode run -m <model>
            OUTPUT=$(opencode run -m "$MODEL" --format json "$FULL_PROMPT" 2>/dev/null | jq -rs '[.[] | select(.type == "text")] | last | .part.text // empty')
            STEP_OUTPUTS[$STEP_NAME]="$OUTPUT"

            # Save for debugging
            echo "$OUTPUT" > "$STATE_DIR/last-llm-output.json"
            log "  -> saved to last-llm-output.json"
            ;;
        *)
            log "  -> unknown step type: $STEP_TYPE"
            ;;
    esac
done

log "Workflow complete: $WORKFLOW"
