#!/usr/bin/env bash
#
# run-agent.sh - Execute an agent workflow
#
# Usage: run-agent.sh <agent-name>
#
# Reads agent configuration from agents.yaml, instantiates the appropriate
# workflow template, and executes it.

set -euo pipefail

AGENTS_DIR="$(cd "$(dirname "$0")/.." && pwd)"
source "$AGENTS_DIR/lib/common.sh"

AGENT_NAME="${1:?Usage: run-agent.sh <agent-name>}"
REGISTRY="$AGENTS_DIR/agents.yaml"

# Validate agent exists and is enabled
if ! yq -e ".agents.$AGENT_NAME" "$REGISTRY" >/dev/null 2>&1; then
    die "Agent not found: $AGENT_NAME"
fi

ENABLED=$(yq -r ".agents.$AGENT_NAME.enabled" "$REGISTRY")
if [[ "$ENABLED" != "true" ]]; then
    log "Agent $AGENT_NAME is disabled, skipping"
    exit 0
fi

# Load agent configuration
AGENT_TYPE=$(yq -r ".agents.$AGENT_NAME.type" "$REGISTRY")
MODEL=$(yq -r ".agents.$AGENT_NAME.model" "$REGISTRY")
TIER=$(yq -r ".agents.$AGENT_NAME.tier" "$REGISTRY")

# Load workflow template
WORKFLOW_FILE="$AGENTS_DIR/workflows/${AGENT_TYPE}.yaml"
if [[ ! -f "$WORKFLOW_FILE" ]]; then
    die "Workflow template not found: $WORKFLOW_FILE"
fi

# Set up session and state
SESSION_ID="agent-${AGENT_NAME}-$$"
STATE_DIR="$AGENTS_DIR/state/$AGENT_NAME"
mkdir -p "$STATE_DIR"

log "Starting agent: $AGENT_NAME (type: $AGENT_TYPE, session: $SESSION_ID)"

# Build variable context from agent config
declare -A VARS
VARS[agent_name]="$AGENT_NAME"
VARS[tier]="$TIER"
VARS[model]="$MODEL"

# Extract nested config values (use empty array default before join to avoid null iteration)
VARS[persona.voice]=$(yq -r ".agents.$AGENT_NAME.persona.voice // \"\"" "$REGISTRY")
VARS[persona.avoids]=$(yq -r "(.agents.$AGENT_NAME.persona.avoids // []) | join(\", \")" "$REGISTRY")
VARS[persona.interests]=$(yq -r "(.agents.$AGENT_NAME.persona.interests // []) | join(\", \")" "$REGISTRY")
VARS[channels.read]=$(yq -r "(.agents.$AGENT_NAME.channels.read // []) | join(\", \")" "$REGISTRY")
VARS[channels.write]=$(yq -r "(.agents.$AGENT_NAME.channels.write // []) | join(\", \")" "$REGISTRY")
VARS[behavior.post_probability]=$(yq -r ".agents.$AGENT_NAME.behavior.post_probability // 0.5" "$REGISTRY")
VARS[behavior.min_digest_posts]=$(yq -r ".agents.$AGENT_NAME.behavior.min_digest_posts // 2" "$REGISTRY")
VARS[tasks]=$(yq -r "(.agents.$AGENT_NAME.tasks // []) | join(\", \")" "$REGISTRY")
VARS[test_suites]=$(yq -r "(.agents.$AGENT_NAME.test_suites // []) | join(\", \")" "$REGISTRY")
VARS[report_channel]=$(yq -r ".agents.$AGENT_NAME.report_channel // \"engineering\"" "$REGISTRY")

# Build persona description
VARS[persona_description]="Your voice is ${VARS[persona.voice]}. You avoid: ${VARS[persona.avoids]}. You're drawn to: ${VARS[persona.interests]}."

# Step outputs accumulator
declare -A STEP_OUTPUTS

# Template substitution function
substitute_vars() {
    local text="$1"

    # Substitute static variables
    for key in "${!VARS[@]}"; do
        text="${text//\$\{$key\}/${VARS[$key]}}"
    done

    # Substitute step outputs
    for key in "${!STEP_OUTPUTS[@]}"; do
        local output="${STEP_OUTPUTS[$key]}"
        text="${text//\$\{steps.$key.output\}/$output}"

        # Handle JSON field access
        if echo "$output" | jq -e . >/dev/null 2>&1; then
            for field in action channel title body reasoning passed issues suggestions report_worthy tasks_to_run; do
                local value=$(echo "$output" | jq -r ".$field // empty" 2>/dev/null || echo "")
                text="${text//\$\{steps.$key.output.$field\}/$value}"
            done
        fi
    done

    echo "$text"
}

# Evaluate condition
eval_condition() {
    local cond="$1"
    cond=$(substitute_vars "$cond")

    # Handle simple equality: X == Y
    if [[ "$cond" == *" == "* ]]; then
        local lhs=$(echo "$cond" | sed 's/ *==.*//')
        local rhs=$(echo "$cond" | sed 's/.*== *//')
        [[ "$lhs" == "$rhs" ]]
    # Handle "in" check: X in Y
    elif [[ "$cond" == *" in "* ]]; then
        local needle=$(echo "$cond" | sed 's/ *in .*//')
        local haystack=$(echo "$cond" | sed 's/.* in //')
        [[ "$haystack" == *"$needle"* ]]
    else
        # Default: treat as truthy
        [[ -n "$cond" && "$cond" != "false" && "$cond" != "null" ]]
    fi
}

# Execute workflow steps
STEP_COUNT=$(yq -r '.steps | length' "$WORKFLOW_FILE")

for ((i=0; i<STEP_COUNT; i++)); do
    STEP_NAME=$(yq -r ".steps[$i].name" "$WORKFLOW_FILE")
    STEP_TYPE=$(yq -r ".steps[$i].type" "$WORKFLOW_FILE")
    STEP_WHEN=$(yq -r ".steps[$i].when // \"\"" "$WORKFLOW_FILE")
    STEP_ALWAYS=$(yq -r ".steps[$i].always // false" "$WORKFLOW_FILE")

    log "Step $((i+1))/$STEP_COUNT: $STEP_NAME ($STEP_TYPE)"

    # Check when condition (unless always=true)
    if [[ -n "$STEP_WHEN" && "$STEP_ALWAYS" != "true" ]]; then
        if ! eval_condition "$STEP_WHEN"; then
            log "  -> skipped (condition: $STEP_WHEN)"
            continue
        fi
    fi

    case "$STEP_TYPE" in
        fold)
            EXPR=$(yq -r ".steps[$i].expr" "$WORKFLOW_FILE")
            EXPR=$(substitute_vars "$EXPR")

            OUTPUT=$("$AGENTS_DIR/bin/fold-ipc.sh" "$SESSION_ID" "$EXPR" 2>&1) || {
                log "  -> ERROR: $OUTPUT"
                [[ "$STEP_ALWAYS" == "true" ]] || exit 1
            }
            STEP_OUTPUTS[$STEP_NAME]="$OUTPUT"
            log "  -> ${OUTPUT:0:80}..."
            ;;

        llm)
            SYSTEM=$(yq -r ".steps[$i].system // \"\"" "$WORKFLOW_FILE")
            PROMPT=$(yq -r ".steps[$i].prompt" "$WORKFLOW_FILE")

            SYSTEM=$(substitute_vars "$SYSTEM")
            PROMPT=$(substitute_vars "$PROMPT")

            # Build full message
            if [[ -n "$SYSTEM" ]]; then
                FULL_PROMPT="<system>
$SYSTEM
</system>

$PROMPT"
            else
                FULL_PROMPT="$PROMPT"
            fi

            # Call LLM
            OUTPUT=$(opencode run -m "$MODEL" --format json "$FULL_PROMPT" 2>/dev/null \
                | jq -rs '[.[] | select(.type == "text")] | last | .part.text // empty')

            # Extract JSON from markdown code blocks if present
            if [[ "$OUTPUT" == *'```json'* ]]; then
                OUTPUT=$(echo "$OUTPUT" | sed -n '/```json/,/```/p' | sed '1d;$d')
            elif [[ "$OUTPUT" == *'```'* ]]; then
                OUTPUT=$(echo "$OUTPUT" | sed -n '/```/,/```/p' | sed '1d;$d')
            fi

            STEP_OUTPUTS[$STEP_NAME]="$OUTPUT"
            echo "$OUTPUT" > "$STATE_DIR/step-${STEP_NAME}.json"
            log "  -> saved to step-${STEP_NAME}.json"
            ;;

        *)
            log "  -> unknown step type: $STEP_TYPE"
            ;;
    esac
done

log "Agent complete: $AGENT_NAME"
