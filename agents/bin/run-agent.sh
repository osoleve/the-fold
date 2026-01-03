#!/usr/bin/env bash
export PATH=/usr/local/bin:/usr/bin:/bin:/home/oso/.local/bin
#
# run-agent.sh - Execute a persona's workflow
#
# Usage: run-agent.sh <persona-name> [workflow-override]
#
# Reads persona configuration from personas/<name>.yaml,
# merges with defaults.yaml, and executes the workflow.
# Optional workflow-override runs a different workflow than persona's default.

set -euo pipefail

AGENTS_DIR="$(cd "$(dirname "$0")/.." && pwd)"
source "$AGENTS_DIR/lib/common.sh"

PERSONA_NAME="${1:?Usage: run-agent.sh <persona-name> [workflow]}"
WORKFLOW_OVERRIDE="${2:-}"
PERSONA_FILE="$AGENTS_DIR/personas/$PERSONA_NAME.yaml"
DEFAULTS_FILE="$AGENTS_DIR/defaults.yaml"

# Validate persona exists
if [[ ! -f "$PERSONA_FILE" ]]; then
    die "Persona not found: $PERSONA_FILE"
fi

# Check if enabled
ENABLED=$(yq -r '.enabled // true' "$PERSONA_FILE")
if [[ "$ENABLED" != "true" ]]; then
    log "Persona $PERSONA_NAME is disabled, skipping"
    exit 0
fi

# Load configuration (persona overrides defaults)
get_config() {
    local key="$1"
    local default="$2"
    local persona_val=$(yq -r "$key // null" "$PERSONA_FILE")
    if [[ "$persona_val" != "null" ]]; then
        echo "$persona_val"
    else
        yq -r "$key // \"$default\"" "$DEFAULTS_FILE"
    fi
}

MODEL=$(get_config '.model' 'opencode/big-pickle')
TIER=$(get_config '.tier' 'haiku')

# Use workflow override if provided, otherwise use persona's default
if [[ -n "$WORKFLOW_OVERRIDE" ]]; then
    WORKFLOW_TYPE="$WORKFLOW_OVERRIDE"
else
    WORKFLOW_TYPE=$(get_config '.workflow' 'forum-poster')
fi

# Load workflow template
WORKFLOW_FILE="$AGENTS_DIR/workflows/${WORKFLOW_TYPE}.yaml"
if [[ ! -f "$WORKFLOW_FILE" ]]; then
    die "Workflow template not found: $WORKFLOW_FILE"
fi

# Set up session and state
SESSION_ID="agent-${PERSONA_NAME}-$$"
STATE_DIR="$AGENTS_DIR/state/$PERSONA_NAME"
mkdir -p "$STATE_DIR"

log "Starting persona: $PERSONA_NAME (workflow: $WORKFLOW_TYPE, session: $SESSION_ID)"

# Build variable context
declare -A VARS
VARS[persona_name]="$PERSONA_NAME"
VARS[tier]="$TIER"
VARS[model]="$MODEL"

# Extract persona-specific config
# Try DSL generation first, fall back to YAML if DSL not available
VARS[system_prompt]=$("$AGENTS_DIR/bin/generate-persona-prompt.sh" "$PERSONA_NAME" || yq -r '.system // ""' "$PERSONA_FILE")
VARS[channels.read]=$(yq -r '(.channels.read // []) | join(", ")' "$PERSONA_FILE")
VARS[channels.write]=$(yq -r '(.channels.write // []) | join(", ")' "$PERSONA_FILE")
# Scheme list format for digest filtering: '(poetry philosophy design)
VARS[channels.write.scheme]=$(yq -r '(.channels.write // []) | map("'"'"'" + .) | join(" ") | "(list " + . + ")"' "$PERSONA_FILE")
VARS[post_probability]=$(yq -r '.post_probability // 0.5' "$PERSONA_FILE")

# Probabilistic skipping: skip if random() > post_probability
# This prevents wasting tokens reading when we won't post anyway
POST_PROB="${VARS[post_probability]}"
RANDOM_VAL=$((RANDOM % 1000))  # 0-999 for better precision
THRESHOLD_VAL=$(echo "$POST_PROB" | sed 's/\.//')  # Convert 0.6 -> 6, then scale to 600
THRESHOLD_VAL=$((THRESHOLD_VAL * 100))  # Scale to 0-1000 range
if (( RANDOM_VAL >= THRESHOLD_VAL )); then
    log "Skipped (probability check: $RANDOM_VAL >= $THRESHOLD_VAL)"
    exit 0
fi

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
                # Escape for Scheme string: \ -> \\, " -> \"
                value="${value//\\/\\\\}"
                value="${value//\"/\\\"}"
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

            # Prepend persona's system prompt if available
            if [[ -n "${VARS[system_prompt]}" ]]; then
                SYSTEM="${VARS[system_prompt]}

$SYSTEM"
            fi

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
            if [[ "$MODEL" == gemini* || "$MODEL" == "default" ]]; then
                # Use gemini CLI
                gemini_args=("-o" "json" "--approval-mode" "yolo")
                if [[ "$MODEL" != "default" ]]; then
                    gemini_args+=("-m" "$MODEL")
                fi
                
                # gemini CLI output is a JSON object with 'response' field containing the text
                # Note: we use --approval-mode yolo to avoid interactive prompts
                # Redirect stderr to a temp file for debugging
                err_log=$(mktemp)
                set +e
                RAW_OUTPUT=$(gemini "${gemini_args[@]}" "$FULL_PROMPT" 2>"$err_log")
                gemini_exit=$?
                set -e
                
                echo "$RAW_OUTPUT" > "$STATE_DIR/raw-gemini-output.json"
                
                if [[ $gemini_exit -ne 0 || -z "$RAW_OUTPUT" ]]; then
                    log "  -> gemini CLI failed (exit code $gemini_exit). Stderr:"
                    cat "$err_log" >&2
                    rm -f "$err_log"
                    exit 1
                fi
                rm -f "$err_log"
                OUTPUT=$(echo "$RAW_OUTPUT" | jq -r '.response // empty')
            else
                # Fallback to opencode
                OUTPUT=$(opencode run -m "$MODEL" --format json "$FULL_PROMPT" 2>/dev/null \
                    | jq -rs '[.[] | select(.type == "text")] | last | .part.text // empty')
            fi

            # Robust JSON extraction
            # 1. Try extracting from ```json block
            CLEAN_JSON=$(echo "$OUTPUT" | sed -n '/^```json$/,/^```$/p' | sed '1d;$d')

            # 2. If empty, try extracting from generic ``` block
            if [[ -z "$CLEAN_JSON" || "$CLEAN_JSON" =~ ^[[:space:]]*$ ]]; then
                CLEAN_JSON=$(echo "$OUTPUT" | sed -n '/^```$/,/^```$/p' | sed '1d;$d')
            fi

            # 3. If still empty, assume raw output is JSON
            if [[ -z "$CLEAN_JSON" || "$CLEAN_JSON" =~ ^[[:space:]]*$ ]]; then
                CLEAN_JSON="$OUTPUT"
            fi

            # 4. If not valid JSON, try to extract {...} from within prose
            if ! echo "$CLEAN_JSON" | jq . >/dev/null 2>&1; then
                # Try to find JSON object pattern in the text
                # Use perl for reliable multiline JSON extraction
                EXTRACTED_JSON=$(echo "$OUTPUT" | perl -0777 -ne 'print $1 if /(\{[^{}]*(?:\{[^{}]*\}[^{}]*)*\})/s' 2>/dev/null)
                if [[ -n "$EXTRACTED_JSON" ]] && echo "$EXTRACTED_JSON" | jq . >/dev/null 2>&1; then
                    log "  -> Extracted JSON from prose"
                    CLEAN_JSON="$EXTRACTED_JSON"
                else
                    log "  -> WARNING: Failed to parse JSON from LLM output. Saving raw output."
                    # Don't fail hard here, let the next steps handle empty/invalid JSON if they need it
                fi
            fi
            
            OUTPUT="$CLEAN_JSON"

            STEP_OUTPUTS[$STEP_NAME]="$OUTPUT"
            echo "$OUTPUT" > "$STATE_DIR/step-${STEP_NAME}.json"
            log "  -> saved to step-${STEP_NAME}.json"
            ;;

        shell)
            COMMAND=$(yq -r ".steps[$i].command" "$WORKFLOW_FILE")
            COMMAND=$(substitute_vars "$COMMAND")

            OUTPUT=$(bash -c "$COMMAND" 2>&1) || {
                log "  -> shell command failed: $OUTPUT"
                [[ "$STEP_ALWAYS" == "true" ]] || exit 1
            }
            STEP_OUTPUTS[$STEP_NAME]="$OUTPUT"
            log "  -> ${OUTPUT:0:80}..."
            ;;

        *)
            log "  -> unknown step type: $STEP_TYPE"
            ;;
    esac
done

log "Persona complete: $PERSONA_NAME"
