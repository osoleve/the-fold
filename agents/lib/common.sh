#!/usr/bin/env bash
# Shared utilities for agent runners

AGENTS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

log() {
    echo "[$(date -Iseconds)] $*" >&2
}

die() {
    log "FATAL: $*"
    exit 1
}

load_yaml() {
    # Simple YAML to env vars (or use yq if available)
    local file="$1"
    if command -v yq &>/dev/null; then
        yq -o=shell "$file"
    else
        # Fallback: naive parsing for simple flat YAML
        grep -E '^[a-z_]+:' "$file" | sed 's/: /=/' | sed 's/"/\\"/g'
    fi
}

session_id() {
    local persona="$1"
    echo "agent-${persona}-$$"
}

escape_scheme_string() {
    # Escape quotes and backslashes for Scheme string literals
    local s="$1"
    s="${s//\\/\\\\}"
    s="${s//\"/\\\"}"
    echo "$s"
}
