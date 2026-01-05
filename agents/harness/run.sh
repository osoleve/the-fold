#!/bin/bash
# agents/harness/run.sh — Wrapper that loads env before running harness
#
# Usage:
#   ./agents/harness/run.sh -p groq "Your goal here"
#   ./agents/harness/run.sh -p groq --steps 3 "Simple task"

set -e

# Source environment (API keys, etc.)
if [[ -f ~/.env ]]; then
    set -a  # Auto-export all variables
    source ~/.env
    set +a
fi

# Run the harness
cd /home/oso/the-fold
exec python3 -m agents.harness.run "$@"
