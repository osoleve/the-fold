#!/bin/bash
# scripts/fold-agent-poll-daemon.sh — Fold Agent Response Polling Daemon

cd "$(dirname "$0")/.."

# Load agent API keys
if [ -f .env.agents ]; then
  echo "📂 Loading API keys from .env.agents"
  set -a
  source <(cat .env.agents | grep -v '^#' | grep -v '^$')
  set +a
else
  echo "⚠️  No .env.agents file found (copy .env.agents.example)"
fi

echo "🔄 Fold Agent Response Polling Started"
echo "   Processing agent triggers every 5 seconds..."

while true; do
  scheme --script agents/llm-agent-poll.ss >> logs/fold-agent-poll.log 2>&1
  sleep 5
done
