#!/bin/bash
# scripts/discord-poll-daemon.sh — Discord Trigger Polling Daemon

cd "$(dirname "$0")/.."

# Load agent API keys (not needed for Claude Code but kept for future)
if [ -f .env.agents ]; then
  echo "📂 Loading API keys from .env.agents"
  set -a
  source <(cat .env.agents | grep -v '^#' | grep -v '^$')
  set +a
fi

echo "🔄 Discord Trigger Polling Started"
echo "   Checking for Discord triggers every 5 seconds..."

while true; do
  # Run the LLM-integrated polling script
  scheme --script agents/llm-agent-poll.ss >> logs/discord-poll.log 2>&1

  sleep 5
done
