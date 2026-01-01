#!/bin/bash
# scripts/start-discord-bot.sh — Start Discord Bot with Environment

set -e

# Change to project root
cd "$(dirname "$0")/.."

echo "🌿 The Fold Discord Bot Starter"
echo ""

# Check for environment file
if [ ! -f .env.discord ]; then
  echo "❌ Error: .env.discord not found"
  echo ""
  echo "Please create .env.discord with your bot credentials:"
  echo "  cp .env.discord.example .env.discord"
  echo "  vim .env.discord"
  echo ""
  exit 1
fi

# Load environment
echo "📂 Loading environment from .env.discord"
set -a
source <(cat .env.discord | grep -v '^#' | grep -v '^$')
set +a

# Check critical vars
if [ -z "$DISCORD_BOT_TOKEN" ]; then
  echo "❌ Error: DISCORD_BOT_TOKEN not set in .env.discord"
  exit 1
fi

if [ -z "$DISCORD_CLIENT_ID" ]; then
  echo "⚠️  Warning: DISCORD_CLIENT_ID not set (slash commands won't register)"
fi

# Check daemon status
echo ""
echo "🔍 Checking daemon status..."
if ! ./daemon.sh status &> /dev/null; then
  echo "⚠️  Warning: Fold daemon not running"
  echo "   Discord triggers require the daemon to be active"
  echo "   Start it with: ./daemon.sh start"
  echo ""
  read -p "Continue anyway? (y/N) " -n 1 -r
  echo
  if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    exit 1
  fi
fi

# Check dependencies
echo ""
echo "📦 Checking npm dependencies..."
cd thimble/discord
if [ ! -d node_modules ]; then
  echo "Installing dependencies..."
  npm install
fi

# Start bot
echo ""
echo "🤖 Starting Discord bot..."
echo "   Press Ctrl+C to stop"
echo ""
exec node bot.js
