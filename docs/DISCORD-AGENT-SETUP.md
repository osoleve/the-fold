# Discord Agent Setup Guide

**Status**: Ready for testing
**First Agent**: opus (Shepherd architecture advisor)
**Date**: 2025-12-31

---

## Overview

This guide walks through setting up the first Discord agent (opus) end-to-end.

### What You'll Build

```
User types "@opus what's the best state management approach?"
         ↓
Discord Bot (bot.js) receives message
         ↓
Bot writes trigger file: .fold-repl/requests/opus-discord-trigger.ss
         ↓
Daemon polls, detects trigger (discord-daemon-poll.ss)
         ↓
Runs opus-discord-pipeline with Discord context
         ↓
Pipeline calls LLM, gets response
         ↓
Writes to .fold-repl/discord-outbox/12345.json
         ↓
Bridge (bridge.js) watches outbox, posts to Discord
         ↓
User sees opus's reply in Discord
```

---

## Prerequisites

- [ ] Discord server (you have admin access)
- [ ] Node.js 18+ installed
- [ ] The Fold daemon running (`./daemon.sh start`)
- [ ] LLM API configured (for opus to call)

---

## Step 1: Discord Bot Setup

### 1.1 Create Discord Application

1. Go to https://discord.com/developers/applications
2. Click "New Application"
3. Name: "The Fold" (or your preference)
4. Go to "Bot" tab → "Add Bot"
5. **Copy Bot Token** (you'll need this)
6. Enable these Privileged Gateway Intents:
   - ✓ Server Members Intent
   - ✓ Message Content Intent
7. Go to "OAuth2" → "General"
8. **Copy Client ID**

### 1.2 Invite Bot to Server

1. Go to "OAuth2" → "URL Generator"
2. Select scopes:
   - ✓ bot
   - ✓ applications.commands
3. Select bot permissions:
   - ✓ Read Messages/View Channels
   - ✓ Send Messages
   - ✓ Create Public Threads
   - ✓ Manage Webhooks
   - ✓ Add Reactions
   - ✓ Use Slash Commands
4. Copy the generated URL, open in browser
5. Select your server, authorize

### 1.3 Get Channel IDs

In Discord (with Developer Mode enabled):
1. Right-click each channel → "Copy Channel ID"
2. Note down IDs for:
   - `#consult` (where @opus will respond)
   - Any other channels you want to map

### 1.4 Get Guild ID

Right-click your server icon → "Copy Server ID"

---

## Step 2: Configure Environment

Create `/home/oso/the-fold/.env.discord`:

```bash
# Copy from example
cp .env.discord.example .env.discord

# Edit with your values
vim .env.discord
```

**Minimum required for opus:**

```bash
DISCORD_BOT_TOKEN=YOUR_BOT_TOKEN_HERE
DISCORD_CLIENT_ID=YOUR_CLIENT_ID_HERE
DISCORD_GUILD_ID=YOUR_GUILD_ID_HERE  # Optional
DISCORD_CHANNEL_CONSULT=YOUR_CONSULT_CHANNEL_ID
```

### Load environment:

```bash
# Add to your shell rc (~/.bashrc or ~/.zshrc):
if [ -f /home/oso/the-fold/.env.discord ]; then
  export $(cat /home/oso/the-fold/.env.discord | grep -v '^#' | xargs)
fi

# Or source manually:
export $(cat .env.discord | grep -v '^#' | xargs)
```

---

## Step 3: Install Discord Bot Dependencies

```bash
cd /home/oso/the-fold/thimble/discord
npm install
```

This installs `discord.js` and dependencies.

---

## Step 4: Integrate Daemon Polling

Edit your daemon's main loop to poll for Discord triggers.

**Option A: Modify existing daemon**

Add to `thimble/repl-daemon.ss` or wherever your daemon loop is:

```scheme
(load "agents/discord-daemon-poll.ss")

;; In your daemon loop:
(define (daemon-loop)
  (let loop ()
       ;; Existing work
       (process-repl-requests)

       ;; New: Poll for Discord triggers
       (poll-discord-triggers)

       (sleep 5)  ; Poll every 5 seconds
       (loop)))
```

**Option B: Run separate Discord poller**

```bash
# In a separate terminal/screen/tmux session:
while true; do
  ./fold.sh -c "(load \"agents/discord-daemon-poll.ss\") (poll-discord-triggers)"
  sleep 5
done
```

---

## Step 5: Start the Discord Bot

### 5.1 Test bot connection first

```bash
cd /home/oso/the-fold/thimble/discord
node bot.js
```

You should see:
```
✅ Logged in as TheFold#1234
📡 Session ID: discord-bot-12345
🔗 Watching 11 channels
```

If you see errors, check:
- Bot token is correct
- Bot has been invited to the server
- Intents are enabled in Discord Developer Portal

### 5.2 Test bridge (separate terminal)

The bridge watches the outbox and posts to Discord:

```bash
cd /home/oso/the-fold/thimble/discord
# Bridge is integrated into bot.js, so it starts automatically
```

---

## Step 6: Test Opus Agent

### 6.1 Manual Trigger Test (No Discord)

Test the pipeline without Discord:

```bash
./fold.sh agents/test-opus-discord.ss
```

In the REPL:

```scheme
;; Create a test trigger file
(create-test-trigger 'opus "What are the core principles of The Fold?")

;; Manually poll (simulates daemon)
(load "agents/discord-daemon-poll.ss")
(poll-discord-triggers)
```

**Expected:**
1. Pipeline runs
2. LLM is called (if API configured)
3. Response written to `.fold-repl/discord-outbox/*.json`
4. Bridge picks it up and would post to Discord

### 6.2 Live Discord Test

With bot running:

1. Go to `#consult` channel in Discord
2. Type: `@opus What's the best way to handle state in The Fold?`
3. Watch for:
   - Bot reacts with 🤔 (acknowledging)
   - Daemon log shows: `📬 Discord trigger for opus`
   - Pipeline runs
   - Opus replies in Discord

---

## Step 7: Verify the Flow

### Check logs:

**Bot log:**
```bash
# Should show:
Agent mention detected: @opus
✅ Posted to Discord #consult
```

**Daemon log:**
```bash
tail -f .fold-repl/daemon.log
# Should show:
📬 Discord trigger for opus
▶️  Running opus pipeline
✅ opus: Pipeline complete
```

**Outbox:**
```bash
ls -la .fold-repl/discord-outbox/
# Should create/delete files as messages are processed
```

---

## Troubleshooting

### Bot doesn't see messages

- Enable "Message Content Intent" in Discord Developer Portal
- Re-invite bot with correct permissions

### Daemon not detecting triggers

- Check `.fold-repl/requests/` for trigger files
- Verify daemon is running: `./daemon.sh status`
- Check daemon is loading `discord-daemon-poll.ss`

### Pipeline errors

- Check LLM API is configured
- Verify `opus-discord-pipeline` loads: `./fold.sh -c "(load \"agents/pipelines/discord-agent.ss\") opus-discord-pipeline"`

### No response in Discord

- Check `.fold-repl/discord-outbox/` has files
- Verify bridge.js is running
- Check webhook permissions in Discord channel

### Bot replies but opus doesn't

- Check channel mapping: `DISCORD_CHANNEL_CONSULT` is set
- Verify trigger file was created in `.fold-repl/requests/`
- Check daemon polling interval (should be ≤ 15 seconds)

---

## Next Steps

Once opus works:

1. **Add pedagogue and archivist** - Same pattern, already coded
2. **Set up other channels** - Map engineering, philosophy, etc.
3. **Enable scheduled agents** - kimi news, sentinel reviews
4. **Add council** - Multi-model deliberation with `@council`

---

## Quick Command Reference

```bash
# Start daemon
./daemon.sh start

# Start Discord bot
cd thimble/discord && node bot.js

# Check logs
tail -f .fold-repl/daemon.log
tail -f logs/agents.log

# Test opus manually
./fold.sh agents/test-opus-discord.ss

# Create test trigger
./fold.sh -c "(load \"agents/test-opus-discord.ss\") (create-test-trigger 'opus \"test question\")"

# Poll triggers manually
./fold.sh -c "(load \"agents/discord-daemon-poll.ss\") (poll-discord-triggers)"
```

---

## Files Created

| File | Purpose |
|------|---------|
| `fabric/stitches/pipeline/effects.ss` | Discord effects (post, reply, react, etc.) |
| `fabric/stitches/pipeline/context.ss` | Discord context extensions |
| `thimble/pipeline/interpreter.ss` | Discord effect handlers |
| `thimble/discord/bridge.js` | Outbox → Discord bridge |
| `agents/pipelines/discord-agent.ss` | opus, pedagogue, archivist pipelines |
| `agents/discord-daemon-poll.ss` | Trigger polling for daemon |
| `agents/test-opus-discord.ss` | Test script |
| `.env.discord.example` | Environment template |

---

**Ready to test?** Start with Step 1.1 and work your way through. If you hit issues, check the troubleshooting section.
