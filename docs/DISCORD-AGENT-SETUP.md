# Discord Agent Setup Guide

**Status**: ✅ Production (Opus working)
**Date**: 2026-01-01

---

## Architecture

```
User: "@opus what's the best state management?"
         ↓
Discord Bot (bot.js) detects mention, reacts with 🤔
         ↓
Creates trigger: .fold-repl/triggers/opus-discord-trigger.ss
         ↓
discord-poll-daemon polls every 5s, finds trigger
         ↓
Calls: node agents/invoke-opus.js <trigger-file>
         ↓
invoke-opus.js runs: claude --print --model opus
         ↓
Real Claude Opus responds with architectural guidance
         ↓
Response written to:
  - forum/chat/<timestamp>-opus.sexp (Fold forum)
  - .fold-repl/discord-outbox/<timestamp>-opus.json
         ↓
Discord bridge (in bot.js) watches outbox, posts to Discord
         ↓
User sees Opus reply in Discord
```

**Key Innovation**: Trigger files in dedicated `.fold-repl/triggers/` directory (not `.fold-repl/requests/`) to avoid conflict with REPL daemon.

---

## Prerequisites

- Discord server (admin access)
- Node.js 18+
- Claude Code CLI (already installed)
- `.env.discord` file with bot credentials

---

## Quick Start

### 1. Set Up Environment

```bash
cd /home/oso/the-fold

# Copy template and fill in your Discord credentials
cp .env.discord.example .env.discord
vim .env.discord  # Add DISCORD_BOT_TOKEN, CLIENT_ID, channel IDs
```

**Minimum required:**
```bash
DISCORD_BOT_TOKEN=your-bot-token
DISCORD_CLIENT_ID=your-client-id
DISCORD_GUILD_ID=your-guild-id  # Optional
DISCORD_CHANNEL_GENERAL=your-chat-channel-id
DISCORD_CHANNEL_CONSULT=your-consult-channel-id
```

### 2. Install Discord Bot Dependencies

```bash
cd thimble/discord
npm install
```

### 3. Start All Components

```bash
# Terminal 1: Discord bot
cd /home/oso/the-fold/thimble/discord
./start-bot.sh

# Terminal 2: Agent polling daemon (handles BOTH Discord and Fold triggers)
cd /home/oso/the-fold
./scripts/discord-poll-daemon.sh
```

**Note:** Only run ONE polling daemon. It checks for both `*-discord-trigger.ss` and `*-fold-trigger.ss` files. Running multiple daemons causes duplicate responses.

### 4. Test

In Discord: `@opus What are the core principles of The Fold?`

Expected:
- Bot reacts with 🤔
- ~10-30s delay (LLM thinking)
- Opus replies with architectural guidance

---

## How It Works

### Discord Bot (`thimble/discord/bot.js`)

- Listens for @mentions of opus, pedagogue, archivist
- Writes trigger files to `.fold-repl/triggers/<agent>-discord-trigger.ss`
- Runs Discord → Fold bridge (watches outbox, posts responses)

### Polling Daemon

**discord-poll-daemon** (`scripts/discord-poll-daemon.sh`):
- Polls `.fold-repl/triggers/` every 5 seconds
- Processes **both** Discord triggers (`*-discord-trigger.ss`) AND Fold triggers (`*-fold-trigger.ss`)
- Calls LLM via invoke scripts
- Implements loop prevention (max 3 bot→bot turns)

Runs `agents/llm-agent-poll.ss` which:
- Detects trigger files (checks both types)
- Calls `node agents/invoke-<agent>.js <trigger-file>`
- Writes responses to Fold chat + Discord outbox

### LLM Invokers (`agents/invoke-*.js`)

Simple Node.js scripts that:
- Read trigger file (S-expression)
- Build prompt with system message + user question
- Run: `claude --print --model <model>`
- Return response to stdout

**Current agents:**
- `invoke-opus.js` - Architectural guidance (Opus model)
- `invoke-pedagogue.js` - Teaching (samples from multiple models)
- `invoke-archivist.js` - Research (Sonnet model)

---

## Trigger File Format

Location: `.fold-repl/triggers/<agent>-discord-trigger.ss` or `<agent>-fold-trigger.ss`

Format (S-expression):
```scheme
((session-id . "discord-1234567890")
 (agent . opus)
 (channel . chat)
 (author . "username")
 (body . "@opus What is content-addressable storage?"))
```

---

## Loop Prevention

Bot-to-bot conversations are limited to prevent infinite loops:

- **Human message** → Reset counter to 1, respond
- **Bot message, count < 3** → Increment counter, respond
- **Bot message, count ≥ 3** → Skip response, reset counter

Counter stored in: `.fold-repl/bot-message-count.txt`

---

## Logs

```bash
# Discord bot output
tail -f /tmp/discord-bot.log

# Discord trigger polling
tail -f logs/discord-poll.log

# Fold chat trigger polling
tail -f logs/fold-agent-poll.log

# Forum posts
ls -lt forum/chat/ | head
```

---

## Troubleshooting

### Bot doesn't see messages
- Enable "Message Content Intent" in Discord Developer Portal
- Verify bot invited with correct permissions

### No response to @opus
- Check bot is running: `ps aux | grep "node bot.js"`
- Check trigger created: `ls -la .fold-repl/triggers/`
- Check daemon running: `ps aux | grep discord-poll`
- Check logs: `tail -f logs/discord-poll.log`

### "REPL daemon consuming triggers" error
- Old bug: Fixed by moving triggers from `.fold-repl/requests/` to `.fold-repl/triggers/`
- If trigger files show "invalid syntax" errors, check they're in correct directory

### Response delay
- Normal: 10-30s for Claude Opus to respond
- Check LLM is being called: `tail -f logs/discord-poll.log` should show "🤖 Calling LLM API..."

---

## Configuration

### Add New Agent

1. Create invoker: `agents/invoke-<name>.js`
   ```javascript
   #!/usr/bin/env node
   const { execSync } = require('child_process');
   const fs = require('fs');

   // Parse trigger file...
   // Build prompt with system message...
   // Call: claude --print --model <model>
   ```

2. Add to `agents/llm-agent-poll.ss`:
   ```scheme
   (define *agents* '(opus pedagogue archivist newagent))
   ```

3. Update Discord bot `thimble/discord/config.js`:
   ```javascript
   const CONSULTATION_AGENTS = ['opus', 'pedagogue', 'archivist', 'newagent'];
   ```

4. Restart daemons

### Change Model

Edit `agents/invoke-<agent>.js`, change:
```javascript
execSync(`claude --print --model opus < ${promptFile}`)
```

Options: `opus`, `sonnet`, `haiku`

---

## Files

| File | Purpose |
|------|---------|
| `thimble/discord/bot.js` | Discord bot, bridge, slash commands |
| `thimble/discord/start-bot.sh` | Convenience script to start bot |
| `scripts/discord-poll-daemon.sh` | Agent polling daemon (Discord + Fold triggers) |
| `agents/llm-agent-poll.ss` | Core polling logic + loop prevention |
| `agents/invoke-opus.js` | Opus LLM invoker (Claude Code headless) |
| `agents/invoke-pedagogue.js` | Pedagogue LLM invoker |
| `agents/invoke-archivist.js` | Archivist LLM invoker |
| `.env.discord.example` | Environment template |

---

## Next Steps

- [ ] Add pedagogue and archivist invoke scripts (if not done)
- [ ] Test loop prevention (bot→bot conversations)
- [ ] Set up additional Discord channels (engineering, philosophy, etc.)
- [ ] Enable scheduled agents (kimi, sentinel, etc.)
- [ ] Add council deliberation (@council for multi-model consensus)

---

**Production Ready**: Opus is live! Tag @opus in Discord to test.
