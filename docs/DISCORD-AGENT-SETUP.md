# Discord Agent Setup Guide

**Status**: ✅ Production (5 consultation agents: Opus, Pedagogue, Archivist, Sonnet, Haiku + agent-to-agent communication)
**Date**: 2026-01-01

---

## Architecture

### Gateway Mode (Recommended)

**Latency**: <2 seconds (instant dispatch + LLM time)

```
User: "@opus what's the best state management?"
         ↓
Discord Bot detects mention, checks anti-loop policies
         ↓
dispatcher.checkDispatch() → allowed
         ↓
queue.enqueue(task) + react with 🤔
         ↓
worker polls queue, invokes pipeline
         ↓
node agents/invoke-opus.js <trigger-file>
         ↓
Response posted via webhook + saved to forum
         ↓
User sees Opus reply in Discord (~2s total)
```

**Enable/Disable**: Set `GATEWAY_ENABLED=true` (default) or `GATEWAY_ENABLED=false` in environment.

### Trigger File Mode (Fallback)

**Latency**: 5-30 seconds (5s polling + LLM time)

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

## Gateway Anti-Loop Policies

The gateway implements multiple anti-loop policies:

| Policy | Default | Description |
|--------|---------|-------------|
| Thread Depth | 3 | Max agent turns per thread before blocking |
| Message Budget (Hourly) | 20 | Max messages per agent per hour |
| Message Budget (Daily) | 100 | Max messages per agent per day |
| Circuit Breaker | 5/30s | Pause if >5 replies in 30 seconds same thread |

**Human messages reset the thread depth counter.**

### Configuration

Set via environment variables or `config/discord-agents.json`:

```bash
# Environment variables
ANTILOOP_MAX_DEPTH=3
ANTILOOP_HOURLY_LIMIT=20
ANTILOOP_DAILY_LIMIT=100
ANTILOOP_CIRCUIT_THRESHOLD=5
ANTILOOP_CIRCUIT_WINDOW=30000
ANTILOOP_AGENT_TO_AGENT=true
```

State is persisted to `state/discord-antiloop.json`.

---

## Monitoring

### Health Endpoint

When gateway is enabled, a health endpoint is available:

```bash
curl http://localhost:8081/healthz
```

Response:
```json
{
  "status": "ok",
  "timestamp": "2026-01-01T12:00:00.000Z",
  "uptime": 3600,
  "gateway": {
    "enabled": true,
    "queueDepth": 0,
    "spilloverDepth": 0
  },
  "worker": {
    "isRunning": true,
    "tasksProcessed": 42,
    "tasksSucceeded": 40,
    "tasksFailed": 2,
    "lastTaskAt": 1704067200000
  },
  "lastEventAt": 1704067200000,
  "sessionId": "discord-bot-12345"
}
```

Configure port: `HEALTH_PORT=8081` (default)

### Logs

```bash
# Bot output (includes gateway dispatch logs)
tail -f /tmp/discord-bot.log

# Look for gateway-specific messages
grep "Gateway:" /tmp/discord-bot.log
```

---

## Rollback Procedure

If the gateway causes issues, rollback to trigger file mode:

1. **Disable gateway**:
   ```bash
   export GATEWAY_ENABLED=false
   ```

2. **Restart bot**:
   ```bash
   cd /home/oso/the-fold/shell/discord
   ./start-bot.sh
   ```

3. **Start polling daemon**:
   ```bash
   cd /home/oso/the-fold
   ./scripts/discord-poll-daemon.sh
   ```

The polling daemon will handle all agent invocations via trigger files.

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
cd shell/discord
npm install
```

### 3. Start All Components

```bash
# Terminal 1: Discord bot
cd /home/oso/the-fold/shell/discord
./start-bot.sh

# Terminal 2: Agent polling daemon (handles BOTH Discord and Fold triggers)
cd /home/oso/the-fold
./scripts/discord-poll-daemon.sh
```

**Note:** Only run ONE polling daemon. It checks for both `*-discord-trigger.ss` and `*-fold-trigger.ss` files. Running multiple daemons causes duplicate responses.

**Alternative:** The Discord bot will also auto-start when you run `./start.sh` if:
- `.env.discord` exists and is configured
- No Discord bot is already running

### 4. Test

In Discord, try any of these:
- `@opus What are the core principles of The Fold?` - Architecture and strategy
- `@pedagogue Explain de Bruijn indices` - Teaching and explanations
- `@archivist What prior work exists on homoiconic systems?` - Research
- `@sonnet Help me debug this function` - Practical implementation
- `@haiku Quick question: how do I start the REPL?` - Fast assistance

Expected:
- Bot reacts with 🤔
- ~5-30s delay (LLM thinking, varies by model)
- Agent replies with appropriate guidance

---

## How It Works

### Discord Bot (`shell/discord/bot.js`)

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
- `invoke-pedagogue.js` - Teaching (Sonnet model)
- `invoke-archivist.js` - Research (Sonnet model)
- `invoke-sonnet.js` - Builder/practical implementation (Sonnet model)
- `invoke-haiku.js` - Quick assistance (Haiku model)

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

## Agent-to-Agent Communication

Agents can tag other agents in their responses to create multi-agent conversations:

```
User: "@opus How should we design learning experiences in The Fold?"
Opus: "Great question! Let me bring in @pedagogue who specializes in teaching..."
→ Pedagogue automatically responds to the discussion
```

**How it works:**
- The bot detects `@agent` mentions in both human AND bot messages
- When an agent's response includes `@pedagogue`, `@archivist`, `@opus`, `@sonnet`, or `@haiku`, a trigger is created
- The loop prevention system (see below) ensures conversations don't run forever
- Bot messages are NOT logged to the Fold forum (only human messages are logged)

**Use cases:**
- Opus can delegate teaching questions to pedagogue
- Pedagogue can request historical context from archivist
- Archivist can ask opus for architectural guidance
- Anyone can tag sonnet for implementation help
- Anyone can tag haiku for quick answers
- Creates natural multi-perspective discussions

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
   (define *agents* '(opus pedagogue archivist sonnet haiku newagent))
   ```

3. Update Discord bot `shell/discord/config.js`:
   ```javascript
   const CONSULTATION_AGENTS = ['opus', 'pedagogue', 'archivist', 'sonnet', 'haiku', 'newagent'];
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

### Core Bot Files

| File | Purpose |
|------|---------|
| `shell/discord/bot.js` | Discord bot, bridge, slash commands, gateway dispatch |
| `shell/discord/bridge.js` | Fold → Discord message bridging via webhooks |
| `shell/discord/config.js` | Channel/role mappings, agent display config |
| `shell/discord/start-bot.sh` | Convenience script to start bot |

### Gateway Integration (Phase 1)

| File | Purpose |
|------|---------|
| `shell/discord/dispatcher.js` | Mention routing + anti-loop policy enforcement |
| `shell/discord/queue.js` | In-memory task queue with disk spillover |
| `shell/discord/worker.js` | Queue consumer, pipeline invocation |
| `shell/discord/gateway-config.js` | Gateway configuration and parameters |
| `config/discord-agents.json` | Agent configuration (pipelines, budgets) |
| `state/discord-antiloop.json` | Anti-loop state (thread depths, budgets) |

### Trigger File Mode (Fallback)

| File | Purpose |
|------|---------|
| `scripts/discord-poll-daemon.sh` | Agent polling daemon (Discord + Fold triggers) |
| `agents/llm-agent-poll.ss` | Core polling logic + loop prevention |

### LLM Invokers

| File | Purpose |
|------|---------|
| `agents/invoke-opus.js` | Opus LLM invoker (Claude Code headless) |
| `agents/invoke-pedagogue.js` | Pedagogue LLM invoker |
| `agents/invoke-archivist.js` | Archivist LLM invoker |
| `agents/invoke-sonnet.js` | Sonnet LLM invoker |
| `agents/invoke-haiku.js` | Haiku LLM invoker |

### Configuration

| File | Purpose |
|------|---------|
| `.env.discord` | Bot credentials and channel IDs |
| `.env.discord.example` | Environment template |

---

## Next Steps

- [x] Gateway integration (Phase 1) — instant dispatch, anti-loop policies
- [ ] Phase 2: Remove trigger file dependency entirely
- [ ] Set up additional Discord channels (engineering, philosophy, etc.)
- [ ] Enable scheduled agents (kimi, sentinel, etc.)
- [ ] Add council deliberation (@council for multi-model consensus)

---

**Production Ready**: Gateway mode is live! Responses in <2 seconds.

Tag @opus, @pedagogue, @archivist, @sonnet, or @haiku in Discord to test.
