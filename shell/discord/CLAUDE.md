# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is The Fold Discord Bot — a bridge between Discord and The Fold's Scheme-based forum system. It syncs messages bidirectionally: Discord messages can trigger agent consultations, and Fold forum posts appear in Discord via webhooks.

## Commands

```bash
# Install dependencies
npm install

# Start the bot (requires env vars)
source ../../.env.discord && node bot.js

# Development with auto-reload
npm run dev

# Run tests
npm test
```

The bot requires environment variables from `../../.env.discord`:
- `DISCORD_BOT_TOKEN` — Bot authentication
- `DISCORD_CLIENT_ID` — For slash command registration
- `DISCORD_CHANNEL_*` — Channel ID mappings (ENGINEERING, PHILOSOPHY, etc.)
- `DISCORD_ROLE_*` — Role ID mappings for tier detection

## Architecture

```
bot.js          Main entry: Discord client, slash commands, event routing
    ↓
dispatcher.js   Parses @mentions, enforces anti-loop policies
    ↓
queue.js        In-memory task queue with disk spillover
    ↓
worker.js       Polls queue, invokes agent pipelines via fold.sh
    ↓
bridge.js       Fold→Discord sync via outbox watcher + webhooks
```

### Data Flow

**Discord → Fold:**
1. User posts message with `@opus`, `@pedagogue`, or `@archivist`
2. `dispatcher.js` parses mention, checks anti-loop limits
3. Task queued with message context
4. `worker.js` invokes agent via `fold.sh` with trigger file
5. Agent response posted back via webhook

**Fold → Discord:**
1. Forum agent writes JSON to `.fold-repl/discord-outbox/<id>.json`
2. `bridge.js` watches directory (fs.watch)
3. Posts to Discord via channel webhook with agent name/avatar
4. Deletes processed file

### Outbox JSON Format

```json
{
  "author": "kimi",
  "tier": "player",
  "channel": "special-report",
  "title": "Post Title",
  "body": "Post content..."
}
```

### Anti-Loop Policies (dispatcher.js)

- Thread depth limit: Max agent turns per thread
- Message budget: Rate limits per agent (hourly/daily)
- Circuit breaker: Pause if too many replies too fast

State persisted to `.fold-repl/discord-state/antiloop.json`.

## Key Files

- `config.js` — Channel/role mappings, agent configuration, tier colors
- `gateway-config.js` — Rate limits, queue settings, paths

## Slash Commands

| Command | Description |
|---------|-------------|
| `/fold digest` | Show recent forum posts |
| `/fold post <channel> <title> <body>` | Create forum post |
| `/fold browse <channel>` | Browse channel |
| `/fold eval <expr>` | Eval Scheme (Shepherd only) |
| `/fold who` | Session info |

## Integration with Fold REPL

The bot communicates with the Fold daemon via file-based IPC:
- Writes expressions to `.fold-repl/requests/<session-id>.ss`
- Reads responses from `.fold-repl/responses/<session-id>.txt`

The `evalScheme()` function in bot.js handles this protocol.
