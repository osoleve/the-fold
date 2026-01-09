# Opus LLM Integration Setup

The Fold Discord agents are now wired to call the real Claude Opus via Claude Code headless mode!

## Quick Start

### 1. No Configuration Needed!

Since we're using Claude Code headless mode, no API keys are required. The system uses the Claude Code CLI that's already running this session.

### 2. Start the Agent Daemon

```bash
# Stop old daemon
pkill -f fold-agent-poll

# Start new LLM-integrated daemon
./scripts/fold-agent-poll-daemon.sh &
```

### 3. Test It!

```bash
# Run the test (creates bluegown → @opus message)
scheme --script test-bluegown-tags-opus.ss

# Watch the magic happen
tail -f logs/fold-agent-poll.log
tail -f /tmp/discord-bot.log
```

## What Happens

```
1. Bluegown posts: "Hey @opus, what are the core principles of The Fold?"
   ↓
2. Trigger created: .fold-repl/requests/opus-fold-trigger.ss
   ↓
3. Agent daemon calls: node agents/invoke-opus.js <trigger-file>
   ↓
4. Node script calls Claude Code headless mode:
   - Command: claude --print --model opus
   - System: Opus persona from invoke-opus.js
   - User: bluegown's question
   ↓
5. Opus responds with actual architectural guidance
   ↓
6. Response posted to:
   - Fold chat: forum/chat/<timestamp>-opus.sexp
   - Discord: via .fold-repl/discord-outbox/
   ↓
7. User sees real Opus response in Discord!
```

## Architecture

### Files
- `agents/invoke-opus.js` - Node.js wrapper that calls Claude Code headless mode
- `agents/llm-agent-poll.ss` - Polling daemon with LLM integration
- System prompt embedded in invoke-opus.js (concise architectural guidance)

### Loop Prevention
- **Depth limit**: Max 3 consecutive bot messages
- **Reset on human**: Counter resets when human posts
- **@ tags preserved**: Allows multi-turn conversations

### Agent Support
Currently implemented:
- ✅ **opus** - via Claude Code `--model opus`

To add pedagogue/archivist:
1. Create `agents/invoke-pedagogue.js` (copy invoke-opus.js)
2. Update system prompt
3. Change model flag if needed (--model haiku, --model sonnet)
4. They'll automatically work with existing infrastructure!

## Testing Without LLM

If you want to test the infrastructure without calling the LLM, switch back to mock responses:

```bash
# Edit daemon script to use depth-limited version
nano scripts/fold-agent-poll-daemon.sh

# Change:
#   scheme --script agents/llm-agent-poll.ss
# To:
#   scheme --script agents/depth-limited-agent-poll.ss
```

## No API Costs!

Since we're using Claude Code headless mode from within this session, there are no additional API costs. The depth limit (max 3 consecutive bot messages) prevents runaway conversations.

## Troubleshooting

### "claude: command not found"
→ Make sure you're running this from within a Claude Code session

### "node: command not found"
→ Install Node.js: `apt-get install nodejs npm`

### Responses are still mock messages
→ Make sure daemon is using `llm-agent-poll.ss` not `depth-limited-agent-poll.ss`
→ Check logs/fold-agent-poll.log for errors

## Next Steps

1. **Add pedagogue**: Copy invoke-opus.js → invoke-pedagogue.js, update model
2. **Add archivist**: Same process
3. **Tune prompts**: Edit persona YAML files
4. **Monitor usage**: Check Anthropic dashboard for token usage
5. **Adjust depth limit**: Edit `*max-depth*` in llm-agent-poll.ss

---

**You now have real Claude Opus responding to questions in Discord!** 🎉
