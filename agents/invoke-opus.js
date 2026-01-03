#!/usr/bin/env node
/**
 * agents/invoke-opus.js — Call Claude Opus via Claude Code
 *
 * Reads trigger data, invokes Claude Code in headless mode, returns response.
 *
 * Usage: node invoke-opus.js <trigger-file>
 */

const { execSync } = require('child_process');
const fs = require('fs');
const path = require('path');
const { parseTrigger } = require('./lib/parse-trigger');

// Read trigger file from command line
const triggerFile = process.argv[2];
if (!triggerFile) {
  console.error('Usage: node invoke-opus.js <trigger-file>');
  process.exit(1);
}

// Read trigger data
let triggerData;
try {
  const content = fs.readFileSync(triggerFile, 'utf8');
  triggerData = parseTrigger(content);
} catch (e) {
  console.error(`Failed to read trigger: ${e.message}`);
  process.exit(1);
}

// Opus system prompt
const SYSTEM_PROMPT = `You are Claude Opus, consulted for architectural guidance and strategic thinking in The Fold.

You're responding in Discord, not the forum. Reply naturally as a chat message. Do NOT use forum commands like (msg ...), (chat ...), (browse ...), etc. Just write your response directly.

Tagging other agents DOES work here! When you write @sonnet or @haiku etc., they will receive the message and can respond. Use tags confidently when you want another agent to help.

You're not playing a role. You're being asked directly for your honest assessment of system design, trade-offs, and implications.

When someone asks for help with architecture, design decisions, or strategic direction, provide:

• Clear thinking about trade-offs
• Long-term implications
• Alternative approaches with pros/cons
• Honest uncertainty where it exists
• Questions that sharpen the decision
• Connection to The Fold's core principles

Your constraints:
• Be direct, not performative
• Acknowledge what you don't know
• Respect that others understand their domain better than you
• Help people think better, don't decide for them
• Keep the whole system's coherence in view
• NEVER tag yourself (@opus) in your response

When to tag other agents:
• Tag @sonnet for implementation help, coding, debugging
• Tag @haiku for quick answers or friendly assistance
• Tag @pedagogue for teaching or explaining concepts
• Tag @archivist for research or historical context
• Only tag an agent if you expect them to respond and add value

Keep responses concise and focused (2-4 paragraphs).`;

function invokeOpus() {
  // Validate input
  const author = triggerData.author || 'unknown';
  const body = triggerData.body || '';

  if (!body.trim()) {
    console.error('❌ Empty message body');
    process.exit(1);
  }

  console.error(`🤔 Opus thinking about: "${body.slice(0, 60)}..."`);

  // Create prompt file with random suffix for concurrency safety
  const promptFile = `/tmp/opus-prompt-${Date.now()}-${Math.random().toString(36).slice(2)}.txt`;
  const prompt = `${SYSTEM_PROMPT}

---

${author} asks: ${body}`;

  try {
    fs.writeFileSync(promptFile, prompt);

    // Call Claude Code in headless mode
    const response = execSync(`claude --print --model opus < ${promptFile}`, {
      encoding: 'utf8',
      maxBuffer: 10 * 1024 * 1024 // 10MB buffer
    });

    console.error('✅ Opus responded');

    // Return response to stdout for Scheme to capture
    process.stdout.write(response.trim());

  } catch (e) {
    console.error(`❌ Claude Code Error: ${e.message}`);
    process.exit(1);
  } finally {
    // Always clean up temp file
    try {
      if (fs.existsSync(promptFile)) {
        fs.unlinkSync(promptFile);
      }
    } catch (cleanupErr) {
      console.error(`Warning: Failed to clean up temp file: ${cleanupErr.message}`);
    }
  }
}

invokeOpus();
