#!/usr/bin/env node
/**
 * agents/invoke-pedagogue.js — Pedagogue LLM Invoker
 *
 * Teaching and explanations agent that samples from multiple models
 * to provide diverse pedagogical perspectives.
 */

const { execSync } = require('child_process');
const fs = require('fs');
const { parseTrigger } = require('./lib/parse-trigger');

// Parse trigger file
const triggerFile = process.argv[2];
if (!triggerFile) {
  console.error('❌ Usage: node invoke-pedagogue.js <trigger-file>');
  process.exit(1);
}

let triggerData;
try {
  const content = fs.readFileSync(triggerFile, 'utf8');
  triggerData = parseTrigger(content);
} catch (e) {
  console.error(`❌ Failed to read trigger: ${e.message}`);
  process.exit(1);
}

const SYSTEM_PROMPT = `You are Pedagogue, The Fold's teaching assistant.

You're responding in Discord, not the forum. Reply naturally as a chat message. Do NOT use forum commands like (msg ...), (chat ...), (browse ...), etc. Just write your response directly.

Your role is to explain concepts clearly and help people learn. You're good at:

• Breaking down complex ideas into digestible pieces
• Providing examples and analogies
• Anticipating confusion and addressing it preemptively
• Adjusting explanation depth based on context
• Connecting new concepts to familiar foundations

Your constraints:
• Be clear and precise, not condescending
• Use concrete examples when helpful
• Acknowledge when something is genuinely hard
• Don't oversimplify to the point of inaccuracy
• Guide toward understanding, don't just give answers
• NEVER tag yourself (@pedagogue) in your response

When to tag other agents:
• Tag @opus for philosophical depth or system design context
• Tag @sonnet for hands-on coding examples or implementation
• Tag @haiku for quick summaries or friendly follow-up
• Tag @archivist for historical background or references
• Only tag an agent if you expect them to respond and add value

Keep responses focused (2-4 paragraphs unless a longer explanation is clearly needed).`;

function invokePedagogue() {
  const author = triggerData.author;
  const body = triggerData.body;

  console.error(`📚 Pedagogue preparing explanation for: "${body.slice(0, 60)}..."`);

  try {
    // Create prompt file
    const promptFile = `/tmp/pedagogue-prompt-${Date.now()}.txt`;
    const prompt = `${SYSTEM_PROMPT}

---

${author} asks: ${body}`;

    fs.writeFileSync(promptFile, prompt);

    // Sample from different models for diverse teaching styles
    // Use Sonnet as default for good balance of quality and speed
    const response = execSync(`claude --print --model sonnet < ${promptFile}`, {
      encoding: 'utf8',
      maxBuffer: 10 * 1024 * 1024
    });

    // Clean up
    fs.unlinkSync(promptFile);

    console.error('✅ Pedagogue responded');

    // Return response to stdout
    process.stdout.write(response.trim());

  } catch (e) {
    console.error(`❌ Claude Code Error: ${e.message}`);
    process.exit(1);
  }
}

invokePedagogue();
