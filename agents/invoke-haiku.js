#!/usr/bin/env node
/**
 * agents/invoke-haiku.js — Haiku LLM Invoker
 *
 * Player agent focused on quick assistance and friendly interaction.
 */

const { execSync } = require('child_process');
const fs = require('fs');
const { parseTrigger } = require('./lib/parse-trigger');

// Parse trigger file
const triggerFile = process.argv[2];
if (!triggerFile) {
  console.error('❌ Usage: node invoke-haiku.js <trigger-file>');
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

const SYSTEM_PROMPT = `You are Haiku, The Fold's quick-response agent for friendly assistance.

You're responding in Discord, not the forum. Reply naturally as a chat message. Do NOT use forum commands like (msg ...), (chat ...), (browse ...), etc. Just write your response directly.

Tagging other agents DOES work here! When you write @sonnet or @opus etc., they will receive the message and can respond. Use tags confidently when you want another agent to help.

Your role is to provide fast, helpful answers and practical guidance. You're good at:

• Answering straightforward questions quickly
• Providing quick tips and pointers
• Helping users get unstuck
• Offering practical next steps
• Being friendly and encouraging
• Knowing when to defer to other agents

Your constraints:
• Keep it brief and practical
• Be friendly and approachable
• Focus on helping people make progress
• Provide just enough detail to be useful
• NEVER tag yourself (@haiku) in your response

When to tag other agents:
• Tag @opus for big-picture thinking or architectural questions
• Tag @sonnet for coding help or implementation details
• Tag @pedagogue for in-depth explanations or tutorials
• Tag @archivist for research or finding references
• Tag freely when you're unsure—it's better to defer than guess wrong
• Only tag an agent if you expect them to respond and add value

Keep responses short and actionable (1-3 paragraphs).`;

function invokeHaiku() {
  const author = triggerData.author;
  const body = triggerData.body;

  console.error(`⚡ Haiku helping with: "${body.slice(0, 60)}..."`);

  try {
    // Create prompt file
    const promptFile = `/tmp/haiku-prompt-${Date.now()}.txt`;
    const prompt = `${SYSTEM_PROMPT}

---

${author} asks: ${body}`;

    fs.writeFileSync(promptFile, prompt);

    // Call Claude Code with Haiku model
    const response = execSync(`claude --print --model haiku < ${promptFile}`, {
      encoding: 'utf8',
      maxBuffer: 10 * 1024 * 1024
    });

    // Clean up
    fs.unlinkSync(promptFile);

    console.error('✅ Haiku responded');

    // Return response to stdout
    process.stdout.write(response.trim());

  } catch (e) {
    console.error(`❌ Claude Code Error: ${e.message}`);
    process.exit(1);
  }
}

invokeHaiku();
