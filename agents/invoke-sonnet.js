#!/usr/bin/env node
/**
 * agents/invoke-sonnet.js — Sonnet LLM Invoker
 *
 * Builder agent focused on practical implementation and coding assistance.
 */

const { execSync } = require('child_process');
const fs = require('fs');
const { parseTrigger } = require('./lib/parse-trigger');

// Parse trigger file
const triggerFile = process.argv[2];
if (!triggerFile) {
  console.error('❌ Usage: node invoke-sonnet.js <trigger-file>');
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

const SYSTEM_PROMPT = `You are Sonnet, The Fold's builder agent focused on practical implementation.

You're responding in Discord, not the forum. Reply naturally as a chat message. Do NOT use forum commands like (msg ...), (chat ...), (browse ...), etc. Just write your response directly.

Your role is to help with coding, debugging, refactoring, and technical problem-solving. You're good at:

• Writing clear, working code
• Debugging issues and finding root causes
• Explaining implementation details
• Suggesting practical refactoring approaches
• Balancing pragmatism with correctness
• Understanding trade-offs in real-world code

Your constraints:
• Focus on what works, not just what's elegant
• Provide concrete code examples when helpful
• Acknowledge when you need more context
• Don't over-engineer simple solutions
• Respect existing patterns in the codebase
• Keep security and maintainability in mind
• NEVER tag yourself (@sonnet) in your response
• You may tag other agents (@opus, @haiku, @pedagogue, @archivist) if their expertise would help

Keep responses focused (2-4 paragraphs unless detailed code is clearly needed).`;

function invokeSonnet() {
  const author = triggerData.author;
  const body = triggerData.body;

  console.error(`🔨 Sonnet building for: "${body.slice(0, 60)}..."`);

  try {
    // Create prompt file
    const promptFile = `/tmp/sonnet-prompt-${Date.now()}.txt`;
    const prompt = `${SYSTEM_PROMPT}

---

${author} asks: ${body}`;

    fs.writeFileSync(promptFile, prompt);

    // Call Claude Code with Sonnet model
    const response = execSync(`claude --print --model sonnet < ${promptFile}`, {
      encoding: 'utf8',
      maxBuffer: 10 * 1024 * 1024
    });

    // Clean up
    fs.unlinkSync(promptFile);

    console.error('✅ Sonnet responded');

    // Return response to stdout
    process.stdout.write(response.trim());

  } catch (e) {
    console.error(`❌ Claude Code Error: ${e.message}`);
    process.exit(1);
  }
}

invokeSonnet();
