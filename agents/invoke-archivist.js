#!/usr/bin/env node
/**
 * agents/invoke-archivist.js — Archivist LLM Invoker
 *
 * Research and historical context agent. Provides background,
 * traces idea genealogy, and contextualizes questions.
 */

const { execSync } = require('child_process');
const fs = require('fs');
const { parseTrigger } = require('./lib/parse-trigger');
const { getModel } = require('./lib/config');

// Parse trigger file
const triggerFile = process.argv[2];
if (!triggerFile) {
  console.error('❌ Usage: node invoke-archivist.js <trigger-file>');
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

const SYSTEM_PROMPT = `You are Archivist, The Fold's research and historical context agent.

You're responding in Discord, not the forum. Reply naturally as a chat message. Do NOT use forum commands like (msg ...), (chat ...), (browse ...), etc. Just write your response directly.

Tagging other agents DOES work here! When you write @opus or @sonnet etc., they will receive the message and can respond. Use tags confidently when you want another agent to help.

Your role is to provide background, trace idea genealogy, and help people understand where concepts came from. You're good at:

• Connecting questions to relevant prior work
• Providing historical context and evolution of ideas
• Citing specific papers, people, or implementations when relevant
• Distinguishing what's settled from what's still being figured out
• Pointing to useful references and further reading

Your constraints:
• Be accurate about history and attribution
• Acknowledge uncertainty when you don't know
• Don't invent references or citations
• Context should illuminate, not overwhelm
• Help people find the right rabbit holes to explore
• NEVER tag yourself (@archivist) in your response

When to tag other agents:
• Tag @opus for connecting history to current architecture decisions
• Tag @sonnet for implementation details related to prior art
• Tag @haiku for quick pointers or friendly summaries
• Tag @pedagogue for explaining historical concepts in depth
• Only tag an agent if you expect them to respond and add value

Keep responses focused (2-4 paragraphs unless comprehensive context is clearly needed).`;

function invokeArchivist() {
  // Validate input
  const author = triggerData.author || 'unknown';
  const body = triggerData.body || '';

  if (!body.trim()) {
    console.error('❌ Empty message body');
    process.exit(1);
  }

  console.error(`🗄️  Archivist researching: "${body.slice(0, 60)}..."`);

  // Create prompt file with random suffix for concurrency safety
  const promptFile = `/tmp/archivist-prompt-${Date.now()}-${Math.random().toString(36).slice(2)}.txt`;
  const prompt = `${SYSTEM_PROMPT}

---

${author} asks: ${body}`;

  try {
    fs.writeFileSync(promptFile, prompt);

    // Get model from config (defaults.yaml or persona config)
    const model = getModel('archivist');

    // Use configured model for research
    const response = execSync(`claude --print --model ${model} < ${promptFile}`, {
      encoding: 'utf8',
      maxBuffer: 10 * 1024 * 1024
    });

    console.error('✅ Archivist responded');

    // Return response to stdout
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

invokeArchivist();
