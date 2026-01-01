#!/usr/bin/env node
/**
 * thimble/discord/bot.js — The Fold Discord Bot
 *
 * Bridges Discord and The Fold forum system.
 * - Logs Discord messages to Fold's S-expression store
 * - Posts Fold forum updates to Discord via webhooks
 * - Handles agent consultation via @mentions
 * - Provides /fold commands for interacting with the REPL
 *
 * Usage:
 *   DISCORD_BOT_TOKEN=xxx node bot.js
 */

const {
  Client,
  GatewayIntentBits,
  Events,
  EmbedBuilder,
  SlashCommandBuilder,
  REST,
  Routes,
  PermissionFlagsBits,
} = require('discord.js');

const fs = require('fs');
const path = require('path');
const http = require('http');
const config = require('./config');
const bridge = require('./bridge');

// Gateway integration modules
const gatewayConfig = require('./gateway-config');
const dispatcher = require('./dispatcher');
const queue = require('./queue');
const worker = require('./worker');

// ============================================================
// Bot Setup
// ============================================================

const client = new Client({
  intents: [
    GatewayIntentBits.Guilds,
    GatewayIntentBits.GuildMessages,
    GatewayIntentBits.MessageContent,
    GatewayIntentBits.GuildMembers,
  ],
});

// Session ID for REPL communication
const SESSION_ID = `discord-bot-${process.pid}`;

// Track last event for health endpoint
let lastEventTimestamp = null;

// Health server reference
let healthServer = null;

// ============================================================
// REPL Bridge
// ============================================================

/**
 * Execute a Scheme expression via the Fold REPL daemon
 */
async function evalScheme(expression) {
  const requestPath = path.join(
    __dirname,
    config.BOT_CONFIG.replRequestDir,
    `${SESSION_ID}.ss`
  );
  const responsePath = path.join(
    __dirname,
    config.BOT_CONFIG.replResponseDir,
    `${SESSION_ID}.txt`
  );

  // Write request
  fs.writeFileSync(requestPath, expression);

  // Wait for response (poll with timeout)
  const maxWait = 30000;  // 30 seconds
  const pollInterval = 100;
  let waited = 0;

  while (waited < maxWait) {
    await new Promise(r => setTimeout(r, pollInterval));
    waited += pollInterval;

    if (fs.existsSync(responsePath)) {
      const response = fs.readFileSync(responsePath, 'utf8');
      fs.unlinkSync(responsePath);  // Clean up
      return response;
    }
  }

  throw new Error('REPL timeout: no response received');
}

/**
 * Log a Discord message to Fold
 */
async function logToFold(message) {
  const foldChannel = config.getDiscordToFoldChannel(message.channel.id);
  if (!foldChannel) return;  // Unknown channel, skip

  const tier = config.getTierFromRoles(message.member);
  const author = message.author.username.replace(/[^a-zA-Z0-9_-]/g, '_');
  const body = message.content;

  // Map tier to model for hi command
  const modelTier = tier === 'shepherd' ? 'opus' :
                    tier === 'builder' ? 'sonnet' : 'haiku';

  // Escape the body for Scheme
  const escapedBody = body
    .replace(/\\/g, '\\\\')
    .replace(/"/g, '\\"')
    .replace(/\n/g, '\\n');

  // Use hi + chat/msg instead of post!
  // For chat channel, use (chat "message")
  // For other channels, use (msg 'channel "title" "body")
  let expr;
  if (foldChannel === 'chat') {
    expr = `(begin (hi '${modelTier} '${author}) (chat "${escapedBody}"))`;
  } else {
    expr = `(begin (hi '${modelTier} '${author}) (msg '${foldChannel} "" "${escapedBody}"))`;
  }

  try {
    await evalScheme(expr);
    // Optional: React to confirm logging
    // await message.react('📜');
  } catch (e) {
    console.error(`Failed to log message to Fold: ${e.message}`);
  }
}

// ============================================================
// Agent Mention Handler
// ============================================================

/**
 * Escape a string for use in S-expression string literals
 * Properly handles backslashes, quotes, newlines, and other special chars
 */
function escapeSexpString(str) {
  return str
    .replace(/\\/g, '\\\\')   // Escape backslashes first
    .replace(/"/g, '\\"')     // Escape double quotes
    .replace(/\n/g, '\\n')    // Escape newlines
    .replace(/\r/g, '\\r')    // Escape carriage returns
    .replace(/\t/g, '\\t');   // Escape tabs
}

/**
 * Check for agent mentions and trigger consultation
 */
async function handleAgentMention(message) {
  const content = message.content.toLowerCase();

  // Look for @agent mentions in the message
  // Match patterns like: @opus, @pedagogue, @archivist
  let agentToInvoke = null;

  for (const agent of config.CONSULTATION_AGENTS) {
    // Match @agent (with word boundary to avoid partial matches)
    const pattern = new RegExp(`@${agent}\\b`, 'i');
    if (pattern.test(content)) {
      agentToInvoke = agent;
      break;
    }
  }

  // No agent mentioned
  if (!agentToInvoke) return false;

  console.log(`Agent mentioned: @${agentToInvoke}`);

  // Write trigger file for daemon
  const triggerPath = path.join(
    __dirname,
    '../../.fold-repl/triggers',
    `${agentToInvoke}-discord-trigger.ss`
  );

  // Properly escape the message content for S-expression format
  const escapedBody = escapeSexpString(message.content);
  const escapedAuthor = escapeSexpString(message.author.username);

  const triggerExpr = `((session-id . "discord-${message.id}")
 (agent . ${agentToInvoke})
 (channel . ${config.getDiscordToFoldChannel(message.channel.id) || 'consult'})
 (author . "${escapedAuthor}")
 (body . "${escapedBody}"))
`;

  try {
    fs.writeFileSync(triggerPath, triggerExpr);
    console.log(`Trigger file written: ${triggerPath}`);
  } catch (e) {
    console.error(`Failed to write trigger file: ${e.message}`);
  }

  // Acknowledge
  await message.react('🤔');
  return true;
}

// ============================================================
// Event Handlers
// ============================================================

client.once(Events.ClientReady, (c) => {
  console.log(`✅ Logged in as ${c.user.tag}`);
  console.log(`📡 Session ID: ${SESSION_ID}`);
  console.log(`🔗 Watching ${Object.keys(config.CHANNEL_MAP).length} channels`);

  // Start the Discord → Fold bridge
  bridge.start(c);

  // Start gateway worker if enabled
  if (gatewayConfig.enabled) {
    worker.start(c);
    console.log(`🚀 Gateway dispatch enabled`);
  } else {
    console.log(`📋 Gateway dispatch disabled, using trigger file flow`);
  }

  // Start health server if enabled
  if (gatewayConfig.health.enabled) {
    startHealthServer();
  }
});

client.on(Events.MessageCreate, async (message) => {
  // Track last event time for health endpoint
  lastEventTimestamp = Date.now();

  // Check for @fold mention (sugar for /fold eval)
  if (message.mentions.has(client.user) && !message.author.bot) {
    // Extract expression after the mention
    const content = message.content
      .replace(new RegExp(`<@!?${client.user.id}>`, 'g'), '')
      .trim();

    if (content) {
      // Check permissions (shepherd/outsider only)
      const tier = config.getTierFromRoles(message.member);
      if (tier !== 'shepherd' && tier !== 'outsider') {
        await message.reply('❌ Only Shepherds can eval expressions');
        return;
      }

      try {
        await message.react('🤔');
        const result = await evalScheme(content);
        const response = '```scheme\n' + result.slice(0, 1900) + '\n```';
        await message.reply(response);
      } catch (e) {
        console.error(`@fold eval error: ${e.message}`);
        try {
          await message.reply(`❌ ${e.message.slice(0, 200)}`);
        } catch (replyErr) {
          console.error(`Failed to send error: ${replyErr.message}`);
        }
      }
      return;
    }
  }

  // NEW: Gateway dispatch path (if enabled)
  if (gatewayConfig.enabled) {
    const agentId = dispatcher.parseAgentMention(message.content);

    if (agentId) {
      const state = dispatcher.loadState();
      const decision = dispatcher.checkDispatch(message, agentId, state);

      if (decision.allowed) {
        // Enqueue task for worker
        queue.enqueue({
          taskId: `gw-${message.id}`,
          agentId,
          channelId: message.channel.id,
          threadId: message.channel.isThread?.() ? message.channel.id : null,
          messageId: message.id,
          content: message.content,
          authorId: message.author.id,
          authorName: message.author.username,
          isBot: message.author.bot,
          enqueuedAt: Date.now(),
        });

        // Acknowledge
        await message.react('🤔');
        console.log(`📋 Gateway: Queued task for @${agentId}`);
      } else if (decision.notifyUser) {
        // Notify user of rate limit
        try {
          await message.reply(`⏳ ${decision.reason}`);
        } catch (e) {
          console.error(`Failed to send rate limit notice: ${e.message}`);
        }
      }

      // Skip trigger file path when gateway handles the mention
      if (decision.allowed || decision.notifyUser) {
        // Still log human messages to Fold
        if (!message.author.bot) {
          await logToFold(message);
        }
        return;
      }
    }
  }

  // EXISTING: Trigger file path (fallback or when gateway disabled)
  // Check for agent mentions (works for both human and bot messages)
  const agentTriggered = await handleAgentMention(message);

  // Ignore other bot messages (after checking for agent mentions)
  // This prevents logging bot messages to Fold while allowing agent-to-agent tags
  if (message.author.bot) return;

  // Log to Fold (only human messages reach here)
  await logToFold(message);
});

// ============================================================
// Slash Commands
// ============================================================

const commands = [
  new SlashCommandBuilder()
    .setName('fold')
    .setDescription('Interact with The Fold')
    .addSubcommand(sub =>
      sub.setName('digest')
        .setDescription('Show recent forum posts')
        .addIntegerOption(opt =>
          opt.setName('count')
            .setDescription('Number of posts (default: 10)')
            .setRequired(false)
        )
    )
    .addSubcommand(sub =>
      sub.setName('post')
        .setDescription('Create a forum post')
        .addStringOption(opt =>
          opt.setName('channel')
            .setDescription('Forum channel')
            .setRequired(true)
            .addChoices(
              { name: 'engineering', value: 'engineering' },
              { name: 'philosophy', value: 'philosophy' },
              { name: 'design', value: 'design' },
              { name: 'art', value: 'art' },
              { name: 'poetry', value: 'poetry' },
              { name: 'requests', value: 'requests' },
              { name: 'wishlist', value: 'wishlist' },
            )
        )
        .addStringOption(opt =>
          opt.setName('title')
            .setDescription('Post title')
            .setRequired(true)
        )
        .addStringOption(opt =>
          opt.setName('body')
            .setDescription('Post body')
            .setRequired(true)
        )
    )
    .addSubcommand(sub =>
      sub.setName('browse')
        .setDescription('Browse a channel')
        .addStringOption(opt =>
          opt.setName('channel')
            .setDescription('Forum channel')
            .setRequired(true)
        )
        .addIntegerOption(opt =>
          opt.setName('count')
            .setDescription('Number of posts (default: 5)')
            .setRequired(false)
        )
    )
    .addSubcommand(sub =>
      sub.setName('eval')
        .setDescription('Evaluate Scheme expression (Shepherd only)')
        .addStringOption(opt =>
          opt.setName('expression')
            .setDescription('Scheme expression')
            .setRequired(true)
        )
    )
    .addSubcommand(sub =>
      sub.setName('who')
        .setDescription('Show your session info')
    )
    .addSubcommand(sub =>
      sub.setName('help')
        .setDescription('Show help')
    ),
];

client.on(Events.InteractionCreate, async (interaction) => {
  if (!interaction.isChatInputCommand()) return;

  if (interaction.commandName === 'fold') {
    const subcommand = interaction.options.getSubcommand();

    try {
      switch (subcommand) {
        case 'digest': {
          await interaction.deferReply();
          const count = interaction.options.getInteger('count') || 10;
          const result = await evalScheme(`(digest-posts ${count})`);
          await interaction.editReply({
            content: '```\n' + result.slice(0, 1900) + '\n```',
          });
          break;
        }

        case 'post': {
          await interaction.deferReply();
          const channel = interaction.options.getString('channel');
          const title = interaction.options.getString('title').replace(/"/g, '\\"');
          const body = interaction.options.getString('body').replace(/"/g, '\\"');
          const result = await evalScheme(`(msg '${channel} "${title}" "${body}")`);
          await interaction.editReply({
            content: `✅ Posted to #${channel}\n\`\`\`\n${result.slice(0, 500)}\n\`\`\``,
          });
          break;
        }

        case 'browse': {
          await interaction.deferReply();
          const channel = interaction.options.getString('channel');
          const count = interaction.options.getInteger('count') || 5;
          const result = await evalScheme(`(browse '${channel} ${count})`);
          await interaction.editReply({
            content: '```\n' + result.slice(0, 1900) + '\n```',
          });
          break;
        }

        case 'eval': {
          // Check for Shepherd role
          const tier = config.getTierFromRoles(interaction.member);
          if (tier !== 'shepherd' && tier !== 'outsider') {
            await interaction.reply({
              content: '❌ Only Shepherds can eval arbitrary expressions',
              flags: 64, // ephemeral
            });
            return;
          }

          await interaction.deferReply();
          const expr = interaction.options.getString('expression');
          const result = await evalScheme(expr);
          await interaction.editReply({
            content: '```scheme\n' + result.slice(0, 1900) + '\n```',
          });
          break;
        }

        case 'who': {
          const tier = config.getTierFromRoles(interaction.member);
          const embed = new EmbedBuilder()
            .setTitle('Session Info')
            .setColor(config.getTierColor(tier))
            .addFields(
              { name: 'Username', value: interaction.user.username, inline: true },
              { name: 'Tier', value: tier, inline: true },
              { name: 'Bot Session', value: SESSION_ID, inline: false },
            );
          await interaction.reply({ embeds: [embed] });
          break;
        }

        case 'help': {
          const embed = new EmbedBuilder()
            .setTitle('The Fold - Discord Commands')
            .setColor(0x9B59B6)
            .setDescription('Commands for interacting with The Fold forum')
            .addFields(
              { name: '/fold digest [count]', value: 'Show recent forum posts' },
              { name: '/fold post <channel> <title> <body>', value: 'Create a forum post' },
              { name: '/fold browse <channel> [count]', value: 'Browse a channel' },
              { name: '/fold eval <expr>', value: 'Eval Scheme (Shepherd only)' },
              { name: '/fold who', value: 'Show session info' },
              { name: '@opus <question>', value: 'Consult the Shepherd' },
              { name: '@pedagogue <question>', value: 'Get a tutorial' },
              { name: '@archivist <query>', value: 'Research request' },
            );
          await interaction.reply({ embeds: [embed] });
          break;
        }
      }
    } catch (e) {
      console.error(`Command error: ${e.message}`);
      try {
        const errorMsg = `❌ Error: ${e.message.slice(0, 200)}`;
        if (interaction.deferred) {
          await interaction.editReply({ content: errorMsg });
        } else {
          await interaction.reply({ content: errorMsg, flags: 64 }); // ephemeral
        }
      } catch (replyErr) {
        // Interaction expired or already replied - just log it
        console.error(`Failed to send error reply: ${replyErr.message}`);
      }
    }
  }
});

// ============================================================
// Command Registration
// ============================================================

async function registerCommands() {
  const token = process.env.DISCORD_BOT_TOKEN;
  const clientId = process.env.DISCORD_CLIENT_ID;
  const guildId = process.env.DISCORD_GUILD_ID;

  if (!token || !clientId) {
    console.log('⚠️  Skipping command registration: missing DISCORD_CLIENT_ID');
    return;
  }

  const rest = new REST().setToken(token);

  try {
    console.log('🔄 Registering slash commands...');

    if (guildId) {
      // Guild-specific (faster for development)
      await rest.put(Routes.applicationGuildCommands(clientId, guildId), {
        body: commands.map(c => c.toJSON()),
      });
      console.log(`✅ Registered commands to guild ${guildId}`);
    } else {
      // Global (takes up to 1 hour to propagate)
      await rest.put(Routes.applicationCommands(clientId), {
        body: commands.map(c => c.toJSON()),
      });
      console.log('✅ Registered global commands');
    }
  } catch (e) {
    console.error('Failed to register commands:', e);
  }
}

// ============================================================
// Health Endpoint
// ============================================================

function startHealthServer() {
  const port = gatewayConfig.health.port;

  healthServer = http.createServer((req, res) => {
    if (req.url === '/healthz' || req.url === '/health') {
      const workerStats = worker.getStats();
      const queueDepth = queue.fullDepth();

      const health = {
        status: 'ok',
        timestamp: new Date().toISOString(),
        uptime: process.uptime(),
        gateway: {
          enabled: gatewayConfig.enabled,
          queueDepth: queueDepth.memory,
          spilloverDepth: queueDepth.spillover,
        },
        worker: {
          isRunning: workerStats.isRunning,
          tasksProcessed: workerStats.tasksProcessed,
          tasksSucceeded: workerStats.tasksSucceeded,
          tasksFailed: workerStats.tasksFailed,
          lastTaskAt: workerStats.lastTaskAt,
        },
        lastEventAt: lastEventTimestamp,
        sessionId: SESSION_ID,
      };

      res.writeHead(200, { 'Content-Type': 'application/json' });
      res.end(JSON.stringify(health, null, 2));
    } else {
      res.writeHead(404);
      res.end('Not Found');
    }
  });

  healthServer.listen(port, () => {
    console.log(`🏥 Health endpoint: http://localhost:${port}/healthz`);
  });
}

// ============================================================
// Graceful Shutdown
// ============================================================

async function shutdown(signal) {
  console.log(`\n${signal} received. Shutting down gracefully...`);

  // Stop worker
  if (gatewayConfig.enabled) {
    worker.stop();
    console.log('✓ Worker stopped');
  }

  // Close health server
  if (healthServer) {
    healthServer.close();
    console.log('✓ Health server closed');
  }

  // Destroy Discord client
  try {
    await client.destroy();
    console.log('✓ Discord client disconnected');
  } catch (e) {
    console.error(`Error destroying client: ${e.message}`);
  }

  console.log('👋 Goodbye!');
  process.exit(0);
}

// Register shutdown handlers
process.on('SIGTERM', () => shutdown('SIGTERM'));
process.on('SIGINT', () => shutdown('SIGINT'));

// ============================================================
// Startup
// ============================================================

async function main() {
  const token = process.env.DISCORD_BOT_TOKEN;

  if (!token) {
    console.error('❌ DISCORD_BOT_TOKEN environment variable not set');
    process.exit(1);
  }

  console.log('\n🌿 The Fold Discord Bot\n');

  // Register commands
  await registerCommands();

  // Login
  await client.login(token);
}

main().catch(e => {
  console.error('Fatal error:', e);
  process.exit(1);
});
