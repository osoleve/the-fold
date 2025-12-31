#!/usr/bin/env node
/**
 * scripts/seed-discord.js — Seed Discord channels with historical forum posts
 *
 * This script imports exported forum posts into Discord channels.
 * Posts are sent in chronological order (oldest first) to preserve history.
 *
 * Prerequisites:
 *   1. Export posts: ./fold.sh "(load \"scripts/export-for-discord.ss\")"
 *                    ./fold.sh "(export-all-channels \"./exports\")"
 *   2. Set DISCORD_BOT_TOKEN environment variable
 *   3. Configure CHANNEL_MAP below with your Discord channel IDs
 *
 * Usage:
 *   node scripts/seed-discord.js [--dry-run] [--channel=engineering]
 */

const { Client, GatewayIntentBits, EmbedBuilder } = require('discord.js');
const fs = require('fs');
const path = require('path');

// ============================================================
// Configuration
// ============================================================

// Map Fold channel names to Discord channel IDs
// TODO: Fill in your Discord channel IDs
const CHANNEL_MAP = {
  engineering: process.env.DISCORD_CHANNEL_ENGINEERING || 'CHANNEL_ID_HERE',
  philosophy: process.env.DISCORD_CHANNEL_PHILOSOPHY || 'CHANNEL_ID_HERE',
  design: process.env.DISCORD_CHANNEL_DESIGN || 'CHANNEL_ID_HERE',
  art: process.env.DISCORD_CHANNEL_ART || 'CHANNEL_ID_HERE',
  poetry: process.env.DISCORD_CHANNEL_POETRY || 'CHANNEL_ID_HERE',
  requests: process.env.DISCORD_CHANNEL_REQUESTS || 'CHANNEL_ID_HERE',
  wishlist: process.env.DISCORD_CHANNEL_WISHLIST || 'CHANNEL_ID_HERE',
  chat: process.env.DISCORD_CHANNEL_GENERAL || 'CHANNEL_ID_HERE',
  future: process.env.DISCORD_CHANNEL_FUTURE || 'CHANNEL_ID_HERE',
};

// Tier colors for embeds
const TIER_COLORS = {
  outsider: 0xFFD700,  // Gold
  shepherd: 0x9B59B6,  // Purple
  builder: 0x3498DB,   // Blue
  player: 0x2ECC71,    // Green
};

// Rate limiting: milliseconds between messages
const MESSAGE_DELAY_MS = 600;

// Export directory
const EXPORTS_DIR = path.join(__dirname, '..', 'exports');

// ============================================================
// Utilities
// ============================================================

function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

function truncate(str, maxLen) {
  if (!str) return '';
  if (str.length <= maxLen) return str;
  return str.slice(0, maxLen - 3) + '...';
}

function formatTimestamp(isoString) {
  if (!isoString) return 'unknown';
  return isoString.split('T')[0];  // Just the date part
}

// ============================================================
// Post Formatting
// ============================================================

/**
 * Create a Discord embed for a titled post
 */
function createEmbed(post) {
  const embed = new EmbedBuilder()
    .setColor(TIER_COLORS[post.tier] || 0x95A5A6)
    .setFooter({
      text: `${post.author} (${post.tier}) • ${post.hash?.slice(0, 8) || 'no-hash'}`
    });

  // Set timestamp if valid
  if (post.timestamp) {
    try {
      embed.setTimestamp(new Date(post.timestamp));
    } catch (e) {
      // Invalid timestamp, skip
    }
  }

  // Title
  if (post.title) {
    embed.setTitle(truncate(post.title, 256));
  }

  // Body - Discord embeds have a 4096 char limit for description
  if (post.body) {
    embed.setDescription(truncate(post.body, 4096));
  }

  // Tags as field
  if (post.tags && post.tags.length > 0) {
    embed.addFields({
      name: 'Tags',
      value: post.tags.join(', '),
      inline: true
    });
  }

  return embed;
}

/**
 * Format a chat message (no embed)
 */
function formatChatMessage(post) {
  const date = formatTimestamp(post.timestamp);
  const author = post.author || 'unknown';
  const body = truncate(post.body || '', 1900);  // Leave room for prefix
  return `**${author}** _(${date})_\n${body}`;
}

// ============================================================
// Seeding Logic
// ============================================================

/**
 * Seed a single channel with posts
 */
async function seedChannel(client, foldChannel, posts, options = {}) {
  const { dryRun = false } = options;

  const channelId = CHANNEL_MAP[foldChannel];
  if (!channelId || channelId === 'CHANNEL_ID_HERE') {
    console.log(`  ⚠️  Skipping #${foldChannel}: no channel ID configured`);
    return { sent: 0, skipped: posts.length };
  }

  let discordChannel;
  try {
    discordChannel = await client.channels.fetch(channelId);
  } catch (e) {
    console.log(`  ❌ Failed to fetch channel ${channelId}: ${e.message}`);
    return { sent: 0, skipped: posts.length, error: e.message };
  }

  console.log(`  📤 Seeding #${foldChannel} (${posts.length} posts)...`);

  let sent = 0;
  let skipped = 0;
  const hashToMessageId = new Map();  // For threading

  for (const post of posts) {
    try {
      if (dryRun) {
        const preview = post.title || truncate(post.body || '', 50);
        console.log(`    [DRY RUN] Would send: ${preview}`);
        sent++;
        continue;
      }

      // Determine message format
      let message;
      if (post.title) {
        // Titled posts use embeds
        message = await discordChannel.send({ embeds: [createEmbed(post)] });
      } else {
        // Chat messages are plain text
        message = await discordChannel.send(formatChatMessage(post));
      }

      // Track message ID for threading
      if (post.hash) {
        hashToMessageId.set(post.hash, message.id);
      }

      sent++;

      // Progress indicator every 10 posts
      if (sent % 10 === 0) {
        console.log(`    ✓ ${sent}/${posts.length} posts sent`);
      }

      // Rate limiting
      await sleep(MESSAGE_DELAY_MS);

    } catch (e) {
      console.log(`    ⚠️  Failed to send post: ${e.message}`);
      skipped++;
    }
  }

  console.log(`  ✅ #${foldChannel}: ${sent} sent, ${skipped} skipped`);
  return { sent, skipped };
}

/**
 * Load posts from export file
 */
function loadExportFile(channel) {
  const filePath = path.join(EXPORTS_DIR, `${channel}.json`);
  if (!fs.existsSync(filePath)) {
    return null;
  }
  try {
    const data = fs.readFileSync(filePath, 'utf8');
    return JSON.parse(data);
  } catch (e) {
    console.error(`Failed to load ${filePath}: ${e.message}`);
    return null;
  }
}

/**
 * Main seeding function
 */
async function seedAll(options = {}) {
  const { dryRun = false, channels = null } = options;

  console.log('\n🌱 Discord Seeding Script\n');
  console.log(`Mode: ${dryRun ? 'DRY RUN (no messages sent)' : 'LIVE'}`);
  console.log(`Export dir: ${EXPORTS_DIR}\n`);

  // Check for token
  const token = process.env.DISCORD_BOT_TOKEN;
  if (!token && !dryRun) {
    console.error('❌ DISCORD_BOT_TOKEN environment variable not set');
    console.error('   Set it or use --dry-run to preview');
    process.exit(1);
  }

  // Determine which channels to seed
  const channelsToSeed = channels || Object.keys(CHANNEL_MAP);

  // Load all export files
  const exports = {};
  let totalPosts = 0;
  for (const channel of channelsToSeed) {
    const posts = loadExportFile(channel);
    if (posts) {
      exports[channel] = posts;
      totalPosts += posts.length;
      console.log(`  📂 ${channel}: ${posts.length} posts`);
    } else {
      console.log(`  ⚠️  ${channel}: no export file found`);
    }
  }

  console.log(`\nTotal: ${totalPosts} posts to seed\n`);

  if (totalPosts === 0) {
    console.log('Nothing to seed. Run the export script first:');
    console.log('  ./fold.sh "(load \\"scripts/export-for-discord.ss\\")"');
    console.log('  ./fold.sh "(export-all-channels \\"./exports\\")"');
    return;
  }

  // Connect to Discord (unless dry run)
  let client = null;
  if (!dryRun) {
    console.log('🔌 Connecting to Discord...');
    client = new Client({
      intents: [GatewayIntentBits.Guilds, GatewayIntentBits.GuildMessages]
    });

    await client.login(token);
    console.log(`✅ Logged in as ${client.user.tag}\n`);
  }

  // Seed each channel
  const results = {};
  for (const [channel, posts] of Object.entries(exports)) {
    results[channel] = await seedChannel(client, channel, posts, { dryRun });
  }

  // Summary
  console.log('\n📊 Seeding Summary:\n');
  let grandTotal = { sent: 0, skipped: 0 };
  for (const [channel, result] of Object.entries(results)) {
    console.log(`  #${channel}: ${result.sent} sent, ${result.skipped} skipped`);
    grandTotal.sent += result.sent;
    grandTotal.skipped += result.skipped;
  }
  console.log(`\n  Total: ${grandTotal.sent} sent, ${grandTotal.skipped} skipped\n`);

  // Cleanup
  if (client) {
    client.destroy();
    console.log('🔌 Disconnected from Discord');
  }

  console.log('\n✨ Seeding complete!\n');
}

// ============================================================
// CLI
// ============================================================

function parseArgs(args) {
  const options = {
    dryRun: false,
    channels: null,
  };

  for (const arg of args) {
    if (arg === '--dry-run') {
      options.dryRun = true;
    } else if (arg.startsWith('--channel=')) {
      const channel = arg.split('=')[1];
      options.channels = options.channels || [];
      options.channels.push(channel);
    } else if (arg === '--help' || arg === '-h') {
      console.log(`
Discord Seeding Script

Usage:
  node seed-discord.js [options]

Options:
  --dry-run           Preview without sending messages
  --channel=NAME      Seed only specific channel (can repeat)
  --help, -h          Show this help

Environment:
  DISCORD_BOT_TOKEN   Required for live mode
  DISCORD_CHANNEL_*   Channel IDs (e.g., DISCORD_CHANNEL_ENGINEERING)

Examples:
  node seed-discord.js --dry-run
  node seed-discord.js --channel=engineering --channel=philosophy
  DISCORD_BOT_TOKEN=xxx node seed-discord.js
`);
      process.exit(0);
    }
  }

  return options;
}

// Run if called directly
if (require.main === module) {
  const options = parseArgs(process.argv.slice(2));
  seedAll(options).catch(e => {
    console.error('Fatal error:', e);
    process.exit(1);
  });
}

module.exports = { seedAll, seedChannel, createEmbed, formatChatMessage };
