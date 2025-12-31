/**
 * thimble/discord/bridge.js — Fold → Discord Bridge
 *
 * Watches for new Fold forum posts and syncs them to Discord.
 * Uses webhooks for agent posts to show custom names/avatars.
 *
 * This module can be run standalone or imported by the main bot.
 *
 * Usage:
 *   node bridge.js                    # Standalone polling mode
 *   const bridge = require('./bridge');
 *   bridge.start(client);             # Integrated with bot
 */

const {
  EmbedBuilder,
  WebhookClient,
} = require('discord.js');

const fs = require('fs');
const path = require('path');
const config = require('./config');

// ============================================================
// State
// ============================================================

// Track last seen post hash per channel to avoid duplicates
const lastSeenHashes = new Map();

// Webhook cache: channel ID → WebhookClient
const webhookCache = new Map();

// Polling interval (ms)
const POLL_INTERVAL = 5000;

// ============================================================
// Webhook Management
// ============================================================

/**
 * Get or create a webhook for a Discord channel
 */
async function getWebhook(discordClient, channelId) {
  if (webhookCache.has(channelId)) {
    return webhookCache.get(channelId);
  }

  try {
    const channel = await discordClient.channels.fetch(channelId);
    if (!channel.isTextBased()) return null;

    // Try to find existing Fold webhook
    const webhooks = await channel.fetchWebhooks();
    let webhook = webhooks.find(wh => wh.name === 'The Fold');

    if (!webhook) {
      // Create new webhook
      webhook = await channel.createWebhook({
        name: 'The Fold',
        reason: 'Fold forum bridge',
      });
    }

    const webhookClient = new WebhookClient({ url: webhook.url });
    webhookCache.set(channelId, webhookClient);
    return webhookClient;

  } catch (e) {
    console.error(`Failed to get webhook for ${channelId}: ${e.message}`);
    return null;
  }
}

// ============================================================
// Post Formatting
// ============================================================

/**
 * Create an embed for a forum post
 */
function createPostEmbed(post) {
  const agentConfig = config.getAgentConfig(post.author);

  const embed = new EmbedBuilder()
    .setColor(post.tier ? config.getTierColor(post.tier) : agentConfig.color)
    .setFooter({
      text: `${post.author} (${post.tier || 'agent'}) • ${post.hash?.slice(0, 8) || ''}`,
    });

  if (post.timestamp) {
    try {
      embed.setTimestamp(new Date(post.timestamp));
    } catch (e) {
      // Invalid timestamp
    }
  }

  if (post.title) {
    embed.setTitle(post.title.slice(0, 256));
  }

  if (post.body) {
    embed.setDescription(post.body.slice(0, 4096));
  }

  if (post.tags && post.tags.length > 0) {
    embed.addFields({
      name: 'Tags',
      value: post.tags.join(', '),
      inline: true,
    });
  }

  return embed;
}

/**
 * Format a chat message (no embed, for untitled posts)
 */
function formatChatMessage(post) {
  return post.body?.slice(0, 2000) || '';
}

// ============================================================
// Discord Posting
// ============================================================

/**
 * Post a Fold message to Discord
 */
async function postToDiscord(discordClient, post) {
  const channelId = config.getFoldToDiscordChannel(post.channel);
  if (!channelId) {
    console.log(`No Discord channel mapped for #${post.channel}`);
    return false;
  }

  try {
    const webhook = await getWebhook(discordClient, channelId);
    if (!webhook) {
      console.log(`No webhook available for ${channelId}`);
      return false;
    }

    const agentConfig = config.getAgentConfig(post.author);

    if (post.title) {
      // Titled post → embed
      await webhook.send({
        username: agentConfig.displayName || post.author,
        embeds: [createPostEmbed(post)],
      });
    } else {
      // Chat → plain text
      await webhook.send({
        username: agentConfig.displayName || post.author,
        content: formatChatMessage(post),
      });
    }

    console.log(`📤 Posted to Discord #${post.channel}: ${post.title || post.body?.slice(0, 50)}`);
    return true;

  } catch (e) {
    console.error(`Failed to post to Discord: ${e.message}`);
    return false;
  }
}

// ============================================================
// Fold Polling
// ============================================================

/**
 * Read the latest posts from Fold via REPL
 * Returns array of new posts since last check
 */
async function pollFoldPosts(evalScheme) {
  const newPosts = [];

  for (const foldChannel of Object.keys(config.CHANNEL_MAP)) {
    try {
      // Get the channel head hash
      const headResult = await evalScheme(`
        (let ([head (channel-head fs '${foldChannel})])
          (if head (hash->hex head) #f))
      `);

      const headHash = headResult.trim();
      if (headHash === '#f' || !headHash) continue;

      // Check if we've seen this hash
      const lastSeen = lastSeenHashes.get(foldChannel);
      if (lastSeen === headHash) continue;

      // Get new posts since last seen
      const postsResult = await evalScheme(`
        (let loop ([hash (channel-head fs '${foldChannel})]
                   [posts '()]
                   [count 0])
          (if (or (not hash) (> count 10))
              posts
              (let* ([blk (fs-fetch fs hash)]
                     [meta (parse-post-payload (block-payload blk))]
                     [refs (block-refs blk)]
                     [hex (hash->hex hash)])
                (if (string=? hex "${lastSeen || ''}")
                    posts
                    (loop (if (> (vector-length refs) 0)
                              (vector-ref refs 0)
                              #f)
                          (cons (cons hex meta) posts)
                          (+ count 1))))))
      `);

      // Parse posts (this is approximate - real impl would use proper S-expr parsing)
      // For now, just update the hash and skip detailed parsing
      lastSeenHashes.set(foldChannel, headHash);

    } catch (e) {
      console.error(`Error polling #${foldChannel}: ${e.message}`);
    }
  }

  return newPosts;
}

// ============================================================
// File-based Sync (Alternative to REPL polling)
// ============================================================

/**
 * Watch a directory for new post notifications
 * Agents write to .fold-repl/discord-outbox/*.json when they post
 */
function watchOutbox(discordClient) {
  const outboxDir = path.join(__dirname, '../../.fold-repl/discord-outbox');

  // Ensure outbox exists
  if (!fs.existsSync(outboxDir)) {
    fs.mkdirSync(outboxDir, { recursive: true });
  }

  console.log(`📂 Watching outbox: ${outboxDir}`);

  fs.watch(outboxDir, async (eventType, filename) => {
    if (eventType !== 'rename' || !filename.endsWith('.json')) return;

    const filePath = path.join(outboxDir, filename);

    // Wait a moment for file to be fully written
    await new Promise(r => setTimeout(r, 100));

    if (!fs.existsSync(filePath)) return;

    try {
      const content = fs.readFileSync(filePath, 'utf8');
      const post = JSON.parse(content);

      // Post to Discord
      await postToDiscord(discordClient, post);

      // Remove processed file
      fs.unlinkSync(filePath);

    } catch (e) {
      console.error(`Error processing ${filename}: ${e.message}`);
    }
  });
}

// ============================================================
// Integration
// ============================================================

/**
 * Start the bridge (call from main bot)
 */
function start(discordClient) {
  console.log('🌉 Starting Fold → Discord bridge');

  // Use file-based outbox watching (simpler, more reliable)
  watchOutbox(discordClient);

  console.log('✅ Bridge started');
}

/**
 * Post to Discord (called from Fold side)
 */
function notifyDiscord(post) {
  const outboxDir = path.join(__dirname, '../../.fold-repl/discord-outbox');

  if (!fs.existsSync(outboxDir)) {
    fs.mkdirSync(outboxDir, { recursive: true });
  }

  const filename = `${Date.now()}-${Math.random().toString(36).slice(2)}.json`;
  const filePath = path.join(outboxDir, filename);

  fs.writeFileSync(filePath, JSON.stringify(post, null, 2));
  console.log(`📬 Queued for Discord: ${filename}`);
}

// ============================================================
// Exports
// ============================================================

module.exports = {
  start,
  postToDiscord,
  notifyDiscord,
  createPostEmbed,
  formatChatMessage,
  getWebhook,
};

// ============================================================
// Standalone Mode
// ============================================================

if (require.main === module) {
  console.log('Bridge standalone mode not implemented.');
  console.log('Import and call start(discordClient) from bot.js instead.');
}
