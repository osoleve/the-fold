/**
 * shell/discord/bridge.js — Fold → Discord Bridge
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

// ====
// State
// ====

// Track last seen post hash per channel to avoid duplicates
const lastSeenHashes = new Map();

// Webhook cache: channel ID → WebhookClient
const webhookCache = new Map();

// Polling interval (ms)
const POLL_INTERVAL = 5000;

// ====
// Webhook Management
// ====

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

// ====
// Post Formatting
// ====

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

// ====
// Discord Posting
// ====

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
    const agentConfig = config.getAgentConfig(post.author);

    // Prepare attachment if present
    const { AttachmentBuilder } = require('discord.js');
    let files = [];
    if (post.attachment && post.attachment.path) {
      try {
        const attachment = new AttachmentBuilder(post.attachment.path, {
          name: post.attachment.name || path.basename(post.attachment.path),
          description: post.attachment.description
        });
        files.push(attachment);
      } catch (e) {
        console.error(`Failed to load attachment ${post.attachment.path}: ${e.message}`);
      }
    }

    if (webhook) {
      // Use webhook (allows custom username/avatar)
      if (post.title) {
        // Titled post → embed
        await webhook.send({
          username: agentConfig.displayName || post.author,
          embeds: [createPostEmbed(post)],
          files: files,
        });
      } else {
        // Chat → plain text
        await webhook.send({
          username: agentConfig.displayName || post.author,
          content: formatChatMessage(post),
          files: files,
        });
      }
      console.log(`📤 Posted to Discord #${post.channel} via webhook: ${post.title || post.body?.slice(0, 50)}${files.length > 0 ? ' (with attachment)' : ''}`);
    } else {
      // Fallback: post directly as bot (no custom username)
      console.log(`⚠️ Webhook unavailable for ${channelId}, using fallback`);
      const channel = await discordClient.channels.fetch(channelId);

      const displayName = agentConfig.displayName || post.author;
      const prefix = `**${displayName}:**\n`;

      if (post.title) {
        // Titled post → embed
        await channel.send({ embeds: [createPostEmbed(post)], files: files });
      } else {
        // Chat → plain text with username prefix
        await channel.send({ content: prefix + formatChatMessage(post), files: files });
      }
      console.log(`📤 Posted to Discord #${post.channel} via fallback: ${post.title || post.body?.slice(0, 50)}${files.length > 0 ? ' (with attachment)' : ''}`);
    }

    return true;

  } catch (e) {
    console.error(`Failed to post to Discord: ${e.message}`);
    return false;
  }
}

// ====
// Fold Polling
// ====

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

// ====
// File-based Sync (Alternative to REPL polling)
// ====

/**
 * Watch a directory for new post notifications
 * Agents write to .fold-repl/discord-outbox/*.json when they post
 *
 * Supported outbox message types:
 *   - Regular post: { channel, title?, body, author, tier }
 *   - Reply: { reply_to, body, author }
 *   - Reaction: { react_to, emoji }
 *   - Thread: { create_thread_from, thread_name, body, author }
 *   - DM: { dm_to, body, author }
 *   - Embed: { channel, embed, author, tier }
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
      const message = JSON.parse(content);

      // Dispatch based on message type
      await dispatchOutboxMessage(discordClient, message);

      // Remove processed file
      fs.unlinkSync(filePath);

    } catch (e) {
      console.error(`Error processing ${filename}: ${e.message}`);
    }
  });
}

/**
 * Dispatch an outbox message to the appropriate handler
 */
async function dispatchOutboxMessage(discordClient, message) {
  try {
    if (message.reply_to) {
      // Reply to a message
      await handleReply(discordClient, message);
    } else if (message.react_to) {
      // Add reaction to a message
      await handleReaction(discordClient, message);
    } else if (message.create_thread_from) {
      // Create a thread from a message
      await handleThread(discordClient, message);
    } else if (message.dm_to) {
      // Direct message to a user
      await handleDM(discordClient, message);
    } else if (message.embed) {
      // Post embed to channel
      await handleEmbed(discordClient, message);
    } else if (message.channel) {
      // Regular post/chat
      await postToDiscord(discordClient, message);
    } else {
      console.log('Unknown outbox message type:', Object.keys(message));
    }
  } catch (e) {
    console.error(`Failed to dispatch outbox message: ${e.message}`);
  }
}

/**
 * Handle a reply to an existing message
 */
async function handleReply(discordClient, message) {
  try {
    // Find the message to reply to
    // message.reply_to is a Discord message ID
    // We need to search channels for it (or it should include channel info)
    const channel = await findChannelWithMessage(discordClient, message.reply_to);
    if (!channel) {
      console.log(`Could not find message ${message.reply_to} to reply to`);
      return;
    }

    const originalMessage = await channel.messages.fetch(message.reply_to);
    const agentConfig = config.getAgentConfig(message.author);

    // Get webhook for the channel
    const webhook = await getWebhook(discordClient, channel.id);

    if (webhook) {
      // Use webhook with agent's display name
      await webhook.send({
        content: message.body?.slice(0, 2000) || '',
        username: agentConfig.displayName,
        avatarURL: message.avatarURL || undefined,  // Optional: use agent-specific avatar
        // Note: Webhooks can't use Discord's native reply feature,
        // but the context is usually clear from conversation flow
      });
      console.log(`↩️ ${agentConfig.displayName} replied to message ${message.reply_to} via webhook`);
    } else {
      // Fallback: use native Discord reply (with bot username)
      console.log(`⚠️ Webhook unavailable, using fallback reply`);
      const displayName = agentConfig.displayName || message.author;
      const prefix = `**${displayName}:**\n`;
      await originalMessage.reply(prefix + (message.body?.slice(0, 2000) || ''));
      console.log(`↩️ ${displayName} replied to message ${message.reply_to} via fallback`);
    }
  } catch (e) {
    console.error(`Failed to reply: ${e.message}`);
  }
}

/**
 * Handle adding a reaction to a message
 */
async function handleReaction(discordClient, message) {
  try {
    const channel = await findChannelWithMessage(discordClient, message.react_to);
    if (!channel) {
      console.log(`Could not find message ${message.react_to} to react to`);
      return;
    }

    const targetMessage = await channel.messages.fetch(message.react_to);
    await targetMessage.react(message.emoji);

    console.log(`😀 Added reaction ${message.emoji} to message ${message.react_to}`);
  } catch (e) {
    console.error(`Failed to react: ${e.message}`);
  }
}

/**
 * Handle creating a thread from a message
 */
async function handleThread(discordClient, message) {
  try {
    const channel = await findChannelWithMessage(discordClient, message.create_thread_from);
    if (!channel) {
      console.log(`Could not find message ${message.create_thread_from} to create thread from`);
      return;
    }

    const parentMessage = await channel.messages.fetch(message.create_thread_from);

    // Create thread
    const thread = await parentMessage.startThread({
      name: message.thread_name?.slice(0, 100) || 'Discussion',
      autoArchiveDuration: 60, // 1 hour
    });

    // Send first message in thread
    if (message.body) {
      const agentConfig = config.getAgentConfig(message.author);
      await thread.send({
        content: message.body.slice(0, 2000),
      });
    }

    console.log(`🧵 Created thread "${message.thread_name}" from message ${message.create_thread_from}`);
  } catch (e) {
    console.error(`Failed to create thread: ${e.message}`);
  }
}

/**
 * Handle sending a DM to a user
 */
async function handleDM(discordClient, message) {
  try {
    const user = await discordClient.users.fetch(message.dm_to);
    if (!user) {
      console.log(`Could not find user ${message.dm_to} to DM`);
      return;
    }

    await user.send({
      content: message.body?.slice(0, 2000) || '',
    });

    console.log(`📨 Sent DM to user ${message.dm_to}`);
  } catch (e) {
    console.error(`Failed to send DM: ${e.message}`);
  }
}

/**
 * Handle posting an embed to a channel
 */
async function handleEmbed(discordClient, message) {
  const channelId = config.getFoldToDiscordChannel(message.channel);
  if (!channelId) {
    console.log(`No Discord channel mapped for #${message.channel}`);
    return;
  }

  try {
    const webhook = await getWebhook(discordClient, channelId);
    if (!webhook) return;

    const agentConfig = config.getAgentConfig(message.author);
    const embedSpec = message.embed;

    // Build embed from spec
    const embed = new EmbedBuilder();

    if (embedSpec.title) embed.setTitle(embedSpec.title.slice(0, 256));
    if (embedSpec.description) embed.setDescription(embedSpec.description.slice(0, 4096));
    if (embedSpec.color) embed.setColor(embedSpec.color);
    if (embedSpec.url) embed.setURL(embedSpec.url);
    if (embedSpec.timestamp) embed.setTimestamp(new Date(embedSpec.timestamp));

    if (embedSpec.footer) {
      embed.setFooter({ text: embedSpec.footer.slice(0, 2048) });
    }

    if (embedSpec.fields && Array.isArray(embedSpec.fields)) {
      for (const field of embedSpec.fields.slice(0, 25)) {
        embed.addFields({
          name: (field.name || 'Field').slice(0, 256),
          value: (field.value || '-').slice(0, 1024),
          inline: !!field.inline,
        });
      }
    }

    await webhook.send({
      username: agentConfig.displayName || message.author,
      embeds: [embed],
    });

    console.log(`📤 Posted embed to Discord #${message.channel}`);
  } catch (e) {
    console.error(`Failed to post embed: ${e.message}`);
  }
}

/**
 * Find the channel containing a specific message ID
 * This is a helper for replies/reactions/threads
 */
async function findChannelWithMessage(discordClient, messageId) {
  // Try each mapped channel
  for (const channelId of Object.values(config.CHANNEL_MAP)) {
    try {
      const channel = await discordClient.channels.fetch(channelId);
      if (!channel?.isTextBased()) continue;

      // Try to fetch the message
      const msg = await channel.messages.fetch(messageId);
      if (msg) return channel;
    } catch (e) {
      // Message not in this channel, continue
    }
  }
  return null;
}

// ====
// Integration
// ====

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

// ====
// Exports
// ====

module.exports = {
  start,
  postToDiscord,
  notifyDiscord,
  createPostEmbed,
  formatChatMessage,
  getWebhook,
  // New handlers for pipeline integration
  dispatchOutboxMessage,
  handleReply,
  handleReaction,
  handleThread,
  handleDM,
  handleEmbed,
  findChannelWithMessage,
};

// ====
// Standalone Mode
// ====

if (require.main === module) {
  console.log('Bridge standalone mode not implemented.');
  console.log('Import and call start(discordClient) from bot.js instead.');
}
