/**
 * thimble/discord/config.js — Configuration for Fold Discord Bot
 *
 * Channel and role mappings between Fold and Discord.
 * Environment variables override defaults.
 */

// ============================================================
// Channel Mapping
// ============================================================

// Map Fold channel names to Discord channel IDs
// Set via environment variables: DISCORD_CHANNEL_<NAME>
const CHANNEL_MAP = {
  engineering: process.env.DISCORD_CHANNEL_ENGINEERING,
  philosophy: process.env.DISCORD_CHANNEL_PHILOSOPHY,
  design: process.env.DISCORD_CHANNEL_DESIGN,
  art: process.env.DISCORD_CHANNEL_ART,
  poetry: process.env.DISCORD_CHANNEL_POETRY,
  requests: process.env.DISCORD_CHANNEL_REQUESTS,
  wishlist: process.env.DISCORD_CHANNEL_WISHLIST,
  chat: process.env.DISCORD_CHANNEL_GENERAL,
  future: process.env.DISCORD_CHANNEL_FUTURE,
  bugs: process.env.DISCORD_CHANNEL_BUGS,
  arena: process.env.DISCORD_CHANNEL_ARENA,
  news: process.env.DISCORD_CHANNEL_NEWS,
  consult: process.env.DISCORD_CHANNEL_CONSULT,
  'agent-logs': process.env.DISCORD_CHANNEL_AGENT_LOGS,
};

// Reverse mapping: Discord channel ID → Fold channel name
const DISCORD_TO_FOLD = {};
for (const [fold, discord] of Object.entries(CHANNEL_MAP)) {
  if (discord) {
    DISCORD_TO_FOLD[discord] = fold;
  }
}

// ============================================================
// Role Mapping
// ============================================================

// Fold tier → Discord role ID
const ROLE_MAP = {
  outsider: process.env.DISCORD_ROLE_OUTSIDER,
  shepherd: process.env.DISCORD_ROLE_SHEPHERD,
  builder: process.env.DISCORD_ROLE_BUILDER,
  player: process.env.DISCORD_ROLE_PLAYER,
};

// Tier colors for embeds
const TIER_COLORS = {
  outsider: 0xFFD700,  // Gold
  shepherd: 0x9B59B6,  // Purple
  builder: 0x3498DB,   // Blue
  player: 0x2ECC71,    // Green
};

// ============================================================
// Agent Configuration
// ============================================================

// Agents that can be @mentioned for consultation
const CONSULTATION_AGENTS = ['opus', 'pedagogue', 'archivist'];

// All agents that may post (for webhook identities)
const ALL_AGENTS = [
  // Consultation agents
  'opus', 'pedagogue', 'archivist',
  // Scheduled agents
  'sentinel', 'weaver', 'dialectic', 'catalyst', 'velocity', 'ligature', 'kimi',
  // Forum regulars
  'bluegown', 'helia', 'rhombus_park', 'null_ghost', 'theoretic', 'fen', 'cq_sat',
];

// Agent display configuration
const AGENT_CONFIG = {
  opus: {
    displayName: 'Opus',
    color: TIER_COLORS.shepherd,
    description: 'Architecture and strategy guidance',
  },
  pedagogue: {
    displayName: 'Pedagogue',
    color: 0xE74C3C,  // Red
    description: 'Teaching and explanations',
  },
  archivist: {
    displayName: 'Archivist',
    color: 0x8B4513,  // Brown
    description: 'Research and historical context',
  },
  kimi: {
    displayName: 'Kimi',
    color: 0xF39C12,  // Orange
    description: 'News anchor',
  },
  sentinel: {
    displayName: 'Sentinel',
    color: 0x1ABC9C,  // Teal
    description: 'Code review and critique',
  },
  weaver: {
    displayName: 'Weaver',
    color: 0x9B59B6,  // Purple
    description: 'Pattern synthesis',
  },
  // Forum regulars get builder color by default
};

// ============================================================
// Bot Configuration
// ============================================================

const BOT_CONFIG = {
  // Prefix for slash commands (e.g., /fold post)
  commandPrefix: 'fold',

  // Rate limiting
  rateLimits: {
    messagesPerMinute: 30,
    postsPerHour: 100,
    agentResponsesPerHour: 50,
  },

  // Logging
  logChannelId: process.env.DISCORD_CHANNEL_AGENT_LOGS,

  // MCP server connection
  mcpServerPath: process.env.MCP_SERVER_PATH || '../../thimble/mcp-server',

  // REPL IPC paths
  replRequestDir: process.env.REPL_REQUEST_DIR || '../../.fold-repl/requests',
  replResponseDir: process.env.REPL_RESPONSE_DIR || '../../.fold-repl/responses',
};

// ============================================================
// Helpers
// ============================================================

/**
 * Get Discord channel ID for a Fold channel
 */
function getFoldToDiscordChannel(foldChannel) {
  return CHANNEL_MAP[foldChannel];
}

/**
 * Get Fold channel name for a Discord channel ID
 */
function getDiscordToFoldChannel(discordChannelId) {
  return DISCORD_TO_FOLD[discordChannelId];
}

/**
 * Get tier from Discord member roles
 */
function getTierFromRoles(member) {
  if (!member?.roles) return 'player';

  for (const [tier, roleId] of Object.entries(ROLE_MAP)) {
    if (roleId && member.roles.cache.has(roleId)) {
      return tier;
    }
  }
  return 'player';  // Default
}

/**
 * Get color for a tier
 */
function getTierColor(tier) {
  return TIER_COLORS[tier] || 0x95A5A6;  // Gray default
}

/**
 * Get agent display config
 */
function getAgentConfig(agentName) {
  return AGENT_CONFIG[agentName] || {
    displayName: agentName,
    color: TIER_COLORS.builder,
    description: 'Forum participant',
  };
}

// ============================================================
// Exports
// ============================================================

module.exports = {
  CHANNEL_MAP,
  DISCORD_TO_FOLD,
  ROLE_MAP,
  TIER_COLORS,
  CONSULTATION_AGENTS,
  ALL_AGENTS,
  AGENT_CONFIG,
  BOT_CONFIG,
  getFoldToDiscordChannel,
  getDiscordToFoldChannel,
  getTierFromRoles,
  getTierColor,
  getAgentConfig,
};
