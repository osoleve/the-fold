/**
 * shell/discord/gateway-config.js — Gateway Configuration
 *
 * Centralized configuration for the Gateway-driven Discord integration.
 * Loads from environment variables or config/discord-agents.json.
 */

const fs = require('fs');
const path = require('path');

// ====
// Load Configuration
// ====

/**
 * Load agent config from JSON file if it exists
 */
function loadAgentConfig() {
  const configPath = path.join(__dirname, '../../config/discord-agents.json');
  if (fs.existsSync(configPath)) {
    try {
      return JSON.parse(fs.readFileSync(configPath, 'utf8'));
    } catch (e) {
      console.error(`Failed to load agent config: ${e.message}`);
    }
  }
  return null;
}

const fileConfig = loadAgentConfig();

// ====
// Configuration
// ====

const config = {
  // Feature flag - enable/disable gateway dispatch
  enabled: process.env.GATEWAY_ENABLED !== 'false',

  // Agent identity map: agent name → pipeline configuration
  agents: fileConfig?.agents || {
    opus: {
      discordUserId: null,  // Set if agent has its own Discord account
      pipelinePath: 'agents/invoke-opus.js',
      model: 'opus',
    },
    pedagogue: {
      discordUserId: null,
      pipelinePath: 'agents/invoke-pedagogue.js',
      model: 'sonnet',
    },
    archivist: {
      discordUserId: null,
      pipelinePath: 'agents/invoke-archivist.js',
      model: 'sonnet',
    },
    sonnet: {
      discordUserId: null,
      pipelinePath: 'agents/invoke-sonnet.js',
      model: 'sonnet',
    },
    haiku: {
      discordUserId: null,
      pipelinePath: 'agents/invoke-haiku.js',
      model: 'haiku',
    },
  },

  // List of agent names that can be @mentioned for consultation
  consultationAgents: fileConfig?.consultationAgents || [
    'opus', 'pedagogue', 'archivist', 'sonnet', 'haiku'
  ],

  // Channel allowlist (null = all mapped channels allowed)
  allowedChannels: fileConfig?.allowedChannels || null,

  // Anti-loop parameters
  antiloop: {
    // Maximum agent turns per thread before blocking
    maxThreadDepth: parseInt(process.env.ANTILOOP_MAX_DEPTH, 10) ||
      fileConfig?.antiloop?.maxThreadDepth || 10,

    // Message budget per agent (count, not tokens)
    messageBudget: {
      hourly: parseInt(process.env.ANTILOOP_HOURLY_LIMIT, 10) ||
        fileConfig?.antiloop?.messageBudget?.hourly || 20,
      daily: parseInt(process.env.ANTILOOP_DAILY_LIMIT, 10) ||
        fileConfig?.antiloop?.messageBudget?.daily || 100,
    },

    // Circuit breaker: pause if too many messages in short window
    circuitBreaker: {
      threshold: parseInt(process.env.ANTILOOP_CIRCUIT_THRESHOLD, 10) ||
        fileConfig?.antiloop?.circuitBreaker?.threshold || 5,
      windowMs: parseInt(process.env.ANTILOOP_CIRCUIT_WINDOW, 10) ||
        fileConfig?.antiloop?.circuitBreaker?.windowMs || 30000,
    },

    // Allow agent-to-agent communication (controlled by depth/circuit breaker)
    agentToAgentAllowed: process.env.ANTILOOP_AGENT_TO_AGENT !== 'false' &&
      (fileConfig?.antiloop?.agentToAgentAllowed !== false),
  },

  // Queue parameters
  queue: {
    // Max items in memory before spillover to disk
    memoryLimit: parseInt(process.env.QUEUE_MEMORY_LIMIT, 10) ||
      fileConfig?.queue?.memoryLimit || 100,

    // Path for disk spillover (relative to project root)
    spilloverPath: fileConfig?.queue?.spilloverPath ||
      'state/discord-queue-spillover.jsonl',
  },

  // Worker parameters
  worker: {
    // How often to poll the queue (ms)
    pollIntervalMs: parseInt(process.env.WORKER_POLL_INTERVAL, 10) ||
      fileConfig?.worker?.pollIntervalMs || 100,

    // Timeout for pipeline execution (ms)
    timeoutMs: parseInt(process.env.WORKER_TIMEOUT, 10) ||
      fileConfig?.worker?.timeoutMs || 120000,
  },

  // Health endpoint
  health: {
    port: parseInt(process.env.HEALTH_PORT, 10) || 8081,
    enabled: process.env.HEALTH_ENABLED !== 'false',
  },

  // State file paths
  paths: {
    stateDir: path.join(__dirname, '../../state'),
    antiloopState: 'state/discord-antiloop.json',
  },
};

// ====
// Helper Functions
// ====

/**
 * Check if an agent is configured for consultation
 */
function isConsultationAgent(agentName) {
  return config.consultationAgents.includes(agentName);
}

/**
 * Get agent configuration by name
 */
function getAgentConfig(agentName) {
  return config.agents[agentName] || null;
}

/**
 * Check if a channel is allowed for gateway dispatch
 */
function isChannelAllowed(channelId) {
  if (!config.allowedChannels) return true;  // All channels allowed
  return config.allowedChannels.includes(channelId);
}

/**
 * Get the full path for a relative state file
 */
function getStatePath(relativePath) {
  return path.join(__dirname, '../..', relativePath);
}

// ====
// Exports
// ====

module.exports = {
  ...config,
  isConsultationAgent,
  getAgentConfig,
  isChannelAllowed,
  getStatePath,
};
