/**
 * shell/discord/dispatcher.js — Mention Routing & Anti-Loop Enforcement
 *
 * Parses @mentions to resolve agent identity and enforces anti-loop policies
 * to prevent runaway agent conversations.
 *
 * Policies:
 * - Thread depth limit: Max N agent turns per thread
 * - Message budget: X messages/hr, Y messages/day per agent
 * - Circuit breaker: Pause if >N replies in M seconds in same thread
 */

const fs = require('fs');
const path = require('path');
const gatewayConfig = require('./gateway-config');

// ====
// State Management
// ====

/**
 * Load anti-loop state from disk
 * @returns {AntiLoopState}
 */
function loadState() {
  const statePath = gatewayConfig.getStatePath(gatewayConfig.paths.antiloopState);

  if (fs.existsSync(statePath)) {
    try {
      const data = JSON.parse(fs.readFileSync(statePath, 'utf8'));
      return {
        threadDepths: data.threadDepths || {},
        messageBudgets: data.messageBudgets || {},
        circuitBreakers: data.circuitBreakers || {},
      };
    } catch (e) {
      console.error(`Failed to load anti-loop state: ${e.message}`);
    }
  }

  return {
    threadDepths: {},
    messageBudgets: {},
    circuitBreakers: {},
  };
}

/**
 * Save anti-loop state to disk
 * @param {AntiLoopState} state
 */
function saveState(state) {
  const statePath = gatewayConfig.getStatePath(gatewayConfig.paths.antiloopState);
  const stateDir = path.dirname(statePath);

  // Ensure state directory exists
  if (!fs.existsSync(stateDir)) {
    fs.mkdirSync(stateDir, { recursive: true });
  }

  try {
    fs.writeFileSync(statePath, JSON.stringify(state, null, 2));
  } catch (e) {
    console.error(`Failed to save anti-loop state: ${e.message}`);
  }
}

// ====
// Mention Parsing
// ====

/**
 * Parse a message to extract agent mention
 * @param {string} content - Message content
 * @returns {string|null} - Agent name or null if no agent mentioned
 */
function parseAgentMention(content) {
  const lowerContent = content.toLowerCase();

  for (const agent of gatewayConfig.consultationAgents) {
    // Match @agent (with word boundary to avoid partial matches)
    const pattern = new RegExp(`@${agent}\\b`, 'i');
    if (pattern.test(lowerContent)) {
      return agent;
    }
  }

  return null;
}

// ====
// Anti-Loop Policy Checks
// ====

/**
 * Check thread depth limit
 * @param {AntiLoopState} state
 * @param {string} threadId
 * @param {boolean} isBot - Is the message from a bot?
 * @returns {{ allowed: boolean, reason?: string, currentDepth: number }}
 */
function checkThreadDepth(state, threadId, isBot) {
  const maxDepth = gatewayConfig.antiloop.maxThreadDepth;
  const currentDepth = state.threadDepths[threadId] || 0;

  // Human messages reset depth
  if (!isBot) {
    return { allowed: true, currentDepth: 0 };
  }

  // Bot messages increment depth
  if (currentDepth >= maxDepth) {
    return {
      allowed: false,
      reason: `Thread depth limit reached (${currentDepth}/${maxDepth})`,
      currentDepth,
    };
  }

  return { allowed: true, currentDepth };
}

/**
 * Check message budget for an agent
 * @param {AntiLoopState} state
 * @param {string} agentId
 * @returns {{ allowed: boolean, reason?: string, hourlyCount: number, dailyCount: number }}
 */
function checkMessageBudget(state, agentId) {
  const { hourly, daily } = gatewayConfig.antiloop.messageBudget;
  const now = Date.now();

  // Get or initialize budget state for this agent
  let budget = state.messageBudgets[agentId];
  if (!budget) {
    budget = {
      hourlyCount: 0,
      dailyCount: 0,
      hourlyResetAt: now + 3600000,  // 1 hour
      dailyResetAt: now + 86400000,  // 24 hours
    };
    state.messageBudgets[agentId] = budget;
  }

  // Reset counters if window has passed
  if (now >= budget.hourlyResetAt) {
    budget.hourlyCount = 0;
    budget.hourlyResetAt = now + 3600000;
  }
  if (now >= budget.dailyResetAt) {
    budget.dailyCount = 0;
    budget.dailyResetAt = now + 86400000;
  }

  // Check limits
  if (budget.hourlyCount >= hourly) {
    const resetIn = Math.ceil((budget.hourlyResetAt - now) / 60000);
    return {
      allowed: false,
      reason: `Hourly message limit reached (${hourly}/hr). Resets in ${resetIn} minutes.`,
      hourlyCount: budget.hourlyCount,
      dailyCount: budget.dailyCount,
    };
  }

  if (budget.dailyCount >= daily) {
    const resetIn = Math.ceil((budget.dailyResetAt - now) / 3600000);
    return {
      allowed: false,
      reason: `Daily message limit reached (${daily}/day). Resets in ${resetIn} hours.`,
      hourlyCount: budget.hourlyCount,
      dailyCount: budget.dailyCount,
    };
  }

  return {
    allowed: true,
    hourlyCount: budget.hourlyCount,
    dailyCount: budget.dailyCount,
  };
}

/**
 * Check circuit breaker for a thread
 * @param {AntiLoopState} state
 * @param {string} threadId
 * @returns {{ allowed: boolean, reason?: string, count: number }}
 */
function checkCircuitBreaker(state, threadId) {
  const { threshold, windowMs } = gatewayConfig.antiloop.circuitBreaker;
  const now = Date.now();

  // Get or initialize circuit breaker state
  let breaker = state.circuitBreakers[threadId];
  if (!breaker) {
    breaker = { count: 0, windowStart: now };
    state.circuitBreakers[threadId] = breaker;
  }

  // Reset if window has passed
  if (now - breaker.windowStart > windowMs) {
    breaker.count = 0;
    breaker.windowStart = now;
  }

  // Check threshold
  if (breaker.count >= threshold) {
    const resetIn = Math.ceil((breaker.windowStart + windowMs - now) / 1000);
    return {
      allowed: false,
      reason: `Too many messages in this thread (${breaker.count} in ${windowMs / 1000}s). Cooling down for ${resetIn}s.`,
      count: breaker.count,
    };
  }

  return { allowed: true, count: breaker.count };
}

/**
 * Check if agent-to-agent communication is allowed
 * @param {boolean} isBot
 * @returns {{ allowed: boolean, reason?: string }}
 */
function checkAgentToAgent(isBot) {
  if (isBot && !gatewayConfig.antiloop.agentToAgentAllowed) {
    return {
      allowed: false,
      reason: 'Agent-to-agent communication is disabled',
    };
  }
  return { allowed: true };
}

/**
 * Check if this is a self-mention (agent tagging itself)
 * @param {string} authorName - Name of the message author
 * @param {string} agentId - Target agent being mentioned
 * @returns {{ allowed: boolean, reason?: string }}
 */
function checkSelfMention(authorName, agentId) {
  // Normalize names for comparison
  const normalizedAuthor = authorName.toLowerCase().replace(/[^a-z]/g, '');
  const normalizedAgent = agentId.toLowerCase();

  // Check if author name contains the agent name (e.g., "Haiku" mentioning "@haiku")
  if (normalizedAuthor.includes(normalizedAgent) || normalizedAgent.includes(normalizedAuthor)) {
    return {
      allowed: false,
      reason: `Self-mention blocked: ${authorName} cannot tag @${agentId}`,
    };
  }
  return { allowed: true };
}

// ====
// Main Dispatch Check
// ====

/**
 * Check if a message should be dispatched to an agent
 * @param {Object} message - Discord message object (or mock)
 * @param {string} agentId - Target agent name
 * @param {AntiLoopState} state - Current anti-loop state
 * @returns {DispatchDecision}
 */
function checkDispatch(message, agentId, state) {
  const isBot = message.author?.bot || false;
  const threadId = message.channel?.isThread?.() ?
    message.channel.id :
    (message.channel?.id || 'unknown');
  const channelId = message.channel?.id || 'unknown';

  // Check channel allowlist
  if (!gatewayConfig.isChannelAllowed(channelId)) {
    return {
      allowed: false,
      reason: 'Channel not allowed for gateway dispatch',
      notifyUser: false,
      policy: 'channel_allowlist',
    };
  }

  // Check agent-to-agent policy
  const a2aCheck = checkAgentToAgent(isBot);
  if (!a2aCheck.allowed) {
    return {
      allowed: false,
      reason: a2aCheck.reason,
      notifyUser: false,
      policy: 'agent_to_agent',
    };
  }

  // Check self-mention (agent tagging itself)
  const authorName = message.author?.username || message.author?.displayName || '';
  const selfCheck = checkSelfMention(authorName, agentId);
  if (!selfCheck.allowed) {
    return {
      allowed: false,
      reason: selfCheck.reason,
      notifyUser: false,
      policy: 'self_mention',
    };
  }

  // Check thread depth
  const depthCheck = checkThreadDepth(state, threadId, isBot);
  if (!depthCheck.allowed) {
    return {
      allowed: false,
      reason: depthCheck.reason,
      notifyUser: true,
      policy: 'thread_depth',
    };
  }

  // Check message budget
  const budgetCheck = checkMessageBudget(state, agentId);
  if (!budgetCheck.allowed) {
    return {
      allowed: false,
      reason: budgetCheck.reason,
      notifyUser: true,
      policy: 'message_budget',
    };
  }

  // Check circuit breaker
  const circuitCheck = checkCircuitBreaker(state, threadId);
  if (!circuitCheck.allowed) {
    return {
      allowed: false,
      reason: circuitCheck.reason,
      notifyUser: true,
      policy: 'circuit_breaker',
    };
  }

  // All checks passed - update state
  return {
    allowed: true,
    notifyUser: false,
    policy: 'allowed',
    stateUpdates: {
      threadId,
      isBot,
      agentId,
    },
  };
}

/**
 * Update state after a successful dispatch
 * @param {AntiLoopState} state
 * @param {string} threadId
 * @param {string} agentId
 * @param {boolean} isBot
 */
function updateStateAfterDispatch(state, threadId, agentId, isBot) {
  // Update thread depth
  if (isBot) {
    state.threadDepths[threadId] = (state.threadDepths[threadId] || 0) + 1;
  } else {
    // Human message resets depth
    state.threadDepths[threadId] = 1;
  }

  // Update message budget
  if (!state.messageBudgets[agentId]) {
    state.messageBudgets[agentId] = {
      hourlyCount: 0,
      dailyCount: 0,
      hourlyResetAt: Date.now() + 3600000,
      dailyResetAt: Date.now() + 86400000,
    };
  }
  state.messageBudgets[agentId].hourlyCount++;
  state.messageBudgets[agentId].dailyCount++;

  // Update circuit breaker
  if (!state.circuitBreakers[threadId]) {
    state.circuitBreakers[threadId] = {
      count: 0,
      windowStart: Date.now(),
    };
  }
  state.circuitBreakers[threadId].count++;

  // Save updated state
  saveState(state);
}

/**
 * Clean up old state entries (call periodically)
 * @param {AntiLoopState} state
 */
function cleanupState(state) {
  const now = Date.now();
  const windowMs = gatewayConfig.antiloop.circuitBreaker.windowMs;

  // Clean up old circuit breaker entries (older than 2x window)
  for (const threadId of Object.keys(state.circuitBreakers)) {
    const breaker = state.circuitBreakers[threadId];
    if (now - breaker.windowStart > windowMs * 2) {
      delete state.circuitBreakers[threadId];
    }
  }

  // Clean up old thread depths (older than 1 hour with no activity)
  // This is approximate since we don't track last activity per thread
  // In practice, thread depths reset on human messages anyway

  saveState(state);
}

// ====
// Exports
// ====

module.exports = {
  // State management
  loadState,
  saveState,
  cleanupState,

  // Parsing
  parseAgentMention,

  // Policy checks
  checkDispatch,
  checkThreadDepth,
  checkMessageBudget,
  checkCircuitBreaker,
  checkAgentToAgent,
  checkSelfMention,

  // State updates
  updateStateAfterDispatch,
};
