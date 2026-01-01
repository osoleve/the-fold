/**
 * thimble/discord/worker.js — Queue Consumer & Pipeline Invocation
 *
 * Polls the task queue and invokes agent pipelines via fold.sh or
 * direct Node.js invokers. Posts replies via Discord webhooks.
 */

const { execSync, spawn } = require('child_process');
const path = require('path');
const fs = require('fs');
const queue = require('./queue');
const dispatcher = require('./dispatcher');
const bridge = require('./bridge');
const config = require('./config');
const gatewayConfig = require('./gateway-config');

// ============================================================
// Worker State
// ============================================================

let isRunning = false;
let pollInterval = null;
let discordClient = null;

// Statistics
const stats = {
  startedAt: null,
  tasksProcessed: 0,
  tasksSucceeded: 0,
  tasksFailed: 0,
  lastTaskAt: null,
  lastError: null,
};

// ============================================================
// Pipeline Invocation
// ============================================================

/**
 * Build the context/trigger file content for an agent
 * @param {Task} task
 * @returns {string} S-expression trigger content
 */
function buildTriggerContent(task) {
  // Escape strings for S-expression format
  const escape = (str) => str
    .replace(/\\/g, '\\\\')
    .replace(/"/g, '\\"')
    .replace(/\n/g, '\\n')
    .replace(/\r/g, '\\r')
    .replace(/\t/g, '\\t');

  const channel = config.getDiscordToFoldChannel(task.channelId) || 'consult';

  return `((session-id . "gateway-${task.taskId}")
 (agent . ${task.agentId})
 (channel . ${channel})
 (author . "${escape(task.authorName)}")
 (body . "${escape(task.content)}"))
`;
}

/**
 * Invoke the agent pipeline and get response
 * @param {Task} task
 * @returns {Promise<{ success: boolean, response?: string, error?: string }>}
 */
async function invokeAgent(task) {
  const agentConfig = gatewayConfig.getAgentConfig(task.agentId);
  if (!agentConfig) {
    return { success: false, error: `Unknown agent: ${task.agentId}` };
  }

  const projectRoot = path.join(__dirname, '../..');
  const pipelinePath = path.join(projectRoot, agentConfig.pipelinePath);

  // Check if invoker exists
  if (!fs.existsSync(pipelinePath)) {
    return { success: false, error: `Pipeline not found: ${pipelinePath}` };
  }

  // Create temporary trigger file
  const triggerContent = buildTriggerContent(task);
  const triggerPath = path.join(
    projectRoot,
    '.fold-repl/triggers',
    `gateway-${task.taskId}.ss`
  );

  // Ensure triggers directory exists
  const triggersDir = path.dirname(triggerPath);
  if (!fs.existsSync(triggersDir)) {
    fs.mkdirSync(triggersDir, { recursive: true });
  }

  try {
    fs.writeFileSync(triggerPath, triggerContent);

    // Invoke the Node.js invoker script
    const result = execSync(`node ${pipelinePath} ${triggerPath}`, {
      cwd: projectRoot,
      timeout: gatewayConfig.worker.timeoutMs,
      encoding: 'utf8',
      stdio: ['pipe', 'pipe', 'pipe'],
    });

    // Clean up trigger file
    if (fs.existsSync(triggerPath)) {
      fs.unlinkSync(triggerPath);
    }

    // Check for error marker
    if (result.startsWith('❌')) {
      return { success: false, error: result };
    }

    return { success: true, response: result.trim() };

  } catch (e) {
    // Clean up trigger file on error
    if (fs.existsSync(triggerPath)) {
      fs.unlinkSync(triggerPath);
    }

    if (e.killed) {
      return { success: false, error: 'Pipeline execution timed out' };
    }
    return { success: false, error: e.message };
  }
}

// ============================================================
// Discord Posting
// ============================================================

/**
 * Post the agent response to Discord
 * @param {Task} task
 * @param {string} response
 */
async function postResponse(task, response) {
  const foldChannel = config.getDiscordToFoldChannel(task.channelId) || 'chat';
  const agentConfig = config.getAgentConfig(task.agentId);

  // Write to Discord outbox (bridge will pick it up)
  const outboxPath = path.join(
    __dirname, '../..',
    '.fold-repl/discord-outbox',
    `${Date.now()}-${task.agentId}.json`
  );

  const outboxMessage = {
    channel: foldChannel,
    body: response,
    author: task.agentId,
    tier: agentConfig?.tier || 'shepherd',
  };

  try {
    fs.writeFileSync(outboxPath, JSON.stringify(outboxMessage, null, 2));
    console.log(`📤 Response queued for Discord: ${task.agentId}`);
  } catch (e) {
    console.error(`Failed to write outbox: ${e.message}`);
  }
}

/**
 * Also save response to Fold forum
 * @param {Task} task
 * @param {string} response
 */
function saveToForum(task, response) {
  const projectRoot = path.join(__dirname, '../..');
  const chatPath = path.join(
    projectRoot,
    'forum/chat',
    `${Date.now()}-${task.agentId}.sexp`
  );

  // Escape for S-expression
  const escape = (str) => str
    .replace(/\\/g, '\\\\')
    .replace(/"/g, '\\"')
    .replace(/\n/g, '\\n');

  const content = `((author . ${task.agentId})
 (tier . shepherd)
 (channel . chat)
 (body . "${escape(response)}")
 (timestamp . "${new Date().toISOString()}")
 (in-reply-to . "gateway-${task.taskId}"))
`;

  try {
    fs.writeFileSync(chatPath, content);
  } catch (e) {
    console.error(`Failed to save to forum: ${e.message}`);
  }
}

// ============================================================
// Task Processing
// ============================================================

/**
 * Process a single task from the queue
 * @returns {Promise<boolean>} - True if a task was processed
 */
async function processNextTask() {
  const task = queue.dequeue();
  if (!task) return false;

  console.log(`🔄 Processing task: ${task.agentId} from ${task.authorName}`);
  stats.lastTaskAt = Date.now();

  try {
    // Invoke the agent
    const result = await invokeAgent(task);

    if (result.success) {
      // Post response to Discord
      await postResponse(task, result.response);

      // Save to Fold forum
      saveToForum(task, result.response);

      // Update anti-loop state
      const state = dispatcher.loadState();
      const threadId = task.threadId || task.channelId;
      dispatcher.updateStateAfterDispatch(state, threadId, task.agentId, task.isBot);

      stats.tasksSucceeded++;
      console.log(`✅ Task completed: ${task.agentId}`);
    } else {
      stats.tasksFailed++;
      stats.lastError = result.error;
      console.error(`❌ Task failed: ${result.error}`);
    }

    stats.tasksProcessed++;
    return true;

  } catch (e) {
    stats.tasksFailed++;
    stats.tasksProcessed++;
    stats.lastError = e.message;
    console.error(`❌ Task error: ${e.message}`);
    return true;
  }
}

// ============================================================
// Worker Control
// ============================================================

/**
 * Start the worker
 * @param {Object} client - Discord.js client
 */
function start(client) {
  if (isRunning) {
    console.log('Worker already running');
    return;
  }

  discordClient = client;
  isRunning = true;
  stats.startedAt = Date.now();

  // Initialize queue (recover from spillover)
  queue.initialize();

  // Start polling
  const intervalMs = gatewayConfig.worker.pollIntervalMs;
  pollInterval = setInterval(async () => {
    if (!isRunning) return;

    try {
      await processNextTask();
    } catch (e) {
      console.error(`Worker poll error: ${e.message}`);
    }
  }, intervalMs);

  console.log(`🚀 Worker started (polling every ${intervalMs}ms)`);
}

/**
 * Stop the worker
 */
function stop() {
  if (!isRunning) {
    console.log('Worker not running');
    return;
  }

  isRunning = false;

  if (pollInterval) {
    clearInterval(pollInterval);
    pollInterval = null;
  }

  // Save pending tasks to spillover
  queue.shutdown();

  console.log('🛑 Worker stopped');
}

/**
 * Get worker statistics
 * @returns {WorkerStats}
 */
function getStats() {
  return {
    ...stats,
    isRunning,
    queueDepth: queue.depth(),
    uptime: stats.startedAt ? Date.now() - stats.startedAt : 0,
  };
}

/**
 * Process a single task directly (for testing)
 * @param {Task} task
 * @returns {Promise<{ success: boolean, response?: string, error?: string }>}
 */
async function processTask(task) {
  return invokeAgent(task);
}

// ============================================================
// Exports
// ============================================================

module.exports = {
  start,
  stop,
  getStats,
  processTask,
  processNextTask,
};
