/**
 * thimble/discord/queue.js — In-Memory Task Queue with Disk Spillover
 *
 * Simple FIFO queue for agent tasks. Spills to disk when memory
 * threshold is exceeded for resilience during high load.
 */

const fs = require('fs');
const path = require('path');
const gatewayConfig = require('./gateway-config');

// ============================================================
// Queue State
// ============================================================

// In-memory queue
const memoryQueue = [];

// Flag to track if we're recovering from spillover
let isRecovering = false;

// ============================================================
// Spillover Management
// ============================================================

/**
 * Get the full path to the spillover file
 */
function getSpilloverPath() {
  return gatewayConfig.getStatePath(gatewayConfig.queue.spilloverPath);
}

/**
 * Append a task to the spillover file
 * @param {Task} task
 */
function appendToSpillover(task) {
  const spilloverPath = getSpilloverPath();
  const dir = path.dirname(spilloverPath);

  // Ensure directory exists
  if (!fs.existsSync(dir)) {
    fs.mkdirSync(dir, { recursive: true });
  }

  try {
    fs.appendFileSync(spilloverPath, JSON.stringify(task) + '\n');
  } catch (e) {
    console.error(`Failed to append to spillover: ${e.message}`);
  }
}

/**
 * Load tasks from spillover file (on startup)
 * @returns {Task[]}
 */
function loadFromSpillover() {
  const spilloverPath = getSpilloverPath();
  const tasks = [];

  if (!fs.existsSync(spilloverPath)) {
    return tasks;
  }

  try {
    const content = fs.readFileSync(spilloverPath, 'utf8');
    const lines = content.split('\n').filter(line => line.trim());

    for (const line of lines) {
      try {
        tasks.push(JSON.parse(line));
      } catch (e) {
        console.error(`Failed to parse spillover line: ${e.message}`);
      }
    }

    // Clear spillover file after successful load
    fs.unlinkSync(spilloverPath);
    console.log(`📥 Recovered ${tasks.length} tasks from spillover`);

  } catch (e) {
    console.error(`Failed to load from spillover: ${e.message}`);
  }

  return tasks;
}

/**
 * Write all remaining tasks to spillover (on shutdown)
 * @param {Task[]} tasks
 */
function writeToSpillover(tasks) {
  if (tasks.length === 0) return;

  const spilloverPath = getSpilloverPath();
  const dir = path.dirname(spilloverPath);

  if (!fs.existsSync(dir)) {
    fs.mkdirSync(dir, { recursive: true });
  }

  try {
    const content = tasks.map(t => JSON.stringify(t)).join('\n') + '\n';
    fs.writeFileSync(spilloverPath, content);
    console.log(`📤 Saved ${tasks.length} tasks to spillover`);
  } catch (e) {
    console.error(`Failed to write spillover: ${e.message}`);
  }
}

// ============================================================
// Queue Operations
// ============================================================

/**
 * Add a task to the queue
 * @param {Task} task
 */
function enqueue(task) {
  const memoryLimit = gatewayConfig.queue.memoryLimit;

  if (memoryQueue.length >= memoryLimit) {
    // Spill to disk
    appendToSpillover(task);
    console.log(`📋 Queue at capacity (${memoryLimit}), spilled task to disk`);
  } else {
    memoryQueue.push(task);
  }
}

/**
 * Remove and return the next task from the queue
 * @returns {Task|null}
 */
function dequeue() {
  if (memoryQueue.length > 0) {
    return memoryQueue.shift();
  }

  // Try to recover from spillover if memory queue is empty
  if (!isRecovering) {
    const spilloverPath = getSpilloverPath();
    if (fs.existsSync(spilloverPath)) {
      isRecovering = true;
      const tasks = loadFromSpillover();
      memoryQueue.push(...tasks);
      isRecovering = false;

      if (memoryQueue.length > 0) {
        return memoryQueue.shift();
      }
    }
  }

  return null;
}

/**
 * Peek at the next task without removing it
 * @returns {Task|null}
 */
function peek() {
  return memoryQueue.length > 0 ? memoryQueue[0] : null;
}

/**
 * Get the current queue depth (memory only)
 * @returns {number}
 */
function depth() {
  return memoryQueue.length;
}

/**
 * Get total depth including spillover estimate
 * @returns {{ memory: number, spillover: number }}
 */
function fullDepth() {
  let spilloverCount = 0;
  const spilloverPath = getSpilloverPath();

  if (fs.existsSync(spilloverPath)) {
    try {
      const content = fs.readFileSync(spilloverPath, 'utf8');
      spilloverCount = content.split('\n').filter(line => line.trim()).length;
    } catch (e) {
      // Ignore errors
    }
  }

  return {
    memory: memoryQueue.length,
    spillover: spilloverCount,
  };
}

/**
 * Drain all tasks from the queue (for shutdown)
 * @returns {Task[]}
 */
function drain() {
  const tasks = [...memoryQueue];
  memoryQueue.length = 0;

  // Also load from spillover
  const spilloverTasks = loadFromSpillover();
  tasks.push(...spilloverTasks);

  return tasks;
}

/**
 * Clear the queue (for testing or reset)
 */
function clear() {
  memoryQueue.length = 0;
  const spilloverPath = getSpilloverPath();
  if (fs.existsSync(spilloverPath)) {
    fs.unlinkSync(spilloverPath);
  }
}

/**
 * Initialize queue (recover from spillover on startup)
 */
function initialize() {
  const tasks = loadFromSpillover();
  if (tasks.length > 0) {
    memoryQueue.push(...tasks);
    console.log(`📋 Queue initialized with ${tasks.length} recovered tasks`);
  }
}

/**
 * Shutdown queue (save pending tasks to spillover)
 */
function shutdown() {
  if (memoryQueue.length > 0) {
    writeToSpillover(memoryQueue);
    memoryQueue.length = 0;
  }
}

// ============================================================
// Exports
// ============================================================

module.exports = {
  enqueue,
  dequeue,
  peek,
  depth,
  fullDepth,
  drain,
  clear,
  initialize,
  shutdown,
};
