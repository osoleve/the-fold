/**
 * Tests for queue.js — In-Memory Queue with Disk Spillover
 */

const fs = require('fs');
const path = require('path');

// Mock gateway-config before requiring queue
jest.mock('../gateway-config', () => ({
  queue: {
    memoryLimit: 5,  // Low limit for testing spillover
    spilloverPath: 'state/test-queue-spillover.jsonl',
  },
  getStatePath: (relativePath) => {
    // Extract basename manually without using path module
    const parts = relativePath.split('/');
    return `/tmp/test-fold-queue/${parts[parts.length - 1]}`;
  },
}));

const queue = require('../queue');

describe('queue', () => {
  beforeEach(() => {
    // Clear queue before each test
    queue.clear();
    // Ensure test directory exists
    const testDir = '/tmp/test-fold-queue';
    if (!fs.existsSync(testDir)) {
      fs.mkdirSync(testDir, { recursive: true });
    }
  });

  afterAll(() => {
    // Clean up test directory
    const testDir = '/tmp/test-fold-queue';
    if (fs.existsSync(testDir)) {
      fs.rmSync(testDir, { recursive: true, force: true });
    }
  });

  function createTask(id) {
    return {
      taskId: `task-${id}`,
      agentId: 'opus',
      channelId: 'channel-1',
      threadId: null,
      messageId: `msg-${id}`,
      content: `Test message ${id}`,
      authorId: 'user-1',
      authorName: 'testuser',
      isBot: false,
      enqueuedAt: Date.now(),
    };
  }

  describe('basic operations', () => {
    test('enqueue adds task to queue', () => {
      expect(queue.depth()).toBe(0);
      queue.enqueue(createTask(1));
      expect(queue.depth()).toBe(1);
    });

    test('dequeue returns task in FIFO order', () => {
      queue.enqueue(createTask(1));
      queue.enqueue(createTask(2));
      queue.enqueue(createTask(3));

      expect(queue.dequeue().taskId).toBe('task-1');
      expect(queue.dequeue().taskId).toBe('task-2');
      expect(queue.dequeue().taskId).toBe('task-3');
    });

    test('dequeue returns null when empty', () => {
      expect(queue.dequeue()).toBeNull();
    });

    test('peek returns next task without removing', () => {
      queue.enqueue(createTask(1));
      queue.enqueue(createTask(2));

      expect(queue.peek().taskId).toBe('task-1');
      expect(queue.peek().taskId).toBe('task-1');  // Still same task
      expect(queue.depth()).toBe(2);  // Not removed
    });

    test('peek returns null when empty', () => {
      expect(queue.peek()).toBeNull();
    });

    test('depth returns correct count', () => {
      expect(queue.depth()).toBe(0);
      queue.enqueue(createTask(1));
      expect(queue.depth()).toBe(1);
      queue.enqueue(createTask(2));
      expect(queue.depth()).toBe(2);
      queue.dequeue();
      expect(queue.depth()).toBe(1);
    });
  });

  describe('spillover', () => {
    test('spills to disk when memory limit exceeded', () => {
      // Memory limit is 5 in our mock config
      for (let i = 1; i <= 7; i++) {
        queue.enqueue(createTask(i));
      }

      // Should have 5 in memory
      expect(queue.depth()).toBe(5);

      // Check spillover file exists
      const spilloverPath = '/tmp/test-fold-queue/test-queue-spillover.jsonl';
      expect(fs.existsSync(spilloverPath)).toBe(true);

      // Spillover should have 2 tasks
      const content = fs.readFileSync(spilloverPath, 'utf8');
      const lines = content.split('\n').filter(l => l.trim());
      expect(lines.length).toBe(2);
    });

    test('recovers from spillover when memory queue empty', () => {
      // Fill queue past limit
      for (let i = 1; i <= 7; i++) {
        queue.enqueue(createTask(i));
      }

      // Drain memory queue
      for (let i = 0; i < 5; i++) {
        queue.dequeue();
      }

      // Next dequeue should recover from spillover
      const task = queue.dequeue();
      expect(task.taskId).toBe('task-6');

      // And the next one
      const task2 = queue.dequeue();
      expect(task2.taskId).toBe('task-7');
    });

    test('fullDepth reports memory and spillover counts', () => {
      for (let i = 1; i <= 7; i++) {
        queue.enqueue(createTask(i));
      }

      const depth = queue.fullDepth();
      expect(depth.memory).toBe(5);
      expect(depth.spillover).toBe(2);
    });
  });

  describe('drain', () => {
    test('returns all tasks from memory', () => {
      queue.enqueue(createTask(1));
      queue.enqueue(createTask(2));
      queue.enqueue(createTask(3));

      const tasks = queue.drain();
      expect(tasks.length).toBe(3);
      expect(tasks[0].taskId).toBe('task-1');
      expect(tasks[2].taskId).toBe('task-3');
    });

    test('includes tasks from spillover', () => {
      for (let i = 1; i <= 7; i++) {
        queue.enqueue(createTask(i));
      }

      const tasks = queue.drain();
      expect(tasks.length).toBe(7);
      expect(queue.depth()).toBe(0);
    });

    test('clears queue after drain', () => {
      queue.enqueue(createTask(1));
      queue.drain();
      expect(queue.depth()).toBe(0);
      expect(queue.dequeue()).toBeNull();
    });
  });

  describe('clear', () => {
    test('removes all tasks from memory', () => {
      queue.enqueue(createTask(1));
      queue.enqueue(createTask(2));
      queue.clear();
      expect(queue.depth()).toBe(0);
    });

    test('removes spillover file', () => {
      for (let i = 1; i <= 7; i++) {
        queue.enqueue(createTask(i));
      }
      queue.clear();

      const spilloverPath = '/tmp/test-fold-queue/test-queue-spillover.jsonl';
      expect(fs.existsSync(spilloverPath)).toBe(false);
    });
  });

  describe('initialize and shutdown', () => {
    test('initialize recovers tasks from spillover', () => {
      // Manually create spillover file
      const spilloverPath = '/tmp/test-fold-queue/test-queue-spillover.jsonl';
      const testDir = '/tmp/test-fold-queue';

      // Ensure directory exists
      if (!fs.existsSync(testDir)) {
        fs.mkdirSync(testDir, { recursive: true });
      }

      // Create spillover file with tasks
      const tasks = [createTask(100), createTask(101)];
      fs.writeFileSync(spilloverPath, tasks.map(t => JSON.stringify(t)).join('\n') + '\n');

      // Initialize should recover from spillover
      queue.initialize();

      expect(queue.depth()).toBe(2);
      expect(queue.dequeue().taskId).toBe('task-100');
    });

    test('shutdown saves pending tasks to spillover', () => {
      queue.enqueue(createTask(1));
      queue.enqueue(createTask(2));
      queue.shutdown();

      // Queue should be empty
      expect(queue.depth()).toBe(0);

      // Spillover should have tasks
      const spilloverPath = '/tmp/test-fold-queue/test-queue-spillover.jsonl';
      expect(fs.existsSync(spilloverPath)).toBe(true);
    });
  });

  describe('concurrent operations', () => {
    test('handles rapid enqueue/dequeue', () => {
      // Rapid enqueue (note: with memory limit 5, tasks 6-10 spill to disk)
      for (let i = 1; i <= 10; i++) {
        queue.enqueue(createTask(i));
      }

      // Drain all (includes recovering from spillover)
      const tasks = queue.drain();

      // Should have all 10 tasks
      expect(tasks.length).toBe(10);

      // Should be in order
      for (let i = 0; i < 10; i++) {
        expect(tasks[i].taskId).toBe(`task-${i + 1}`);
      }
    });
  });
});
