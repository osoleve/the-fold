/**
 * Tests for dispatcher.js — Anti-Loop Policy Enforcement
 */

const fs = require('fs');
const path = require('path');

// Test directory path
const TEST_STATE_DIR = '/tmp/test-fold-state';

// Mock gateway-config before requiring dispatcher
const mockIsChannelAllowed = jest.fn().mockReturnValue(true);

jest.mock('../gateway-config', () => ({
  consultationAgents: ['opus', 'pedagogue', 'archivist', 'sonnet', 'haiku'],
  antiloop: {
    maxThreadDepth: 3,
    messageBudget: { hourly: 20, daily: 100 },
    circuitBreaker: { threshold: 5, windowMs: 30000 },
    agentToAgentAllowed: true,
  },
  paths: {
    stateDir: '/tmp/test-fold-state',
    antiloopState: '/tmp/test-fold-state/discord-antiloop.json',
  },
  isChannelAllowed: mockIsChannelAllowed,
  getStatePath: (relativePath) => {
    // Extract basename manually without using path module
    const parts = relativePath.split('/');
    return `/tmp/test-fold-state/${parts[parts.length - 1]}`;
  },
}));

const dispatcher = require('../dispatcher');
const gatewayConfig = require('../gateway-config');

describe('dispatcher', () => {
  beforeEach(() => {
    // Clean up state file before each test
    const statePath = '/tmp/test-fold-state/discord-antiloop.json';
    if (fs.existsSync(statePath)) {
      fs.unlinkSync(statePath);
    }
    // Ensure test directory exists
    if (!fs.existsSync('/tmp/test-fold-state')) {
      fs.mkdirSync('/tmp/test-fold-state', { recursive: true });
    }
  });

  afterAll(() => {
    // Clean up test state directory
    const stateDir = '/tmp/test-fold-state';
    if (fs.existsSync(stateDir)) {
      fs.rmSync(stateDir, { recursive: true, force: true });
    }
  });

  describe('parseAgentMention', () => {
    test('extracts opus from @opus mention', () => {
      expect(dispatcher.parseAgentMention('@opus what is content addressing?')).toBe('opus');
    });

    test('extracts pedagogue from @pedagogue mention', () => {
      expect(dispatcher.parseAgentMention('@pedagogue explain monads')).toBe('pedagogue');
    });

    test('extracts archivist from @archivist mention', () => {
      expect(dispatcher.parseAgentMention('hey @archivist find prior work')).toBe('archivist');
    });

    test('is case insensitive', () => {
      expect(dispatcher.parseAgentMention('@OPUS help me')).toBe('opus');
      expect(dispatcher.parseAgentMention('@Pedagogue teach me')).toBe('pedagogue');
    });

    test('returns null for no agent mention', () => {
      expect(dispatcher.parseAgentMention('hello world')).toBeNull();
    });

    test('returns null for partial matches', () => {
      expect(dispatcher.parseAgentMention('opus is great')).toBeNull();
      expect(dispatcher.parseAgentMention('the @opusX agent')).toBeNull();
    });

    test('handles mention at end of message', () => {
      expect(dispatcher.parseAgentMention('please help @opus')).toBe('opus');
    });

    test('returns first agent if multiple mentioned', () => {
      const result = dispatcher.parseAgentMention('@opus and @pedagogue help');
      expect(result).toBe('opus');
    });
  });

  describe('checkThreadDepth', () => {
    test('allows human messages', () => {
      const state = { threadDepths: { 'thread-1': 5 } };
      const result = dispatcher.checkThreadDepth(state, 'thread-1', false);
      expect(result.allowed).toBe(true);
      expect(result.currentDepth).toBe(0);  // Human resets depth
    });

    test('allows bot messages under depth limit', () => {
      const state = { threadDepths: { 'thread-1': 2 } };
      const result = dispatcher.checkThreadDepth(state, 'thread-1', true);
      expect(result.allowed).toBe(true);
      expect(result.currentDepth).toBe(2);
    });

    test('denies bot messages at depth limit', () => {
      const state = { threadDepths: { 'thread-1': 3 } };
      const result = dispatcher.checkThreadDepth(state, 'thread-1', true);
      expect(result.allowed).toBe(false);
      expect(result.reason).toContain('depth limit');
    });

    test('allows new threads', () => {
      const state = { threadDepths: {} };
      const result = dispatcher.checkThreadDepth(state, 'new-thread', true);
      expect(result.allowed).toBe(true);
      expect(result.currentDepth).toBe(0);
    });
  });

  describe('checkMessageBudget', () => {
    test('allows messages under hourly limit', () => {
      const state = {
        messageBudgets: {
          opus: {
            hourlyCount: 5,
            dailyCount: 10,
            hourlyResetAt: Date.now() + 3600000,
            dailyResetAt: Date.now() + 86400000,
          },
        },
      };
      const result = dispatcher.checkMessageBudget(state, 'opus');
      expect(result.allowed).toBe(true);
      expect(result.hourlyCount).toBe(5);
    });

    test('denies messages at hourly limit', () => {
      const state = {
        messageBudgets: {
          opus: {
            hourlyCount: 20,
            dailyCount: 20,
            hourlyResetAt: Date.now() + 3600000,
            dailyResetAt: Date.now() + 86400000,
          },
        },
      };
      const result = dispatcher.checkMessageBudget(state, 'opus');
      expect(result.allowed).toBe(false);
      expect(result.reason).toContain('Hourly');
    });

    test('denies messages at daily limit', () => {
      const state = {
        messageBudgets: {
          opus: {
            hourlyCount: 5,
            dailyCount: 100,
            hourlyResetAt: Date.now() + 3600000,
            dailyResetAt: Date.now() + 86400000,
          },
        },
      };
      const result = dispatcher.checkMessageBudget(state, 'opus');
      expect(result.allowed).toBe(false);
      expect(result.reason).toContain('Daily');
    });

    test('resets hourly counter after window expires', () => {
      const state = {
        messageBudgets: {
          opus: {
            hourlyCount: 20,
            dailyCount: 20,
            hourlyResetAt: Date.now() - 1000,  // Already expired
            dailyResetAt: Date.now() + 86400000,
          },
        },
      };
      const result = dispatcher.checkMessageBudget(state, 'opus');
      expect(result.allowed).toBe(true);
      expect(result.hourlyCount).toBe(0);  // Reset
    });

    test('creates budget for new agent', () => {
      const state = { messageBudgets: {} };
      const result = dispatcher.checkMessageBudget(state, 'newagent');
      expect(result.allowed).toBe(true);
      expect(result.hourlyCount).toBe(0);
    });
  });

  describe('checkCircuitBreaker', () => {
    test('allows messages under threshold', () => {
      const state = {
        circuitBreakers: {
          'thread-1': { count: 2, windowStart: Date.now() },
        },
      };
      const result = dispatcher.checkCircuitBreaker(state, 'thread-1');
      expect(result.allowed).toBe(true);
      expect(result.count).toBe(2);
    });

    test('denies messages at threshold', () => {
      const state = {
        circuitBreakers: {
          'thread-1': { count: 5, windowStart: Date.now() },
        },
      };
      const result = dispatcher.checkCircuitBreaker(state, 'thread-1');
      expect(result.allowed).toBe(false);
      expect(result.reason).toContain('Too many messages');
    });

    test('resets after window expires', () => {
      const state = {
        circuitBreakers: {
          'thread-1': { count: 10, windowStart: Date.now() - 60000 },  // Past window
        },
      };
      const result = dispatcher.checkCircuitBreaker(state, 'thread-1');
      expect(result.allowed).toBe(true);
      expect(result.count).toBe(0);  // Reset
    });

    test('creates breaker for new thread', () => {
      const state = { circuitBreakers: {} };
      const result = dispatcher.checkCircuitBreaker(state, 'new-thread');
      expect(result.allowed).toBe(true);
      expect(result.count).toBe(0);
    });
  });

  describe('checkDispatch', () => {
    function createMockMessage(isBot, channelId = 'channel-1') {
      return {
        author: { bot: isBot },
        channel: {
          id: channelId,
          isThread: () => false,
        },
      };
    }

    test('allows dispatch for human messages', () => {
      const message = createMockMessage(false);
      const state = dispatcher.loadState();
      const result = dispatcher.checkDispatch(message, 'opus', state);
      expect(result.allowed).toBe(true);
      expect(result.policy).toBe('allowed');
    });

    test('allows dispatch for bot messages under limits', () => {
      const message = createMockMessage(true);
      const state = dispatcher.loadState();
      const result = dispatcher.checkDispatch(message, 'opus', state);
      expect(result.allowed).toBe(true);
    });

    test('denies when thread depth exceeded', () => {
      const message = createMockMessage(true);
      const state = {
        threadDepths: { 'channel-1': 3 },
        messageBudgets: {},
        circuitBreakers: {},
      };
      const result = dispatcher.checkDispatch(message, 'opus', state);
      expect(result.allowed).toBe(false);
      expect(result.policy).toBe('thread_depth');
      expect(result.notifyUser).toBe(true);
    });

    test('denies when message budget exceeded', () => {
      const message = createMockMessage(false);
      const state = {
        threadDepths: {},
        messageBudgets: {
          opus: {
            hourlyCount: 20,
            dailyCount: 20,
            hourlyResetAt: Date.now() + 3600000,
            dailyResetAt: Date.now() + 86400000,
          },
        },
        circuitBreakers: {},
      };
      const result = dispatcher.checkDispatch(message, 'opus', state);
      expect(result.allowed).toBe(false);
      expect(result.policy).toBe('message_budget');
      expect(result.notifyUser).toBe(true);
    });

    test('denies when circuit breaker tripped', () => {
      const message = createMockMessage(false);
      const state = {
        threadDepths: {},
        messageBudgets: {},
        circuitBreakers: {
          'channel-1': { count: 5, windowStart: Date.now() },
        },
      };
      const result = dispatcher.checkDispatch(message, 'opus', state);
      expect(result.allowed).toBe(false);
      expect(result.policy).toBe('circuit_breaker');
      expect(result.notifyUser).toBe(true);
    });

    test('checks channel allowlist', () => {
      gatewayConfig.isChannelAllowed.mockReturnValueOnce(false);
      const message = createMockMessage(false, 'blocked-channel');
      const state = dispatcher.loadState();
      const result = dispatcher.checkDispatch(message, 'opus', state);
      expect(result.allowed).toBe(false);
      expect(result.policy).toBe('channel_allowlist');
      expect(result.notifyUser).toBe(false);
    });
  });

  describe('updateStateAfterDispatch', () => {
    test('increments thread depth for bot messages', () => {
      const state = dispatcher.loadState();
      dispatcher.updateStateAfterDispatch(state, 'thread-1', 'opus', true);
      expect(state.threadDepths['thread-1']).toBe(1);
    });

    test('resets thread depth for human messages', () => {
      const state = { threadDepths: { 'thread-1': 5 }, messageBudgets: {}, circuitBreakers: {} };
      dispatcher.updateStateAfterDispatch(state, 'thread-1', 'opus', false);
      expect(state.threadDepths['thread-1']).toBe(1);
    });

    test('increments message budget counters', () => {
      const state = dispatcher.loadState();
      dispatcher.updateStateAfterDispatch(state, 'thread-1', 'opus', false);
      expect(state.messageBudgets.opus.hourlyCount).toBe(1);
      expect(state.messageBudgets.opus.dailyCount).toBe(1);
    });

    test('increments circuit breaker count', () => {
      const state = dispatcher.loadState();
      dispatcher.updateStateAfterDispatch(state, 'thread-1', 'opus', false);
      expect(state.circuitBreakers['thread-1'].count).toBe(1);
    });
  });

  describe('state persistence', () => {
    test('saves and loads state correctly', () => {
      const state = {
        threadDepths: { 'thread-1': 2 },
        messageBudgets: {
          opus: { hourlyCount: 5, dailyCount: 10, hourlyResetAt: 0, dailyResetAt: 0 },
        },
        circuitBreakers: {
          'thread-1': { count: 3, windowStart: Date.now() },
        },
      };

      dispatcher.saveState(state);
      const loaded = dispatcher.loadState();

      expect(loaded.threadDepths['thread-1']).toBe(2);
      expect(loaded.messageBudgets.opus.hourlyCount).toBe(5);
      expect(loaded.circuitBreakers['thread-1'].count).toBe(3);
    });

    test('returns empty state when no file exists', () => {
      const state = dispatcher.loadState();
      expect(state.threadDepths).toEqual({});
      expect(state.messageBudgets).toEqual({});
      expect(state.circuitBreakers).toEqual({});
    });
  });
});
