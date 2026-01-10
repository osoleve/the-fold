/**
 * Tests for agents/lib/parse-trigger.js
 *
 * Uses Node.js built-in test runner (no external dependencies)
 * Run with: npm test (from agents/ directory)
 */

const { describe, it } = require('node:test');
const assert = require('node:assert');
const { parseTrigger } = require('../lib/parse-trigger');

describe('parseTrigger', () => {

  describe('basic parsing', () => {
    it('parses simple unquoted values', () => {
      const input = '((key . value))';
      const result = parseTrigger(input);
      assert.deepStrictEqual(result, { key: 'value' });
    });

    it('parses quoted string values', () => {
      const input = '((name . "Claude Opus"))';
      const result = parseTrigger(input);
      assert.deepStrictEqual(result, { name: 'Claude Opus' });
    });

    it('parses multiple key-value pairs', () => {
      const input = '((author . andy)(channel . engineering)(priority . 1))';
      const result = parseTrigger(input);
      assert.deepStrictEqual(result, {
        author: 'andy',
        channel: 'engineering',
        priority: '1'
      });
    });

    it('handles hyphenated keys', () => {
      const input = '((message-id . abc123))';
      const result = parseTrigger(input);
      assert.deepStrictEqual(result, { 'message-id': 'abc123' });
    });
  });

  describe('escape sequences', () => {
    it('unescapes newlines', () => {
      const input = '((body . "line1\\nline2"))';
      const result = parseTrigger(input);
      assert.strictEqual(result.body, 'line1\nline2');
    });

    it('unescapes tabs', () => {
      const input = '((body . "col1\\tcol2"))';
      const result = parseTrigger(input);
      assert.strictEqual(result.body, 'col1\tcol2');
    });

    it('unescapes quotes', () => {
      const input = '((body . "He said \\"hello\\""))';
      const result = parseTrigger(input);
      assert.strictEqual(result.body, 'He said "hello"');
    });

    it('unescapes backslashes', () => {
      const input = '((path . "C:\\\\Users\\\\fold"))';
      const result = parseTrigger(input);
      assert.strictEqual(result.path, 'C:\\Users\\fold');
    });

    it('handles multiple escape sequences', () => {
      const input = '((body . "line1\\nline2\\ttab\\nline3"))';
      const result = parseTrigger(input);
      assert.strictEqual(result.body, 'line1\nline2\ttab\nline3');
    });
  });

  describe('whitespace handling', () => {
    it('handles extra whitespace around dots', () => {
      const input = '((key   .   value))';
      const result = parseTrigger(input);
      assert.deepStrictEqual(result, { key: 'value' });
    });

    it('trims input', () => {
      const input = '  ((key . value))  ';
      const result = parseTrigger(input);
      assert.deepStrictEqual(result, { key: 'value' });
    });

    it('handles newlines between pairs', () => {
      const input = `((author . andy)
(body . "test message")
(channel . general))`;
      const result = parseTrigger(input);
      assert.deepStrictEqual(result, {
        author: 'andy',
        body: 'test message',
        channel: 'general'
      });
    });
  });

  describe('edge cases', () => {
    it('returns empty object for empty input', () => {
      const result = parseTrigger('');
      assert.deepStrictEqual(result, {});
    });

    it('returns empty object for malformed input', () => {
      const result = parseTrigger('not valid sexp');
      assert.deepStrictEqual(result, {});
    });

    it('handles single pair without outer parens', () => {
      const input = '(key . value)';
      const result = parseTrigger(input);
      assert.deepStrictEqual(result, { key: 'value' });
    });
  });

  describe('real-world trigger formats', () => {
    it('parses a typical Discord trigger', () => {
      const input = `((author . "andy")
(channel . "engineering")
(body . "@opus How should we structure the type system?")
(timestamp . "2025-01-02T18:00:00Z"))`;

      const result = parseTrigger(input);

      assert.strictEqual(result.author, 'andy');
      assert.strictEqual(result.channel, 'engineering');
      assert.strictEqual(result.body, '@opus How should we structure the type system?');
      assert.strictEqual(result.timestamp, '2025-01-02T18:00:00Z');
    });

    it('parses a forum tag trigger', () => {
      const input = `((author . pedagogue)
(tag . opus)
(topic . "architecture")
(body . "Need guidance on effect system design"))`;

      const result = parseTrigger(input);

      assert.strictEqual(result.author, 'pedagogue');
      assert.strictEqual(result.tag, 'opus');
      assert.strictEqual(result.topic, 'architecture');
    });
  });
});
