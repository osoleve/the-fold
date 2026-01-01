/**
 * agents/lib/parse-trigger.js — Parse S-expression trigger files
 *
 * Handles multi-line bodies, escaped characters, and unquoted symbols.
 */

/**
 * Parse a trigger file in S-expression alist format
 * @param {string} text - Raw file content
 * @returns {Object} - Parsed key-value pairs
 */
function parseTrigger(text) {
  const result = {};

  // Match patterns like: (key . value) or (key . "quoted value")
  // Handle multi-line by processing the whole text

  // First, normalize the text (remove outer parens if present)
  let normalized = text.trim();
  if (normalized.startsWith('((') && normalized.endsWith('))')) {
    normalized = normalized.slice(1, -1);
  }

  // Match each key-value pair
  // Pattern: (key . value) where value can be quoted or unquoted
  const pairRegex = /\((\w[\w-]*)\s+\.\s+("(?:[^"\\]|\\.)*"|[^)\s]+)\)/g;

  let match;
  while ((match = pairRegex.exec(normalized)) !== null) {
    const key = match[1];
    let value = match[2];

    // Remove quotes and unescape if quoted
    if (value.startsWith('"') && value.endsWith('"')) {
      value = value.slice(1, -1)
        .replace(/\\n/g, '\n')
        .replace(/\\r/g, '\r')
        .replace(/\\t/g, '\t')
        .replace(/\\"/g, '"')
        .replace(/\\\\/g, '\\');
    }

    result[key] = value;
  }

  return result;
}

module.exports = { parseTrigger };
