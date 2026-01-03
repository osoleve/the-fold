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

  // Strip comments (lines starting with ; after trimming)
  let normalized = text
    .split('\n')
    .filter(line => !line.trim().startsWith(';'))
    .join('\n')
    .trim();

  // Remove outer parens if present (alist wrapper)
  if (normalized.startsWith('((') && normalized.endsWith('))')) {
    normalized = normalized.slice(1, -1);
  }

  // Match each key-value pair
  // Pattern: (key . value) where value can be:
  //   - Quoted string: "..."
  //   - Symbol/number: balanced parens or atom ending at close paren
  const pairRegex = /\((\w[\w-]*)\s+\.\s+("(?:[^"\\]|\\.)*"|[^)]+)\)/g;

  let match;
  while ((match = pairRegex.exec(normalized)) !== null) {
    const key = match[1];
    let value = match[2].trim();

    // Remove quotes and unescape if quoted
    if (value.startsWith('"') && value.endsWith('"')) {
      value = value.slice(1, -1)
        .replace(/\\n/g, '\n')
        .replace(/\\r/g, '\r')
        .replace(/\\t/g, '\t')
        .replace(/\\b/g, '\b')
        .replace(/\\f/g, '\f')
        .replace(/\\"/g, '"')
        .replace(/\\\\/g, '\\');
    }

    result[key] = value;
  }

  return result;
}

module.exports = { parseTrigger };
