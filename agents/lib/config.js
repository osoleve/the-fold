#!/usr/bin/env node
/**
 * agents/lib/config.js — Simple YAML config loader
 *
 * Reads defaults.yaml and persona-specific config, merges them.
 * No third-party dependencies - uses simple YAML subset parsing.
 */

const fs = require('fs');
const path = require('path');

/**
 * Parse simple YAML subset (key: value format, ignores complex structures)
 * Only extracts top-level scalar values.
 */
function parseSimpleYAML(content) {
  const result = {};
  const lines = content.split('\n');

  for (const line of lines) {
    // Skip comments and empty lines
    if (line.trim().startsWith('#') || !line.trim()) continue;

    // Match simple key: value pattern
    const match = line.match(/^(\w+):\s*(.+)$/);
    if (match) {
      const [, key, value] = match;
      // Remove quotes and trim
      result[key] = value.replace(/^["']|["']$/g, '').trim();
    }
  }

  return result;
}

/**
 * Load config for a given persona
 *
 * @param {string} personaName - Name of the persona (e.g., 'opus', 'sonnet')
 * @returns {object} Merged config with defaults + persona overrides
 */
function loadConfig(personaName) {
  const agentsDir = path.join(__dirname, '..');
  const defaultsPath = path.join(agentsDir, 'defaults.yaml');
  const personaPath = path.join(agentsDir, 'personas', `${personaName}.yaml`);

  // Load defaults
  let config = {};
  if (fs.existsSync(defaultsPath)) {
    try {
      const content = fs.readFileSync(defaultsPath, 'utf8');
      config = parseSimpleYAML(content);
    } catch (e) {
      console.error(`Warning: Failed to read defaults.yaml: ${e.message}`);
    }
  }

  // Load persona-specific config and merge
  if (fs.existsSync(personaPath)) {
    try {
      const content = fs.readFileSync(personaPath, 'utf8');
      const personaConfig = parseSimpleYAML(content);
      config = { ...config, ...personaConfig };
    } catch (e) {
      console.error(`Warning: Failed to read ${personaName}.yaml: ${e.message}`);
    }
  }

  return config;
}

/**
 * Get model name from config, with fallback
 */
function getModel(personaName) {
  const config = loadConfig(personaName);
  return config.model || personaName;  // Fallback to persona name
}

/**
 * Get tier from config
 */
function getTier(personaName) {
  const config = loadConfig(personaName);
  return config.tier || 'haiku';  // Default to haiku
}

module.exports = {
  loadConfig,
  getModel,
  getTier,
  parseSimpleYAML
};
