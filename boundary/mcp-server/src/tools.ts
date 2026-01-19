/**
 * MCP tool definitions for The Fold REPL
 *
 * Core REPL tools:
 * - fold_login: Login with tier and name
 * - fold_eval: Evaluate expression
 * - fold_help: Get help
 * - fold_who: Session info
 * - fold_logout: End session
 * - fold_status: Check daemon status
 *
 * LSP tools for Scheme code intelligence:
 * - fold_lsp_hover: Get type info and documentation at position
 * - fold_lsp_definition: Go to symbol definition
 * - fold_lsp_references: Find all references to symbol
 * - fold_lsp_symbols: Search for symbols by name
 * - fold_lsp_diagnostics: Get errors/warnings for a file
 * - fold_lsp_format: Format Scheme code
 * - fold_lsp_lookup: Combined symbol lookup (hover + def + refs)
 * - fold_lsp_status: Check LSP server status
 */

import { Tool } from '@modelcontextprotocol/sdk/types.js';

export const tools: Tool[] = [
  {
    name: 'fold_login',
    description: 'Login to The Fold REPL with tier and name. Must be called before using other tools.',
    inputSchema: {
      type: 'object',
      properties: {
        tier: {
          type: 'string',
          enum: ['opus', 'sonnet', 'haiku'],
          description: 'Your tier: opus (shepherd), sonnet (builder), or haiku (player)'
        },
        name: {
          type: 'string',
          description: 'Your chosen name (will be converted to symbol)',
          pattern: '^[a-zA-Z][a-zA-Z0-9_-]*$'
        },
        message: {
          type: 'string',
          description: 'Announcement message for login'
        }
      },
      required: ['tier', 'name', 'message']
    }
  },

  {
    name: 'fold_eval',
    description: 'Evaluate a Scheme expression in your session context. Requires prior login.',
    inputSchema: {
      type: 'object',
      properties: {
        expression: {
          type: 'string',
          description: 'Scheme expression to evaluate'
        }
      },
      required: ['expression']
    }
  },

  {
    name: 'fold_help',
    description: 'Get help on available REPL commands.',
    inputSchema: {
      type: 'object',
      properties: {},
      required: []
    }
  },

  {
    name: 'fold_who',
    description: 'Show current session information (tier, name, login status).',
    inputSchema: {
      type: 'object',
      properties: {},
      required: []
    }
  },

  {
    name: 'fold_logout',
    description: 'Logout from the current session.',
    inputSchema: {
      type: 'object',
      properties: {},
      required: []
    }
  },

  {
    name: 'fold_status',
    description: 'Check daemon connection status and diagnostics. Does not require login.',
    inputSchema: {
      type: 'object',
      properties: {},
      required: []
    }
  },

  // ============================================================
  // LSP Tools - Scheme Code Intelligence
  // ============================================================

  {
    name: 'fold_lsp_hover',
    description: 'Get type information and documentation for a symbol at a specific position in a Scheme file. Returns inferred types, function signatures, and documentation.',
    inputSchema: {
      type: 'object',
      properties: {
        file: {
          type: 'string',
          description: 'Path to the Scheme file (absolute or relative to project root)'
        },
        line: {
          type: 'number',
          description: 'Line number (0-indexed)'
        },
        character: {
          type: 'number',
          description: 'Character/column position (0-indexed)'
        }
      },
      required: ['file', 'line', 'character']
    }
  },

  {
    name: 'fold_lsp_definition',
    description: 'Go to the definition of a symbol at a specific position. Returns the file and location where the symbol is defined.',
    inputSchema: {
      type: 'object',
      properties: {
        file: {
          type: 'string',
          description: 'Path to the Scheme file (absolute or relative to project root)'
        },
        line: {
          type: 'number',
          description: 'Line number (0-indexed)'
        },
        character: {
          type: 'number',
          description: 'Character/column position (0-indexed)'
        }
      },
      required: ['file', 'line', 'character']
    }
  },

  {
    name: 'fold_lsp_references',
    description: 'Find all references to a symbol at a specific position. Returns all locations where the symbol is used.',
    inputSchema: {
      type: 'object',
      properties: {
        file: {
          type: 'string',
          description: 'Path to the Scheme file (absolute or relative to project root)'
        },
        line: {
          type: 'number',
          description: 'Line number (0-indexed)'
        },
        character: {
          type: 'number',
          description: 'Character/column position (0-indexed)'
        },
        includeDeclaration: {
          type: 'boolean',
          description: 'Include the declaration in results (default: true)',
          default: true
        }
      },
      required: ['file', 'line', 'character']
    }
  },

  {
    name: 'fold_lsp_symbols',
    description: 'Search for symbols by name across the workspace. Returns matching function definitions, variables, and other symbols.',
    inputSchema: {
      type: 'object',
      properties: {
        query: {
          type: 'string',
          description: 'Search query (partial name match supported)'
        }
      },
      required: ['query']
    }
  },

  {
    name: 'fold_lsp_diagnostics',
    description: 'Get errors and warnings for a Scheme file. Returns syntax errors, type errors, and other diagnostics.',
    inputSchema: {
      type: 'object',
      properties: {
        file: {
          type: 'string',
          description: 'Path to the Scheme file (absolute or relative to project root)'
        }
      },
      required: ['file']
    }
  },

  {
    name: 'fold_lsp_format',
    description: 'Format a Scheme file using the standard pretty-printer. Returns the formatted code.',
    inputSchema: {
      type: 'object',
      properties: {
        file: {
          type: 'string',
          description: 'Path to the Scheme file (absolute or relative to project root)'
        }
      },
      required: ['file']
    }
  },

  {
    name: 'fold_lsp_lookup',
    description: 'Combined symbol lookup: gets hover info, definition location, and all references in one call. More efficient than calling each separately.',
    inputSchema: {
      type: 'object',
      properties: {
        file: {
          type: 'string',
          description: 'Path to the Scheme file (absolute or relative to project root)'
        },
        line: {
          type: 'number',
          description: 'Line number (0-indexed)'
        },
        character: {
          type: 'number',
          description: 'Character/column position (0-indexed)'
        }
      },
      required: ['file', 'line', 'character']
    }
  },

  {
    name: 'fold_lsp_status',
    description: 'Check LSP server status. Shows whether the LSP is running and its capabilities.',
    inputSchema: {
      type: 'object',
      properties: {},
      required: []
    }
  }
];
