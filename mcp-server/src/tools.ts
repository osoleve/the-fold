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
 * - fold_lsp_completion: Get code completion suggestions at position
 * - fold_lsp_document_symbols: Get document symbols for file outline
 * - fold_lsp_semantic_tokens: Get semantic tokens for syntax highlighting
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
  // Env/Memory Tools - Session State Management
  // ============================================================

  {
    name: 'fold_env_store',
    description: 'Evaluate a Scheme expression and store the result with a named key in the session environment. Requires login.',
    inputSchema: {
      type: 'object',
      properties: {
        key: {
          type: 'string',
          description: 'Name for the stored value (e.g. "data", "matrix", "result")'
        },
        expression: {
          type: 'string',
          description: 'Scheme expression to evaluate and store'
        }
      },
      required: ['key', 'expression']
    }
  },

  {
    name: 'fold_env_retrieve',
    description: 'Retrieve a stored value from the session environment by key. Requires login.',
    inputSchema: {
      type: 'object',
      properties: {
        key: {
          type: 'string',
          description: 'Key of the stored value'
        }
      },
      required: ['key']
    }
  },

  {
    name: 'fold_env_peek',
    description: 'Preview the first N characters of a stored value. Useful for large/chunked values. Requires login.',
    inputSchema: {
      type: 'object',
      properties: {
        key: {
          type: 'string',
          description: 'Key of the stored value'
        },
        n: {
          type: 'number',
          description: 'Number of characters to preview (default: 500)',
          default: 500
        }
      },
      required: ['key']
    }
  },

  {
    name: 'fold_env_grep',
    description: 'Search through chunks of a stored value using BM25 keyword search. Only works on chunked (large) values. Requires login.',
    inputSchema: {
      type: 'object',
      properties: {
        key: {
          type: 'string',
          description: 'Key of the chunked value'
        },
        pattern: {
          type: 'string',
          description: 'Search pattern'
        },
        k: {
          type: 'number',
          description: 'Number of top results (default: 5)',
          default: 5
        }
      },
      required: ['key', 'pattern']
    }
  },

  {
    name: 'fold_env_keys',
    description: 'List all stored values in the session environment with their types and sizes. Requires login.',
    inputSchema: {
      type: 'object',
      properties: {},
      required: []
    }
  },

  {
    name: 'fold_env_ingest',
    description: 'Chunk and store large text in the session environment. The text is split into chunks for efficient search and retrieval. Requires login.',
    inputSchema: {
      type: 'object',
      properties: {
        key: {
          type: 'string',
          description: 'Name for the stored chunks'
        },
        text: {
          type: 'string',
          description: 'Text to chunk and store'
        },
        chunk_size: {
          type: 'number',
          description: 'Approximate chunk size in characters (default: 2000)',
          default: 2000
        }
      },
      required: ['key', 'text']
    }
  },

  {
    name: 'fold_memory_store',
    description: 'Save a key-value pair to persistent memory that survives across sessions. Requires login.',
    inputSchema: {
      type: 'object',
      properties: {
        key: {
          type: 'string',
          description: 'Memory key/tag'
        },
        text: {
          type: 'string',
          description: 'Text to remember'
        }
      },
      required: ['key', 'text']
    }
  },

  {
    name: 'fold_memory_search',
    description: 'Search persistent memory by keyword using BM25 ranking. Returns matching memories from any previous session. Requires login.',
    inputSchema: {
      type: 'object',
      properties: {
        query: {
          type: 'string',
          description: 'Search query'
        },
        k: {
          type: 'number',
          description: 'Number of results (default: 5)',
          default: 5
        }
      },
      required: ['query']
    }
  },

  // ============================================================
  // Lattice Meta Tools - Skill Discovery
  // ============================================================

  {
    name: 'fold_search',
    description: 'Search the lattice skill library by keyword. Returns matching skills, functions, and modules. Does not require login.',
    inputSchema: {
      type: 'object',
      properties: {
        query: {
          type: 'string',
          description: 'Search query (e.g. "eigenvalue", "matrix multiply", "parser")'
        },
        limit: {
          type: 'number',
          description: 'Maximum number of results (default: 20)',
          default: 20
        }
      },
      required: ['query']
    }
  },

  {
    name: 'fold_inspect',
    description: 'Get detailed information about a lattice skill or module: description, dependencies, modules, and exported functions. Does not require login.',
    inputSchema: {
      type: 'object',
      properties: {
        skill: {
          type: 'string',
          description: 'Skill name (e.g. "linalg", "optics", "autodiff")'
        }
      },
      required: ['skill']
    }
  },

  {
    name: 'fold_exports',
    description: 'List all exported functions from a lattice skill, grouped by module. Does not require login.',
    inputSchema: {
      type: 'object',
      properties: {
        skill: {
          type: 'string',
          description: 'Skill name (e.g. "linalg", "optics", "autodiff")'
        }
      },
      required: ['skill']
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
  },

  {
    name: 'fold_lsp_completion',
    description: 'Get code completion suggestions at a specific position in a Scheme file. Returns matching symbols, keywords, snippets, and primitives based on the prefix at that position.',
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
    name: 'fold_lsp_document_symbols',
    description: 'Get document symbols for a Scheme file to show its outline/structure. Returns function definitions, variables, and other top-level forms with their locations and nested children.',
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
    name: 'fold_lsp_semantic_tokens',
    description: 'Get semantic tokens for a Scheme file to enable rich syntax highlighting. Returns tokens with types (keyword, function, variable, string, number, comment, operator, macro, parameter, type) and modifiers (definition, declaration, readonly).',
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
  }
];
