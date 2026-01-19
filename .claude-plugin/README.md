# The Fold Plugin for Claude Code

This plugin provides Claude Code integration for The Fold - a content-addressable homoiconic universe built on Chez Scheme.

## Features

### LSP-Powered Code Intelligence

The plugin extends The Fold's MCP server with LSP tools for Scheme code intelligence:

- **`fold_lsp_hover`** - Get type information and documentation at position
- **`fold_lsp_definition`** - Go to symbol definition
- **`fold_lsp_references`** - Find all references to a symbol
- **`fold_lsp_symbols`** - Search workspace symbols by name
- **`fold_lsp_diagnostics`** - Get errors and warnings for a file
- **`fold_lsp_format`** - Format Scheme code
- **`fold_lsp_lookup`** - Combined symbol lookup (hover + def + refs)
- **`fold_lsp_status`** - Check LSP server status

### Skills

- **fold-lsp** - Guidance on using LSP tools effectively for Scheme development

### Commands

- `/lsp-status` - Check LSP server status
- `/check-scheme <file>` - Check a Scheme file for errors

## How It Works

The plugin extends the existing MCP server at `boundary/mcp-server/` with an LSP client that communicates with The Fold's Scheme LSP server at `boundary/lsp/`.

```
Claude Code
    ↓ MCP tools
boundary/mcp-server (Node.js)
    ↓ JSON-RPC over stdio
boundary/lsp (Scheme LSP)
    ↓ parses/analyzes
Scheme source files
```

The LSP server provides:
- Type inference for Scheme code
- Hover information with function signatures
- Go-to-definition navigation
- Find-all-references
- Syntax error detection
- Code formatting

## Prerequisites

1. The Fold daemon must be running (`./daemon.sh start`)
2. The MCP server must be rebuilt after adding LSP support:
   ```bash
   cd boundary/mcp-server
   node ./node_modules/typescript/bin/tsc
   ```

## Usage

Once configured, Claude Code can use LSP tools automatically when working with Scheme files:

1. **Before editing**: Claude can check types and signatures
2. **After editing**: Claude can verify no errors were introduced
3. **When refactoring**: Claude can find all references before making changes
4. **When exploring**: Claude can search symbols and navigate definitions

## Development

### Files Added/Modified

**New files:**
- `boundary/mcp-server/src/lsp-client.ts` - LSP client implementation

**Modified files:**
- `boundary/mcp-server/src/tools.ts` - Added LSP tool definitions
- `boundary/mcp-server/src/index.ts` - Added LSP tool handlers

**Plugin files:**
- `.claude-plugin/plugin.json` - Plugin manifest
- `.claude-plugin/skills/fold-lsp/SKILL.md` - LSP usage skill
- `.claude-plugin/commands/lsp-status.md` - Status command
- `.claude-plugin/commands/check-scheme.md` - Check command

### Testing

Test the LSP tools manually:
```bash
# Via MCP client or Claude Code:
fold_lsp_hover(file: "lattice/linalg/vec.ss", line: 10, character: 5)
fold_lsp_diagnostics(file: "core/base/prelude.ss")
fold_lsp_symbols(query: "matrix")
```

## License

MIT
