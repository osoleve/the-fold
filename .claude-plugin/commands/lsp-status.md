---
name: lsp-status
description: Check the status of The Fold's LSP server
---

# LSP Status Command

Check the status of The Fold's LSP (Language Server Protocol) server.

## Instructions

1. Call the `fold_lsp_status` MCP tool to get LSP server status
2. Report the status to the user including:
   - Whether the LSP server is running
   - The project root path
   - Brief explanation of what the LSP provides

## Output Format

Present the status clearly with:
- Server status (running/not started)
- Project root location
- Available capabilities

Note: The LSP server starts automatically on first use of any LSP tool.
