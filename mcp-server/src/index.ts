#!/usr/bin/env node

/**
 * The Fold MCP Server
 *
 * Provides multitenancy support for The Fold REPL via Model Context Protocol.
 * Multiple Claude instances can connect and maintain independent sessions.
 */

import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import {
  CallToolRequestSchema,
  ListToolsRequestSchema
} from '@modelcontextprotocol/sdk/types.js';
import { randomUUID } from 'crypto';

import { SessionManager, Session, Tier } from './session.js';
import { sendRequest, sendCommand, initIPC, isDaemonRunning, waitForDaemon, getDaemonStatus } from './ipc.js';
import { tools } from './tools.js';
import { LSPClient, formatLocation, symbolKindName, severityName, completionKindName, semanticTokenTypeName, semanticTokenModifierNames } from './lsp-client.js';

/**
 * Sanitize a symbol name for safe embedding in Scheme eval fallback.
 * Strips anything that isn't alphanumeric, hyphen, underscore, or slash.
 */
function sanitizeSymbol(s: string): string {
  return s.replace(/[^a-zA-Z0-9_\-\/]/g, '');
}

/**
 * Main MCP server class
 */
class FoldMCPServer {
  private server: Server;
  private sessionManager: SessionManager;
  private sessionsByConnection = new Map<string, string>(); // connection -> session ID
  private connectionId: string; // Unique per server instance for session isolation
  private lspClient: LSPClient; // LSP client for Scheme code intelligence
  private projectRoot: string;

  constructor() {
    // Generate unique connection ID for this server instance.
    // Each MCP client spawns its own server process via stdio transport,
    // so a unique ID per server instance provides true multi-session isolation.
    this.connectionId = `mcp_${randomUUID()}`;

    // Determine project root (assume we're running from within the project)
    this.projectRoot = process.env.FOLD_ROOT || process.cwd();
    this.lspClient = new LSPClient(this.projectRoot);

    this.server = new Server(
      {
        name: 'fold-repl',
        version: '0.1.0'
      },
      {
        capabilities: {
          tools: {}
        }
      }
    );

    this.sessionManager = new SessionManager();
    this.setupHandlers();
    this.setupCleanupTimer();
  }

  /**
   * Setup request handlers
   */
  private setupHandlers(): void {
    // List available tools
    this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
      tools
    }));

    // Handle tool calls
    this.server.setRequestHandler(CallToolRequestSchema, async (request) => {
      const { name, arguments: args } = request.params;

      try {
        // Get or create session for this connection
        const session = this.getOrCreateSession();

        switch (name) {
          case 'fold_login':
            return await this.handleLogin(session, args);
          case 'fold_eval':
            return await this.handleEval(session, args);
          case 'fold_help':
            return await this.handleHelp(session);
          case 'fold_who':
            return await this.handleWho(session);
          case 'fold_logout':
            return await this.handleLogout(session);
          case 'fold_status':
            return await this.handleStatus(session);

          // Env/Memory Tools
          case 'fold_env_store':
            return await this.handleEnvStore(session, args);
          case 'fold_env_retrieve':
            return await this.handleEnvRetrieve(session, args);
          case 'fold_env_peek':
            return await this.handleEnvPeek(session, args);
          case 'fold_env_grep':
            return await this.handleEnvGrep(session, args);
          case 'fold_env_keys':
            return await this.handleEnvKeys(session, args);
          case 'fold_env_ingest':
            return await this.handleEnvIngest(session, args);
          case 'fold_memory_store':
            return await this.handleMemoryStore(session, args);
          case 'fold_memory_search':
            return await this.handleMemorySearch(session, args);

          // Lattice Meta Tools
          case 'fold_search':
            return await this.handleSearch(session, args);
          case 'fold_inspect':
            return await this.handleInspect(session, args);
          case 'fold_exports':
            return await this.handleExports(session, args);

          // LSP Tools
          case 'fold_lsp_hover':
            return await this.handleLspHover(args);
          case 'fold_lsp_definition':
            return await this.handleLspDefinition(args);
          case 'fold_lsp_references':
            return await this.handleLspReferences(args);
          case 'fold_lsp_symbols':
            return await this.handleLspSymbols(args);
          case 'fold_lsp_diagnostics':
            return await this.handleLspDiagnostics(args);
          case 'fold_lsp_format':
            return await this.handleLspFormat(args);
          case 'fold_lsp_lookup':
            return await this.handleLspLookup(args);
          case 'fold_lsp_status':
            return await this.handleLspStatus();
          case 'fold_lsp_completion':
            return await this.handleLspCompletion(args);
          case 'fold_lsp_document_symbols':
            return await this.handleLspDocumentSymbols(args);
          case 'fold_lsp_semantic_tokens':
            return await this.handleLspSemanticTokens(args);

          default:
            throw new Error(`Unknown tool: ${name}`);
        }
      } catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        return {
          content: [{ type: 'text', text: `Error: ${message}` }],
          isError: true
        };
      }
    });
  }

  /**
   * Get or create session for current connection.
   * Uses the unique connection ID generated at server construction time.
   * This provides session isolation: each MCP client process gets its own session.
   */
  private getOrCreateSession(): Session {
    let sessionId = this.sessionsByConnection.get(this.connectionId);

    // Check if we have a cached session ID
    if (sessionId) {
      const session = this.sessionManager.getSession(sessionId);
      if (session) {
        return session;
      }
      // Session was cleaned up (expired), remove stale mapping
      this.sessionsByConnection.delete(this.connectionId);
    }

    // Create new session
    const session = this.sessionManager.createSession();
    this.sessionsByConnection.set(this.connectionId, session.id);
    return session;
  }

  /**
   * Handle login tool
   */
  private async handleLogin(session: Session, args: any) {
    const { tier, name, message } = args;

    // Validate tier
    if (!['opus', 'sonnet', 'haiku'].includes(tier)) {
      throw new Error(`Invalid tier: ${tier}`);
    }

    // Validate name
    if (!name || !/^[a-zA-Z][a-zA-Z0-9_-]*$/.test(name)) {
      throw new Error('Invalid name: must start with letter and contain only letters, numbers, hyphens, underscores');
    }

    // Send login request to daemon - call session-login! directly
    const expression = `(session-login! "${session.id}" '${tier} '${name})`;
    const response = await sendRequest(session.id, expression);

    if (response.error) {
      throw new Error(response.error);
    }

    // Update session
    this.sessionManager.login(session.id, tier as Tier, name);

    // Format output message (previously done by hi wrapper)
    const output = `${name} (${tier}) logged in: ${message}`;
    return {
      content: [{ type: 'text', text: output }]
    };
  }

  /**
   * Handle eval tool
   */
  private async handleEval(session: Session, args: any) {
    if (!session.loggedIn) {
      throw new Error('Not logged in. Use fold_login first.');
    }

    const { expression } = args;
    const response = await sendRequest(session.id, expression);

    if (response.error) {
      throw new Error(response.error);
    }

    return {
      content: [{ type: 'text', text: response.output }]
    };
  }

  /**
   * Handle help tool
   */
  private async handleHelp(session: Session) {
    if (!session.loggedIn) {
      throw new Error('Not logged in. Use fold_login first.');
    }

    const response = await sendRequest(session.id, '(help)');

    if (response.error) {
      throw new Error(response.error);
    }

    return {
      content: [{ type: 'text', text: response.output }]
    };
  }

  /**
   * Handle who tool
   */
  private async handleWho(session: Session) {
    if (!session.loggedIn) {
      return {
        content: [{
          type: 'text',
          text: `Session ${session.id}\nStatus: Not logged in\nUse fold_login to login.`
        }]
      };
    }

    const response = await sendRequest(session.id, '(who)');

    if (response.error) {
      throw new Error(response.error);
    }

    return {
      content: [{ type: 'text', text: response.output }]
    };
  }

  /**
   * Handle logout tool
   */
  private async handleLogout(session: Session) {
    if (!session.loggedIn) {
      return {
        content: [{ type: 'text', text: 'Not logged in.' }]
      };
    }

    const response = await sendRequest(session.id, '(bye)');
    this.sessionManager.logout(session.id);

    return {
      content: [{ type: 'text', text: response.output || 'Logged out.' }]
    };
  }

  /**
   * Handle status tool - check daemon connection and diagnostics
   */
  private async handleStatus(session: Session) {
    const daemonRunning = await isDaemonRunning();
    const status = await getDaemonStatus();

    const lines = [
      '=== The Fold MCP Server Status ===',
      '',
      `Daemon: ${daemonRunning ? '✓ Running' : '✗ Not running'}`,
      `Connection ID: ${this.connectionId}`,
      `Session ID: ${session.id}`,
      `Logged in: ${session.loggedIn ? `Yes (${session.tier} / ${session.name})` : 'No'}`,
      '',
      'IPC Paths:',
      `  Ready file: ${status.readyFile}`,
      `  Requests:   ${status.requestsDir}`,
      `  Responses:  ${status.responsesDir}`,
      '',
      `Active sessions: ${this.sessionManager.getSessionCount()}`
    ];

    if (!daemonRunning) {
      lines.push('');
      lines.push('⚠️  Daemon not running. Start with:');
      lines.push('    ./daemon.sh start');
    }

    return {
      content: [{ type: 'text', text: lines.join('\n') }]
    };
  }

  // ============================================================
  // Env/Memory Tool Handlers
  // ============================================================

  private async handleEnvStore(session: Session, args: any) {
    if (!session.loggedIn) throw new Error('Not logged in. Use fold_login first.');
    const { key, expression } = args;
    const response = await sendCommand(session.id, 'env-store', { key, expression });
    if (response.error) throw new Error(response.error);
    return { content: [{ type: 'text', text: response.output }] };
  }

  private async handleEnvRetrieve(session: Session, args: any) {
    if (!session.loggedIn) throw new Error('Not logged in. Use fold_login first.');
    const { key } = args;
    const response = await sendCommand(session.id, 'env-retrieve', { key });
    if (response.error) throw new Error(response.error);
    return { content: [{ type: 'text', text: response.output }] };
  }

  private async handleEnvPeek(session: Session, args: any) {
    if (!session.loggedIn) throw new Error('Not logged in. Use fold_login first.');
    const { key, n = 500 } = args;
    const response = await sendCommand(session.id, 'env-peek', { key, n });
    if (response.error) throw new Error(response.error);
    return { content: [{ type: 'text', text: response.output }] };
  }

  private async handleEnvGrep(session: Session, args: any) {
    if (!session.loggedIn) throw new Error('Not logged in. Use fold_login first.');
    const { key, pattern, k = 5 } = args;
    const response = await sendCommand(session.id, 'env-grep', { key, pattern, k });
    if (response.error) throw new Error(response.error);
    return { content: [{ type: 'text', text: response.output }] };
  }

  private async handleEnvKeys(session: Session, args: any) {
    if (!session.loggedIn) throw new Error('Not logged in. Use fold_login first.');
    const response = await sendCommand(session.id, 'env-keys', {});
    if (response.error) throw new Error(response.error);
    return { content: [{ type: 'text', text: response.output }] };
  }

  private async handleEnvIngest(session: Session, args: any) {
    if (!session.loggedIn) throw new Error('Not logged in. Use fold_login first.');
    const { key, text, chunk_size = 2000 } = args;
    const response = await sendCommand(session.id, 'env-ingest', { key, text, 'chunk-size': chunk_size });
    if (response.error) throw new Error(response.error);
    return { content: [{ type: 'text', text: response.output }] };
  }

  private async handleMemoryStore(session: Session, args: any) {
    if (!session.loggedIn) throw new Error('Not logged in. Use fold_login first.');
    const { key, text } = args;
    const response = await sendCommand(session.id, 'memory-store', { key, text });
    if (response.error) throw new Error(response.error);
    return { content: [{ type: 'text', text: response.output }] };
  }

  private async handleMemorySearch(session: Session, args: any) {
    if (!session.loggedIn) throw new Error('Not logged in. Use fold_login first.');
    const { query, k = 5 } = args;
    const response = await sendCommand(session.id, 'memory-search', { query, k });
    if (response.error) throw new Error(response.error);
    return { content: [{ type: 'text', text: response.output }] };
  }

  // ============================================================
  // Lattice Meta Tool Handlers
  // ============================================================

  /**
   * Handle search tool — lattice skill search
   * Uses v2 command when socket is available, falls back to eval
   */
  private async handleSearch(session: Session, args: any) {
    const { query, limit = 20 } = args;

    try {
      const response = await sendCommand(session.id, 'search', { query, limit });
      if (response.error) {
        throw new Error(response.error);
      }
      return {
        content: [{ type: 'text', text: response.output }]
      };
    } catch {
      // Fallback: send as eval expression
      const response = await sendRequest(session.id, `(lf ${JSON.stringify(query)})`);
      if (response.error) {
        throw new Error(response.error);
      }
      return {
        content: [{ type: 'text', text: response.output }]
      };
    }
  }

  /**
   * Handle inspect tool — skill description and deps
   */
  private async handleInspect(session: Session, args: any) {
    const { skill } = args;

    try {
      const response = await sendCommand(session.id, 'inspect', { skill });
      if (response.error || (response.output && response.output.includes('not found'))) {
        throw new Error(response.error || 'not found');
      }
      return {
        content: [{ type: 'text', text: response.output }]
      };
    } catch {
      // Fallback: li handles both skills and modules
      const safe = sanitizeSymbol(skill);
      const response = await sendRequest(session.id, `(li '${safe})`);
      if (response.error) {
        throw new Error(response.error);
      }
      return {
        content: [{ type: 'text', text: response.output }]
      };
    }
  }

  /**
   * Handle exports tool — skill exported functions
   */
  private async handleExports(session: Session, args: any) {
    const { skill } = args;

    try {
      const response = await sendCommand(session.id, 'exports', { skill });
      if (response.error) {
        throw new Error(response.error);
      }
      return {
        content: [{ type: 'text', text: response.output }]
      };
    } catch {
      const safe = sanitizeSymbol(skill);
      const response = await sendRequest(session.id, `(le '${safe})`);
      if (response.error) {
        throw new Error(response.error);
      }
      return {
        content: [{ type: 'text', text: response.output }]
      };
    }
  }

  // ============================================================
  // LSP Tool Handlers
  // ============================================================

  /**
   * Handle LSP hover - get type info and docs at position
   * Tries IPC v2 command first (worker-side LSP), falls back to subprocess
   */
  private async handleLspHover(args: any) {
    const { file, line, character } = args;

    // Try IPC v2 first — extract symbol name from file position
    // Fall back to subprocess LSP for positional queries
    const result = await this.lspClient.hover(file, line, character);

    if (!result) {
      return {
        content: [{ type: 'text', text: 'No hover information available at this position.' }]
      };
    }

    return {
      content: [{ type: 'text', text: result }]
    };
  }

  /**
   * Handle LSP definition - go to symbol definition
   */
  private async handleLspDefinition(args: any) {
    const { file, line, character } = args;

    const locations = await this.lspClient.definition(file, line, character);

    if (!locations || locations.length === 0) {
      return {
        content: [{ type: 'text', text: 'No definition found for symbol at this position.' }]
      };
    }

    const lines = ['Definition location(s):'];
    for (const loc of locations) {
      lines.push(`  ${formatLocation(loc)}`);
    }

    return {
      content: [{ type: 'text', text: lines.join('\n') }]
    };
  }

  /**
   * Handle LSP references - find all references
   */
  private async handleLspReferences(args: any) {
    const { file, line, character, includeDeclaration = true } = args;

    const locations = await this.lspClient.references(file, line, character, includeDeclaration);

    if (!locations || locations.length === 0) {
      return {
        content: [{ type: 'text', text: 'No references found for symbol at this position.' }]
      };
    }

    const lines = [`Found ${locations.length} reference(s):`];
    for (const loc of locations) {
      lines.push(`  ${formatLocation(loc)}`);
    }

    return {
      content: [{ type: 'text', text: lines.join('\n') }]
    };
  }

  /**
   * Handle LSP symbols - search workspace symbols
   */
  private async handleLspSymbols(args: any) {
    const { query } = args;

    const symbols = await this.lspClient.workspaceSymbol(query);

    if (!symbols || symbols.length === 0) {
      return {
        content: [{ type: 'text', text: `No symbols found matching "${query}".` }]
      };
    }

    const lines = [`Found ${symbols.length} symbol(s) matching "${query}":`];
    for (const sym of symbols) {
      const kind = symbolKindName(sym.kind);
      const container = sym.containerName ? ` (in ${sym.containerName})` : '';
      // Use 0-indexed line numbers so output is directly usable as input to hover/lookup/references
      const file = sym.location?.uri?.replace(/^file:\/\//, '') ?? '?';
      const line = sym.location?.range?.start?.line ?? '?';
      const col = sym.location?.range?.start?.character ?? '?';
      lines.push(`  ${sym.name} [${kind}]${container}`);
      lines.push(`    ${file}:${line}:${col}`);
    }

    return {
      content: [{ type: 'text', text: lines.join('\n') }]
    };
  }

  /**
   * Handle LSP diagnostics - get errors/warnings
   */
  private async handleLspDiagnostics(args: any) {
    const { file } = args;

    const diagnostics = await this.lspClient.getDiagnostics(file);

    if (diagnostics.length === 0) {
      return {
        content: [{ type: 'text', text: `No diagnostics for ${file}. File looks clean!` }]
      };
    }

    const lines = [`Found ${diagnostics.length} diagnostic(s) in ${file}:`];
    for (const diag of diagnostics) {
      const severity = severityName(diag.severity);
      const line = diag.range.start.line + 1;
      const col = diag.range.start.character + 1;
      lines.push(`  [${severity}] Line ${line}:${col}: ${diag.message}`);
    }

    return {
      content: [{ type: 'text', text: lines.join('\n') }]
    };
  }

  /**
   * Handle LSP format - format Scheme code
   */
  private async handleLspFormat(args: any) {
    const { file } = args;

    const formatted = await this.lspClient.format(file);

    if (!formatted) {
      return {
        content: [{ type: 'text', text: 'Failed to format file.' }],
        isError: true
      };
    }

    return {
      content: [{ type: 'text', text: formatted }]
    };
  }

  /**
   * Handle LSP lookup - combined hover + definition + references
   */
  private async handleLspLookup(args: any) {
    const { file, line, character } = args;

    const result = await this.lspClient.lookup(file, line, character);

    const sections: string[] = [];

    // Hover section
    sections.push('=== Type Information ===');
    if (result.hover) {
      sections.push(result.hover);
    } else {
      sections.push('No type information available.');
    }

    // Definition section
    sections.push('');
    sections.push('=== Definition ===');
    if (result.definition && result.definition.length > 0) {
      for (const loc of result.definition) {
        sections.push(formatLocation(loc));
      }
    } else {
      sections.push('No definition found.');
    }

    // References section
    sections.push('');
    sections.push('=== References ===');
    if (result.references && result.references.length > 0) {
      sections.push(`Found ${result.references.length} reference(s):`);
      for (const loc of result.references.slice(0, 20)) { // Limit to 20
        sections.push(`  ${formatLocation(loc)}`);
      }
      if (result.references.length > 20) {
        sections.push(`  ... and ${result.references.length - 20} more`);
      }
    } else {
      sections.push('No references found.');
    }

    return {
      content: [{ type: 'text', text: sections.join('\n') }]
    };
  }

  /**
   * Handle LSP status - check LSP server status
   */
  private async handleLspStatus() {
    const running = this.lspClient.isRunning();

    const lines = [
      '=== LSP Server Status ===',
      '',
      `Status: ${running ? '✓ Running' : '○ Not started'}`,
      `Project root: ${this.projectRoot}`,
      '',
      'The LSP server starts automatically on first use.',
      'It provides type inference, go-to-definition, and other',
      'code intelligence features for Scheme files.'
    ];

    return {
      content: [{ type: 'text', text: lines.join('\n') }]
    };
  }

  /**
   * Handle LSP completion - get code completion suggestions
   */
  private async handleLspCompletion(args: any) {
    const { file, line, character } = args;

    const items = await this.lspClient.completion(file, line, character);

    if (!items || items.length === 0) {
      return {
        content: [{ type: 'text', text: 'No completions available at this position.' }]
      };
    }

    // Format completion items nicely
    const lines = [`Found ${items.length} completion(s):`];
    for (const item of items.slice(0, 50)) { // Limit to 50 items
      const kind = completionKindName(item.kind);
      const detail = item.detail ? ` - ${item.detail}` : '';
      lines.push(`  ${item.label} [${kind}]${detail}`);
    }
    if (items.length > 50) {
      lines.push(`  ... and ${items.length - 50} more`);
    }

    return {
      content: [{ type: 'text', text: lines.join('\n') }]
    };
  }

  private async handleLspDocumentSymbols(args: any) {
    const { file } = args;

    const symbols = await this.lspClient.documentSymbols(file);

    if (!symbols || symbols.length === 0) {
      return {
        content: [{ type: 'text', text: 'No symbols found in this file.' }]
      };
    }

    // Format symbols as a tree structure
    const lines = [`Document symbols for ${file}:`, ''];
    this.formatSymbolTree(symbols, lines, 0);

    return {
      content: [{ type: 'text', text: lines.join('\n') }]
    };
  }

  /**
   * Recursively format document symbols as a tree
   */
  private formatSymbolTree(symbols: any[], lines: string[], depth: number): void {
    const indent = '  '.repeat(depth);
    for (const sym of symbols) {
      const kind = symbolKindName(sym.kind);
      // Use 0-indexed line numbers to match what hover/lookup/references expect
      const line = sym.range?.start?.line !== undefined ? sym.range.start.line : '?';
      lines.push(`${indent}${sym.name} [${kind}] :${line}`);
      if (sym.children && sym.children.length > 0) {
        this.formatSymbolTree(sym.children, lines, depth + 1);
      }
    }
  }

  private async handleLspSemanticTokens(args: any) {
    const { file } = args;

    const tokens = await this.lspClient.semanticTokens(file);

    if (!tokens || tokens.length === 0) {
      return {
        content: [{ type: 'text', text: 'No semantic tokens found in this file.' }]
      };
    }

    // Format tokens grouped by type for readability
    const byType: { [key: string]: any[] } = {};
    for (const tok of tokens) {
      const typeName = semanticTokenTypeName(tok.tokenType);
      if (!byType[typeName]) {
        byType[typeName] = [];
      }
      const mods = semanticTokenModifierNames(tok.modifiers);
      byType[typeName].push({
        line: tok.line + 1,  // 1-indexed for display
        col: tok.character + 1,
        len: tok.length,
        mods: mods.length > 0 ? mods : undefined
      });
    }

    // Build output
    const lines = [`Semantic tokens for ${file}:`, `Total: ${tokens.length} tokens`, ''];
    for (const [typeName, typeTokens] of Object.entries(byType)) {
      lines.push(`${typeName} (${typeTokens.length}):`);
      // Show first 10 of each type to avoid overwhelming output
      const display = typeTokens.slice(0, 10);
      for (const t of display) {
        const modStr = t.mods ? ` [${t.mods.join(', ')}]` : '';
        lines.push(`  :${t.line}:${t.col} len=${t.len}${modStr}`);
      }
      if (typeTokens.length > 10) {
        lines.push(`  ... and ${typeTokens.length - 10} more`);
      }
    }

    return {
      content: [{ type: 'text', text: lines.join('\n') }]
    };
  }

  /**
   * Setup periodic cleanup of expired sessions
   */
  private setupCleanupTimer(): void {
    setInterval(() => {
      const cleaned = this.sessionManager.cleanupExpiredSessions();
      if (cleaned > 0) {
        console.error(`Cleaned up ${cleaned} expired sessions`);
      }
    }, 300000); // 5 minutes
  }

  /**
   * Start the server
   */
  async start(): Promise<void> {
    // Initialize IPC directories first
    await initIPC();

    // Check if daemon is running, with retries
    console.error('Checking for The Fold daemon...');

    const daemonReady = await waitForDaemon((attempt, maxAttempts) => {
      console.error(`Daemon not ready, waiting... (attempt ${attempt}/${maxAttempts})`);
    });

    if (!daemonReady) {
      const status = await getDaemonStatus();
      console.error('');
      console.error('ERROR: The Fold daemon is not running.');
      console.error('');
      console.error('To start the daemon:');
      console.error('  Unix/Mac:  ./daemon.sh start');
      console.error('  Windows:   .\\DAEMON.cmd start');
      console.error('');
      console.error('Diagnostics:');
      console.error(`  Ready file: ${status.readyFile} (not found)`);
      console.error(`  Requests:   ${status.requestsDir}`);
      console.error(`  Responses:  ${status.responsesDir}`);
      console.error('');
      console.error('The MCP server will now exit. Restart after starting the daemon.');
      process.exit(1);
    }

    console.error('Daemon connected.');

    // Start the server
    const transport = new StdioServerTransport();
    await this.server.connect(transport);

    // Exit gracefully when stdin closes (parent disconnected)
    process.stdin.on('end', () => {
      console.error('stdin closed, exiting...');
      process.exit(0);
    });

    process.stdin.on('close', () => {
      console.error('stdin closed (close event), exiting...');
      process.exit(0);
    });

    // Also handle the transport closing
    this.server.onclose = () => {
      console.error('MCP transport closed, exiting...');
      process.exit(0);
    };

    console.error('The Fold MCP Server started');
    console.error(`Connection ID: ${this.connectionId}`);
    console.error(`Active sessions: ${this.sessionManager.getSessionCount()}`);
  }
}

/**
 * Escape a string for use in Scheme
 */
function escapeString(str: string): string {
  return str
    .replace(/\\/g, '\\\\')
    .replace(/"/g, '\\"')
    .replace(/\n/g, '\\n')
    .replace(/\t/g, '\\t');
}

/**
 * Main entry point
 */
async function main() {
  const server = new FoldMCPServer();
  await server.start();
}

main().catch((error) => {
  console.error('Fatal error:', error);
  process.exit(1);
});
