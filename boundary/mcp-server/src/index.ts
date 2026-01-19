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
import { sendRequest, initIPC, isDaemonRunning, waitForDaemon, getDaemonStatus } from './ipc.js';
import { tools } from './tools.js';
import { LSPClient, formatLocation, symbolKindName, severityName } from './lsp-client.js';

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
    if (!sessionId) {
      const session = this.sessionManager.createSession();
      sessionId = session.id;
      this.sessionsByConnection.set(this.connectionId, sessionId);
    }

    const session = this.sessionManager.getSession(sessionId);
    if (!session) {
      throw new Error('Session not found');
    }

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

    // Send login request to daemon
    const expression = `(hi '${tier} '${name} "${escapeString(message)}")`;
    const response = await sendRequest(session.id, expression);

    if (response.error) {
      throw new Error(response.error);
    }

    // Update session
    this.sessionManager.login(session.id, tier as Tier, name);

    return {
      content: [{ type: 'text', text: response.output }]
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
  // LSP Tool Handlers
  // ============================================================

  /**
   * Handle LSP hover - get type info and docs at position
   */
  private async handleLspHover(args: any) {
    const { file, line, character } = args;

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
      lines.push(`  ${sym.name} [${kind}]${container}`);
      lines.push(`    ${formatLocation(sym.location)}`);
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
