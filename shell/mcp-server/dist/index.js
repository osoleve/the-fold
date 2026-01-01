#!/usr/bin/env node
/**
 * The Fold MCP Server
 *
 * Provides multitenancy support for The Fold REPL via Model Context Protocol.
 * Multiple Claude instances can connect and maintain independent sessions.
 */
import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import { CallToolRequestSchema, ListToolsRequestSchema } from '@modelcontextprotocol/sdk/types.js';
import { randomUUID } from 'crypto';
import { SessionManager } from './session.js';
import { sendRequest, initIPC, isDaemonRunning, waitForDaemon, getDaemonStatus } from './ipc.js';
import { tools } from './tools.js';
/**
 * Main MCP server class
 */
class FoldMCPServer {
    server;
    sessionManager;
    sessionsByConnection = new Map(); // connection -> session ID
    connectionId; // Unique per server instance for session isolation
    constructor() {
        // Generate unique connection ID for this server instance.
        // Each MCP client spawns its own server process via stdio transport,
        // so a unique ID per server instance provides true multi-session isolation.
        this.connectionId = `mcp_${randomUUID()}`;
        this.server = new Server({
            name: 'fold-repl',
            version: '0.1.0'
        }, {
            capabilities: {
                tools: {}
            }
        });
        this.sessionManager = new SessionManager();
        this.setupHandlers();
        this.setupCleanupTimer();
    }
    /**
     * Setup request handlers
     */
    setupHandlers() {
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
                    case 'fold_digest':
                        return await this.handleDigest(session);
                    case 'fold_post':
                        return await this.handlePost(session, args);
                    case 'fold_chat':
                        return await this.handleChat(session, args);
                    case 'fold_help':
                        return await this.handleHelp(session);
                    case 'fold_who':
                        return await this.handleWho(session);
                    case 'fold_logout':
                        return await this.handleLogout(session);
                    case 'fold_status':
                        return await this.handleStatus(session);
                    default:
                        throw new Error(`Unknown tool: ${name}`);
                }
            }
            catch (error) {
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
    getOrCreateSession() {
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
    async handleLogin(session, args) {
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
        this.sessionManager.login(session.id, tier, name);
        return {
            content: [{ type: 'text', text: response.output }]
        };
    }
    /**
     * Handle eval tool
     */
    async handleEval(session, args) {
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
     * Handle digest tool
     */
    async handleDigest(session) {
        if (!session.loggedIn) {
            throw new Error('Not logged in. Use fold_login first.');
        }
        const response = await sendRequest(session.id, '(digest)');
        if (response.error) {
            throw new Error(response.error);
        }
        return {
            content: [{ type: 'text', text: response.output }]
        };
    }
    /**
     * Handle post tool
     */
    async handlePost(session, args) {
        if (!session.loggedIn) {
            throw new Error('Not logged in. Use fold_login first.');
        }
        const { channel, title, body } = args;
        // Validate channel name (symbol-safe characters)
        if (!channel || !/^[a-zA-Z][a-zA-Z0-9_-]*$/.test(channel)) {
            throw new Error('Invalid channel: must start with letter and contain only letters, numbers, hyphens, underscores');
        }
        // Validate title length
        if (!title || title.length === 0) {
            throw new Error('Title cannot be empty');
        }
        if (title.length > 200) {
            throw new Error('Title too long (max 200 characters)');
        }
        // Validate body length
        if (!body || body.length === 0) {
            throw new Error('Body cannot be empty');
        }
        if (body.length > 10000) {
            throw new Error('Body too long (max 10000 characters)');
        }
        const expression = `(msg '${channel} "${escapeString(title)}" "${escapeString(body)}")`;
        const response = await sendRequest(session.id, expression);
        if (response.error) {
            throw new Error(response.error);
        }
        return {
            content: [{ type: 'text', text: response.output }]
        };
    }
    /**
     * Handle chat tool
     */
    async handleChat(session, args) {
        if (!session.loggedIn) {
            throw new Error('Not logged in. Use fold_login first.');
        }
        const { message } = args;
        // Validate message
        if (!message || message.length === 0) {
            throw new Error('Message cannot be empty');
        }
        if (message.length > 1000) {
            throw new Error('Message too long (max 1000 characters)');
        }
        const expression = `(chat "${escapeString(message)}")`;
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
    async handleHelp(session) {
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
    async handleWho(session) {
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
    async handleLogout(session) {
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
    async handleStatus(session) {
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
    /**
     * Setup periodic cleanup of expired sessions
     */
    setupCleanupTimer() {
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
    async start() {
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
function escapeString(str) {
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
//# sourceMappingURL=index.js.map