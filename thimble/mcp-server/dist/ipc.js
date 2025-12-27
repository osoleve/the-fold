/**
 * IPC utilities for communicating with The Fold REPL daemon
 *
 * Implements session-aware file-based IPC:
 * - Writes requests to .fold-repl/requests/<session-id>.ss
 * - Reads responses from .fold-repl/responses/<session-id>.txt
 * - Handles concurrent sessions
 */
import { promises as fs } from 'fs';
import { join } from 'path';
const REPL_DIR = '.fold-repl';
const REQUESTS_DIR = join(REPL_DIR, 'requests');
const RESPONSES_DIR = join(REPL_DIR, 'responses');
const POLL_INTERVAL_MS = 100;
const TIMEOUT_MS = 30000; // 30 seconds
/**
 * Initialize IPC directories
 */
export async function initIPC() {
    await fs.mkdir(REQUESTS_DIR, { recursive: true });
    await fs.mkdir(RESPONSES_DIR, { recursive: true });
}
/**
 * Send an expression to the daemon and wait for response
 */
export async function sendRequest(sessionId, expression) {
    const requestFile = join(REQUESTS_DIR, `${sessionId}.ss`);
    const responseFile = join(RESPONSES_DIR, `${sessionId}.txt`);
    const errorFile = join(RESPONSES_DIR, `${sessionId}.error.txt`);
    // Clean up any old response files
    await cleanupFiles(responseFile, errorFile);
    // Write the request
    const request = {
        sessionId,
        expression,
        timestamp: Date.now()
    };
    // Format as Scheme S-expression
    const requestContent = formatRequest(request);
    await fs.writeFile(requestFile, requestContent, 'utf-8');
    // Poll for response
    const startTime = Date.now();
    while (Date.now() - startTime < TIMEOUT_MS) {
        // Check for error first
        try {
            const errorContent = await fs.readFile(errorFile, 'utf-8');
            await cleanupFiles(requestFile, responseFile, errorFile);
            return {
                output: '',
                error: errorContent
            };
        }
        catch {
            // No error file, continue
        }
        // Check for response
        try {
            const output = await fs.readFile(responseFile, 'utf-8');
            await cleanupFiles(requestFile, responseFile, errorFile);
            return { output };
        }
        catch {
            // Not ready yet, poll again
            await sleep(POLL_INTERVAL_MS);
        }
    }
    // Timeout
    await cleanupFiles(requestFile, responseFile, errorFile);
    throw new Error(`Request timeout after ${TIMEOUT_MS}ms`);
}
/**
 * Format a request as a Scheme S-expression
 */
function formatRequest(request) {
    // Escape the expression properly
    const escapedExpr = request.expression.replace(/\\/g, '\\\\').replace(/"/g, '\\"');
    return `((session-id . "${request.sessionId}")
 (expression . ${request.expression})
 (timestamp . ${request.timestamp}))`;
}
/**
 * Clean up files (ignore errors if they don't exist)
 */
async function cleanupFiles(...files) {
    await Promise.all(files.map(file => fs.unlink(file).catch(() => { })));
}
/**
 * Sleep for specified milliseconds
 */
function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}
/**
 * Check if the daemon is running
 */
export async function isDaemonRunning() {
    const readyFile = join(REPL_DIR, 'ready');
    try {
        await fs.access(readyFile);
        return true;
    }
    catch {
        return false;
    }
}
//# sourceMappingURL=ipc.js.map