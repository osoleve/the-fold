/**
 * IPC utilities for communicating with The Fold REPL daemon
 *
 * Supports two transports:
 * 1. Socket-based (preferred): Unix domain socket with length-prefixed s-expression frames
 * 2. File-based (fallback): Write to .fold-repl/requests/, poll .fold-repl/responses/
 *
 * Transport is auto-detected from the ready file contents.
 */

import { promises as fs } from 'fs';
import { join } from 'path';
import { createConnection, Socket } from 'net';

export interface FoldRequest {
  sessionId: string;
  expression: string;
  timestamp: number;
}

export interface FoldResponse {
  output: string;
  error?: string;
}

const REPL_DIR = '.fold-repl';
const REQUESTS_DIR = join(REPL_DIR, 'requests');
const RESPONSES_DIR = join(REPL_DIR, 'responses');
const POLL_INTERVAL_MS = 100;
const TIMEOUT_MS = 30000; // 30 seconds
const DAEMON_RETRY_ATTEMPTS = 5;
const DAEMON_RETRY_DELAY_MS = 2000;
const SOCKET_TIMEOUT_MS = 30000;

/**
 * Initialize IPC directories
 */
export async function initIPC(): Promise<void> {
  await fs.mkdir(REQUESTS_DIR, { recursive: true });
  await fs.mkdir(RESPONSES_DIR, { recursive: true });
}

/**
 * Get socket path from ready file, or null if file-based daemon
 */
async function getSocketPath(): Promise<string | null> {
  const readyFile = join(REPL_DIR, 'ready');
  try {
    const content = (await fs.readFile(readyFile, 'utf-8')).trim();
    if (content.startsWith('socket:')) {
      const sockPath = content.slice('socket:'.length);
      await fs.access(sockPath);
      return sockPath;
    }
  } catch {
    // No socket daemon
  }
  return null;
}

/**
 * Send request via Unix domain socket with length-prefixed s-expression framing
 */
async function sendRequestSocket(
  sessionId: string,
  expression: string,
  sockPath: string
): Promise<FoldResponse> {
  return new Promise((resolve, reject) => {
    const sock = createConnection({ path: sockPath });
    const reqId = Math.random().toString(36).slice(2, 10);

    // Escape expression for s-expression embedding
    const escapedExpr = expression.replace(/\\/g, '\\\\').replace(/"/g, '\\"');
    const msg = `((type . request) (id . "${reqId}") (session . "${sessionId}") (expr . "${escapedExpr}"))`;
    const payload = Buffer.from(msg, 'utf-8');
    const header = Buffer.alloc(4);
    header.writeUInt32BE(payload.length);
    sock.write(Buffer.concat([header, payload]));

    // Collect response
    let buf = Buffer.alloc(0);
    sock.on('data', (chunk: Buffer) => {
      buf = Buffer.concat([buf, chunk]);
      if (buf.length >= 4) {
        const len = buf.readUInt32BE(0);
        if (buf.length >= 4 + len) {
          const resp = buf.slice(4, 4 + len).toString('utf-8');
          sock.destroy();
          resolve(parseSexpResponse(resp));
        }
      }
    });
    sock.on('error', (err: Error) => {
      reject(err);
    });
    sock.setTimeout(SOCKET_TIMEOUT_MS, () => {
      sock.destroy();
      reject(new Error('Socket timeout'));
    });
  });
}

/**
 * Parse s-expression response alist into FoldResponse
 */
function parseSexpResponse(resp: string): FoldResponse {
  const typeMatch = resp.match(/\(type\s+\.\s+(\w+)\)/);
  const msgType = typeMatch ? typeMatch[1] : 'unknown';

  if (msgType === 'result') {
    const valueMatch = resp.match(/\(value\s+\.\s+"((?:[^"\\]|\\.)*)"\)/);
    const value = valueMatch
      ? valueMatch[1].replace(/\\"/g, '"').replace(/\\\\/g, '\\')
      : '';
    return { output: value };
  } else if (msgType === 'error') {
    const errMatch = resp.match(/\(message\s+\.\s+"((?:[^"\\]|\\.)*)"\)/);
    const errorMsg = errMatch
      ? errMatch[1].replace(/\\"/g, '"').replace(/\\\\/g, '\\')
      : 'unknown error';
    return { output: '', error: errorMsg };
  }
  return { output: '', error: `Unexpected response type: ${msgType}` };
}

/**
 * Send an expression to the daemon and wait for response.
 * Auto-detects socket vs file-based transport.
 */
export async function sendRequest(
  sessionId: string,
  expression: string
): Promise<FoldResponse> {
  // Try socket transport first
  const sockPath = await getSocketPath();
  if (sockPath) {
    return sendRequestSocket(sessionId, expression, sockPath);
  }

  // Fall back to file-based IPC
  const requestFile = join(REQUESTS_DIR, `${sessionId}.ss`);
  const responseFile = join(RESPONSES_DIR, `${sessionId}.txt`);
  const errorFile = join(RESPONSES_DIR, `${sessionId}.error.txt`);

  // Clean up any old response files
  await cleanupFiles(responseFile, errorFile);

  // Write the request
  const request: FoldRequest = {
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
    } catch {
      // No error file, continue
    }

    // Check for response
    try {
      const output = await fs.readFile(responseFile, 'utf-8');
      await cleanupFiles(requestFile, responseFile, errorFile);
      return { output };
    } catch {
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
function formatRequest(request: FoldRequest): string {
  // Escape the expression properly
  const escapedExpr = request.expression.replace(/\\/g, '\\\\').replace(/"/g, '\\"');

  return `((session-id . "${request.sessionId}")
 (expression . ${request.expression})
 (timestamp . ${request.timestamp}))`;
}

/**
 * Clean up files (ignore errors if they don't exist)
 */
async function cleanupFiles(...files: string[]): Promise<void> {
  await Promise.all(
    files.map(file => fs.unlink(file).catch(() => {}))
  );
}

/**
 * Sleep for specified milliseconds
 */
function sleep(ms: number): Promise<void> {
  return new Promise(resolve => setTimeout(resolve, ms));
}

/**
 * Check if the daemon is running (file-based or socket-based)
 */
export async function isDaemonRunning(): Promise<boolean> {
  const readyFile = join(REPL_DIR, 'ready');
  try {
    await fs.access(readyFile);
    return true;
  } catch {
    return false;
  }
}

/**
 * Wait for daemon to become available with retries
 * Returns true if daemon is running, false if all retries exhausted
 */
export async function waitForDaemon(
  onRetry?: (attempt: number, maxAttempts: number) => void
): Promise<boolean> {
  for (let attempt = 1; attempt <= DAEMON_RETRY_ATTEMPTS; attempt++) {
    if (await isDaemonRunning()) {
      return true;
    }

    if (attempt < DAEMON_RETRY_ATTEMPTS) {
      if (onRetry) {
        onRetry(attempt, DAEMON_RETRY_ATTEMPTS);
      }
      await sleep(DAEMON_RETRY_DELAY_MS);
    }
  }
  return false;
}

/**
 * Connection status information
 */
export interface DaemonStatus {
  running: boolean;
  transport: 'socket' | 'file' | 'none';
  socketPath?: string;
  readyFile: string;
  requestsDir: string;
  responsesDir: string;
}

/**
 * Get detailed daemon status for diagnostics
 */
export async function getDaemonStatus(): Promise<DaemonStatus> {
  const readyFile = join(REPL_DIR, 'ready');
  const running = await isDaemonRunning();
  const socketPath = running ? await getSocketPath() : null;
  return {
    running,
    transport: socketPath ? 'socket' : (running ? 'file' : 'none'),
    socketPath: socketPath || undefined,
    readyFile,
    requestsDir: REQUESTS_DIR,
    responsesDir: RESPONSES_DIR
  };
}
