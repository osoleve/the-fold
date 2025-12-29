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

/**
 * Initialize IPC directories
 */
export async function initIPC(): Promise<void> {
  await fs.mkdir(REQUESTS_DIR, { recursive: true });
  await fs.mkdir(RESPONSES_DIR, { recursive: true });
}

/**
 * Send an expression to the daemon and wait for response
 */
export async function sendRequest(
  sessionId: string,
  expression: string
): Promise<FoldResponse> {
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
 * Check if the daemon is running
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
  readyFile: string;
  requestsDir: string;
  responsesDir: string;
}

/**
 * Get detailed daemon status for diagnostics
 */
export async function getDaemonStatus(): Promise<DaemonStatus> {
  const readyFile = join(REPL_DIR, 'ready');
  return {
    running: await isDaemonRunning(),
    readyFile,
    requestsDir: REQUESTS_DIR,
    responsesDir: RESPONSES_DIR
  };
}
