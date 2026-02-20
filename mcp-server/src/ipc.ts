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
  data?: any; // v2 structured data
}

const REPL_DIR = '.fold-repl';
const REQUESTS_DIR = join(REPL_DIR, 'requests');
const RESPONSES_DIR = join(REPL_DIR, 'responses');
const POLL_INTERVAL_MS = 100;
const TIMEOUT_MS = 120000; // 120 seconds — must exceed worker eval timeout (90s)
const DAEMON_RETRY_ATTEMPTS = 5;
const DAEMON_RETRY_DELAY_MS = 2000;
const SOCKET_TIMEOUT_MS = 120000;

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

// ============================================================
// S-expression Parser
// ============================================================

type SExp = string | number | boolean | symbol | SExp[] | { car: SExp; cdr: SExp } | null;

interface ParseState {
  src: string;
  pos: number;
}

function sexpPeek(s: ParseState): string {
  return s.pos < s.src.length ? s.src[s.pos] : '';
}

function sexpAdvance(s: ParseState): string {
  return s.src[s.pos++] ?? '';
}

function sexpSkipWs(s: ParseState): void {
  while (s.pos < s.src.length) {
    const c = s.src[s.pos];
    if (c === ' ' || c === '\t' || c === '\n' || c === '\r') {
      s.pos++;
    } else if (c === ';') {
      // Skip line comment
      while (s.pos < s.src.length && s.src[s.pos] !== '\n') s.pos++;
    } else {
      break;
    }
  }
}

function sexpReadString(s: ParseState): string {
  sexpAdvance(s); // consume opening "
  let result = '';
  while (s.pos < s.src.length) {
    const c = sexpAdvance(s);
    if (c === '"') return result;
    if (c === '\\') {
      const next = sexpAdvance(s);
      switch (next) {
        case 'n': result += '\n'; break;
        case 't': result += '\t'; break;
        case 'r': result += '\r'; break;
        case '"': result += '"'; break;
        case '\\': result += '\\'; break;
        default: result += next; break;
      }
    } else {
      result += c;
    }
  }
  return result;
}

function isAtomChar(c: string): boolean {
  return c !== '' && c !== '(' && c !== ')' && c !== '"' && c !== ' '
    && c !== '\t' && c !== '\n' && c !== '\r' && c !== ';';
}

function sexpReadAtom(s: ParseState): SExp {
  let token = '';
  while (s.pos < s.src.length && isAtomChar(s.src[s.pos])) {
    token += sexpAdvance(s);
  }
  // Booleans
  if (token === '#t') return true;
  if (token === '#f') return false;
  // Numbers (integer and decimal)
  if (/^-?(\d+\.?\d*|\.\d+)(e[+-]?\d+)?$/i.test(token)) {
    return Number(token);
  }
  // Symbol (returned as string — we use alist key matching)
  return token;
}

function sexpReadList(s: ParseState): SExp {
  sexpAdvance(s); // consume (
  const items: SExp[] = [];
  let dotted = false;
  let cdrVal: SExp = null;

  while (true) {
    sexpSkipWs(s);
    const c = sexpPeek(s);
    if (c === '' || c === ')') {
      sexpAdvance(s); // consume )
      break;
    }
    if (c === '.') {
      // Check for dotted pair: peek ahead to see if it's ". " (dotted) vs ".123" (number)
      const next = s.pos + 1 < s.src.length ? s.src[s.pos + 1] : '';
      if (!isAtomChar(next) || next === '') {
        sexpAdvance(s); // consume .
        sexpSkipWs(s);
        cdrVal = sexpRead(s);
        dotted = true;
        sexpSkipWs(s);
        if (sexpPeek(s) === ')') sexpAdvance(s); // consume )
        break;
      }
    }
    items.push(sexpRead(s));
  }

  if (dotted && items.length === 1) {
    // Proper dotted pair: (a . b) → {car, cdr}
    return { car: items[0], cdr: cdrVal };
  }
  if (dotted) {
    // Improper list: (a b . c) → store as array with __dotted marker
    return items; // Treat as list; dotted tail is rare in our protocol
  }
  return items;
}

function sexpRead(s: ParseState): SExp {
  sexpSkipWs(s);
  const c = sexpPeek(s);
  if (c === '(') return sexpReadList(s);
  if (c === '"') return sexpReadString(s);
  if (c === "'") {
    sexpAdvance(s); // consume '
    return ['quote', sexpRead(s)];
  }
  return sexpReadAtom(s);
}

/**
 * Parse a full s-expression string into a JS value
 */
function parseSexp(src: string): SExp {
  const state: ParseState = { src, pos: 0 };
  return sexpRead(state);
}

/**
 * Extract value from an alist (list of dotted pairs) by string key
 */
function alistGet(alist: SExp, key: string): SExp | undefined {
  if (!Array.isArray(alist)) return undefined;
  for (const item of alist) {
    if (item && typeof item === 'object' && 'car' in item) {
      if (item.car === key) return item.cdr;
    }
  }
  return undefined;
}

/**
 * Parse s-expression response alist into FoldResponse
 * Handles both v1 and v2 response formats.
 */
function parseSexpResponse(resp: string): FoldResponse {
  const parsed = parseSexp(resp);
  const msgType = alistGet(parsed, 'type');

  if (msgType === 'result') {
    const value = alistGet(parsed, 'value');
    const data = alistGet(parsed, 'data');
    return {
      output: typeof value === 'string' ? value : (value !== undefined ? String(value) : ''),
      data: data ?? undefined
    };
  } else if (msgType === 'error') {
    const errorMsg = alistGet(parsed, 'message');
    return {
      output: '',
      error: typeof errorMsg === 'string' ? errorMsg : 'unknown error'
    };
  }
  return { output: '', error: `Unexpected response type: ${String(msgType)}` };
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
 * Send a v2 structured command to the daemon.
 * Commands are dispatched by the worker without eval — they map to
 * specific handlers (search, inspect, exports, lsp-*, env-*).
 */
export async function sendCommand(
  sessionId: string,
  cmd: string,
  args: Record<string, any>
): Promise<FoldResponse> {
  const sockPath = await getSocketPath();
  if (sockPath) {
    return sendCommandSocket(sessionId, cmd, args, sockPath);
  }
  // File-based fallback: wrap as eval expression
  // This is a best-effort fallback — v2 commands need socket transport
  const expr = `(${cmd} ${Object.values(args).map(v => JSON.stringify(v)).join(' ')})`;
  return sendRequest(sessionId, expr);
}

/**
 * Send v2 command via socket
 */
async function sendCommandSocket(
  sessionId: string,
  cmd: string,
  args: Record<string, any>,
  sockPath: string
): Promise<FoldResponse> {
  return new Promise((resolve, reject) => {
    const sock = createConnection({ path: sockPath });
    const reqId = Math.random().toString(36).slice(2, 10);

    // Build v2 request as s-expression alist
    const argPairs = Object.entries(args)
      .map(([k, v]) => {
        const val = typeof v === 'string'
          ? `"${v.replace(/\\/g, '\\\\').replace(/"/g, '\\"')}"`
          : String(v);
        return `(${k} . ${val})`;
      })
      .join(' ');

    const msg = `((type . request) (v . 2) (id . "${reqId}") (session . "${sessionId}") (cmd . ${cmd}) (args . (${argPairs})))`;
    const payload = Buffer.from(msg, 'utf-8');
    const header = Buffer.alloc(4);
    header.writeUInt32BE(payload.length);
    sock.write(Buffer.concat([header, payload]));

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
    sock.on('error', (err: Error) => reject(err));
    sock.setTimeout(SOCKET_TIMEOUT_MS, () => {
      sock.destroy();
      reject(new Error('Socket timeout'));
    });
  });
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
