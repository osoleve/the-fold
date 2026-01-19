/**
 * LSP Client for The Fold MCP Server
 *
 * Manages a Scheme LSP server subprocess and provides methods for
 * LSP operations (hover, definition, references, etc.).
 *
 * Uses "reload-on-query" strategy: file content is synced on each request
 * rather than maintaining persistent document state.
 */

import { spawn, ChildProcessWithoutNullStreams } from 'child_process';
import { join, dirname } from 'path';
import { readFile } from 'fs/promises';
import { fileURLToPath } from 'url';

// LSP JSON-RPC message types
interface LSPRequest {
  jsonrpc: '2.0';
  id: number;
  method: string;
  params?: any;
}

interface LSPNotification {
  jsonrpc: '2.0';
  method: string;
  params?: any;
}

interface LSPResponse {
  jsonrpc: '2.0';
  id: number;
  result?: any;
  error?: {
    code: number;
    message: string;
    data?: any;
  };
}

// LSP Position and Location types
interface Position {
  line: number;      // 0-indexed
  character: number; // 0-indexed, UTF-16 code units
}

interface Range {
  start: Position;
  end: Position;
}

interface Location {
  uri: string;
  range: Range;
}

interface TextDocumentIdentifier {
  uri: string;
}

interface TextDocumentPositionParams {
  textDocument: TextDocumentIdentifier;
  position: Position;
}

interface SymbolInformation {
  name: string;
  kind: number;
  location: Location;
  containerName?: string;
}

interface Diagnostic {
  range: Range;
  severity?: number;
  code?: number | string;
  source?: string;
  message: string;
}

interface CompletionItem {
  label: string;
  kind?: number;
  detail?: string;
  insertText?: string;
  insertTextFormat?: number; // 1 = PlainText, 2 = Snippet
}

interface DocumentSymbol {
  name: string;
  kind: number;
  range: Range;
  selectionRange: Range;
  children?: DocumentSymbol[];
}

/**
 * LSP Client class
 * Manages LSP server process and provides high-level API
 */
export class LSPClient {
  private process: ChildProcessWithoutNullStreams | null = null;
  private requestId = 0;
  private pendingRequests = new Map<number, {
    resolve: (result: any) => void;
    reject: (error: Error) => void;
  }>();
  private buffer = '';
  private contentLength = -1;
  private initialized = false;
  private projectRoot: string;
  private diagnostics = new Map<string, Diagnostic[]>();

  constructor(projectRoot: string) {
    this.projectRoot = projectRoot;
  }

  /**
   * Start the LSP server process
   */
  async start(): Promise<void> {
    if (this.process) {
      return; // Already running
    }

    const lspScript = join(this.projectRoot, 'boundary', 'lsp', 'start-lsp.ss');

    this.process = spawn('scheme', ['--script', lspScript], {
      cwd: this.projectRoot,
      stdio: ['pipe', 'pipe', 'pipe']
    });

    // Handle stdout (LSP responses)
    this.process.stdout.on('data', (chunk: Buffer) => {
      this.handleData(chunk.toString('utf-8'));
    });

    // Log stderr for debugging
    this.process.stderr.on('data', (chunk: Buffer) => {
      console.error('[LSP]', chunk.toString('utf-8').trim());
    });

    // Handle process exit
    this.process.on('exit', (code) => {
      console.error(`[LSP] Process exited with code ${code}`);
      this.process = null;
      this.initialized = false;
      // Reject all pending requests
      for (const [id, handlers] of this.pendingRequests) {
        handlers.reject(new Error(`LSP server exited with code ${code}`));
      }
      this.pendingRequests.clear();
    });

    this.process.on('error', (err) => {
      console.error('[LSP] Process error:', err);
    });

    // Initialize the LSP server
    await this.initialize();
  }

  /**
   * Stop the LSP server process
   */
  async stop(): Promise<void> {
    if (!this.process) {
      return;
    }

    // Send shutdown request
    try {
      await this.sendRequest('shutdown', {});
    } catch (e) {
      // Ignore errors during shutdown
    }

    // Send exit notification
    this.sendNotification('exit', {});

    // Kill the process if it doesn't exit gracefully
    setTimeout(() => {
      if (this.process) {
        this.process.kill();
        this.process = null;
      }
    }, 1000);
  }

  /**
   * Check if LSP server is running
   */
  isRunning(): boolean {
    return this.process !== null && this.initialized;
  }

  /**
   * Handle incoming data from LSP server
   */
  private handleData(data: string): void {
    this.buffer += data;

    while (true) {
      // Parse Content-Length header if we don't have one
      if (this.contentLength < 0) {
        const headerEnd = this.buffer.indexOf('\r\n\r\n');
        if (headerEnd < 0) {
          return; // Wait for complete header
        }

        const header = this.buffer.slice(0, headerEnd);
        const match = header.match(/Content-Length:\s*(\d+)/i);
        if (!match) {
          console.error('[LSP] Invalid header:', header);
          this.buffer = this.buffer.slice(headerEnd + 4);
          continue;
        }

        this.contentLength = parseInt(match[1], 10);
        this.buffer = this.buffer.slice(headerEnd + 4);
      }

      // Check if we have the complete message
      if (Buffer.byteLength(this.buffer, 'utf-8') < this.contentLength) {
        return; // Wait for complete message
      }

      // Extract message (handling UTF-8 correctly)
      const messageBytes = Buffer.from(this.buffer, 'utf-8').slice(0, this.contentLength);
      const message = messageBytes.toString('utf-8');
      this.buffer = Buffer.from(this.buffer, 'utf-8').slice(this.contentLength).toString('utf-8');
      this.contentLength = -1;

      // Parse and handle the message
      try {
        const json = JSON.parse(message);
        this.handleMessage(json);
      } catch (e) {
        console.error('[LSP] Failed to parse message:', e);
      }
    }
  }

  /**
   * Handle a parsed LSP message
   */
  private handleMessage(message: any): void {
    // Check if it's a response to a request
    if ('id' in message && message.id !== null) {
      const handlers = this.pendingRequests.get(message.id);
      if (handlers) {
        this.pendingRequests.delete(message.id);
        if (message.error) {
          handlers.reject(new Error(message.error.message));
        } else {
          handlers.resolve(message.result);
        }
      }
      return;
    }

    // Handle notifications
    if (message.method === 'textDocument/publishDiagnostics') {
      const { uri, diagnostics } = message.params;
      this.diagnostics.set(uri, diagnostics);
    }
  }

  /**
   * Send a request and wait for response
   */
  private async sendRequest(method: string, params: any): Promise<any> {
    if (!this.process) {
      throw new Error('LSP server not running');
    }

    const id = ++this.requestId;
    const request: LSPRequest = {
      jsonrpc: '2.0',
      id,
      method,
      params
    };

    return new Promise((resolve, reject) => {
      this.pendingRequests.set(id, { resolve, reject });
      this.writeMessage(request);

      // Timeout after 30 seconds
      setTimeout(() => {
        if (this.pendingRequests.has(id)) {
          this.pendingRequests.delete(id);
          reject(new Error(`LSP request timeout: ${method}`));
        }
      }, 30000);
    });
  }

  /**
   * Send a notification (no response expected)
   */
  private sendNotification(method: string, params: any): void {
    if (!this.process) {
      return;
    }

    const notification: LSPNotification = {
      jsonrpc: '2.0',
      method,
      params
    };

    this.writeMessage(notification);
  }

  /**
   * Write a JSON-RPC message to the LSP server
   */
  private writeMessage(message: any): void {
    if (!this.process) {
      return;
    }

    const content = JSON.stringify(message);
    const contentLength = Buffer.byteLength(content, 'utf-8');
    const header = `Content-Length: ${contentLength}\r\n\r\n`;

    this.process.stdin.write(header);
    this.process.stdin.write(content);
  }

  /**
   * Initialize the LSP server
   */
  private async initialize(): Promise<void> {
    const result = await this.sendRequest('initialize', {
      processId: process.pid,
      rootUri: `file://${this.projectRoot}`,
      capabilities: {
        textDocument: {
          hover: { contentFormat: ['markdown', 'plaintext'] },
          completion: { completionItem: { snippetSupport: true } },
          definition: {},
          references: {},
          documentSymbol: {},
          formatting: {},
          publishDiagnostics: {}
        },
        workspace: {
          symbol: {}
        }
      }
    });

    // Send initialized notification
    this.sendNotification('initialized', {});
    this.initialized = true;

    console.error('[LSP] Server initialized');
    console.error('[LSP] Capabilities:', JSON.stringify(result.capabilities, null, 2).split('\n').slice(0, 5).join('\n') + '...');
  }

  /**
   * Convert file path to URI
   */
  private pathToUri(path: string): string {
    if (path.startsWith('file://')) {
      return path;
    }
    // Handle relative paths
    if (!path.startsWith('/')) {
      path = join(this.projectRoot, path);
    }
    return `file://${path}`;
  }

  /**
   * Convert URI to file path
   */
  private uriToPath(uri: string): string {
    if (uri.startsWith('file://')) {
      return uri.slice(7);
    }
    return uri;
  }

  /**
   * Sync a document with the LSP server (reload-on-query strategy)
   */
  private async syncDocument(filePath: string): Promise<string> {
    const uri = this.pathToUri(filePath);
    const content = await readFile(this.uriToPath(uri), 'utf-8');

    // Open the document (LSP server handles re-opening gracefully)
    this.sendNotification('textDocument/didOpen', {
      textDocument: {
        uri,
        languageId: 'scheme',
        version: 1,
        text: content
      }
    });

    return uri;
  }

  /**
   * Get hover information at a position
   */
  async hover(filePath: string, line: number, character: number): Promise<string | null> {
    await this.ensureRunning();
    const uri = await this.syncDocument(filePath);

    try {
      const result = await this.sendRequest('textDocument/hover', {
        textDocument: { uri },
        position: { line, character }
      });

      if (!result || !result.contents) {
        return null;
      }

      // Format the hover content
      if (typeof result.contents === 'string') {
        return result.contents;
      }
      if (result.contents.value) {
        return result.contents.value;
      }
      if (Array.isArray(result.contents)) {
        return result.contents.map((c: any) =>
          typeof c === 'string' ? c : c.value
        ).join('\n\n');
      }

      return JSON.stringify(result.contents);
    } catch (e) {
      console.error('[LSP] Hover error:', e);
      return null;
    }
  }

  /**
   * Go to definition
   */
  async definition(filePath: string, line: number, character: number): Promise<Location[] | null> {
    await this.ensureRunning();
    const uri = await this.syncDocument(filePath);

    try {
      const result = await this.sendRequest('textDocument/definition', {
        textDocument: { uri },
        position: { line, character }
      });

      if (!result) {
        return null;
      }

      // Normalize to array
      const locations = Array.isArray(result) ? result : [result];
      return locations.map((loc: any) => ({
        uri: this.uriToPath(loc.uri || loc.targetUri),
        range: loc.range || loc.targetRange
      }));
    } catch (e) {
      console.error('[LSP] Definition error:', e);
      return null;
    }
  }

  /**
   * Find references
   */
  async references(filePath: string, line: number, character: number, includeDeclaration = true): Promise<Location[] | null> {
    await this.ensureRunning();
    const uri = await this.syncDocument(filePath);

    try {
      const result = await this.sendRequest('textDocument/references', {
        textDocument: { uri },
        position: { line, character },
        context: { includeDeclaration }
      });

      if (!result) {
        return null;
      }

      return result.map((loc: any) => ({
        uri: this.uriToPath(loc.uri),
        range: loc.range
      }));
    } catch (e) {
      console.error('[LSP] References error:', e);
      return null;
    }
  }

  /**
   * Search workspace symbols
   */
  async workspaceSymbol(query: string): Promise<SymbolInformation[] | null> {
    await this.ensureRunning();

    try {
      const result = await this.sendRequest('workspace/symbol', { query });

      if (!result) {
        return null;
      }

      return result.map((sym: any) => ({
        name: sym.name,
        kind: sym.kind,
        location: {
          uri: this.uriToPath(sym.location.uri),
          range: sym.location.range
        },
        containerName: sym.containerName
      }));
    } catch (e) {
      console.error('[LSP] Workspace symbol error:', e);
      return null;
    }
  }

  /**
   * Get document symbols
   */
  async documentSymbol(filePath: string): Promise<SymbolInformation[] | null> {
    await this.ensureRunning();
    const uri = await this.syncDocument(filePath);

    try {
      const result = await this.sendRequest('textDocument/documentSymbol', {
        textDocument: { uri }
      });

      if (!result) {
        return null;
      }

      // Flatten hierarchical symbols if needed
      const flatten = (symbols: any[], container?: string): SymbolInformation[] => {
        const flat: SymbolInformation[] = [];
        for (const sym of symbols) {
          flat.push({
            name: sym.name,
            kind: sym.kind,
            location: {
              uri: this.uriToPath(uri),
              range: sym.range || sym.selectionRange
            },
            containerName: container
          });
          if (sym.children) {
            flat.push(...flatten(sym.children, sym.name));
          }
        }
        return flat;
      };

      return flatten(result);
    } catch (e) {
      console.error('[LSP] Document symbol error:', e);
      return null;
    }
  }

  /**
   * Get diagnostics for a file
   */
  async getDiagnostics(filePath: string): Promise<Diagnostic[]> {
    await this.ensureRunning();
    const uri = await this.syncDocument(filePath);

    // Wait a bit for diagnostics to be published
    await new Promise(resolve => setTimeout(resolve, 200));

    return this.diagnostics.get(uri) || [];
  }

  /**
   * Format a document
   */
  async format(filePath: string): Promise<string | null> {
    await this.ensureRunning();
    const uri = await this.syncDocument(filePath);
    const content = await readFile(this.uriToPath(uri), 'utf-8');

    try {
      const result = await this.sendRequest('textDocument/formatting', {
        textDocument: { uri },
        options: {
          tabSize: 2,
          insertSpaces: true
        }
      });

      if (!result || result.length === 0) {
        return content; // No changes
      }

      // Apply text edits (assumes whole-document replacement for simplicity)
      // The Fold's LSP returns a single edit covering the whole document
      if (result.length === 1 && result[0].newText) {
        return result[0].newText;
      }

      // For multiple edits, apply in reverse order
      let text = content;
      const edits = [...result].sort((a: any, b: any) => {
        if (a.range.start.line !== b.range.start.line) {
          return b.range.start.line - a.range.start.line;
        }
        return b.range.start.character - a.range.start.character;
      });

      for (const edit of edits) {
        const lines = text.split('\n');
        const startOffset = this.positionToOffset(lines, edit.range.start);
        const endOffset = this.positionToOffset(lines, edit.range.end);
        text = text.slice(0, startOffset) + edit.newText + text.slice(endOffset);
      }

      return text;
    } catch (e) {
      console.error('[LSP] Format error:', e);
      return null;
    }
  }

  /**
   * Combined lookup: hover + definition + references
   */
  async lookup(filePath: string, line: number, character: number): Promise<{
    hover: string | null;
    definition: Location[] | null;
    references: Location[] | null;
  }> {
    await this.ensureRunning();
    const uri = await this.syncDocument(filePath);

    // Run all three in parallel
    const [hover, definition, references] = await Promise.all([
      this.hover(filePath, line, character),
      this.definition(filePath, line, character),
      this.references(filePath, line, character)
    ]);

    return { hover, definition, references };
  }

  /**
   * Get completion suggestions at a position
   */
  async completion(filePath: string, line: number, character: number): Promise<CompletionItem[] | null> {
    await this.ensureRunning();
    const uri = await this.syncDocument(filePath);

    try {
      const result = await this.sendRequest('textDocument/completion', {
        textDocument: { uri },
        position: { line, character }
      });

      if (!result) {
        return null;
      }

      // Handle both CompletionList and CompletionItem[] response formats
      const items = result.items || result;
      if (!Array.isArray(items)) {
        return null;
      }

      return items.map((item: any) => ({
        label: item.label,
        kind: item.kind,
        detail: item.detail,
        insertText: item.insertText,
        insertTextFormat: item.insertTextFormat
      }));
    } catch (e) {
      console.error('[LSP] Completion error:', e);
      return null;
    }
  }

  /**
   * Get document symbols for file outline
   */
  async documentSymbols(filePath: string): Promise<DocumentSymbol[] | null> {
    await this.ensureRunning();
    const uri = await this.syncDocument(filePath);

    try {
      const result = await this.sendRequest('textDocument/documentSymbol', {
        textDocument: { uri }
      });

      if (!result || !Array.isArray(result)) {
        return null;
      }

      return result.map((item: any) => this.mapDocumentSymbol(item));
    } catch (e) {
      console.error('[LSP] Document symbols error:', e);
      return null;
    }
  }

  /**
   * Recursively map a document symbol from LSP response
   */
  private mapDocumentSymbol(item: any): DocumentSymbol {
    const symbol: DocumentSymbol = {
      name: item.name,
      kind: item.kind,
      range: item.range,
      selectionRange: item.selectionRange
    };
    if (item.children && Array.isArray(item.children)) {
      symbol.children = item.children.map((c: any) => this.mapDocumentSymbol(c));
    }
    return symbol;
  }

  /**
   * Convert position to character offset
   */
  private positionToOffset(lines: string[], position: Position): number {
    let offset = 0;
    for (let i = 0; i < position.line && i < lines.length; i++) {
      offset += lines[i].length + 1; // +1 for newline
    }
    offset += position.character;
    return offset;
  }

  /**
   * Ensure LSP server is running, start if needed
   */
  private async ensureRunning(): Promise<void> {
    if (!this.isRunning()) {
      await this.start();
    }
  }
}

// Symbol kinds (LSP specification)
export const SymbolKind = {
  File: 1,
  Module: 2,
  Namespace: 3,
  Package: 4,
  Class: 5,
  Method: 6,
  Property: 7,
  Field: 8,
  Constructor: 9,
  Enum: 10,
  Interface: 11,
  Function: 12,
  Variable: 13,
  Constant: 14,
  String: 15,
  Number: 16,
  Boolean: 17,
  Array: 18,
  Object: 19,
  Key: 20,
  Null: 21,
  EnumMember: 22,
  Struct: 23,
  Event: 24,
  Operator: 25,
  TypeParameter: 26
};

// Diagnostic severity (LSP specification)
export const DiagnosticSeverity = {
  Error: 1,
  Warning: 2,
  Information: 3,
  Hint: 4
};

/**
 * Format a location for display
 */
export function formatLocation(loc: Location): string {
  const { uri, range } = loc;
  const file = uri.replace(/^file:\/\//, '');
  const line = range.start.line + 1; // Convert to 1-indexed
  const col = range.start.character + 1;
  return `${file}:${line}:${col}`;
}

/**
 * Get human-readable symbol kind name
 */
export function symbolKindName(kind: number): string {
  const names: { [key: number]: string } = {
    1: 'File', 2: 'Module', 3: 'Namespace', 4: 'Package', 5: 'Class',
    6: 'Method', 7: 'Property', 8: 'Field', 9: 'Constructor', 10: 'Enum',
    11: 'Interface', 12: 'Function', 13: 'Variable', 14: 'Constant',
    15: 'String', 16: 'Number', 17: 'Boolean', 18: 'Array', 19: 'Object',
    20: 'Key', 21: 'Null', 22: 'EnumMember', 23: 'Struct', 24: 'Event',
    25: 'Operator', 26: 'TypeParameter'
  };
  return names[kind] || 'Unknown';
}

/**
 * Get human-readable diagnostic severity
 */
export function severityName(severity: number | undefined): string {
  switch (severity) {
    case 1: return 'Error';
    case 2: return 'Warning';
    case 3: return 'Info';
    case 4: return 'Hint';
    default: return 'Unknown';
  }
}

/**
 * Get human-readable completion item kind name
 */
export function completionKindName(kind: number | undefined): string {
  const names: { [key: number]: string } = {
    1: 'Text', 2: 'Method', 3: 'Function', 4: 'Constructor', 5: 'Field',
    6: 'Variable', 7: 'Class', 8: 'Interface', 9: 'Module', 10: 'Property',
    11: 'Unit', 12: 'Value', 13: 'Enum', 14: 'Keyword', 15: 'Snippet',
    16: 'Color', 17: 'File', 18: 'Reference', 19: 'Folder', 20: 'EnumMember',
    21: 'Constant', 22: 'Struct', 23: 'Event', 24: 'Operator', 25: 'TypeParameter'
  };
  return kind !== undefined ? (names[kind] || 'Unknown') : 'Unknown';
}
