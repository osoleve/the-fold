/**
 * IPC utilities for communicating with The Fold REPL daemon
 *
 * Implements session-aware file-based IPC:
 * - Writes requests to .fold-repl/requests/<session-id>.ss
 * - Reads responses from .fold-repl/responses/<session-id>.txt
 * - Handles concurrent sessions
 */
export interface FoldRequest {
    sessionId: string;
    expression: string;
    timestamp: number;
}
export interface FoldResponse {
    output: string;
    error?: string;
}
/**
 * Initialize IPC directories
 */
export declare function initIPC(): Promise<void>;
/**
 * Send an expression to the daemon and wait for response
 */
export declare function sendRequest(sessionId: string, expression: string): Promise<FoldResponse>;
/**
 * Check if the daemon is running
 */
export declare function isDaemonRunning(): Promise<boolean>;
//# sourceMappingURL=ipc.d.ts.map