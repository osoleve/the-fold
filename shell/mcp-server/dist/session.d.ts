/**
 * Session management for MCP connections
 *
 * Each MCP connection gets a unique session with:
 * - Session ID (UUID)
 * - Tier (opus/sonnet/haiku)
 * - Name
 * - Creation/activity timestamps
 */
export type Tier = 'opus' | 'sonnet' | 'haiku';
export interface Session {
    id: string;
    tier: Tier | null;
    name: string | null;
    created: number;
    lastActive: number;
    loggedIn: boolean;
}
/**
 * Session manager - manages all active sessions
 */
export declare class SessionManager {
    private sessions;
    private readonly SESSION_TIMEOUT_MS;
    /**
     * Create a new session
     */
    createSession(): Session;
    /**
     * Get a session by ID
     */
    getSession(id: string): Session | undefined;
    /**
     * Login a session with tier and name
     */
    login(id: string, tier: Tier, name: string): void;
    /**
     * Logout a session
     */
    logout(id: string): void;
    /**
     * Delete a session
     */
    deleteSession(id: string): void;
    /**
     * Clean up expired sessions
     */
    cleanupExpiredSessions(): number;
    /**
     * Get all active sessions
     */
    getAllSessions(): Session[];
    /**
     * Get session count
     */
    getSessionCount(): number;
}
//# sourceMappingURL=session.d.ts.map