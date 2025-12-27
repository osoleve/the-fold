/**
 * Session management for MCP connections
 *
 * Each MCP connection gets a unique session with:
 * - Session ID (UUID)
 * - Tier (opus/sonnet/haiku)
 * - Name
 * - Creation/activity timestamps
 */
import { randomUUID } from 'crypto';
/**
 * Session manager - manages all active sessions
 */
export class SessionManager {
    sessions = new Map();
    SESSION_TIMEOUT_MS = 3600000; // 1 hour
    /**
     * Create a new session
     */
    createSession() {
        const session = {
            id: randomUUID(),
            tier: null,
            name: null,
            created: Date.now(),
            lastActive: Date.now(),
            loggedIn: false
        };
        this.sessions.set(session.id, session);
        return session;
    }
    /**
     * Get a session by ID
     */
    getSession(id) {
        const session = this.sessions.get(id);
        if (session) {
            session.lastActive = Date.now();
        }
        return session;
    }
    /**
     * Login a session with tier and name
     */
    login(id, tier, name) {
        const session = this.sessions.get(id);
        if (!session) {
            throw new Error(`Session not found: ${id}`);
        }
        session.tier = tier;
        session.name = name;
        session.loggedIn = true;
        session.lastActive = Date.now();
    }
    /**
     * Logout a session
     */
    logout(id) {
        const session = this.sessions.get(id);
        if (session) {
            session.tier = null;
            session.name = null;
            session.loggedIn = false;
        }
    }
    /**
     * Delete a session
     */
    deleteSession(id) {
        this.sessions.delete(id);
    }
    /**
     * Clean up expired sessions
     */
    cleanupExpiredSessions() {
        const now = Date.now();
        let cleaned = 0;
        for (const [id, session] of this.sessions.entries()) {
            if (now - session.lastActive > this.SESSION_TIMEOUT_MS) {
                this.sessions.delete(id);
                cleaned++;
            }
        }
        return cleaned;
    }
    /**
     * Get all active sessions
     */
    getAllSessions() {
        return Array.from(this.sessions.values());
    }
    /**
     * Get session count
     */
    getSessionCount() {
        return this.sessions.size;
    }
}
//# sourceMappingURL=session.js.map