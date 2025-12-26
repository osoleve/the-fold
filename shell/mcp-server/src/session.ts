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
export class SessionManager {
  private sessions = new Map<string, Session>();
  private readonly SESSION_TIMEOUT_MS = 3600000; // 1 hour

  /**
   * Create a new session
   */
  createSession(): Session {
    const session: Session = {
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
  getSession(id: string): Session | undefined {
    const session = this.sessions.get(id);
    if (session) {
      session.lastActive = Date.now();
    }
    return session;
  }

  /**
   * Login a session with tier and name
   */
  login(id: string, tier: Tier, name: string): void {
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
  logout(id: string): void {
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
  deleteSession(id: string): void {
    this.sessions.delete(id);
  }

  /**
   * Clean up expired sessions
   */
  cleanupExpiredSessions(): number {
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
  getAllSessions(): Session[] {
    return Array.from(this.sessions.values());
  }

  /**
   * Get session count
   */
  getSessionCount(): number {
    return this.sessions.size;
  }
}
