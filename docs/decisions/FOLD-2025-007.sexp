;;; FOLD-2025-007: Session State Persistence
;;; Settled: 2025-12-29

((id . "FOLD-2025-007")
 (date . "2025-12-29T19:00:00Z")
 (settled-by . outsider)
 (context . "Agent sessions maintain state (bindings, role, history). When
the daemon restarts, should this state persist?")
 (decision . "Persist to blocks - session state is saved to the CAS and
restored when an agent reconnects with the same session ID")
 (rationale . "Keeps everything in the content-addressed universe. Session
state becomes auditable, versionable, and portable. Agents can resume
work across daemon restarts or even migrate between servers.")
 (alternatives . ((ephemeral . "Simpler but loses work on restart")
                  (persist-to-files . "Simpler than blocks but not content-addressed")))
 (implications . ("Session state serialized as block with tag 'session"
                  "Session ID maps to block hash in .store/sessions/"
                  "On reconnect, load and restore bindings"
                  "Session history becomes part of the Merkle structure"
                  "Privacy consideration: session blocks may contain sensitive data")))
