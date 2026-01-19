;;; boundary/history/README.sexp — REPL History Module Documentation
;;;
;;; Time-traveling REPL with Git-like branching history.
;;; Tracks commands as content-addressed blocks linked in version chains.

(module history
  (purpose "Undo/redo with branching history for REPL sessions")

  (architecture
    (pattern "Command replay (not state snapshots)")
    (rationale "Scheme environments contain closures/continuations that can't be serialized.
                Commands are strings; replay is portable, debuggable, auditable."))

  (files
    (blocks.ss "History entry block creation")
    (heads.ss "Head file management with lockfiles")
    (store.ss "CAS integration and history walking")
    (replay.ss "Command replay engine")
    (ops.ss "Core undo/redo/branch operations")
    (history.ss "User-facing API entry point"))

  (block-schema
    (tag "history/entry")
    (payload
      ((session-id . string)
       (index . integer)
       (command . string)
       (cmd-type . (definition effect expression))
       (result-type . (success error))
       (result-hash . hex-string)
       (defined-name . (or symbol false))
       (timestamp . iso-8601-string)
       (version . 1)))
    (refs (prev-entry-hash)))

  (head-files
    (location ".store/heads/history/<session-id>/")
    (contents
      (main.head "Branch tip hash")
      (__current__.head "Active branch name")
      (__redo__.sexp "Redo stack (entry hashes)")))

  (api
    (basic
      (undo "Undo last command")
      (redo "Redo undone command")
      (history "Show history")
      (history n "Show last n entries")
      (jump n "Jump to history point n"))
    (branching
      (branch name "Create branch from current position")
      (branches "List all branches")
      (checkout name "Switch to branch")
      (delete-branch name "Delete merged branch"))
    (persistence
      (history-save! "Force checkpoint")
      (history-export "Export as script")))

  (command-types
    (definition "Modifies environment: define, define-syntax. Replayable.")
    (effect "I/O, side effects: load, write-file. Skip on safe replay.")
    (expression "Pure evaluation. Replay if needed for result."))

  (dependencies
    ("core/base/prelude.ss" "Prelude")
    ("core/blocks/block.ss" "Block primitives")
    ("core/blocks/cas.ss" "Content-addressed storage")
    ("boundary/io/atomic.ss" "Atomic file writes")
    ("boundary/io/file-lock.ss" "File locking")))
