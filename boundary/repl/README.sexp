;;; boundary/repl/README.sexp — REPL Infrastructure Modules
;;;
;;; Core REPL system, daemon, and session management.

((name . repl)
 (purpose . "REPL infrastructure, daemon, and session management")
 (tier-access . shell)
 (purity . impure)

 (description . "
Core REPL infrastructure for The Fold. Includes:
- Main REPL entry point and command loading
- Persistent daemon for multi-session support
- Session management and isolation
- Worker processes for parallel execution
- Typed evaluation commands
")

 (modules
  ((file . repl.ss)
   (purpose . "Main REPL entry point")
   (api . (start-repl repl-eval help commands)))

  ((file . repl-daemon.ss)
   (purpose . "Persistent REPL daemon")
   (api . (start-daemon stop-daemon daemon-status)))

  ((file . repl-daemon-mcp.ss)
   (purpose . "Session broker daemon with MCP support")
   (api . (mcp-start mcp-stop mcp-request)))

  ((file . repl-worker.ss)
   (purpose . "Per-session worker process")
   (api . (worker-start worker-eval worker-stop)))

  ((file . repl-quiet.ss)
   (purpose . "Quiet REPL loader (minimal output)")
   (api . ()))

  ((file . session-manager.ss)
   (purpose . "Multi-session management")
   (api . (create-session destroy-session switch-session list-sessions)))

  ((file . eval-repl.ss)
   (purpose . "Typed evaluation REPL commands")
   (api . (eval-typed type-of infer-type)))

  ((file . patches.ss)
   (purpose . "Runtime patch system")
   (api . (apply-patch list-patches patch-history)))

  ((file . cleanup-workers.ss)
   (purpose . "Cleanup stale worker processes")
   (api . (cleanup-workers list-workers kill-worker))))

 (dependencies
  (internal . (core/base/prelude.ss
               boundary/io/fs.ss
               boundary/tools/string-utils.ss
               boundary/commands.ss)))

 (usage . "
Start the REPL:
  (load \"boundary/repl/repl.ss\")

Start the daemon:
  ./daemon.sh start

Use session manager:
  (load \"boundary/repl/session-manager.ss\")
  (create-session 'my-session)
")

 (see-also . (
   "CLAUDE.md"
   "daemon.sh"
   "boundary/commands.ss")))
