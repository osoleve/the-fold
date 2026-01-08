;;; shell/repl/cleanup-workers.ss — One-shot cleanup of stale worker processes
;;;
;;; Usage: scheme --script shell/cleanup-workers.ss

(load "shell/repl-daemon-mcp.ss")

(cleanup-stale-workers!)
(display "Worker cleanup complete.\n")
