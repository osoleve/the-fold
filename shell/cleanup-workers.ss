;;; thimble/cleanup-workers.ss — One-shot cleanup of stale worker processes
;;;
;;; Usage: scheme --script thimble/cleanup-workers.ss

(load "shell/repl-daemon-mcp.ss")

(cleanup-stale-workers!)
(display "Worker cleanup complete.\n")
