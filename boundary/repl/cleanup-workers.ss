;;; boundary/repl/cleanup-workers.ss — One-shot cleanup of stale worker processes
;;;
;;; Usage: scheme --script boundary/repl/cleanup-workers.ss

(load "boundary/repl/repl-daemon-mcp.ss")

(cleanup-stale-workers!)
(cleanup-idle-workers!)
(display "Worker cleanup complete.\n")
