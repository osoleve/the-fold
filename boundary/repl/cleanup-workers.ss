(doc 'module 'cleanup-workers)
(doc 'description "One-shot cleanup of stale worker processes")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'usage "scheme --script boundary/repl/cleanup-workers.ss")

(load "boundary/repl/repl-daemon-mcp.ss")

(cleanup-stale-workers!)
(cleanup-idle-workers!)
(display "Worker cleanup complete.\n")
