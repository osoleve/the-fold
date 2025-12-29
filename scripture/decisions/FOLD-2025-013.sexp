;;; FOLD-2025-013: System Verbosity
;;; Settled: 2025-12-29

((id . "FOLD-2025-013")
 (date . "2025-12-29T19:30:00Z")
 (settled-by . outsider)
 (category . taste)
 (context . "How much should the system explain itself during operation?
Quiet (silence is success) or chatty (explain what's happening)?")
 (decision . "Quiet core, helpful toolkits - the fabric is silent; the
thimble and tools explain themselves")
 (rationale . "Core operations happen constantly; noise would overwhelm.
But user-facing tools should guide and assist. Different layers have
different audiences.")
 (implications . ("fabric/ functions return values, don't print"
                  "thimble/ tools may emit progress, suggestions, warnings"
                  "REPL commands explain their results"
                  "Logging is opt-in, not default")))
