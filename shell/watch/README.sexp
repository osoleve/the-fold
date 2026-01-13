((name "watch")
 (purpose "File watching and auto-reload system")
 (description
  "Watch files and directories for changes, trigger actions on modification.
   Supports auto-reload, auto-test, debouncing, and custom actions.
   Uses polling (file modification times) with thread-safe global registry.")
 (modules
  ((watch.ss "Core file watching: watch-file, watch-dir, auto-reload, stop-watching")
   (watch-INDEX.ss "Index-based watching for larger file sets")))
 (dependencies (base))
 (load-order "watch.ss (standalone), watch-INDEX.ss (optional)"))
