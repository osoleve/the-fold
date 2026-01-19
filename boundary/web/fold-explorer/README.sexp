((name . "fold-explorer")
 (description . "Terminal UI for exploring the content-addressed store")
 (version . "0.1.0")
 (language . "rust")
 (dependencies . ())  ; Zero dependencies - std only

 (binaries
  ((name . "fold-tui")
   (usage . "fold-tui [store-path] [--check]")
   (description . "Interactive terminal explorer for CAS blocks")))

 (modes
  (interactive . "Full TUI with navigation, search, filtering")
  (check . "Non-interactive store verification and stats"))

 (features
  (block-browser . "Scrolling list with tag filtering and search")
  (block-detail . "Payload preview, refs navigation")
  (heads . "Named reference listing and navigation")
  (orphans . "Blocks with no inbound references")
  (popular . "Most-referenced blocks")
  (search . "Hash prefix, tag, and payload content search"))

 (keybindings
  (navigation . "j/k or arrows to move, Enter to select")
  (views . "h=heads, o=orphans, p=popular, t=cycle tags")
  (search . "/ to search, r to reset filters")
  (back . "b or Esc to go back")
  (quit . "q to quit"))

 (security
  (terminal-injection . "All untrusted content sanitized before display")
  (panic-safety . "RAII guard restores terminal on crash")
  (path-traversal . "Hash validation (hex chars only)"))

 (library-modules
  (block . "Block binary parser - handles 64/66 char hashes, dynamic ref sizes")
  (store . "Store traversal and in-memory indexing")
  (graph . "Graph analysis utilities")
  (json . "Hand-rolled JSON serializer"))

 (files
  (source . "src/")
  (binary . "target/release/fold-tui")))
