((name "util")
 (purpose "General utilities")
 (description "Debugging, pretty-printing, help system, and other
general-purpose utilities that don't fit in other categories.")
 (modules
  ((debug.ss "Debugging utilities and tracing")
   (pretty.ss "Pretty-printing S-expressions")
   (help.ss "Help system and documentation lookup")
   (state.ss "State monad utilities")))
 (dependencies (base)))
