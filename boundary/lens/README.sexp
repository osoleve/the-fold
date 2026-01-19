((name "lens")
 (purpose "Code navigation and codebase exploration tools")
 (description
  "IDE-like navigation system for exploring the codebase.
   Builds and maintains call graphs, enables jump-to-definition,
   finds callers/callees, locates tests, and computes dependency
   slices. Provides REPL commands for interactive exploration.")
 (modules
  ((call-graph.ss "Forward and reverse call graph builder from source analysis")
   (dep-slice.ss "Transitive dependency and dependent analysis")
   (jump.ss "Navigation primitives: jump-to-definition, location formatting")
   (navigator.ss "REPL integration: lens-jump, lens-callers, lens-slice-*")
   (test-finder.ss "Find tests related to a given symbol or module")))
 (dependencies (lang/index xref))
 (load-order "jump.ss, call-graph.ss, test-finder.ss, dep-slice.ss -> navigator.ss"))
