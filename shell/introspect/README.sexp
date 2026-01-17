((name "introspect")
 (purpose "Code analysis, profiling, and measurement utilities")
 (description
  "Runtime introspection tools for analyzing code complexity,
   measuring memory allocation, and timing execution. Used by
   benchmarks, profilers, and coverage tools. Computes LOC,
   nesting depth, definition counts, and test coverage metrics.")
 (modules
  ((complexity.ss "Static analysis: LOC, nesting depth, definitions, coverage estimation")
   (memory.ss "Memory measurement: allocation tracking, GC statistics")
   (timing.ss "High-resolution timing: wall-clock, CPU time, benchmark harness")
   (exports.ss "Export discovery: extract defines from any .ss file without manifest")))
 (dependencies (base))
 (load-order "timing.ss, memory.ss, complexity.ss (independent)"))
