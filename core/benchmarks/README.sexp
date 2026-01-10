;;; core/benchmarks/README.sexp — Performance Benchmarks
;;;
;;; Reproducible performance measurement for core modules.

((name . benchmarks)
 (purpose . "Performance benchmarking for core computation")
 (tier-access . shepherd)
 (purity . pure)

 (description . "
Benchmarks for measuring and tracking performance of core modules.
Focuses on computational performance without IO overhead.

Benchmarks are:
- Reproducible (deterministic inputs)
- Pure (no side effects)
- Focused (isolate specific operations)
")

 (modules
  ((file . bench-core.ss)
   (purpose . "Benchmarks for core evaluation and compilation")
   (measures . (eval-speed compile-time type-inference)))

  ((file . bench-prim.ss)
   (purpose . "Benchmarks for primitive operations")
   (measures . (arithmetic list-ops hash-computation))))

 (usage . "
Run core benchmarks:
  scheme --script core/benchmarks/bench-core.ss

Run primitive benchmarks:
  scheme --script core/benchmarks/bench-prim.ss

In REPL:
  (load \"core/benchmarks/bench-core.ss\")
  (run-benchmarks)
")

 (see-also . (
   "shell/benchmarks/README.sexp"
   "core/util/profiling.ss")))
