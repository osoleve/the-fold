;;; boundary/diagnostics/README.sexp — Profiling and Diagnostics Modules
;;;
;;; Performance monitoring, profiling, and fuel analysis.

((name . diagnostics)
 (purpose . "Performance profiling, fuel analysis, and runtime diagnostics")
 (tier-access . boundary)
 (purity . impure)

 (description . "
Diagnostic tools for analyzing and optimizing Fold code performance.
Includes profilers, allocation trackers, and fuel consumption analysis.

Key features:
- Unified instrumented profiler
- Call graph construction from traces
- Fuel cost analysis per primitive
- Real-time performance monitoring
- Profile persistence and comparison
")

 (modules
  ((file . profiler-unified.ss)
   (purpose . "Unified instrumented profiler")
   (api . (profile profile-report profile-stats with-profiling)))

  ((file . profile-analysis.ss)
   (purpose . "Profile analysis and optimization hints")
   (api . (analyze-profile hotspots suggestions)))

  ((file . profile-call-graph.ss)
   (purpose . "Call graph from profiler traces")
   (api . (build-call-graph call-graph-stats call-graph-dot)))

  ((file . profile-persist.ss)
   (purpose . "Profile persistence to disk")
   (api . (save-profile load-profile compare-profiles)))

  ((file . profile-repl.ss)
   (purpose . "REPL profiler integration")
   (api . (profiler-commands profile-last)))

  ((file . fuel-analysis.ss)
   (purpose . "Primitive-level fuel cost analysis")
   (api . (fuel-costs fuel-breakdown estimate-fuel)))

  ((file . fuel-profile.ss)
   (purpose . "Fuel consumption profiler")
   (api . (fuel-profile fuel-hotspots)))

  ((file . alloc-tracker.ss)
   (purpose . "Allocation tracking shim")
   (api . (track-allocations allocation-stats)))

  ((file . perf-monitor.ss)
   (purpose . "Real-time performance monitoring")
   (api . (start-monitor stop-monitor monitor-dashboard))))

 (dependencies
  (internal . (core/base/prelude.ss
               boundary/io/fs.ss)))

 (usage . "
Profile an expression:
  (load \"boundary/diagnostics/profiler-unified.ss\")
  (profile (expensive-computation))
  (profile-report)

Analyze fuel costs:
  (load \"boundary/diagnostics/fuel-analysis.ss\")
  (fuel-costs '(map f xs))
")

 (see-also . (
   "boundary/introspect/README.sexp"
   "core/util/debug.ss")))
