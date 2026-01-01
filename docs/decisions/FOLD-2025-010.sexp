;;; FOLD-2025-010: Core Code Optimization Philosophy
;;; Settled: 2025-12-29

((id . "FOLD-2025-010")
 (date . "2025-12-29T19:30:00Z")
 (settled-by . outsider)
 (category . taste)
 (context . "Should core libraries prioritize clarity (readable reference
implementations) or speed (optimized but potentially opaque)?")
 (decision . "Optimized core with readable reference implementations in docs.
The running code should be fast; understanding comes from documentation.")
 (rationale . "Core code runs constantly. Performance matters. But learners
and auditors need to understand the algorithms. Separate the concerns.")
 (implications . ("Core implementations may use clever optimizations"
                  "Each optimized function should have a doc reference impl"
                  "Reference impl can be property-tested against optimized"
                  "Comments explain WHY optimizations work, not WHAT they do")))
