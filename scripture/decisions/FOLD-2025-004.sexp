;;; FOLD-2025-004: Error Propagation Model
;;; Settled: 2025-12-29

((id . "FOLD-2025-004")
 (date . "2025-12-29T19:00:00Z")
 (settled-by . outsider)
 (context . "How should errors propagate through the system? Pure functional
code favors Result types; Scheme has a condition system; the fabric/thimble
split suggests different strategies might be appropriate for each.")
 (decision . "Hybrid - Result types (Ok/Err) in Core (fabric), exceptions
via Scheme's condition system in Shell (thimble)")
 (rationale . "Core must be pure and total. Result types enforce explicit
error handling and compose well. Shell deals with IO where exceptions are
natural and match Scheme idioms. The boundary validates and converts.")
 (alternatives . ((result-everywhere . "Verbose in shell code, fights Scheme idioms")
                  (exceptions-everywhere . "Breaks purity in core, implicit control flow")))
 (implications . ("Core functions return (ok value) or (err reason)"
                  "Shell catches conditions, converts to Result at boundary"
                  "eval.ss already uses fuel exhaustion as typed error"
                  "Need result-bind, result-map combinators in prelude")))
