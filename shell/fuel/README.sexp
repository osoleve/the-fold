;;; shell/fuel/README.sexp — Adaptive Fuel Allocation Documentation
;;;
;;; This directory contains runtime-adaptive fuel allocation using Kalman filtering.

((module . "shell/fuel")
 (version . "0.1.0")
 (status . "experimental")

 (description . "
Runtime-adaptive fuel allocation for higher-order functions.

Instead of guessing fuel upfront, this module observes early iterations
and refines estimates using a log-space Kalman filter. Features:

  - Per-element fuel allocation with confidence margins
  - Log-space Kalman for heavy-tailed cost distributions
  - Geometric backoff retry on fuel exhaustion
  - Observability through stats returned alongside results
")

 (architecture . "
┌─────────────────────────────────────────────────────────────┐
│                    User Code                                │
│         (adaptive-map-native f xs)                          │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│              Adaptive HOF Wrappers                          │
│    adaptive-map, adaptive-filter, adaptive-fold             │
│    shell/fuel/adaptive-hof.ss                               │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│              Adaptive Allocator                             │
│    - Wraps log-space Kalman filter                          │
│    - Tracks history, efficiency, consumed/allocated         │
│    shell/fuel/adaptive-allocator.ss                         │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│              Kalman Filter (lattice/fp)                     │
│    - State: [mean, variance]                                │
│    - Predict/Update cycle                                   │
│    - Log-space for heavy-tailed distributions               │
│    lattice/fp/control-systems/kalman.ss                     │
└─────────────────────────────────────────────────────────────┘
")

 (files . (
   ((file . "adaptive-allocator.ss")
    (description . "Fuel allocation engine wrapping Kalman filter")
    (exports . (
      make-adaptive-allocator
      default-adaptive-allocator
      allocator?
      allocator-request-fuel
      allocator-observe
      allocator-mark-allocated
      allocator-efficiency
      allocator-summary
      allocator-recent-costs)))

   ((file . "adaptive-hof.ss")
    (description . "User-facing adaptive higher-order functions")
    (exports . (
      adaptive-map
      adaptive-map-native
      adaptive-filter
      adaptive-filter-native
      adaptive-fold-left
      adaptive-fold-left-native
      adaptive-eval)))))

 (usage . "
;;; Basic usage with native Scheme functions
(load \"shell/fuel/adaptive-hof.ss\")

;; Adaptive map - returns (values results stats)
(let-values ([(results stats)
              (adaptive-map-native (lambda (x) (* x x))
                                   '(1 2 3 4 5 6 7 8 9 10))])
  (display \"Results: \") (display results) (newline)
  (display \"Mean cost estimate: \")
  (display (cdr (assq 'mean-estimate stats))) (newline))

;;; With options
(adaptive-map-native expensive-fn xs
  '((initial-estimate . 500)     ; starting cost estimate
    (confidence . 2.5)           ; sigmas for safety margin
    (process-noise . 0.1)        ; Q - cost variability
    (measurement-noise . 0.5)))  ; R - instrumentation noise

;;; Using the allocator directly
(define alloc (make-adaptive-allocator 100 0.1 0.5 2.0))
(display (allocator-request-fuel alloc))   ; => ~749 initially
(set! alloc (allocator-observe alloc 50))  ; observe actual cost
(display (allocator-request-fuel alloc))   ; => ~200 (adapted down)
")

 (options . (
   ((option . initial-estimate)
    (type . number)
    (default . 100)
    (description . "Starting cost estimate per element"))

   ((option . confidence)
    (type . number)
    (default . 2.0)
    (description . "Number of sigmas for safety margin (2.0 = 95% confidence)"))

   ((option . process-noise)
    (type . number)
    (default . 0.1)
    (description . "Kalman Q parameter - how much cost varies between elements"))

   ((option . measurement-noise)
    (type . number)
    (default . 0.5)
    (description . "Kalman R parameter - instrumentation uncertainty"))

   ((option . max-retries)
    (type . integer)
    (default . 10)
    (description . "Maximum retry attempts when fuel exhausted"))))

 (stats-returned . (
   ((key . mean-estimate)
    (description . "Current estimated cost per element"))
   ((key . ci-95-lower)
    (description . "Lower bound of 95% confidence interval"))
   ((key . ci-95-upper)
    (description . "Upper bound of 95% confidence interval"))
   ((key . kalman-gain)
    (description . "Current Kalman gain (0-1, higher = trusts observations more)"))
   ((key . efficiency)
    (description . "Ratio of consumed/allocated fuel (1.0 = perfect)"))
   ((key . total-allocated)
    (description . "Total fuel allocated across all elements"))
   ((key . total-consumed)
    (description . "Total fuel actually consumed"))
   ((key . observations)
    (description . "Number of observations processed"))))

 (tests . (
   ((file . "tests/test-adaptive-allocator.ss")
    (run . "scheme --script shell/fuel/tests/test-adaptive-allocator.ss")
    (description . "Integration tests for allocator and HOFs"))
   ((file . "../../lattice/fp/control-systems/test-kalman.ss")
    (run . "scheme --script lattice/fp/control-systems/test-kalman.ss")
    (description . "Unit tests for Kalman filter math"))))

 (see-also . (
   "lattice/fp/control-systems/kalman.ss"
   "lattice/fp/manifest.sexp"
   "core/lang/eval.ss")))
