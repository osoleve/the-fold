;;; shell/diagnostics/profile-repl.ss --- REPL Profiler Integration
;;;
;;; Provides REPL commands for profiling:
;;;   (profile expr)               - Profile an expression
;;;   (profile-report)             - Show last profile report
;;;   (profile-hints)              - Show optimization hints
;;;   (profile-history)            - Show profile history
;;;   (profile-compare)            - Compare profiles
;;;   (profile-baseline)           - Set baseline for regression
;;;
;;; Extended commands (unified profiler):
;;;   (profile-memory expr)        - Profile with memory tracking
;;;   (profile-callgraph)          - Show call graph for last profile
;;;   (profile-costs)              - Show cost breakdown
;;;   (profile-save-last filename) - Save last profile to file
;;;   (profile-load-and-compare f) - Load and compare with current
;;;
;;; This is Shell code: stateful REPL integration.
;;;
;;; Dependencies:
;;;   - core/util/profile.ss
;;;   - shell/profile-viz.ss
;;;   - shell/profile-analysis.ss
;;;   - shell/profiler-unified.ss
;;;   - shell/profile-persist.ss

(load "core/util/profile.ss")
(load "shell/profile-viz.ss")
(load "shell/profile-analysis.ss")
(load "shell/profiler-unified.ss")
(load "shell/profile-persist.ss")

;;; ====
;;; Profile Session State
;;; ====

;;; Current profiler result (basic)
(define *current-profile* #f)

;;; Current unified profiler result
(define *current-unified-profile* #f)

;;; Profile history (list of (name . profiler) pairs)
(define *profile-history* '())

;;; Unified profile history
(define *unified-profile-history* '())

;;; Maximum history size
(define *max-profile-history* 10)

;;; Baseline fuel consumption for regression detection
(define *profile-baseline* #f)

;;; Default fuel budget for profiling
(define *default-profile-fuel* 10000)

;;; ====
;;; Main Profile Command
;;; ====

;;; profile : Expr [× Fuel] → ProfileResult
;;; Profile an expression and display results
(define repl-profile
  (case-lambda
   [(expr)
    (repl-profile expr *default-profile-fuel*)]
   [(expr fuel)
    (let* ([p (profile-expr expr fuel)]
           [status (profiler-status p)])
          ;; Store result
          (set! *current-profile* p)
          ;; Add to history
          (add-to-history expr p)
          ;; Display summary
          (display (render-summary-box p))
          ;; Check for regressions
          (when *profile-baseline*
                (let ([hints (check-regression p *profile-baseline* 10)])
                     (unless (null? hints)
                             (display "\n  !! REGRESSION DETECTED !!\n")
                             (display (render-hints hints)))))
          ;; Return status and value
          (if (eq? status 'complete)
              (begin
               (display (format "  Result: ~a\n\n" (profiler-expr p)))
               (profiler-expr p))
              (begin
               (display (format "  Status: ~a\n\n" status))
               `(profile-status ,status))))]))

;;; add-to-history : Expr × Profiler → void
(define (add-to-history expr p)
  (let* ([entry (cons expr p)]
         [new-history (cons entry *profile-history*)])
        (set! *profile-history*
              (if (> (length new-history) *max-profile-history*)
                  (take *max-profile-history* new-history)
                  new-history))))

;;; take : Nat × List → List
(define (take n lst)
  (if (or (zero? n) (null? lst))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

;;; ====
;;; Report Commands
;;; ====

;;; profile-report : → void
;;; Display full profile report for last run
(define (repl-profile-report)
  (if *current-profile*
      (begin
       (profile-report *current-profile*)
       (void))
      (display "  No profile available. Run (profile expr) first.\n")))

;;; profile-viz : → void
;;; Display visual profile (flame graph, bars)
(define (repl-profile-viz)
  (if *current-profile*
      (begin
       (display-profile *current-profile*)
       (void))
      (display "  No profile available. Run (profile expr) first.\n")))

;;; profile-hints : → void
;;; Display optimization hints
(define (repl-profile-hints)
  (if *current-profile*
      (begin
       (display-analysis *current-profile*)
       (void))
      (display "  No profile available. Run (profile expr) first.\n")))

;;; profile-tree : → void
;;; Display call tree
(define (repl-profile-tree)
  (if *current-profile*
      (begin
       (display (render-tree-with-bars *current-profile*))
       (void))
      (display "  No profile available. Run (profile expr) first.\n")))

;;; profile-flame : → void
;;; Display flame graph
(define (repl-profile-flame)
  (if *current-profile*
      (begin
       (display (render-flame-graph *current-profile*))
       (void))
      (display "  No profile available. Run (profile expr) first.\n")))

;;; ====
;;; History Commands
;;; ====

;;; profile-history : → void
;;; Display profile history
(define (repl-profile-history)
  (if (null? *profile-history*)
      (display "  No profile history.\n")
      (begin
       (display "\n  PROFILE HISTORY\n")
       (display "  ====\n\n")
       (let loop ([hist *profile-history*] [i 0])
            (unless (null? hist)
                    (let* ([entry (car hist)]
                           [expr (car entry)]
                           [p (cdr entry)]
                           [stats (profile-stats p)]
                           [fuel (cdr (assq 'used-fuel stats))]
                           [status (profiler-status p)])
                          (display (format "  [~a] ~a\n      Status: ~a, Fuel: ~a\n\n"
                                           i
                                           (truncate-expr expr 50)
                                           status fuel))
                          (loop (cdr hist) (+ i 1))))))))

;;; truncate-expr : Expr × Nat → String
(define (truncate-expr expr max-len)
  (let* ([str (format "~a" expr)]
         [len (string-length str)])
        (if (> len max-len)
            (string-append (substring str 0 (- max-len 3)) "...")
            str)))

;;; profile-recall : Nat → Profiler
;;; Recall a profile from history
(define (repl-profile-recall n)
  (if (or (< n 0) (>= n (length *profile-history*)))
      (begin
       (display (format "  Invalid history index. Use 0-~a\n"
                        (- (length *profile-history*) 1)))
       #f)
      (let ([p (cdr (list-ref *profile-history* n))])
           (set! *current-profile* p)
           (display (format "  Recalled profile #~a\n" n))
           p)))

;;; list-ref : List × Nat → Element
(define (list-ref lst n)
  (if (zero? n)
      (car lst)
      (list-ref (cdr lst) (- n 1))))

;;; ====
;;; Comparison Commands
;;; ====

;;; profile-compare : Nat × Nat → void
;;; Compare two profiles from history
(define (repl-profile-compare n1 n2)
  (let ([len (length *profile-history*)])
       (if (or (< n1 0) (>= n1 len) (< n2 0) (>= n2 len))
           (display (format "  Invalid indices. Use 0-~a\n" (- len 1)))
           (let ([p1 (cdr (list-ref *profile-history* n1))]
                 [p2 (cdr (list-ref *profile-history* n2))])
                (display (compare-profiles p1 p2))))))

;;; ====
;;; Baseline Commands
;;; ====

;;; profile-baseline : → void
;;; Set current profile's fuel as baseline
(define (repl-set-baseline)
  (if *current-profile*
      (let* ([stats (profile-stats *current-profile*)]
             [fuel (cdr (assq 'used-fuel stats))])
            (set! *profile-baseline* fuel)
            (display (format "  Baseline set to ~a fuel\n" fuel)))
      (display "  No profile available. Run (profile expr) first.\n")))

;;; profile-clear-baseline : → void
(define (repl-clear-baseline)
  (set! *profile-baseline* #f)
  (display "  Baseline cleared.\n"))

;;; profile-show-baseline : → void
(define (repl-show-baseline)
  (if *profile-baseline*
      (display (format "  Current baseline: ~a fuel\n" *profile-baseline*))
      (display "  No baseline set.\n")))

;;; ====
;;; Configuration
;;; ====

;;; profile-set-fuel : Nat → void
;;; Set default fuel budget
(define (repl-set-profile-fuel fuel)
  (set! *default-profile-fuel* fuel)
  (display (format "  Default profile fuel set to ~a\n" fuel)))

;;; profile-clear-history : → void
(define (repl-clear-profile-history)
  (set! *profile-history* '())
  (display "  Profile history cleared.\n"))

;;; ====
;;; Profile with Environment
;;; ====

;;; profile-prelude : Expr [× Fuel] → ProfileResult
;;; Profile with prelude environment
(define repl-profile-prelude
  (case-lambda
   [(expr)
    (repl-profile-prelude expr *default-profile-fuel*)]
   [(expr fuel)
    (let* ([env (build-prelude-env 1000)]
           [p (profile-with-env expr env fuel)]
           [status (profiler-status p)])
          (set! *current-profile* p)
          (add-to-history expr p)
          (display (render-summary-box p))
          (if (eq? status 'complete)
              (profiler-expr p)
              `(profile-status ,status)))]))

;;; ====
;;; Quick Profile (Minimal Output)
;;; ====

;;; qprofile : Expr [× Fuel] → (fuel-used . result)
;;; Quick profile - minimal output, returns fuel and result
(define qprofile
  (case-lambda
   [(expr) (qprofile expr *default-profile-fuel*)]
   [(expr fuel)
    (let* ([p (profile-expr expr fuel)]
           [stats (profile-stats p)]
           [used (cdr (assq 'used-fuel stats))]
           [status (profiler-status p)]
           [result (if (eq? status 'complete)
                       (profiler-expr p)
                       status)])
          (set! *current-profile* p)
          (cons used result))]))

;;; ====
;;; Aliases for REPL convenience
;;; ====

(define prof repl-profile)
(define prof-report repl-profile-report)
(define prof-viz repl-profile-viz)
(define prof-hints repl-profile-hints)
(define prof-tree repl-profile-tree)
(define prof-flame repl-profile-flame)
(define prof-hist repl-profile-history)

;;; ====
;;; Unified Profiler Commands
;;; ====

;;; profile-memory : Expr [x Fuel] -> UnifiedProfiler
;;; Profile an expression with full memory tracking.
(define repl-profile-memory
  (case-lambda
   [(expr)
    (repl-profile-memory expr *default-profile-fuel*)]
   [(expr fuel)
    (let* ([up (profile-unified expr `((fuel-budget . ,fuel)
                                       (track-memory . #t)
                                       (build-call-graph . #t)))]
           [status (unified-profiler-status up)])
          ;; Store results
          (set! *current-unified-profile* up)
          (set! *current-profile* (unified-profiler-base up))
          ;; Add to history
          (add-to-unified-history expr up)
          ;; Display summary
          (display-unified-profile up)
          ;; Return status and value
          (if (eq? status 'complete)
              (begin
               (display (format "  Result: ~a\n\n" (unified-profiler-expr up)))
               (unified-profiler-expr up))
              (begin
               (display (format "  Status: ~a\n\n" status))
               `(profile-status ,status))))]))

;;; add-to-unified-history : Expr x UnifiedProfiler -> void
(define (add-to-unified-history expr up)
  (let* ([entry (cons expr up)]
         [new-history (cons entry *unified-profile-history*)])
        (set! *unified-profile-history*
              (if (> (length new-history) *max-profile-history*)
                  (take *max-profile-history* new-history)
                  new-history))))

;;; profile-callgraph : -> void
;;; Display call graph for the last unified profile.
(define (repl-profile-callgraph)
  (if *current-unified-profile*
      (begin
       (display (render-call-graph-report *current-unified-profile*))
       (void))
      (if *current-profile*
          ;; Fall back to building graph from basic profile
          (let ([graph (build-call-graph *current-profile*)])
               (display (call-graph-summary graph))
               (display (call-graph->ascii graph))
               (void))
          (display "  No profile available. Run (profile expr) or (profile-memory expr) first.\n"))))

;;; profile-costs : -> void
;;; Display cost breakdown for the last unified profile.
(define (repl-profile-costs)
  (if *current-unified-profile*
      (begin
       (display (render-cost-report *current-unified-profile*))
       (void))
      (display "  No unified profile available. Run (profile-memory expr) first.\n")))

;;; profile-save-last : String -> Boolean
;;; Save the last unified profile to a file.
(define (repl-profile-save-last filename)
  (if *current-unified-profile*
      (if (profile-save *current-unified-profile* filename)
          (begin
           (display (format "  Profile saved to: ~a\n" filename))
           #t)
          (begin
           (display "  Failed to save profile.\n")
           #f))
      (display "  No unified profile available. Run (profile-memory expr) first.\n")))

;;; profile-load-and-compare : String -> void
;;; Load a profile from file and compare with the current profile.
(define (repl-profile-load-and-compare filename)
  (if *current-unified-profile*
      (display (profile-load-and-compare *current-unified-profile* filename))
      (display "  No current profile to compare. Run (profile-memory expr) first.\n")))

;;; profile-unified-history : -> void
;;; Display unified profile history.
(define (repl-profile-unified-history)
  (if (null? *unified-profile-history*)
      (display "  No unified profile history.\n")
      (begin
       (display "\n  UNIFIED PROFILE HISTORY\n")
       (display "  ====\n\n")
       (let loop ([hist *unified-profile-history*] [i 0])
            (unless (null? hist)
                    (let* ([entry (car hist)]
                           [expr (car entry)]
                           [up (cdr entry)]
                           [stats (unified-profile-stats up)]
                           [fuel-stats (cdr (assq 'fuel stats))]
                           [mem-stats (cdr (assq 'memory stats))]
                           [fuel (cdr (assq 'used-fuel fuel-stats))]
                           [mem (cdr (assq 'total-formatted mem-stats))]
                           [status (unified-profiler-status up)])
                          (display (format "  [~a] ~a\n      Status: ~a, Fuel: ~a, Memory: ~a\n\n"
                                           i
                                           (truncate-expr expr 50)
                                           status fuel mem))
                          (loop (cdr hist) (+ i 1))))))))

;;; profile-memory-report : -> void
;;; Display memory allocation report.
(define (repl-profile-memory-report)
  (if *current-unified-profile*
      (begin
       (display (render-memory-report *current-unified-profile*))
       (void))
      (display "  No unified profile available. Run (profile-memory expr) first.\n")))

;;; profile-export : String -> Boolean
;;; Export current profile to CSV.
(define (repl-profile-export-csv filename)
  (if *current-unified-profile*
      (if (profile-export-csv *current-unified-profile* filename)
          (begin
           (display (format "  Profile exported to: ~a\n" filename))
           #t)
          (begin
           (display "  Failed to export profile.\n")
           #f))
      (display "  No unified profile available. Run (profile-memory expr) first.\n")))

;;; profile-with-model : Expr x CostModel [x Fuel] -> UnifiedProfiler
;;; Profile with a specific cost model.
(define repl-profile-with-model
  (case-lambda
   [(expr model)
    (repl-profile-with-model expr model *default-profile-fuel*)]
   [(expr model fuel)
    (let* ([up (profile-unified expr `((fuel-budget . ,fuel)
                                       (cost-model . ,model)
                                       (track-memory . #t)
                                       (build-call-graph . #t)))]
           [status (unified-profiler-status up)])
          (set! *current-unified-profile* up)
          (set! *current-profile* (unified-profiler-base up))
          (add-to-unified-history expr up)
          (display-unified-profile up)
          (if (eq? status 'complete)
              (unified-profiler-expr up)
              `(profile-status ,status)))]))

;;; profile-full-report : -> void
;;; Display complete profile report (all sections).
(define (repl-profile-full-report)
  (if *current-unified-profile*
      (begin
       (display-unified-profile *current-unified-profile*)
       (display (render-memory-report *current-unified-profile*))
       (display (render-cost-report *current-unified-profile*))
       (display (render-call-graph-report *current-unified-profile*))
       (void))
      (display "  No unified profile available. Run (profile-memory expr) first.\n")))

;;; profile-save-report : String -> Boolean
;;; Save full report to a text file.
(define (repl-profile-save-report filename)
  (if *current-unified-profile*
      (if (profile-save-full-report *current-unified-profile* filename)
          (begin
           (display (format "  Report saved to: ~a\n" filename))
           #t)
          (begin
           (display "  Failed to save report.\n")
           #f))
      (display "  No unified profile available. Run (profile-memory expr) first.\n")))

;;; ====
;;; Extended Aliases
;;; ====

(define prof-mem repl-profile-memory)
(define prof-graph repl-profile-callgraph)
(define prof-costs repl-profile-costs)
(define prof-save repl-profile-save-last)
(define prof-load-cmp repl-profile-load-and-compare)
(define prof-uhist repl-profile-unified-history)
(define prof-mem-report repl-profile-memory-report)
(define prof-export repl-profile-export-csv)
(define prof-full repl-profile-full-report)

;;; ====
;;; Help for new commands
;;; ====

(define (profile-help)
  (display "
  PROFILER COMMANDS
  ====

  Basic Profiling:
    (profile expr)              Profile expression (fuel only)
    (profile expr fuel)         Profile with custom fuel budget
    (profile-report)            Show detailed report
    (profile-viz)               Visual profile (flame graph, bars)
    (profile-hints)             Optimization suggestions
    (profile-tree)              Call tree view
    (profile-flame)             Flame graph view

  Unified Profiling (with memory + call graph):
    (profile-memory expr)       Profile with memory tracking
    (profile-callgraph)         Show call graph
    (profile-costs)             Show cost breakdown by category
    (profile-memory-report)     Detailed memory report
    (profile-full-report)       Complete report (all sections)

  History & Comparison:
    (profile-history)           Show basic profile history
    (profile-unified-history)   Show unified profile history
    (profile-recall n)          Recall profile #n from history
    (profile-compare n1 n2)     Compare two profiles

  Persistence:
    (profile-save-last file)    Save last profile to file
    (profile-load-and-compare f) Compare current with saved
    (profile-save-report file)  Save full report as text
    (profile-export-csv file)   Export stats as CSV

  Configuration:
    (profile-set-fuel n)        Set default fuel budget
    (profile-baseline)          Set current as baseline
    (profile-clear-baseline)    Clear baseline
    (profile-clear-history)     Clear history

  Short Aliases:
    prof, prof-mem, prof-graph, prof-costs, prof-save,
    prof-load-cmp, prof-full, prof-export, prof-uhist
"))

;;; ====
;;; Module Loading Confirmation
;;; ====

(define *profile-repl-loaded* #t)
