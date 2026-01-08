;;; shell/diagnostics/profiler-unified.ss --- Unified Instrumented Profiler
;;;
;;; Integrates all profiler components into a single instrumented profiler:
;;;   - Cost model abstraction for pluggable cost tracking
;;;   - Allocation tracker for memory profiling
;;;   - Call graph builder for caller/callee analysis
;;;   - Fuel-based profiling from core
;;;
;;; This is Shell code: orchestrates multiple profiling subsystems.
;;;
;;; Dependencies:
;;;   - core/util/profile.ss (base profiling)
;;;   - core/util/cost-model.ss (cost models)
;;;   - shell/alloc-tracker.ss (memory tracking)
;;;   - shell/profile-call-graph.ss (call graph analysis)

(load "core/util/profile.ss")
(load "core/util/cost-model.ss")
(load "shell/alloc-tracker.ss")
(load "shell/profile-call-graph.ss")

;;; ============================================================
;;; Unified Profiler Data Structure
;;; ============================================================

;;; A unified profiler combines:
;;;   - base-profiler: The core fuel-based profiler result
;;;   - cost-tracker:  Cost model-based cost tracking
;;;   - alloc-tracker: Memory allocation tracking
;;;   - call-graph:    Caller/callee relationship graph
;;;   - metadata:      Timing, options, etc.

;;; make-unified-profiler : Profiler x CostTracker x AllocTracker x CallGraph x Alist -> UnifiedProfiler
(define (make-unified-profiler base-profiler cost-tracker alloc-tracker call-graph metadata)
  `(unified-profiler
    (base-profiler . ,base-profiler)
    (cost-tracker  . ,cost-tracker)
    (alloc-tracker . ,alloc-tracker)
    (call-graph    . ,call-graph)
    (metadata      . ,metadata)))

;;; unified-profiler? : Any -> Boolean
(define (unified-profiler? x)
  (and (pair? x) (eq? (car x) 'unified-profiler)))

;;; unified-profiler-get : UnifiedProfiler x Symbol -> Any
(define (unified-profiler-get up key)
  (let ([entry (assq key (cdr up))])
       (and entry (cdr entry))))

;;; Accessors
(define (unified-profiler-base up) (unified-profiler-get up 'base-profiler))
(define (unified-profiler-cost-tracker up) (unified-profiler-get up 'cost-tracker))
(define (unified-profiler-alloc-tracker up) (unified-profiler-get up 'alloc-tracker))
(define (unified-profiler-call-graph up) (unified-profiler-get up 'call-graph))
(define (unified-profiler-metadata up) (unified-profiler-get up 'metadata))

;;; ============================================================
;;; Profile Options
;;; ============================================================

;;; Default profile options
(define (default-profile-options)
  `((cost-model      . ,fuel-cost-model)
    (track-memory    . #t)
    (build-call-graph . #t)
    (fuel-budget     . 10000)))

;;; merge-options : Alist x Alist -> Alist
;;; Merge user options with defaults (user takes precedence)
(define (merge-options defaults user)
  (let loop ([defaults defaults] [result '()])
       (if (null? defaults)
           (append result user)  ; Add any user options not in defaults
           (let* ([key (caar defaults)]
                  [user-val (assq key user)])
                 (loop (cdr defaults)
                       (cons (if user-val
                                 user-val
                                 (car defaults))
                             result))))))

;;; get-option : Alist x Symbol -> Any
(define (get-option opts key)
  (let ([entry (assq key opts)])
       (and entry (cdr entry))))

;;; ============================================================
;;; Unified Profiling Entry Point
;;; ============================================================

;;; profile-unified : Expr x Alist -> UnifiedProfiler
;;; Profile an expression with all tracking enabled.
;;;
;;; Options:
;;;   cost-model:       CostModel to use (default: fuel-cost-model)
;;;   track-memory:     Boolean (default: #t)
;;;   build-call-graph: Boolean (default: #t)
;;;   fuel-budget:      Nat (default: 10000)
(define (profile-unified expr . user-opts)
  (let* ([opts (merge-options (default-profile-options)
                              (if (null? user-opts) '() (car user-opts)))]
         [cost-model (get-option opts 'cost-model)]
         [track-mem? (get-option opts 'track-memory)]
         [build-graph? (get-option opts 'build-call-graph)]
         [fuel (get-option opts 'fuel-budget)]
         [start-time (get-monotonic-time)])
        
        ;; Initialize trackers
        (let* ([cost-tracker (make-cost-tracker cost-model)]
               [alloc-tracker-init (make-alloc-tracker)])
              
              ;; Run profiling with memory tracking
              (let-values ([(alloc-tracker-final base-profiler)
                            (if track-mem?
                                (alloc-tracker-record-thunk!
                                 alloc-tracker-init
                                 "evaluation"
                                 (lambda () (profile-expr expr fuel)))
                                (values alloc-tracker-init
                                        (profile-expr expr fuel)))])
                          
                          ;; Build call graph if requested
                          (let* ([call-graph (if build-graph?
                                                 (build-call-graph base-profiler)
                                                 (make-call-graph))]
                                 
                                 ;; Compute costs using the cost model
                                 [cost-tracker-final
                                  (compute-costs-from-profiler cost-tracker base-profiler)]
                                 
                                 ;; Calculate elapsed time
                                 [end-time (get-monotonic-time)]
                                 [elapsed-ns (compute-time-difference end-time start-time)]
                                 
                                 ;; Build metadata
                                 [metadata `((elapsed-ns    . ,elapsed-ns)
                                             (cost-model    . ,(cost-model-name cost-model))
                                             (tracked-memory . ,track-mem?)
                                             (built-graph   . ,build-graph?)
                                             (fuel-budget   . ,fuel))])
                                
                                (make-unified-profiler
                                 base-profiler
                                 cost-tracker-final
                                 alloc-tracker-final
                                 call-graph
                                 metadata))))))

;;; profile-unified-with-env : Expr x Env x Alist -> UnifiedProfiler
;;; Profile with a pre-built environment
(define (profile-unified-with-env expr env . user-opts)
  (let* ([opts (merge-options (default-profile-options)
                              (if (null? user-opts) '() (car user-opts)))]
         [cost-model (get-option opts 'cost-model)]
         [track-mem? (get-option opts 'track-memory)]
         [build-graph? (get-option opts 'build-call-graph)]
         [fuel (get-option opts 'fuel-budget)]
         [start-time (get-monotonic-time)])
        
        (let* ([cost-tracker (make-cost-tracker cost-model)]
               [alloc-tracker-init (make-alloc-tracker)])
              
              (let-values ([(alloc-tracker-final base-profiler)
                            (if track-mem?
                                (alloc-tracker-record-thunk!
                                 alloc-tracker-init
                                 "evaluation"
                                 (lambda () (profile-with-env expr env fuel)))
                                (values alloc-tracker-init
                                        (profile-with-env expr env fuel)))])
                          
                          (let* ([call-graph (if build-graph?
                                                 (build-call-graph base-profiler)
                                                 (make-call-graph))]
                                 [cost-tracker-final
                                  (compute-costs-from-profiler cost-tracker base-profiler)]
                                 [end-time (get-monotonic-time)]
                                 [elapsed-ns (compute-time-difference end-time start-time)]
                                 [metadata `((elapsed-ns    . ,elapsed-ns)
                                             (cost-model    . ,(cost-model-name cost-model))
                                             (tracked-memory . ,track-mem?)
                                             (built-graph   . ,build-graph?)
                                             (fuel-budget   . ,fuel))])
                                
                                (make-unified-profiler
                                 base-profiler
                                 cost-tracker-final
                                 alloc-tracker-final
                                 call-graph
                                 metadata))))))

;;; ============================================================
;;; Cost Computation from Profiler Tree
;;; ============================================================

;;; compute-costs-from-profiler : CostTracker x Profiler -> CostTracker
;;; Walk the profiler tree and accumulate costs by category
(define (compute-costs-from-profiler tracker profiler)
  (let ([root (profiler-root profiler)])
       (compute-costs-from-node tracker root)))

;;; compute-costs-from-node : CostTracker x Node -> CostTracker
;;; Recursively compute costs from a node and its children
(define (compute-costs-from-node tracker node)
  (let* ([name (node-name node)]
         [fuel (node-fuel-consumed node)]
         [children (node-children node)]
         [model (cost-tracker-model tracker)]
         
         ;; Categorize the cost based on node type
         [category (categorize-node-type name)]
         
         ;; Track the cost under the category
         [tracker-with-self (track-cost tracker category fuel)])
        
        ;; Recurse into children
        (fold-left compute-costs-from-node
                   tracker-with-self
                   children)))

;;; categorize-node-type : Symbol -> Symbol
;;; Categorize a node name into a cost category
(define (categorize-node-type name)
  (case name
        [(if case match) 'control-flow]
        [(let letrec) 'binding]
        [(fn lambda) 'closure]
        [(fix) 'recursion]
        [(call) 'application]
        [(prim) 'primitive]
        [(quote literal) 'constant]
        [(root) 'root]
        [else
         ;; Check if it looks like a primitive operation
         (if (member name '(+ - * / mod quotient remainder
                            cons car cdr list append reverse
                            eq? eqv? equal? < > <= >=
                            null? pair? number? symbol?
                            map filter fold-left fold-right
                            assq assv assoc member memq memv))
             'primitive
             'function)]))

;;; ============================================================
;;; Unified Statistics
;;; ============================================================

;;; unified-profile-stats : UnifiedProfiler -> Alist
;;; Comprehensive statistics from all tracking systems
(define (unified-profile-stats up)
  (let* ([base (unified-profiler-base up)]
         [cost-tracker (unified-profiler-cost-tracker up)]
         [alloc-tracker (unified-profiler-alloc-tracker up)]
         [call-graph (unified-profiler-call-graph up)]
         [metadata (unified-profiler-metadata up)]
         
         ;; Base profiler stats
         [base-stats (profile-stats base)]
         
         ;; Cost breakdown
         [cost-breakdown (get-costs cost-tracker)]
         [total-cost (get-total-cost cost-tracker)]
         
         ;; Memory stats
         [alloc-summary (alloc-tracker-summary alloc-tracker)]
         
         ;; Call graph metrics
         [all-nodes (call-graph-all-nodes call-graph)]
         [roots (call-graph-roots call-graph)]
         [leaves (call-graph-leaves call-graph)]
         [cycles (find-call-cycles call-graph)])
        
        `((fuel . ,base-stats)
          (costs . ((breakdown . ,cost-breakdown)
                    (total . ,total-cost)))
          (memory . ,alloc-summary)
          (call-graph . ((node-count . ,(length all-nodes))
                         (roots . ,(length roots))
                         (leaves . ,(length leaves))
                         (cycles . ,(length cycles))))
          (metadata . ,metadata))))

;;; ============================================================
;;; Status and Results
;;; ============================================================

;;; unified-profiler-status : UnifiedProfiler -> Symbol
(define (unified-profiler-status up)
  (profiler-status (unified-profiler-base up)))

;;; unified-profiler-result : UnifiedProfiler -> Any
(define (unified-profiler-result up)
  (profiler-result (unified-profiler-base up)))

;;; unified-profiler-expr : UnifiedProfiler -> Any
(define (unified-profiler-expr up)
  (profiler-expr (unified-profiler-base up)))

;;; ============================================================
;;; Convenience Functions for Specific Analyses
;;; ============================================================

;;; unified-profile-memory : Expr -> UnifiedProfiler
;;; Quick profiling focused on memory
(define (unified-profile-memory expr)
  (profile-unified expr '((track-memory . #t))))

;;; unified-profile-costs : Expr x CostModel -> UnifiedProfiler
;;; Profile with a specific cost model
(define (unified-profile-costs expr cost-model)
  (profile-unified expr `((cost-model . ,cost-model))))

;;; unified-profile-call-graph : Expr -> UnifiedProfiler
;;; Quick profiling to get call graph
(define (unified-profile-call-graph expr)
  (profile-unified expr '((build-call-graph . #t))))

;;; unified-profile-minimal : Expr -> UnifiedProfiler
;;; Minimal profiling (no extras)
(define (unified-profile-minimal expr)
  (profile-unified expr '((track-memory . #f)
                          (build-call-graph . #f))))

;;; ============================================================
;;; Time Helpers
;;; ============================================================

;;; We need to rename our time helpers to avoid collision with Chez's current-time

;;; get-monotonic-time : -> Time (Chez time object)
;;; Get current monotonic time
(define (get-monotonic-time)
  (current-time 'time-monotonic))

;;; compute-time-difference : Time x Time -> Integer (nanoseconds)
;;; Compute difference between two Chez time objects
(define (compute-time-difference end start)
  (let* ([end-s (time-second end)]
         [end-ns (time-nanosecond end)]
         [start-s (time-second start)]
         [start-ns (time-nanosecond start)]
         [sec-diff (- end-s start-s)]
         [ns-diff (- end-ns start-ns)])
        (+ (* sec-diff 1000000000) ns-diff)))

;;; ============================================================
;;; Report Generation
;;; ============================================================

;;; render-unified-summary : UnifiedProfiler -> String
;;; Render a summary of all profiling data
(define (render-unified-summary up)
  (let* ([stats (unified-profile-stats up)]
         [fuel-stats (cdr (assq 'fuel stats))]
         [cost-stats (cdr (assq 'costs stats))]
         [mem-stats (cdr (assq 'memory stats))]
         [graph-stats (cdr (assq 'call-graph stats))]
         [metadata (cdr (assq 'metadata stats))]
         
         [used-fuel (cdr (assq 'used-fuel fuel-stats))]
         [total-fuel (cdr (assq 'total-fuel fuel-stats))]
         [total-cost (cdr (assq 'total cost-stats))]
         [cost-breakdown (cdr (assq 'breakdown cost-stats))]
         [mem-total (cdr (assq 'total-formatted mem-stats))]
         [node-count (cdr (assq 'node-count graph-stats))]
         [cycle-count (cdr (assq 'cycles graph-stats))])
        
        (string-append
         "\n"
         "  +============================================+\n"
         "  |        UNIFIED PROFILE SUMMARY            |\n"
         "  +============================================+\n"
         "\n"
         "  EXECUTION\n"
         "  ---------\n"
         (format "    Status:      ~a\n" (unified-profiler-status up))
         (format "    Fuel Used:   ~a / ~a\n" used-fuel total-fuel)
         (format "    Elapsed:     ~a\n" (format-elapsed (cdr (assq 'elapsed-ns metadata))))
         "\n"
         "  MEMORY\n"
         "  ------\n"
         (format "    Allocated:   ~a\n" mem-total)
         "\n"
         (format "  COST MODEL (~a)\n" (cdr (assq 'cost-model metadata)))
         "  ----------\n"
         (format "    Total Cost:  ~a\n" total-cost)
         (render-cost-breakdown cost-breakdown)
         "\n"
         "  CALL GRAPH\n"
         "  ----------\n"
         (format "    Nodes:       ~a\n" node-count)
         (format "    Cycles:      ~a\n" cycle-count)
         "\n"
         "  +============================================+\n")))

;;; render-cost-breakdown : Alist -> String
(define (render-cost-breakdown breakdown)
  (if (null? breakdown)
      "    (no costs recorded)\n"
      (apply string-append
             (map (lambda (entry)
                          (format "    ~a: ~a\n" (car entry) (cdr entry)))
                  (list-sort (lambda (a b) (> (cdr a) (cdr b)))
                             breakdown)))))

;;; format-elapsed : Integer (nanoseconds) -> String
;;; Handles negative values by clamping to 0
(define (format-elapsed ns)
  (let ([ns (max 0 ns)])  ; Clamp to non-negative
       (cond
        [(< ns 1000) (format "~ans" ns)]
        [(< ns 1000000) (format "~aus" (quotient ns 1000))]
        [(< ns 1000000000) (format "~ams" (quotient ns 1000000))]
        [else (format "~as" (quotient ns 1000000000))])))

;;; display-unified-profile : UnifiedProfiler -> void
(define (display-unified-profile up)
  (display (render-unified-summary up)))

;;; ============================================================
;;; Memory Report
;;; ============================================================

;;; render-memory-report : UnifiedProfiler -> String
(define (render-memory-report up)
  (let* ([alloc-tracker (unified-profiler-alloc-tracker up)]
         [summary (alloc-tracker-summary alloc-tracker)]
         [entries (cdr (assq 'entries summary))]
         [total (cdr (assq 'total-formatted summary))])
        
        (string-append
         "\n"
         "  ====== Memory Allocation Report ======\n"
         (format "  Total Allocated: ~a\n\n" total)
         "  Allocations:\n"
         (if (null? entries)
             "    (no allocations recorded)\n"
             (apply string-append
                    (map (lambda (entry)
                                 (format "    ~a: ~a\n"
                                         (car entry)
                                         (format-bytes (cdr entry))))
                         entries)))
         "\n  ======================================\n")))

;;; ============================================================
;;; Cost Report
;;; ============================================================

;;; render-cost-report : UnifiedProfiler -> String
(define (render-cost-report up)
  (let* ([cost-tracker (unified-profiler-cost-tracker up)]
         [model (cost-tracker-model cost-tracker)]
         [costs (get-costs cost-tracker)]
         [total (get-total-cost cost-tracker)])
        
        (string-append
         "\n"
         (format "  ====== Cost Report (~a) ======\n" (cost-model-name model))
         (format "  Total Cost: ~a\n\n" total)
         "  By Category:\n"
         (if (null? costs)
             "    (no costs recorded)\n"
             (let ([sorted (list-sort (lambda (a b) (> (cdr a) (cdr b))) costs)])
                  (apply string-append
                         (map (lambda (entry)
                                      (let* ([cat (car entry)]
                                             [cost (cdr entry)]
                                             [pct (if (zero? total) 0
                                                      (* 100.0 (/ cost total)))])
                                            (format "    ~a: ~a (~a%)\n"
                                                    cat cost (round pct))))
                              sorted))))
         "\n  " (make-string 40 #\=) "\n")))

;;; ============================================================
;;; Call Graph Report
;;; ============================================================

;;; render-call-graph-report : UnifiedProfiler -> String
(define (render-call-graph-report up)
  (let* ([call-graph (unified-profiler-call-graph up)]
         [summary (call-graph-summary call-graph)]
         [ascii (call-graph->ascii call-graph)])
        (string-append summary ascii)))

;;; ============================================================
;;; Export to S-expression (for persistence)
;;; ============================================================

;;; unified-profiler->sexp : UnifiedProfiler -> S-expression
;;; Convert unified profiler to a serializable S-expression
(define (unified-profiler->sexp up)
  (let* ([base (unified-profiler-base up)]
         [cost-tracker (unified-profiler-cost-tracker up)]
         [alloc-tracker (unified-profiler-alloc-tracker up)]
         [call-graph (unified-profiler-call-graph up)]
         [metadata (unified-profiler-metadata up)])
        
        `(unified-profile-data
          (version . 1)
          (base-profiler
           (expr . ,(profiler-expr base))
           (initial-fuel . ,(profiler-initial-fuel base))
           (status . ,(profiler-status base))
           (result . ,(profiler-result base))
           (root . ,(node->sexp (profiler-root base))))
          (costs
           (model . ,(cost-model-name (cost-tracker-model cost-tracker)))
           (accumulated . ,(get-costs cost-tracker)))
          (memory . ,(alloc-tracker-summary alloc-tracker))
          (call-graph
           (nodes . ,(call-graph-all-nodes call-graph))
           (edges . ,(call-graph-edges->sexp call-graph)))
          (metadata . ,metadata))))

;;; node->sexp : Node -> S-expression
(define (node->sexp node)
  `(node
    (name . ,(node-name node))
    (fuel-consumed . ,(node-fuel-consumed node))
    (call-count . ,(node-call-count node))
    (children . ,(map node->sexp (node-children node)))))

;;; call-graph-edges->sexp : CallGraph -> S-expression
(define (call-graph-edges->sexp graph)
  (let ([edges (call-graph-edges graph)])
       (let-values ([(keys vals) (hashtable-entries edges)])
                   (map cons
                        (vector->list keys)
                        (vector->list vals)))))

;;; ============================================================
;;; Module Loading Confirmation
;;; ============================================================

(define *profiler-unified-loaded* #t)
