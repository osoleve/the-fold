(load "core/util/profile.ss")
(load "core/util/cost-model.ss")
(load "boundary/diagnostics/alloc-tracker.ss")
(load "boundary/diagnostics/profile-call-graph.ss")

(doc 'module 'profiler-unified)
(doc 'description "Unified Instrumented Profiler - integrates cost models, allocation tracking, call graph building, and fuel profiling")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(core/util/profile.ss core/util/cost-model.ss boundary/diagnostics/alloc-tracker.ss boundary/diagnostics/profile-call-graph.ss))

(doc 'section 'unified-profiler-data-structure)

(doc make-unified-profiler 'type '(-> Profiler CostTracker AllocTracker CallGraph Alist UnifiedProfiler))
(doc make-unified-profiler 'description "Create unified profiler combining base profiler, cost tracker, allocation tracker, call graph, and metadata")
(doc make-unified-profiler 'export #t)
(define (make-unified-profiler base-profiler cost-tracker alloc-tracker call-graph metadata)
  (doc 'type '(-> Profiler CostTracker AllocTracker CallGraph Alist UnifiedProfiler))
  (doc 'description "Create unified profiler combining base profiler, cost tracker, allocation tracker, call graph, and metadata")
  `(unified-profiler
    (base-profiler . ,base-profiler)
    (cost-tracker  . ,cost-tracker)
    (alloc-tracker . ,alloc-tracker)
    (call-graph    . ,call-graph)
    (metadata      . ,metadata)))

(doc unified-profiler? 'type '(-> Any Boolean))
(doc unified-profiler? 'export #t)
(define (unified-profiler? x)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Check if value is a unified profiler")
  (and (pair? x) (eq? (car x) 'unified-profiler)))

(doc unified-profiler-get 'type '(-> UnifiedProfiler Symbol Any))
(define (unified-profiler-get up key)
  (doc 'type '(-> UnifiedProfiler Symbol Any))
  (doc 'description "Get field from unified profiler by key")
  (let ([entry (assq key (cdr up))])
       (and entry (cdr entry))))

(doc 'section 'accessors)
(define (unified-profiler-base up) (unified-profiler-get up 'base-profiler))
(define (unified-profiler-cost-tracker up) (unified-profiler-get up 'cost-tracker))
(define (unified-profiler-alloc-tracker up) (unified-profiler-get up 'alloc-tracker))
(define (unified-profiler-call-graph up) (unified-profiler-get up 'call-graph))
(define (unified-profiler-metadata up) (unified-profiler-get up 'metadata))

(doc 'section 'profile-options)

(doc default-profile-options 'export #t)
(define (default-profile-options)
  (doc 'description "Default options for unified profiling")
  `((cost-model      . ,fuel-cost-model)
    (track-memory    . #t)
    (build-call-graph . #t)
    (fuel-budget     . 10000)))

(doc merge-options 'type '(-> Alist Alist Alist))
(define (merge-options defaults user)
  (doc 'type '(-> Alist Alist Alist))
  (doc 'description "Merge user options with defaults, user takes precedence")
  (let loop ([defaults defaults] [result '()])
       (if (null? defaults)
           (append result user)
           (let* ([key (caar defaults)]
                  [user-val (assq key user)])
                 (loop (cdr defaults)
                       (cons (if user-val
                                 user-val
                                 (car defaults))
                             result))))))

(define (get-option opts key)
  (let ([entry (assq key opts)])
       (and entry (cdr entry))))

(doc 'section 'unified-profiling)

(doc profile-unified 'type '(-> Expr Alist UnifiedProfiler))
(doc profile-unified 'description "Profile an expression with all tracking enabled - cost model, memory, call graph")
(doc profile-unified 'export #t)
(doc profile-unified 'param '(expr "Expression to profile"))
(doc profile-unified 'param '(user-opts "Options: cost-model, track-memory, build-call-graph, fuel-budget"))
(define (profile-unified expr . user-opts)
  (doc 'type '(-> Expr Alist UnifiedProfiler))
  (doc 'description "Profile an expression with all tracking enabled")
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
                                 (lambda () (profile-expr expr fuel)))
                                (values alloc-tracker-init
                                        (profile-expr expr fuel)))])

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

(doc profile-unified-with-env 'type '(-> Expr Env Alist UnifiedProfiler))
(doc profile-unified-with-env 'export #t)
(define (profile-unified-with-env expr env . user-opts)
  (doc 'type '(-> Expr Env Alist UnifiedProfiler))
  (doc 'description "Profile with a pre-built environment")
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

(doc 'section 'cost-computation)

(doc compute-costs-from-profiler 'type '(-> CostTracker Profiler CostTracker))
(define (compute-costs-from-profiler tracker profiler)
  (doc 'type '(-> CostTracker Profiler CostTracker))
  (doc 'description "Walk the profiler tree and accumulate costs by category")
  (let ([root (profiler-root profiler)])
       (compute-costs-from-node tracker root)))

(define (compute-costs-from-node tracker node)
  (let* ([name (node-name node)]
         [fuel (node-fuel-consumed node)]
         [children (node-children node)]
         [model (cost-tracker-model tracker)]

         [category (categorize-node-type name)]

         [tracker-with-self (track-cost tracker category fuel)])

        (fold-left compute-costs-from-node
                   tracker-with-self
                   children)))

(doc categorize-node-type 'type '(-> Symbol Symbol))
(define (categorize-node-type name)
  (doc 'type '(-> Symbol Symbol))
  (doc 'description "Categorize a node name into a cost category")
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
         (if (member name '(+ - * / mod quotient remainder
                            cons car cdr list append reverse
                            eq? eqv? equal? < > <= >=
                            null? pair? number? symbol?
                            map filter fold-left fold-right
                            assq assv assoc member memq memv))
             'primitive
             'function)]))

(doc 'section 'statistics)

(doc unified-profile-stats 'type '(-> UnifiedProfiler Alist))
(doc unified-profile-stats 'export #t)
(define (unified-profile-stats up)
  (doc 'type '(-> UnifiedProfiler Alist))
  (doc 'description "Comprehensive statistics from all tracking systems")
  (let* ([base (unified-profiler-base up)]
         [cost-tracker (unified-profiler-cost-tracker up)]
         [alloc-tracker (unified-profiler-alloc-tracker up)]
         [call-graph (unified-profiler-call-graph up)]
         [metadata (unified-profiler-metadata up)]

         [base-stats (profile-stats base)]

         [cost-breakdown (get-costs cost-tracker)]
         [total-cost (get-total-cost cost-tracker)]

         [alloc-summary (alloc-tracker-summary alloc-tracker)]

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

(doc 'section 'status-results)

(doc unified-profiler-status 'export #t)
(define (unified-profiler-status up)
  (profiler-status (unified-profiler-base up)))

(doc unified-profiler-result 'export #t)
(define (unified-profiler-result up)
  (profiler-result (unified-profiler-base up)))

(doc unified-profiler-expr 'export #t)
(define (unified-profiler-expr up)
  (profiler-expr (unified-profiler-base up)))

(doc 'section 'convenience-functions)

(doc unified-profile-memory 'export #t)
(define (unified-profile-memory expr)
  (doc 'description "Quick profiling focused on memory")
  (profile-unified expr '((track-memory . #t))))

(doc unified-profile-costs 'export #t)
(define (unified-profile-costs expr cost-model)
  (doc 'description "Profile with a specific cost model")
  (profile-unified expr `((cost-model . ,cost-model))))

(doc unified-profile-call-graph 'export #t)
(define (unified-profile-call-graph expr)
  (doc 'description "Quick profiling to get call graph")
  (profile-unified expr '((build-call-graph . #t))))

(doc unified-profile-minimal 'export #t)
(define (unified-profile-minimal expr)
  (doc 'description "Minimal profiling without memory or call graph tracking")
  (profile-unified expr '((track-memory . #f)
                          (build-call-graph . #f))))

(doc 'section 'time-helpers)

(doc get-monotonic-time 'type '(-> Time))
(define (get-monotonic-time)
  (doc 'type '(-> Time))
  (doc 'description "Get current monotonic time")
  (current-time 'time-monotonic))

(doc compute-time-difference 'type '(-> Time Time Integer))
(define (compute-time-difference end start)
  (doc 'type '(-> Time Time Integer))
  (doc 'description "Compute difference between two Chez time objects in nanoseconds")
  (let* ([end-s (time-second end)]
         [end-ns (time-nanosecond end)]
         [start-s (time-second start)]
         [start-ns (time-nanosecond start)]
         [sec-diff (- end-s start-s)]
         [ns-diff (- end-ns start-ns)])
        (+ (* sec-diff 1000000000) ns-diff)))

(doc 'section 'report-generation)

(doc render-unified-summary 'type '(-> UnifiedProfiler String))
(doc render-unified-summary 'export #t)
(define (render-unified-summary up)
  (doc 'type '(-> UnifiedProfiler String))
  (doc 'description "Render a summary of all profiling data")
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
         "  +====+\n"
         "  |        UNIFIED PROFILE SUMMARY            |\n"
         "  +====+\n"
         "\n"
         "  EXECUTION\n"
         "  ----\n"
         (format "    Status:      ~a\n" (unified-profiler-status up))
         (format "    Fuel Used:   ~a / ~a\n" used-fuel total-fuel)
         (format "    Elapsed:     ~a\n" (format-elapsed (cdr (assq 'elapsed-ns metadata))))
         "\n"
         "  MEMORY\n"
         "  ----\n"
         (format "    Allocated:   ~a\n" mem-total)
         "\n"
         (format "  COST MODEL (~a)\n" (cdr (assq 'cost-model metadata)))
         "  ----\n"
         (format "    Total Cost:  ~a\n" total-cost)
         (render-cost-breakdown cost-breakdown)
         "\n"
         "  CALL GRAPH\n"
         "  ----\n"
         (format "    Nodes:       ~a\n" node-count)
         (format "    Cycles:      ~a\n" cycle-count)
         "\n"
         "  +====+\n")))

(define (render-cost-breakdown breakdown)
  (if (null? breakdown)
      "    (no costs recorded)\n"
      (apply string-append
             (map (lambda (entry)
                          (format "    ~a: ~a\n" (car entry) (cdr entry)))
                  (list-sort (lambda (a b) (> (cdr a) (cdr b)))
                             breakdown)))))

(doc format-elapsed 'type '(-> Integer String))
(define (format-elapsed ns)
  (doc 'type '(-> Integer String))
  (doc 'description "Format elapsed nanoseconds as human-readable duration, handles negative values")
  (let ([ns (max 0 ns)])
       (cond
        [(< ns 1000) (format "~ans" ns)]
        [(< ns 1000000) (format "~aus" (quotient ns 1000))]
        [(< ns 1000000000) (format "~ams" (quotient ns 1000000))]
        [else (format "~as" (quotient ns 1000000000))])))

(doc display-unified-profile 'export #t)
(define (display-unified-profile up)
  (display (render-unified-summary up)))

(doc 'section 'memory-report)

(doc render-memory-report 'export #t)
(define (render-memory-report up)
  (let* ([alloc-tracker (unified-profiler-alloc-tracker up)]
         [summary (alloc-tracker-summary alloc-tracker)]
         [entries (cdr (assq 'entries summary))]
         [total (cdr (assq 'total-formatted summary))])

        (string-append
         "\n"
         "  ==== Memory Allocation Report ====\n"
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
         "\n  ====\n")))

(doc 'section 'cost-report)

(doc render-cost-report 'export #t)
(define (render-cost-report up)
  (let* ([cost-tracker (unified-profiler-cost-tracker up)]
         [model (cost-tracker-model cost-tracker)]
         [costs (get-costs cost-tracker)]
         [total (get-total-cost cost-tracker)])

        (string-append
         "\n"
         (format "  ==== Cost Report (~a) ====\n" (cost-model-name model))
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

(doc 'section 'call-graph-report)

(doc render-call-graph-report 'export #t)
(define (render-call-graph-report up)
  (let* ([call-graph (unified-profiler-call-graph up)]
         [summary (call-graph-summary call-graph)]
         [ascii (call-graph->ascii call-graph)])
        (string-append summary ascii)))

(doc 'section 'export-to-sexp)

(doc unified-profiler->sexp 'type '(-> UnifiedProfiler Sexp))
(doc unified-profiler->sexp 'export #t)
(define (unified-profiler->sexp up)
  (doc 'type '(-> UnifiedProfiler Sexp))
  (doc 'description "Convert unified profiler to a serializable S-expression")
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

(define (node->sexp node)
  `(node
    (name . ,(node-name node))
    (fuel-consumed . ,(node-fuel-consumed node))
    (call-count . ,(node-call-count node))
    (children . ,(map node->sexp (node-children node)))))

(define (call-graph-edges->sexp graph)
  (let ([edges (call-graph-edges graph)])
       (let-values ([(keys vals) (hashtable-entries edges)])
                   (map cons
                        (vector->list keys)
                        (vector->list vals)))))

(define *profiler-unified-loaded* #t)
