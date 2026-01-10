;;; core/lang/depgraph.ss — Module Dependency Graph & Cycle Detection
;;; @module depgraph
;;; @requires prelude
;;;
;;; Builds module dependency graphs, detects cycles, and provides
;;; suggestions for breaking them. Integrates with REPL for inspection.
;;;
;;; API:
;;;   (depgraph-refresh!)          — Rebuild dependency graph
;;;   (depgraph-deps "path")       — Show dependencies of a module
;;;   (depgraph-dependents "path") — Show what depends on a module
;;;   (depgraph-cycles)            — Detect and display all cycles
;;;   (depgraph-tree "path")       — Show dependency tree (ASCII)
;;;   (depgraph-stats)             — Show graph statistics
;;;   (depgraph-suggest "path")    — Suggest how to break cycles involving path
;;;   (depgraph-layers)            — Show modules in dependency layers
;;;
;;; Dependencies:
;;;   - prelude.ss (list utilities)
;;;   - index.ss (for module registry)

(load "core/base/prelude.ss")
(load "shell/tools/index.ss")

;;; ============================================================
;;; Graph Data Structures
;;; ============================================================

;;; *depgraph* : Hashtable String → (List String)
;;; Maps each module path to its direct dependencies.
(define *depgraph* (make-hashtable string-hash string=?))

;;; *reverse-depgraph* : Hashtable String → (List String)
;;; Maps each module path to modules that depend on it.
(define *depgraph-reverse* (make-hashtable string-hash string=?))

;;; *depgraph-cycles* : (List (List String))
;;; List of detected cycles (each cycle is a list of paths).
(define *depgraph-cycles* '())

;;; *depgraph-initialized* : Boolean
(define *depgraph-initialized* #f)

;;; ============================================================
;;; Graph Building
;;; ============================================================

;;; depgraph-refresh! : Unit → Void
;;; Rebuild the dependency graph from the module registry.
(define (depgraph-refresh!)
  ;; First refresh the index if needed
  (when (= 0 (hashtable-size *module-registry*))
        (index-refresh!))
  
  ;; Clear existing graph
  (hashtable-clear! *depgraph*)
  (hashtable-clear! *depgraph-reverse*)
  (set! *depgraph-cycles* '())
  
  (display "Building dependency graph...\n")
  
  ;; Build forward graph
  (vector-for-each
   (lambda (path)
           (let* ([entry (hashtable-ref *module-registry* path #f)]
                  [loads (if entry (cdr (assq 'loads entry)) '())])
                 ;; Store direct dependencies
                 (hashtable-set! *depgraph* path loads)
                 ;; Update reverse graph
                 (for-each
                  (lambda (dep)
                          (let ([existing (hashtable-ref *depgraph-reverse* dep '())])
                               (hashtable-set! *depgraph-reverse* dep (cons path existing))))
                  loads)))
   (hashtable-keys *module-registry*))
  
  ;; Detect cycles
  (set! *depgraph-cycles* (find-all-cycles))
  
  (set! *depgraph-initialized* #t)
  (display "Dependency graph complete.\n")
  (depgraph-stats))

;;; ============================================================
;;; Cycle Detection (Tarjan's SCC Algorithm)
;;; ============================================================

;;; find-all-cycles : Unit → (List (List String))
;;; Find all strongly connected components with more than one node.
(define (find-all-cycles)
  (let ([index 0]
        [stack '()]
        [indices (make-hashtable string-hash string=?)]
        [lowlinks (make-hashtable string-hash string=?)]
        [on-stack (make-hashtable string-hash string=?)]
        [sccs '()])
       
       (define (strongconnect v)
         (hashtable-set! indices v index)
         (hashtable-set! lowlinks v index)
         (set! index (+ index 1))
         (set! stack (cons v stack))
         (hashtable-set! on-stack v #t)
         
         ;; Consider successors of v
         (let ([deps (hashtable-ref *depgraph* v '())])
              (for-each
               (lambda (w)
                       (cond
                        [(not (hashtable-ref indices w #f))
                         ;; w has not been visited
                         (strongconnect w)
                         (hashtable-set! lowlinks v
                                         (min (hashtable-ref lowlinks v 0)
                                              (hashtable-ref lowlinks w 0)))]
                        [(hashtable-ref on-stack w #f)
                         ;; w is in stack, hence in current SCC
                         (hashtable-set! lowlinks v
                                         (min (hashtable-ref lowlinks v 0)
                                              (hashtable-ref indices w 0)))]))
               deps))
         
         ;; If v is a root node, pop the stack and generate an SCC
         (when (= (hashtable-ref lowlinks v 0) (hashtable-ref indices v 0))
               (let loop ([scc '()])
                    (let ([w (car stack)])
                         (set! stack (cdr stack))
                         (hashtable-set! on-stack w #f)
                         (if (string=? w v)
                             ;; Only keep SCCs with more than one node (actual cycles)
                             (when (> (length (cons w scc)) 1)
                                   (set! sccs (cons (cons w scc) sccs)))
                             (loop (cons w scc)))))))
       
       ;; Run on all nodes
       (vector-for-each
        (lambda (v)
                (unless (hashtable-ref indices v #f)
                        (strongconnect v)))
        (hashtable-keys *depgraph*))
       
       sccs))

;;; ============================================================
;;; Query Functions
;;; ============================================================

;;; depgraph-deps : String → (List String)
;;; Get direct dependencies of a module.
(define (depgraph-deps path)
  (ensure-depgraph!)
  (hashtable-ref *depgraph* path '()))

;;; depgraph-dependents : String → (List String)
;;; Get modules that directly depend on this one.
(define (depgraph-dependents path)
  (ensure-depgraph!)
  (hashtable-ref *depgraph-reverse* path '()))

;;; depgraph-all-deps : String → (List String)
;;; Get transitive closure of dependencies.
(define (depgraph-all-deps path)
  (ensure-depgraph!)
  (let ([visited (make-hashtable string-hash string=?)])
       (let loop ([pending (list path)] [result '()])
            (if (null? pending)
                (reverse result)
                (let ([current (car pending)])
                     (if (hashtable-ref visited current #f)
                         (loop (cdr pending) result)
                         (begin
                          (hashtable-set! visited current #t)
                          (let ([deps (hashtable-ref *depgraph* current '())])
                               (loop (append (cdr pending) deps)
                                     (if (string=? current path)
                                         result
                                         (cons current result)))))))))))

;;; depgraph-all-dependents : String → (List String)
;;; Get all modules that transitively depend on this one.
(define (depgraph-all-dependents path)
  (ensure-depgraph!)
  (let ([visited (make-hashtable string-hash string=?)])
       (let loop ([pending (list path)] [result '()])
            (if (null? pending)
                (reverse result)
                (let ([current (car pending)])
                     (if (hashtable-ref visited current #f)
                         (loop (cdr pending) result)
                         (begin
                          (hashtable-set! visited current #t)
                          (let ([deps (hashtable-ref *depgraph-reverse* current '())])
                               (loop (append (cdr pending) deps)
                                     (if (string=? current path)
                                         result
                                         (cons current result)))))))))))

;;; ensure-depgraph! : Unit → Void
(define (ensure-depgraph!)
  (unless *depgraph-initialized*
          (depgraph-refresh!)))

;;; ============================================================
;;; Cycle Analysis
;;; ============================================================

;;; depgraph-cycles : Unit → Void
;;; Display all detected cycles.
(define (depgraph-cycles)
  (ensure-depgraph!)
  (display "\n")
  (display "  ┌────────────────────────────────────────────────────────────────────┐\n")
  (display "  │                    DEPENDENCY CYCLES                               │\n")
  (display "  └────────────────────────────────────────────────────────────────────┘\n")
  (display "\n")
  
  (if (null? *depgraph-cycles*)
      (display "  No cycles detected. The dependency graph is acyclic.\n")
      (begin
       (display (format "  Found ~a cycle(s):\n\n" (length *depgraph-cycles*)))
       (let loop ([cycles *depgraph-cycles*] [n 1])
            (unless (null? cycles)
                    (let ([cycle (car cycles)])
                         (display (format "  Cycle ~a (~a modules):\n" n (length cycle)))
                         (display-cycle cycle)
                         (display "\n"))
                    (loop (cdr cycles) (+ n 1))))))
  (display "\n"))

;;; display-cycle : (List String) → Void
(define (display-cycle cycle)
  (let loop ([nodes cycle])
       (unless (null? nodes)
               (display (format "    ~a\n" (car nodes)))
               (unless (null? (cdr nodes))
                       (display "      ↓\n"))
               (loop (cdr nodes))))
  ;; Show it cycles back
  (display (format "      ↓ (back to ~a)\n" (car cycle))))

;;; depgraph-in-cycle? : String → Boolean
;;; Check if a module is part of any cycle.
(define (depgraph-in-cycle? path)
  (ensure-depgraph!)
  (ormap (lambda (cycle) (member path cycle)) *depgraph-cycles*))

;;; ============================================================
;;; Breaking Cycle Suggestions
;;; ============================================================

;;; depgraph-suggest : String → Void
;;; Suggest how to break cycles involving a module.
(define (depgraph-suggest path)
  (ensure-depgraph!)
  (display "\n")
  (display (format "  Suggestions for: ~a\n" path))
  (display "  ────────────────────────────────────────────────────────────\n\n")
  
  (let ([cycles-involving (filter (lambda (c) (member path c)) *depgraph-cycles*)])
       (if (null? cycles-involving)
           (display "  This module is not part of any cycles.\n")
           (begin
            (display (format "  Module is in ~a cycle(s).\n\n" (length cycles-involving)))
            
            (for-each
             (lambda (cycle)
                     (display "  Cycle:\n")
                     (display-cycle cycle)
                     (display "\n  Suggestions:\n")
                     
                     ;; Find the weakest link (fewest symbols shared)
                     (let ([edges (cycle-edges cycle)])
                          (for-each
                           (lambda (edge)
                                   (let* ([from (car edge)]
                                          [to (cdr edge)]
                                          [from-entry (hashtable-ref *module-registry* from #f)]
                                          [to-entry (hashtable-ref *module-registry* to #f)])
                                         (display (format "    • ~a → ~a\n"
                                                          (path-basename from)
                                                          (path-basename to)))
                                         (display "      Consider: Extract shared code to a third module\n")))
                           edges))
                     (display "\n"))
             cycles-involving))))
  (display "\n"))

;;; cycle-edges : (List String) → (List (Pair String String))
;;; Extract edges from a cycle.
(define (cycle-edges cycle)
  (if (null? cycle)
      '()
      (let loop ([nodes cycle] [edges '()])
           (if (null? (cdr nodes))
               (reverse (cons (cons (car nodes) (car cycle)) edges))
               (loop (cdr nodes)
                     (cons (cons (car nodes) (cadr nodes)) edges))))))

;;; path-basename : String → String
;;; Extract filename from path.
(define (path-basename path)
  (let ([len (string-length path)])
       (let loop ([i (- len 1)])
            (cond
             [(< i 0) path]
             [(char=? (string-ref path i) #\/) (substring path (+ i 1) len)]
             [else (loop (- i 1))]))))

;;; ============================================================
;;; Dependency Tree Visualization
;;; ============================================================

;;; depgraph-tree : String × Nat... → Void
;;; Display dependency tree for a module.
(define depgraph-tree
  (case-lambda
   [(path) (depgraph-tree path 3)]
   [(path max-depth)
    (ensure-depgraph!)
    (display "\n")
    (display (format "  Dependency tree for: ~a\n" path))
    (display "  ────────────────────────────────────────────────────────────\n\n")
    (let ([visited (make-hashtable string-hash string=?)])
         (display-tree path 0 max-depth visited "  "))
    (display "\n")]))

;;; display-tree : String × Nat × Nat × (Hashtable String Boolean) × String → Void
(define (display-tree path depth max-depth visited prefix)
  (when (<= depth max-depth)
        (let ([is-cycle (hashtable-ref visited path #f)]
              [deps (hashtable-ref *depgraph* path '())])
             (display (format "~a~a\n"
                              (path-basename path)
                              (if is-cycle " [visited]" "")))
             (unless is-cycle
                     (hashtable-set! visited path #t)
                     (let loop ([deps deps])
                          (unless (null? deps)
                                  (let* ([dep (car deps)]
                                         [is-last (null? (cdr deps))]
                                         [connector (if is-last "└── " "├── ")]
                                         [new-prefix (string-append prefix (if is-last "    " "│   "))])
                                        (display (format "~a~a" prefix connector))
                                        (display-tree dep (+ depth 1) max-depth visited new-prefix))
                                  (loop (cdr deps))))))))

;;; ============================================================
;;; Layered View
;;; ============================================================

;;; depgraph-layers : Unit → Void
;;; Show modules organized by dependency layers.
(define (depgraph-layers)
  (ensure-depgraph!)
  (display "\n")
  (display "  ┌────────────────────────────────────────────────────────────────────┐\n")
  (display "  │                    DEPENDENCY LAYERS                               │\n")
  (display "  └────────────────────────────────────────────────────────────────────┘\n")
  (display "\n")
  
  (let ([layers (compute-layers)])
       (let loop ([layers layers] [n 0])
            (unless (null? layers)
                    (let ([layer (car layers)])
                         (display (format "  Layer ~a (~a modules):\n" n (length layer)))
                         (for-each
                          (lambda (path)
                                  (display (format "    ~a\n" (path-basename path))))
                          (list-sort string<? layer))
                         (display "\n"))
                    (loop (cdr layers) (+ n 1)))))
  (display "\n"))

;;; compute-layers : Unit → (List (List String))
;;; Compute topological layers (Kahn's algorithm variant).
(define (compute-layers)
  (let ([in-degree (make-hashtable string-hash string=?)]
        [remaining (make-hashtable string-hash string=?)])
       
       ;; Initialize in-degrees
       (vector-for-each
        (lambda (path)
                (hashtable-set! in-degree path 0)
                (hashtable-set! remaining path #t))
        (hashtable-keys *depgraph*))
       
       ;; Count in-degrees
       (vector-for-each
        (lambda (path)
                (let ([deps (hashtable-ref *depgraph* path '())])
                     (for-each
                      (lambda (dep)
                              (when (hashtable-ref in-degree dep #f)
                                    (hashtable-set! in-degree dep
                                                    (+ 1 (hashtable-ref in-degree dep 0)))))
                      deps)))
        (hashtable-keys *depgraph*))
       
       ;; Build layers
       (let loop ([layers '()])
            (let ([current-layer
                   (filter
                    (lambda (path)
                            (and (hashtable-ref remaining path #f)
                                 (= 0 (hashtable-ref in-degree path 0))))
                    (vector->list (hashtable-keys *depgraph*)))])
                 (if (null? current-layer)
                     (reverse layers)
                     (begin
                      ;; Remove current layer from remaining
                      (for-each
                       (lambda (path)
                               (hashtable-set! remaining path #f))
                       current-layer)
                      ;; Decrease in-degrees
                      (for-each
                       (lambda (path)
                               (for-each
                                (lambda (dep)
                                        (when (hashtable-ref in-degree dep #f)
                                              (hashtable-set! in-degree dep
                                                              (- (hashtable-ref in-degree dep 0) 1))))
                                (hashtable-ref *depgraph* path '())))
                       current-layer)
                      (loop (cons current-layer layers))))))))

;;; ============================================================
;;; Statistics
;;; ============================================================

;;; depgraph-stats : Unit → Void
;;; Display dependency graph statistics.
(define (depgraph-stats)
  (ensure-depgraph!)
  (let ([total-modules (hashtable-size *depgraph*)]
        [total-edges 0]
        [max-deps 0]
        [max-deps-module #f]
        [leaf-count 0]
        [root-count 0])
       
       ;; Calculate statistics
       (vector-for-each
        (lambda (path)
                (let ([deps (hashtable-ref *depgraph* path '())]
                      [dependents (hashtable-ref *depgraph-reverse* path '())])
                     (set! total-edges (+ total-edges (length deps)))
                     (when (> (length deps) max-deps)
                           (set! max-deps (length deps))
                           (set! max-deps-module path))
                     (when (null? deps)
                           (set! leaf-count (+ leaf-count 1)))
                     (when (null? dependents)
                           (set! root-count (+ root-count 1)))))
        (hashtable-keys *depgraph*))
       
       (display "\n")
       (display "  ┌────────────────────────────────────────────────────────────────────┐\n")
       (display "  │                    DEPENDENCY GRAPH STATS                          │\n")
       (display "  └────────────────────────────────────────────────────────────────────┘\n")
       (display "\n")
       (display (format "  Modules:            ~a\n" total-modules))
       (display (format "  Dependencies:       ~a\n" total-edges))
       (display (format "  Avg deps/module:    ~a\n"
                        (if (> total-modules 0)
                            (quotient total-edges total-modules)
                            0)))
       (display (format "  Leaf modules:       ~a (no dependencies)\n" leaf-count))
       (display (format "  Root modules:       ~a (nothing depends on them)\n" root-count))
       (display (format "  Cycles detected:    ~a\n" (length *depgraph-cycles*)))
       (when max-deps-module
             (display (format "  Most deps:          ~a (~a deps)\n"
                              (path-basename max-deps-module)
                              max-deps)))
       (display "\n")))

;;; ============================================================
;;; REPL Commands (convenience wrappers)
;;; ============================================================

;;; deps : String → Void
;;; Show what a module depends on.
(define (deps path)
  (ensure-depgraph!)
  (display "\n")
  (display (format "  ~a depends on:\n" (path-basename path)))
  (let ([d (depgraph-deps path)])
       (if (null? d)
           (display "    (nothing - leaf module)\n")
           (for-each
            (lambda (p) (display (format "    ~a\n" p)))
            d)))
  (display "\n"))

;;; rdeps : String → Void
;;; Show what depends on a module.
(define (rdeps path)
  (ensure-depgraph!)
  (display "\n")
  (display (format "  Modules that depend on ~a:\n" (path-basename path)))
  (let ([d (depgraph-dependents path)])
       (if (null? d)
           (display "    (nothing - root module)\n")
           (for-each
            (lambda (p) (display (format "    ~a\n" p)))
            d)))
  (display "\n"))

;;; cycles : Unit → Void
;;; Alias for depgraph-cycles.
(define cycles depgraph-cycles)

;;; ============================================================
;;; Initialization
;;; ============================================================

(display "\nDependency graph system loaded.\n")
(display "Commands: (deps \"path\"), (rdeps \"path\"), (cycles)\n")
(display "          (depgraph-tree \"path\"), (depgraph-layers)\n")
(display "          (depgraph-suggest \"path\"), (depgraph-stats)\n")
(display "          (depgraph-refresh!) to rebuild graph\n")
(display "\n")
