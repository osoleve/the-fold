;;; shell/profile-call-graph.ss — Call Graph Builder from Profiler Traces
;;;
;;; Extracts call graphs from profiler output, enabling analysis of
;;; caller-callee relationships, critical paths, and cycle detection.
;;;
;;; This is Shell code: builds analysis structures from profiler data.
;;;
;;; Dependencies:
;;;   - core/util/profile.ss

(load "core/util/profile.ss")

;;; ============================================================
;;; Call Graph Data Structure
;;; ============================================================

;;; A call graph is a hashtable mapping caller -> ((callee . count) ...)
;;; We also maintain a reverse index: callee -> ((caller . count) ...)
;;;
;;; Structure: (call-graph (edges . hashtable) (reverse . hashtable))

(define (make-call-graph)
  `(call-graph
    (edges . ,(make-hashtable symbol-hash eq?))
    (reverse . ,(make-hashtable symbol-hash eq?))))

(define (call-graph? g)
  (and (pair? g) (eq? (car g) 'call-graph)))

(define (call-graph-edges graph)
  (cdr (assq 'edges (cdr graph))))

(define (call-graph-reverse graph)
  (cdr (assq 'reverse (cdr graph))))

;;; ============================================================
;;; Edge Operations
;;; ============================================================

;;; call-graph-add-edge! : Graph x Symbol x Symbol -> void
;;; Add an edge from caller to callee (mutates graph)
(define (call-graph-add-edge! graph caller callee)
  (let* ([edges (call-graph-edges graph)]
         [reverse-idx (call-graph-reverse graph)]
         [existing (hashtable-ref edges caller '())]
         [entry (assq callee existing)])
        ;; Update forward edge
        (if entry
            (set-cdr! entry (+ 1 (cdr entry)))
            (hashtable-set! edges caller
                            (cons (cons callee 1) existing)))
        ;; Update reverse index
        (let* ([rev-existing (hashtable-ref reverse-idx callee '())]
               [rev-entry (assq caller rev-existing)])
              (if rev-entry
                  (set-cdr! rev-entry (+ 1 (cdr rev-entry)))
                  (hashtable-set! reverse-idx callee
                                  (cons (cons caller 1) rev-existing))))))

;;; call-graph-callees : Graph x Symbol -> List of (callee . count)
;;; Get all functions called by the given node
(define (call-graph-callees graph node)
  (hashtable-ref (call-graph-edges graph) node '()))

;;; call-graph-callers : Graph x Symbol -> List of (caller . count)
;;; Get all functions that call the given node
(define (call-graph-callers graph node)
  (hashtable-ref (call-graph-reverse graph) node '()))

;;; call-graph-all-nodes : Graph -> List of Symbols
;;; Get all unique nodes in the graph
(define (call-graph-all-nodes graph)
  (let* ([edges (call-graph-edges graph)]
         [reverse-idx (call-graph-reverse graph)]
         [callers (vector->list (hashtable-keys edges))]
         [callees (vector->list (hashtable-keys reverse-idx))])
        (remove-duplicates (append callers callees))))

;;; NOTE: remove-duplicates is provided by core/base/prelude.ss
;;; For symbol-only lists, unique-simple uses memq for faster eq? comparison.

;;; ============================================================
;;; Build Call Graph from Profiler
;;; ============================================================

;;; build-call-graph : Profiler -> CallGraph
;;; Walk the call tree and extract caller->callee edges
(define (build-call-graph profiler)
  (let ([graph (make-call-graph)]
        [root (profiler-root profiler)])
       (walk-tree-for-edges root graph)
       graph))

;;; walk-tree-for-edges : Node x Graph -> void
;;; Recursively walk tree adding edges for each parent->child relationship
(define (walk-tree-for-edges node graph)
  (let ([parent-name (node-name node)]
        [children (node-children node)])
       (for-each
        (lambda (child)
                (let ([child-name (node-name child)]
                      [child-count (node-call-count child)])
                     ;; Add edge for each call
                     (let loop ([n child-count])
                          (when (> n 0)
                                (call-graph-add-edge! graph parent-name child-name)
                                (loop (- n 1))))
                     ;; Recurse into child
                     (walk-tree-for-edges child graph)))
        children)))

;;; build-call-graph-from-node : Node -> CallGraph
;;; Build graph starting from a specific node (useful for testing)
(define (build-call-graph-from-node node)
  (let ([graph (make-call-graph)])
       (walk-tree-for-edges node graph)
       graph))

;;; ============================================================
;;; Graph Metrics
;;; ============================================================

;;; call-graph-roots : Graph -> List of Symbols
;;; Entry points: nodes with no callers
(define (call-graph-roots graph)
  (let ([all-nodes (call-graph-all-nodes graph)])
       (filter (lambda (node)
                       (null? (call-graph-callers graph node)))
               all-nodes)))

;;; call-graph-leaves : Graph -> List of Symbols
;;; Leaf functions: nodes with no callees
(define (call-graph-leaves graph)
  (let ([all-nodes (call-graph-all-nodes graph)])
       (filter (lambda (node)
                       (null? (call-graph-callees graph node)))
               all-nodes)))

;;; call-graph-fan-out : Graph x Symbol -> Nat
;;; Number of distinct callees for a node
(define (call-graph-fan-out graph node)
  (length (call-graph-callees graph node)))

;;; call-graph-fan-in : Graph x Symbol -> Nat
;;; Number of distinct callers for a node
(define (call-graph-fan-in graph node)
  (length (call-graph-callers graph node)))

;;; call-graph-depth : Graph x Symbol -> Nat
;;; Max depth from node following callee edges (using BFS to handle cycles)
(define (call-graph-depth graph node)
  (call-graph-depth-with-visited graph node '()))

(define (call-graph-depth-with-visited graph node visited)
  (if (memq node visited)
      0  ; Stop at cycles
      (let* ([callees (call-graph-callees graph node)]
             [new-visited (cons node visited)])
            (if (null? callees)
                0
                (+ 1 (apply max
                            (map (lambda (edge)
                                         (call-graph-depth-with-visited
                                          graph (car edge) new-visited))
                                 callees)))))))

;;; call-graph-total-calls : Graph -> Nat
;;; Total number of call edges in the graph
(define (call-graph-total-calls graph)
  (let ([edges (call-graph-edges graph)])
       (let-values ([(keys vals) (hashtable-entries edges)])
                   (fold-left
                    (lambda (acc callees)
                            (+ acc (fold-left (lambda (a e) (+ a (cdr e)))
                                              0 callees)))
                    0
                    (vector->list vals)))))

;;; ============================================================
;;; Critical Path Analysis
;;; ============================================================

;;; find-critical-path : Graph x (Symbol -> Number) -> List of Symbols
;;; Find the longest cost chain through the graph.
;;; cost-fn maps node names to their cost (e.g., fuel consumed).
(define (find-critical-path graph cost-fn)
  (let ([roots (call-graph-roots graph)])
       (if (null? roots)
           '()
           (let ([paths (map (lambda (root)
                                     (longest-path-from graph root cost-fn '()))
                             roots)])
                (car (list-sort (lambda (a b)
                                        (> (path-total-cost a cost-fn)
                                           (path-total-cost b cost-fn)))
                                paths))))))

;;; longest-path-from : Graph x Symbol x CostFn x Visited -> List of Symbols
;;; Find longest path starting from node (DFS with cycle detection)
(define (longest-path-from graph node cost-fn visited)
  (if (memq node visited)
      '()  ; Cycle detected, stop
      (let* ([callees (call-graph-callees graph node)]
             [new-visited (cons node visited)])
            (if (null? callees)
                (list node)
                (let* ([child-paths
                        (map (lambda (edge)
                                     (longest-path-from graph (car edge)
                                                        cost-fn new-visited))
                             callees)]
                       [non-empty (filter (lambda (p) (not (null? p)))
                                          child-paths)])
                      (if (null? non-empty)
                          (list node)
                          (let ([best (car (list-sort
                                            (lambda (a b)
                                                    (> (path-total-cost a cost-fn)
                                                       (path-total-cost b cost-fn)))
                                            non-empty))])
                               (cons node best))))))))

;;; path-total-cost : List of Symbols x CostFn -> Number
(define (path-total-cost path cost-fn)
  (fold-left (lambda (acc node) (+ acc (cost-fn node)))
             0
             path))

;;; ============================================================
;;; Cycle Detection (Recursive Calls)
;;; ============================================================

;;; find-call-cycles : Graph -> List of (List of Symbols)
;;; Detect all cycles in the call graph (recursive calls)
(define (find-call-cycles graph)
  (let ([all-nodes (call-graph-all-nodes graph)]
        [cycles '()])
       (for-each
        (lambda (start)
                (let ([found (find-cycles-from graph start (list start))])
                     (for-each (lambda (cycle)
                                       (unless (cycle-already-found? cycle cycles)
                                               (set! cycles (cons cycle cycles))))
                               found)))
        all-nodes)
       cycles))

;;; find-cycles-from : Graph x Symbol x Path -> List of Cycles
;;; DFS to find cycles starting from a node
(define (find-cycles-from graph node path)
  (let ([callees (call-graph-callees graph node)])
       (let loop ([edges callees] [found '()])
            (if (null? edges)
                found
                (let* ([callee (caar edges)]
                       [idx (member-index callee path)])
                      (if idx
                          ;; Found cycle: extract it
                          (let ([cycle (take-from-index path idx)])
                               (loop (cdr edges) (cons cycle found)))
                          ;; Continue DFS
                          (let ([deeper (find-cycles-from graph callee
                                                          (append path (list callee)))])
                               (loop (cdr edges)
                                     (append deeper found)))))))))

;;; member-index : Element x List -> Nat or #f
;;; Return index of element in list, or #f
(define (member-index elem lst)
  (let loop ([lst lst] [idx 0])
       (cond
        [(null? lst) #f]
        [(eq? (car lst) elem) idx]
        [else (loop (cdr lst) (+ idx 1))])))

;;; take-from-index : List x Nat -> List
;;; Take elements from index to end
(define (take-from-index lst idx)
  (let loop ([lst lst] [i 0])
       (if (< i idx)
           (loop (cdr lst) (+ i 1))
           lst)))

;;; cycle-already-found? : Cycle x List of Cycles -> Boolean
;;; Check if this cycle (or a rotation of it) is already recorded
(define (cycle-already-found? cycle cycles)
  (any? (lambda (existing)
                (cycles-equal? cycle existing))
        cycles))

;;; cycles-equal? : Cycle x Cycle -> Boolean
;;; Two cycles are equal if one is a rotation of the other
(define (cycles-equal? c1 c2)
  (and (= (length c1) (length c2))
       (let loop ([rotations c2] [n (length c2)])
            (if (zero? n)
                #f
                (or (equal? c1 rotations)
                    (loop (rotate-left rotations) (- n 1)))))))

;;; rotate-left : List -> List
;;; Rotate list one position left
(define (rotate-left lst)
  (if (null? lst)
      lst
      (append (cdr lst) (list (car lst)))))

;;; any? : Predicate x List -> Boolean
(define (any? pred lst)
  (cond
   [(null? lst) #f]
   [(pred (car lst)) #t]
   [else (any? pred (cdr lst))]))

;;; ============================================================
;;; ASCII Visualization
;;; ============================================================

;;; call-graph->ascii : Graph -> String
;;; Render the call graph as ASCII art
(define (call-graph->ascii graph)
  (let* ([all-nodes (call-graph-all-nodes graph)]
         [roots (call-graph-roots graph)]
         [lines '()])
        (string-append
         "\n  CALL GRAPH\n"
         "  " (make-string 50 #\=) "\n\n"
         (if (null? roots)
             (if (null? all-nodes)
                 "  (empty graph)\n"
                 ;; No roots means we have cycles only
                 (apply string-append
                        (map (lambda (node)
                                     (render-node-ascii graph node 0 '()))
                             all-nodes)))
             (apply string-append
                    (map (lambda (root)
                                 (render-node-ascii graph root 0 '()))
                         roots)))
         "\n  " (make-string 50 #\=) "\n"
         (format "  Nodes: ~a  Edges: ~a\n"
                 (length all-nodes)
                 (call-graph-total-calls graph)))))

;;; render-node-ascii : Graph x Symbol x Depth x Visited -> String
;;; Render a node and its callees recursively
(define (render-node-ascii graph node depth visited)
  (if (memq node visited)
      ;; Cycle reference
      (format "~a~a (cycle)\n"
              (make-string (* depth 2) #\space)
              node)
      (let* ([callees (call-graph-callees graph node)]
             [prefix (if (zero? depth)
                         ""
                         (string-append (make-string (* (- depth 1) 2) #\space)
                                        "+-"))]
             [header (format "~a~a~a\n"
                             prefix
                             node
                             (if (null? callees)
                                 ""
                                 (format " (fan-out: ~a)" (length callees))))]
             [new-visited (cons node visited)]
             [child-lines
              (map (lambda (edge)
                           (let ([callee (car edge)]
                                 [count (cdr edge)])
                                (string-append
                                 (format "~a  [~ax] "
                                         (make-string (* depth 2) #\space)
                                         count)
                                 (render-node-ascii graph callee
                                                    (+ depth 1)
                                                    new-visited))))
                   callees)])
            (apply string-append (cons header child-lines)))))

;;; call-graph-summary : Graph -> String
;;; Quick summary statistics
(define (call-graph-summary graph)
  (let* ([all-nodes (call-graph-all-nodes graph)]
         [roots (call-graph-roots graph)]
         [leaves (call-graph-leaves graph)]
         [cycles (find-call-cycles graph)])
        (format (string-append
                 "Call Graph Summary:\n"
                 "  Total nodes:    ~a\n"
                 "  Entry points:   ~a\n"
                 "  Leaf functions: ~a\n"
                 "  Cycles found:   ~a\n")
                (length all-nodes)
                (length roots)
                (length leaves)
                (length cycles))))

;;; ============================================================
;;; Integration with Profiler Statistics
;;; ============================================================

;;; call-graph-with-costs : Profiler -> (Graph x CostTable)
;;; Build graph and collect per-function costs from profiler
(define (call-graph-with-costs profiler)
  (let* ([graph (build-call-graph profiler)]
         [root (profiler-root profiler)]
         [costs (make-hashtable symbol-hash eq?)])
        (collect-costs root costs)
        (cons graph costs)))

;;; collect-costs : Node x Hashtable -> void
(define (collect-costs node costs)
  (let ([name (node-name node)]
        [fuel (node-fuel-consumed node)])
       (hashtable-update! costs name
                          (lambda (v) (+ v fuel))
                          0)
       (for-each (lambda (child)
                         (collect-costs child costs))
                 (node-children node))))

;;; make-cost-fn : Hashtable -> (Symbol -> Number)
;;; Create a cost function from a cost table
(define (make-cost-fn cost-table)
  (lambda (node)
          (hashtable-ref cost-table node 0)))

;;; find-critical-path-from-profiler : Profiler -> (Path x TotalCost)
;;; Convenience function to find critical path with fuel costs
(define (find-critical-path-from-profiler profiler)
  (let* ([result (call-graph-with-costs profiler)]
         [graph (car result)]
         [costs (cdr result)]
         [cost-fn (make-cost-fn costs)]
         [path (find-critical-path graph cost-fn)]
         [total (path-total-cost path cost-fn)])
        (cons path total)))
