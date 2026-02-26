;;; lattice/meta/test-derive-deps-properties.ss — QuickCheck properties for derive-deps

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(load "lattice/meta/derive-deps.ss")

;;; ============================================================================
;;; Generators
;;; ============================================================================

;; Pool of node names for dependency graphs
(define *node-pool*
  '(alpha bravo charlie delta echo foxtrot golf hotel india juliet))

;; Generate a symbol from the pool
(define gen-node
  (gen-elements *node-pool*))

;; Generate a list of unique symbols (subset of node pool)
(define (gen-node-subset min-count max-count)
  (gen-bind (gen-int-range min-count (min max-count (length *node-pool*)))
    (lambda (n)
      (gen-pure (list-head *node-pool* n)))))

;; Helper: sequence a list of generators into a generator of lists
(define (gen-sequence gens)
  (if (null? gens)
      (gen-pure '())
      (gen-bind (car gens)
        (lambda (x)
          (gen-map (lambda (rest) (cons x rest))
                   (gen-sequence (cdr gens)))))))

;; Generate a random DAG as an alist.
;; Node i can only depend on nodes j > i (topological ordering by index).
(define (gen-dag min-nodes max-nodes)
  (gen-bind (gen-int-range min-nodes max-nodes)
    (lambda (n)
      (let ([nodes (list-head *node-pool* n)])
        (gen-bind
          (gen-sequence
            (map (lambda (i)
                   (let* ([node (list-ref nodes i)]
                          [later (list-tail nodes (+ i 1))]
                          [num-later (length later)])
                     (if (= num-later 0)
                         (gen-pure (cons node '()))
                         ;; Each later node has ~30% chance of being a dep
                         (gen-bind
                           (gen-list-of num-later gen-bool)
                           (lambda (flags)
                             (let ([deps (let loop ([fs flags] [ds later] [acc '()])
                                          (if (null? fs)
                                              (reverse acc)
                                              (loop (cdr fs) (cdr ds)
                                                    (if (car fs)
                                                        (cons (car ds) acc)
                                                        acc))))])
                               (gen-pure (cons node deps))))))))
                 (iota n)))
          gen-pure)))))

;; Generate a random graph with guaranteed cycles.
;; Strategy: generate a DAG, then add a back-edge from last node to first.
(define (gen-cyclic-graph min-nodes max-nodes)
  (gen-map
    (lambda (dag)
      (if (< (length dag) 2)
          dag
          ;; Add edge from last node to first node (guaranteed back-edge)
          (let ([last-node (car (list-ref dag (- (length dag) 1)))]
                [first-node (caar dag)])
            (map (lambda (entry)
                   (if (eq? (car entry) last-node)
                       (cons last-node
                             (if (memq first-node (cdr entry))
                                 (cdr entry)
                                 (cons first-node (cdr entry))))
                       entry))
                 dag))))
    (gen-dag min-nodes max-nodes)))

;; Generate two lists of symbols (for deps-diff testing)
(define gen-two-dep-lists
  (gen-bind (gen-node-subset 0 8)
    (lambda (pool)
      (gen-bind (gen-list-of (length pool) gen-bool)
        (lambda (in-a)
          (gen-bind (gen-list-of (length pool) gen-bool)
            (lambda (in-b)
              (let ([pick (lambda (flags syms)
                            (let loop ([fs flags] [ss syms] [acc '()])
                              (if (null? fs)
                                  (reverse acc)
                                  (loop (cdr fs) (cdr ss)
                                        (if (car fs) (cons (car ss) acc) acc)))))])
                (gen-pure
                  (list (pick in-a pool)
                        (pick in-b pool)))))))))))

;; Helper: build manifests from a dep alist (empty modules, for cycle-breaking tests)
(define (dep-alist->manifests dep-alist)
  (map (lambda (entry)
         `((name . ,(car entry))
           (version . "1.0.0")
           (path . ,(string-append "test/" (symbol->string (car entry))))
           (purity . total)
           (stability . stable)
           (fuel-bound . "O(?)")
           (deps . ())
           (description . "test")
           (keywords . ())
           (aliases . ())
           (exports . ())
           (modules . ())
           (concepts . ())))
       dep-alist))

;;; ============================================================================
;;; Properties: break-dep-cycles
;;; ============================================================================

(test-group "break-dep-cycles-properties"

  (define-property "DAGs are unchanged by cycle-breaking"
    (gen-dag 2 8)
    (lambda (dag)
      (let-values ([(result report) (break-dep-cycles dag (dep-alist->manifests dag) '())])
        (null? report)))
    'tests 200)

  (define-property "cycle-breaking always produces acyclic graph"
    (gen-cyclic-graph 3 8)
    (lambda (graph)
      (let-values ([(result report) (break-dep-cycles graph (dep-alist->manifests graph) '())])
        (not (find-any-cycle result))))
    'tests 200)

  (define-property "cycle-breaking preserves all nodes"
    (gen-cyclic-graph 3 8)
    (lambda (graph)
      (let ([original-nodes (map car graph)])
        (let-values ([(result report) (break-dep-cycles graph (dep-alist->manifests graph) '())])
          (let ([result-nodes (map car result)])
            (andmap (lambda (n) (pair? (memq n result-nodes))) original-nodes)))))
    'tests 200)

  (define-property "cycle-breaking only removes edges, never adds"
    (gen-cyclic-graph 3 8)
    (lambda (graph)
      (let-values ([(result report) (break-dep-cycles graph (dep-alist->manifests graph) '())])
        (andmap
          (lambda (entry)
            (let* ([node (car entry)]
                   [result-deps (cdr entry)]
                   [orig-entry (assq node graph)]
                   [orig-deps (if orig-entry (cdr orig-entry) '())])
              (andmap (lambda (d) (pair? (memq d orig-deps))) result-deps)))
          result)))
    'tests 200)
)

;;; ============================================================================
;;; Properties: deps-diff
;;; ============================================================================

(test-group "deps-diff-properties"

  (define-property "diff(A,B).added = diff(B,A).removed (symmetry)"
    gen-two-dep-lists
    (lambda (pair)
      (let* ([a (car pair)]
             [b (cadr pair)]
             [diff-ab (deps-diff a b)]
             [diff-ba (deps-diff b a)]
             [ab-added (cdr (assq 'added diff-ab))]
             [ba-removed (cdr (assq 'removed diff-ba))])
        ;; Same elements, order may differ
        (and (= (length ab-added) (length ba-removed))
             (andmap (lambda (x) (pair? (memq x ba-removed))) ab-added)
             (andmap (lambda (x) (pair? (memq x ab-added))) ba-removed))))
    'tests 300)

  (define-property "diff(A,A) has no added or removed"
    (gen-node-subset 0 8)
    (lambda (a)
      (let ([diff (deps-diff a a)])
        (and (null? (cdr (assq 'added diff)))
             (null? (cdr (assq 'removed diff))))))
    'tests 200)

  (define-property "added + same covers all of derived"
    gen-two-dep-lists
    (lambda (pair)
      (let* ([derived (car pair)]
             [declared (cadr pair)]
             [diff (deps-diff derived declared)]
             [added (cdr (assq 'added diff))]
             [same (cdr (assq 'same diff))]
             [union (append added same)])
        (andmap (lambda (x) (pair? (memq x union))) derived)))
    'tests 300)

  (define-property "removed + same covers all of declared"
    gen-two-dep-lists
    (lambda (pair)
      (let* ([derived (car pair)]
             [declared (cadr pair)]
             [diff (deps-diff derived declared)]
             [removed (cdr (assq 'removed diff))]
             [same (cdr (assq 'same diff))]
             [union (append removed same)])
        (andmap (lambda (x) (pair? (memq x union))) declared)))
    'tests 300)
)

;;; ============================================================================
;;; Properties: find-cycle-dfs
;;; ============================================================================

(test-group "find-cycle-dfs-properties"

  (define-property "DAGs have no cycles reachable from any node"
    (gen-dag 2 8)
    (lambda (dag)
      (andmap (lambda (entry) (not (find-cycle-dfs (car entry) dag))) dag))
    'tests 200)

  (define-property "detected cycles start and end with same node"
    (gen-cyclic-graph 3 8)
    (lambda (graph)
      (let ([cycle (find-any-cycle graph)])
        (if cycle
            (eq? (car cycle) (car (reverse cycle)))
            #t)))  ; vacuously true if no cycle reachable
    'tests 200)
)

;;; ============================================================================
;;; Properties: remove-dep-edge
;;; ============================================================================

(test-group "remove-dep-edge-properties"

  (define-property "removing an edge reduces total edges by at most 1"
    (gen-dag 2 8)
    (lambda (dag)
      (let ([with-deps (filter (lambda (e) (pair? (cdr e))) dag)])
        (if (null? with-deps)
            #t
            (let* ([entry (car with-deps)]
                   [from (car entry)]
                   [to (cadr entry)]
                   [count-edges (lambda (g) (apply + (map (lambda (e) (length (cdr e))) g)))]
                   [original (count-edges dag)]
                   [after (count-edges (remove-dep-edge dag from to))])
              (<= (- original after) 1)))))
    'tests 200)
)

;;; ====
;;; Run
;;; ====

(run-all-tests)
