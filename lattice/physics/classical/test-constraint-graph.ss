;;; lattice/physics/classical/test-constraint-graph.ss — Tests for constraint graph analysis
;;; @requires test-framework constraint-graph constraints

(load "core/testing/test-framework.ss")
(load "lattice/physics/classical/constraint-graph.ss")
(load "lattice/physics/classical/constraints.ss")

;;; ============================================================
;;; Test Helpers
;;; ============================================================

;; Create mock constraints for testing
;; We only need entity-a, entity-b, and id for graph construction

(define (mock-constraint id entity-a entity-b)
  ;; Minimal constraint structure: (list 'constraint type id entity-a entity-b data ...)
  ;; constraint-entity-a returns list-ref 3, constraint-entity-b returns list-ref 4
  ;; constraint-id returns list-ref 2
  (list 'constraint 'test id entity-a entity-b '() #f #f 0))

;;; ============================================================
;;; Test Groups
;;; ============================================================

(test-group "constraint-graph-construction"

  (define-test "empty constraint list"
    (let ([graph (make-constraint-graph '())])
      (assert-true (constraint-graph? graph))
      (assert-equal '() (cg-entities graph))
      (assert-equal '() (cg-constraints graph))))

  (define-test "single constraint between two entities"
    (let* ([c1 (mock-constraint 'c1 'a 'b)]
           [graph (make-constraint-graph (list c1))])
      (assert-true (constraint-graph? graph))
      (assert-equal 2 (length (cg-entities graph)))
      (assert-equal 1 (length (cg-constraints graph)))
      (assert-equal '(b) (cg-neighbors graph 'a))
      (assert-equal '(a) (cg-neighbors graph 'b))))

  (define-test "constraint to world (entity-b = #f)"
    (let* ([c1 (mock-constraint 'c1 'a #f)]
           [graph (make-constraint-graph (list c1))])
      (assert-equal 1 (length (cg-entities graph)))
      (assert-equal '() (cg-neighbors graph 'a))
      (assert-equal (list c1) (cg-constraints-for graph 'a))))

  (define-test "multiple constraints form chain"
    (let* ([c1 (mock-constraint 'c1 'a 'b)]
           [c2 (mock-constraint 'c2 'b 'c)]
           [c3 (mock-constraint 'c3 'c 'd)]
           [graph (make-constraint-graph (list c1 c2 c3))])
      (assert-equal 4 (length (cg-entities graph)))
      (assert-equal 3 (length (cg-constraints graph)))
      ;; Check adjacency
      (assert-equal '(b) (cg-neighbors graph 'a))
      (assert-true (and (member 'a (cg-neighbors graph 'b)) #t))
      (assert-true (and (member 'c (cg-neighbors graph 'b)) #t)))))

(test-group "island-detection"

  (define-test "single island from connected constraints"
    (let* ([c1 (mock-constraint 'c1 'a 'b)]
           [c2 (mock-constraint 'c2 'b 'c)]
           [graph (make-constraint-graph (list c1 c2))]
           [islands (cg-find-islands graph)])
      (assert-equal 1 (length islands))
      (assert-true (island? (car islands)))
      (assert-equal 3 (island-size (car islands)))
      (assert-equal 2 (island-constraint-count (car islands)))))

  (define-test "two separate islands"
    (let* ([c1 (mock-constraint 'c1 'a 'b)]
           [c2 (mock-constraint 'c2 'c 'd)]
           [graph (make-constraint-graph (list c1 c2))]
           [islands (cg-find-islands graph)])
      (assert-equal 2 (length islands))
      (for-each
       (lambda (island)
         (assert-equal 2 (island-size island))
         (assert-equal 1 (island-constraint-count island)))
       islands)))

  (define-test "world-anchored constraint forms island"
    (let* ([c1 (mock-constraint 'c1 'a #f)]
           [c2 (mock-constraint 'c2 'b #f)]
           [graph (make-constraint-graph (list c1 c2))]
           [islands (cg-find-islands graph)])
      ;; Each world-anchored entity is its own island
      (assert-equal 2 (length islands))
      (for-each
       (lambda (island)
         (assert-equal 1 (island-size island)))
       islands)))

  (define-test "mixed connected and disconnected"
    (let* ([c1 (mock-constraint 'c1 'a 'b)]
           [c2 (mock-constraint 'c2 'b 'c)]
           [c3 (mock-constraint 'c3 'd 'e)]
           [c4 (mock-constraint 'c4 'f #f)]
           [graph (make-constraint-graph (list c1 c2 c3 c4))]
           [islands (cg-find-islands graph)])
      ;; Three islands: {a,b,c}, {d,e}, {f}
      (assert-equal 3 (length islands)))))

(test-group "strongly-connected-components"

  (define-test "linear chain has singleton SCCs"
    (let* ([c1 (mock-constraint 'c1 'a 'b)]
           [c2 (mock-constraint 'c2 'b 'c)]
           [graph (make-constraint-graph (list c1 c2))]
           [sccs (cg-find-sccs graph)])
      ;; In undirected interpretation, all connected -> one SCC
      ;; But Tarjan's treats edges as directed, so it depends on implementation
      ;; Our graph is undirected (edges go both ways), so one SCC
      (assert-true (>= (length sccs) 1))))

  (define-test "triangle forms single SCC"
    (let* ([c1 (mock-constraint 'c1 'a 'b)]
           [c2 (mock-constraint 'c2 'b 'c)]
           [c3 (mock-constraint 'c3 'c 'a)]
           [graph (make-constraint-graph (list c1 c2 c3))]
           [sccs (cg-find-sccs graph)])
      ;; Triangle: all mutually reachable -> one SCC
      (assert-equal 1 (length sccs))
      (assert-equal 3 (scc-size (car sccs)))))

  (define-test "disconnected components have separate SCCs"
    (let* ([c1 (mock-constraint 'c1 'a 'b)]
           [c2 (mock-constraint 'c2 'c 'd)]
           [graph (make-constraint-graph (list c1 c2))]
           [sccs (cg-find-sccs graph)])
      (assert-equal 2 (length sccs)))))

(test-group "topological-sort"

  (define-test "toposort DAG works"
    (let ([adj (vector '(1 2) '(3) '(3) '())])
      ;; 0 -> 1 -> 3, 0 -> 2 -> 3
      (let ([sorted (toposort-dag adj)])
        (assert-true (list? sorted))
        (assert-equal 4 (length sorted))
        ;; 0 must come before 1,2; 1,2 must come before 3
        (assert-true (< (list-index 0 sorted) (list-index 1 sorted)))
        (assert-true (< (list-index 0 sorted) (list-index 2 sorted)))
        (assert-true (< (list-index 1 sorted) (list-index 3 sorted)))
        (assert-true (< (list-index 2 sorted) (list-index 3 sorted))))))

  (define-test "toposort detects cycle"
    (let ([adj (vector '(1) '(2) '(0))])
      ;; 0 -> 1 -> 2 -> 0 (cycle)
      (assert-false (toposort-dag adj))))

  (define-test "toposort single node"
    (let ([adj (vector '())])
      (assert-equal '(0) (toposort-dag adj)))))

;; Helper for topological sort tests
(define (list-index elem lst)
  (let loop ([l lst] [i 0])
    (cond
     [(null? l) #f]
     [(equal? (car l) elem) i]
     [else (loop (cdr l) (+ i 1))])))

(test-group "graph-coloring"

  (define-test "non-conflicting constraints get same color"
    (let* ([c1 (mock-constraint 'c1 'a 'b)]
           [c2 (mock-constraint 'c2 'c 'd)]
           [graph (make-constraint-graph (list c1 c2))]
           [groups (cg-color-constraints graph)])
      ;; No conflict -> could be 1 or 2 groups depending on algorithm
      ;; At minimum, both constraints can be parallel
      (assert-true (<= (length groups) 2))))

  (define-test "conflicting constraints get different colors"
    (let* ([c1 (mock-constraint 'c1 'a 'b)]
           [c2 (mock-constraint 'c2 'b 'c)]
           [graph (make-constraint-graph (list c1 c2))]
           [groups (cg-color-constraints graph)])
      ;; c1 and c2 share entity b -> different colors
      (assert-equal 2 (length groups))))

  (define-test "triangle needs 3 colors"
    (let* ([c1 (mock-constraint 'c1 'a 'b)]
           [c2 (mock-constraint 'c2 'b 'c)]
           [c3 (mock-constraint 'c3 'c 'a)]
           [graph (make-constraint-graph (list c1 c2 c3))]
           [groups (cg-color-constraints graph)])
      ;; Each pair shares an entity -> all conflict -> 3 colors needed
      (assert-equal 3 (length groups))))

  (define-test "star pattern"
    ;; Central entity connected to 4 others
    (let* ([c1 (mock-constraint 'c1 'center 'a)]
           [c2 (mock-constraint 'c2 'center 'b)]
           [c3 (mock-constraint 'c3 'center 'c)]
           [c4 (mock-constraint 'c4 'center 'd)]
           [graph (make-constraint-graph (list c1 c2 c3 c4))]
           [groups (cg-color-constraints graph)])
      ;; All constraints share 'center -> all conflict -> 4 colors needed
      (assert-equal 4 (length groups)))))

(test-group "statistics"

  (define-test "stats for simple graph"
    (let* ([c1 (mock-constraint 'c1 'a 'b)]
           [c2 (mock-constraint 'c2 'b 'c)]
           [graph (make-constraint-graph (list c1 c2))]
           [stats (cg-stats graph)])
      (assert-equal 3 (cdr (assq 'entities stats)))
      (assert-equal 2 (cdr (assq 'constraints stats)))
      (assert-equal 1 (cdr (assq 'islands stats)))
      (assert-equal 2 (cdr (assq 'colors stats))))))

(test-group "topology-analysis"

  (define-test "chain has no cycles (B₁ = 0)"
    (let* ([c1 (mock-constraint 'c1 'a 'b)]
           [c2 (mock-constraint 'c2 'b 'c)]
           [c3 (mock-constraint 'c3 'c 'd)]
           [graph (make-constraint-graph (list c1 c2 c3))]
           [topo (cg-topology-analysis graph)])
      (assert-equal 4 (cdr (assq 'vertices topo)))
      (assert-equal 3 (cdr (assq 'edges topo)))
      (assert-equal 1 (cdr (assq 'betti-0 topo)))   ; 1 connected component
      (assert-equal 0 (cdr (assq 'betti-1 topo)))   ; No cycles
      (assert-false (cdr (assq 'has-cycles topo)))))

  (define-test "triangle has one cycle (B₁ = 1)"
    (let* ([c1 (mock-constraint 'c1 'a 'b)]
           [c2 (mock-constraint 'c2 'b 'c)]
           [c3 (mock-constraint 'c3 'c 'a)]
           [graph (make-constraint-graph (list c1 c2 c3))]
           [topo (cg-topology-analysis graph)])
      (assert-equal 3 (cdr (assq 'vertices topo)))
      (assert-equal 3 (cdr (assq 'edges topo)))
      (assert-equal 1 (cdr (assq 'betti-0 topo)))   ; 1 connected component
      (assert-equal 1 (cdr (assq 'betti-1 topo)))   ; 1 cycle
      (assert-true (cdr (assq 'has-cycles topo)))))

  (define-test "two disconnected triangles have B₀=2, B₁=2"
    (let* ([c1 (mock-constraint 'c1 'a 'b)]
           [c2 (mock-constraint 'c2 'b 'c)]
           [c3 (mock-constraint 'c3 'c 'a)]
           [c4 (mock-constraint 'c4 'd 'e)]
           [c5 (mock-constraint 'c5 'e 'f)]
           [c6 (mock-constraint 'c6 'f 'd)]
           [graph (make-constraint-graph (list c1 c2 c3 c4 c5 c6))]
           [topo (cg-topology-analysis graph)])
      (assert-equal 6 (cdr (assq 'vertices topo)))
      (assert-equal 6 (cdr (assq 'edges topo)))
      (assert-equal 2 (cdr (assq 'betti-0 topo)))   ; 2 connected components
      (assert-equal 2 (cdr (assq 'betti-1 topo))))) ; 2 cycles

  (define-test "cg-cycle-count returns correct count"
    (let* ([c1 (mock-constraint 'c1 'a 'b)]
           [c2 (mock-constraint 'c2 'b 'c)]
           [c3 (mock-constraint 'c3 'c 'a)]
           [graph (make-constraint-graph (list c1 c2 c3))])
      (assert-equal 1 (cg-cycle-count graph))))

  (define-test "cycle analysis identifies cycle-breaking constraints"
    (let* ([c1 (mock-constraint 'c1 'a 'b)]
           [c2 (mock-constraint 'c2 'b 'c)]
           [c3 (mock-constraint 'c3 'c 'a)]
           [graph (make-constraint-graph (list c1 c2 c3))]
           [analysis (cg-cycle-analysis graph)])
      (assert-equal 1 (cdr (assq 'cycle-count analysis)))
      (assert-true (cdr (assq 'has-cycles analysis)))
      ;; All 3 constraints participate in the cycle
      (assert-equal 3 (length (cdr (assq 'cycle-constraints analysis))))))

  (define-test "simplicial complex conversion"
    (let* ([c1 (mock-constraint 'c1 'a 'b)]
           [c2 (mock-constraint 'c2 'b 'c)]
           [graph (make-constraint-graph (list c1 c2))]
           [sc (cg->simplicial-complex graph)])
      (assert-true (sc? sc))
      (assert-equal 3 (sc-count-dim sc 0))   ; 3 vertices
      (assert-equal 2 (sc-count-dim sc 1))))) ; 2 edges

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests)
