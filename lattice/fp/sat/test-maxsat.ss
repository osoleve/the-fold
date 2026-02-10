(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/fp/sat/maxsat.ss")

(doc 'module 'test-maxsat)
(doc 'description "Tests for MaxSAT solver")

(test-group "maxsat-basic"
  (define-test "all soft satisfiable has cost 0"
    ;; x1, x2 - both can be satisfied
    (let* ([m (make-maxsat '() '((1) (2)))]
           [result (maxsat-solve m)])
      (assert-true (maxsat-result? result))
      (assert-equal 0 (maxsat-result-cost result))))

  (define-test "contradictory soft has cost 1"
    ;; x1 AND ~x1 - one must be unsatisfied
    (let* ([m (make-maxsat '() '((1) (-1)))]
           [result (maxsat-solve m)])
      (assert-true (maxsat-result? result))
      (assert-equal 1 (maxsat-result-cost result))))

  (define-test "hard constraint satisfied"
    ;; Hard: x1, Soft: x2, ~x2
    ;; Best: x1=T, one of x2/~x2 unsatisfied, cost=1
    (let* ([m (make-maxsat '((1)) '((2) (-2)))]
           [result (maxsat-solve m)])
      (assert-true (maxsat-result? result))
      (assert-equal 1 (maxsat-result-cost result))
      ;; Check x1 is true in model
      (let ([model (maxsat-result-model result)])
        (assert-true (cdr (assoc 1 model))))))

  (define-test "unsatisfiable hard returns #f"
    ;; Hard: x1 AND ~x1 - impossible
    (let* ([m (make-maxsat '((1) (-1)) '((2)))]
           [result (maxsat-solve m)])
      (assert-false result)))

  (define-test "empty soft has cost 0"
    (let* ([m (make-maxsat '((1)) '())]
           [result (maxsat-solve m)])
      (assert-true (maxsat-result? result))
      (assert-equal 0 (maxsat-result-cost result)))))

(test-group "maxsat-algorithms"
  (define-test "linear and binary give same result"
    ;; More complex problem
    (let* ([m (make-maxsat
               '((1 2) (-1 3))  ; Hard: (x1 OR x2) AND (~x1 OR x3)
               '((1) (-2) (3) (-3)))]  ; Soft: prefer x1, ~x2, x3, ~x3
           [linear (maxsat-solve m 'linear)]
           [binary (maxsat-solve m)])
      (assert-equal (maxsat-result-cost linear)
                    (maxsat-result-cost binary)))))

(test-group "vertex-cover"
  (define-test "triangle vertex cover needs 2 nodes"
    ;; Triangle: 0-1, 1-2, 0-2
    ;; Minimum vertex cover has 2 nodes
    (let* ([edges '((0 . 1) (1 . 2) (0 . 2))]
           [m (min-vertex-cover edges 3)]
           [result (maxsat-solve m)])
      (assert-true (maxsat-result? result))
      ;; Soft clauses = "prefer NOT including node i" (negative literals)
      ;; Cost = number of nodes IN cover (violating the soft preference)
      ;; Min cover size = 2, so cost = 2
      (assert-equal 2 (maxsat-result-cost result))))

  (define-test "path vertex cover is alternating"
    ;; Path: 0-1-2-3
    ;; Minimum vertex cover: {1, 3} or {0, 2} - size 2
    (let* ([edges '((0 . 1) (1 . 2) (2 . 3))]
           [m (min-vertex-cover edges 4)]
           [result (maxsat-solve m)])
      (assert-true (maxsat-result? result))
      ;; Cost = cover size = 2
      (assert-equal 2 (maxsat-result-cost result))))

  (define-test "star vertex cover is center"
    ;; Star: 0 connected to 1,2,3
    ;; Minimum vertex cover: just {0}
    (let* ([edges '((0 . 1) (0 . 2) (0 . 3))]
           [m (min-vertex-cover edges 4)]
           [result (maxsat-solve m)])
      (assert-true (maxsat-result? result))
      ;; Cost = cover size = 1
      (assert-equal 1 (maxsat-result-cost result)))))

(test-group "independent-set"
  (define-test "triangle independent set has size 1"
    ;; Triangle: 0-1, 1-2, 0-2
    ;; Max independent set: any single node
    (let* ([edges '((0 . 1) (1 . 2) (0 . 2))]
           [m (max-independent-set edges 3)]
           [result (maxsat-solve m)])
      (assert-true (maxsat-result? result))
      ;; Soft clauses = "prefer INCLUDING node i" (positive literals)
      ;; Cost = nodes NOT in set (violating preference)
      ;; Max set size = 1, cost = 3 - 1 = 2
      (assert-equal 2 (maxsat-result-cost result))))

  (define-test "path independent set is alternating"
    ;; Path: 0-1-2-3
    ;; Max independent set: {0, 2} or {1, 3} - size 2
    (let* ([edges '((0 . 1) (1 . 2) (2 . 3))]
           [m (max-independent-set edges 4)]
           [result (maxsat-solve m)])
      (assert-true (maxsat-result? result))
      ;; Cost = nodes not in set = 4 - 2 = 2
      (assert-equal 2 (maxsat-result-cost result))))

  (define-test "bipartite has large independent set"
    ;; Complete bipartite K_{2,3}: {0,1} fully connected to {2,3,4}
    ;; Max independent set: either {0,1} or {2,3,4}
    (let* ([edges '((0 . 2) (0 . 3) (0 . 4)
                    (1 . 2) (1 . 3) (1 . 4))]
           [m (max-independent-set edges 5)]
           [result (maxsat-solve m)])
      (assert-true (maxsat-result? result))
      ;; Max independent set size = 3, cost = 5 - 3 = 2
      (assert-equal 2 (maxsat-result-cost result)))))

(test-group "diagnosis"
  (define-test "find minimal correction set"
    ;; Inconsistent: x1, ~x1, x2
    ;; Remove either x1 or ~x1 to fix
    (let* ([clauses '((1) (-1) (2))]
           [m (min-correction-set clauses)]
           [result (maxsat-solve m)])
      (assert-true (maxsat-result? result))
      ;; Cost = 1 (one clause removed)
      (assert-equal 1 (maxsat-result-cost result))))

  (define-test "satisfiable formula needs no correction"
    (let* ([clauses '((1 2) (-1 2) (1 -2))]
           [m (min-correction-set clauses)]
           [result (maxsat-solve m)])
      (assert-true (maxsat-result? result))
      (assert-equal 0 (maxsat-result-cost result))))

  (define-test "complex inconsistency"
    ;; x1 AND x2 AND (~x1 OR ~x2) AND ~x1 AND ~x2
    ;; Need to remove at least 2 clauses
    (let* ([clauses '((1) (2) (-1 -2) (-1) (-2))]
           [m (min-correction-set clauses)]
           [result (maxsat-solve m)])
      (assert-true (maxsat-result? result))
      ;; Various minimal corrections exist, but need at least 2
      (assert-true (>= (maxsat-result-cost result) 2)))))

(test-group "partial-maxsat"
  (define-test "partial maxsat finds max satisfiable subset"
    ;; x1, x2, x3, ~x1, ~x2
    ;; Can satisfy at most 3: e.g., x3, ~x1, ~x2 (or x1, x2, x3)
    (let* ([clauses '((1) (2) (3) (-1) (-2))]
           [m (partial-maxsat clauses)]
           [result (maxsat-solve m)])
      (assert-true (maxsat-result? result))
      ;; Cost = 2 (at least 2 unsatisfied)
      (assert-equal 2 (maxsat-result-cost result)))))

(run-all-tests)
