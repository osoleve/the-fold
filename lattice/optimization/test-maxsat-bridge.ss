(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/optimization/maxsat-bridge.ss")

;;; ============================================================
;;; Variable Domain
;;; ============================================================

(test-group "wco-domain"

  (define-test "create empty domain"
    (let ([d (make-wco-domain)])
      (assert-true (wco-domain? d))
      (assert-equal 0 (wco-var-count d))))

  (define-test "declare single variable"
    (let ([d (wco-declare (make-wco-domain) 'x)])
      (assert-equal 1 (wco-var-count d))
      (assert-true (lit-positive? (wco-var d 'x)))))

  (define-test "declare multiple variables"
    (let ([d (wco-declare* (make-wco-domain) '(x y z))])
      (assert-equal 3 (wco-var-count d))
      ;; Each variable gets a distinct literal
      (assert-true (not (= (lit-var (wco-var d 'x))
                           (lit-var (wco-var d 'y)))))))

  (define-test "negated literal"
    (let ([d (wco-declare (make-wco-domain) 'x)])
      (assert-true (lit-negative? (wco-neg-var d 'x)))
      (assert-equal (lit-var (wco-var d 'x))
                    (lit-var (wco-neg-var d 'x)))))

  (define-test "duplicate declaration errors"
    (let ([d (wco-declare (make-wco-domain) 'x)])
      (assert-error (lambda () (wco-declare d 'x)))))

  (define-test "unknown variable errors"
    (let ([d (make-wco-domain)])
      (assert-error (lambda () (wco-var d 'x))))))

;;; ============================================================
;;; Constraint Compilation
;;; ============================================================

(test-group "wco-constraints"

  (define-test "implies produces clause"
    (let ([d (wco-declare* (make-wco-domain) '(a b))])
      (let ([clause (wco-implies d 'a 'b)])
        ;; (~a OR b) = 2 literals
        (assert-equal 2 (length clause)))))

  (define-test "iff produces two clauses"
    (let ([d (wco-declare* (make-wco-domain) '(a b))])
      (let ([clauses (wco-iff d 'a 'b)])
        (assert-equal 2 (length clauses)))))

  (define-test "mutex produces pairwise clauses"
    (let ([d (wco-declare* (make-wco-domain) '(a b c))])
      (let ([clauses (wco-mutex d '(a b c))])
        ;; C(3,2) = 3 pairwise exclusion clauses
        (assert-equal 3 (length clauses)))))

  (define-test "exactly-one produces n+1 clauses for n=3"
    (let ([d (wco-declare* (make-wco-domain) '(a b c))])
      (let ([clauses (wco-exactly-one d '(a b c))])
        ;; 1 at-least-one clause + 3 at-most-one clauses = 4
        (assert-equal 4 (length clauses))))))

;;; ============================================================
;;; Problem Building
;;; ============================================================

(test-group "wco-problem-building"

  (define-test "empty problem"
    (let* ([d (wco-declare* (make-wco-domain) '(x y))]
           [p (make-wco-problem d)])
      (assert-true (wco-problem? p))
      (assert-equal 0 (wco-num-hard p))
      (assert-equal 0 (wco-num-soft p))))

  (define-test "add hard constraints"
    (let* ([d (wco-declare* (make-wco-domain) '(x y))]
           [p (make-wco-problem d)]
           [p (wco-require p (list (list (wco-var d 'x) (wco-var d 'y))))])
      (assert-equal 1 (wco-num-hard p))))

  (define-test "add weighted soft constraints"
    (let* ([d (wco-declare* (make-wco-domain) '(x y))]
           [p (make-wco-problem d)]
           [p (wco-prefer p 10 (list (list (wco-var d 'x))))]
           [p (wco-prefer p 5 (list (list (wco-var d 'y))))])
      (assert-equal 2 (wco-num-soft p))))

  (define-test "prefer-true shortcut"
    (let* ([d (wco-declare* (make-wco-domain) '(x))]
           [p (make-wco-problem d)]
           [p (wco-prefer-true p 10 'x)])
      (assert-equal 1 (wco-num-soft p)))))

;;; ============================================================
;;; Conversion to MaxSAT
;;; ============================================================

(test-group "wco-conversion"

  (define-test "convert to unweighted maxsat"
    (let* ([d (wco-declare* (make-wco-domain) '(x y))]
           [p (make-wco-problem d)]
           [p (wco-require p (list (list (wco-var d 'x) (wco-var d 'y))))]
           [p (wco-prefer p 10 (list (list (wco-var d 'x))))]
           [ms (wco->maxsat p)])
      (assert-true (maxsat? ms))
      (assert-equal 1 (length (maxsat-hard ms)))
      (assert-equal 1 (length (maxsat-soft ms))))))

;;; ============================================================
;;; Solving: No Soft Constraints
;;; ============================================================

(test-group "wco-solve-hard-only"

  (define-test "satisfiable hard constraints"
    (let* ([d (wco-declare* (make-wco-domain) '(x y))]
           [p (make-wco-problem d)]
           ;; x OR y must be true
           [p (wco-require p (list (list (wco-var d 'x) (wco-var d 'y))))]
           [result (wco-solve p)])
      (assert-true (wco-result? result))
      (assert-equal 0 (wco-result-cost result))))

  (define-test "unsatisfiable hard constraints"
    (let* ([d (wco-declare (make-wco-domain) 'x)]
           [p (make-wco-problem d)]
           ;; x AND ~x (contradiction)
           [p (wco-require p (list (list (wco-var d 'x))
                                   (list (wco-neg-var d 'x))))]
           [result (wco-solve p)])
      (assert-true (not result)))))

;;; ============================================================
;;; Solving: Unweighted (Single Tier)
;;; ============================================================

(test-group "wco-solve-unweighted"

  (define-test "all soft clauses satisfiable"
    (let* ([d (wco-declare* (make-wco-domain) '(x y))]
           [p (make-wco-problem d)]
           [p (wco-prefer p 1 (list (list (wco-var d 'x))))]
           [p (wco-prefer p 1 (list (list (wco-var d 'y))))]
           [result (wco-solve p)])
      (assert-true (wco-result? result))
      (assert-equal 0 (wco-result-cost result))))

  (define-test "some soft clauses unsatisfiable"
    (let* ([d (wco-declare (make-wco-domain) 'x)]
           [p (make-wco-problem d)]
           ;; Want both x and ~x — can't have both
           [p (wco-prefer p 1 (list (list (wco-var d 'x))))]
           [p (wco-prefer p 1 (list (list (wco-neg-var d 'x))))]
           [result (wco-solve p)])
      (assert-true (wco-result? result))
      ;; Cost = 1 * 1 = 1 (one unsatisfied clause at weight 1)
      (assert-equal 1 (wco-result-cost result)))))

;;; ============================================================
;;; Solving: Weighted (Multiple Tiers)
;;; ============================================================

(test-group "wco-solve-weighted"

  (define-test "higher weight tier satisfied first"
    ;; x and y conflict (at most one). Prefer x at weight 10, y at weight 1.
    ;; Optimal: x=true, y=false. Cost = 1*1 = 1.
    (let* ([d (wco-declare* (make-wco-domain) '(x y))]
           [p (make-wco-problem d)]
           ;; Hard: at most one of x, y
           [p (wco-require p (wco-mutex d '(x y)))]
           ;; Soft: prefer x (high weight) and y (low weight)
           [p (wco-prefer p 10 (list (list (wco-var d 'x))))]
           [p (wco-prefer p 1 (list (list (wco-var d 'y))))]
           [result (wco-solve p)])
      (assert-true (wco-result? result))
      ;; x should be true (weight 10 tier satisfied)
      (assert-true (wco-result-value result 'x))
      ;; y should be false (weight 1 tier sacrificed)
      (assert-true (not (wco-result-value result 'y)))
      ;; Cost = 1 * 1 = 1 (one weight-1 clause unsatisfied)
      (assert-equal 1 (wco-result-cost result))))

  (define-test "equal weights behave as single tier"
    ;; Two conflicting preferences at same weight
    (let* ([d (wco-declare (make-wco-domain) 'x)]
           [p (make-wco-problem d)]
           [p (wco-prefer p 5 (list (list (wco-var d 'x))))]
           [p (wco-prefer p 5 (list (list (wco-neg-var d 'x))))]
           [result (wco-solve p)])
      (assert-true (wco-result? result))
      ;; Cost = 5 * 1 = 5 (one clause unsatisfied at weight 5)
      (assert-equal 5 (wco-result-cost result))))

  (define-test "three weight tiers"
    ;; Three variables, at-most-1 hard constraint
    ;; Prefer: x (weight 100), y (weight 10), z (weight 1)
    ;; Should satisfy x, sacrifice y and z
    (let* ([d (wco-declare* (make-wco-domain) '(x y z))]
           [p (make-wco-problem d)]
           [p (wco-require p (wco-mutex d '(x y z)))]
           [p (wco-prefer p 100 (list (list (wco-var d 'x))))]
           [p (wco-prefer p 10 (list (list (wco-var d 'y))))]
           [p (wco-prefer p 1 (list (list (wco-var d 'z))))]
           [result (wco-solve p)])
      (assert-true (wco-result? result))
      (assert-true (wco-result-value result 'x))
      ;; Cost = 10*1 + 1*1 = 11
      (assert-equal 11 (wco-result-cost result)))))

;;; ============================================================
;;; Solution Extraction
;;; ============================================================

(test-group "wco-solution-extraction"

  (define-test "assignment returns all variables"
    (let* ([d (wco-declare* (make-wco-domain) '(a b c))]
           [p (make-wco-problem d)]
           [p (wco-prefer p 1 (list (list (wco-var d 'a))))]
           [p (wco-prefer p 1 (list (list (wco-var d 'b))))]
           [p (wco-prefer p 1 (list (list (wco-neg-var d 'c))))]
           [result (wco-solve p)]
           [assignment (wco-result-assignment result)])
      ;; All 3 variables present
      (assert-equal 3 (length assignment))
      ;; a and b should be true
      (assert-true (wco-result-value result 'a))
      (assert-true (wco-result-value result 'b))
      ;; c should be false
      (assert-true (not (wco-result-value result 'c)))))

  (define-test "unknown variable in result errors"
    (let* ([d (wco-declare (make-wco-domain) 'x)]
           [p (make-wco-problem d)]
           [p (wco-prefer-true p 1 'x)]
           [result (wco-solve p)])
      (assert-error (lambda () (wco-result-value result 'y))))))

;;; ============================================================
;;; Group By Weight
;;; ============================================================

(test-group "group-by-weight"

  (define-test "empty list"
    (assert-equal '() (group-by-weight '())))

  (define-test "single entry"
    (let ([groups (group-by-weight '((5 . (1 2))))])
      (assert-equal 1 (length groups))
      (assert-equal 5 (caar groups))))

  (define-test "two tiers"
    (let ([groups (group-by-weight '((10 . (1)) (5 . (2)) (10 . (3))))])
      ;; Two groups: weight 10 (2 clauses), weight 5 (1 clause)
      (assert-equal 2 (length groups))
      ;; First group has weight 10 (highest)
      (assert-equal 10 (caar groups))
      ;; Second group has weight 5
      (assert-equal 5 (caadr groups)))))

;;; ============================================================
;;; Applications: Weighted Vertex Cover
;;; ============================================================

(test-group "weighted-vertex-cover"

  (define-test "triangle graph with uniform weights"
    ;; Triangle: a-b, b-c, a-c. Must cover all edges.
    ;; Minimum cover = 2 nodes.
    (let* ([edges '((a . b) (b . c) (a . c))]
           [weights '((a . 1) (b . 1) (c . 1))]
           [problem (weighted-vertex-cover edges weights)]
           [result (wco-solve problem)])
      (assert-true (wco-result? result))
      ;; Cost = number of nodes IN cover (each has weight 1 for "not include")
      ;; Actually: cost counts unsatisfied "prefer not include" clauses.
      ;; If 2 nodes are in cover → 2 unsatisfied → cost = 2
      ;; Minimum vertex cover of triangle is 2 nodes.
      (assert-equal 2 (wco-result-cost result))))

  (define-test "path graph with different weights"
    ;; Path: a-b-c. Cover needs ≥1 endpoint per edge.
    ;; Weights: a=10, b=1, c=10. Cheapest cover: {b} (covers both edges).
    (let* ([edges '((a . b) (b . c))]
           [weights '((a . 10) (b . 1) (c . 10))]
           [problem (weighted-vertex-cover edges weights)]
           [result (wco-solve problem)])
      (assert-true (wco-result? result))
      ;; b should be in cover (cheapest)
      (assert-true (wco-result-value result 'b))
      ;; Cost = 1 (only b's "prefer not include" is unsatisfied)
      (assert-equal 1 (wco-result-cost result)))))

;;; ============================================================
;;; Applications: Scheduling
;;; ============================================================

(test-group "scheduling"

  (define-test "two tasks two slots no conflict"
    (let* ([problem (scheduling-problem
                     '(task1 task2)
                     '(morning afternoon)
                     '()  ; no conflicts
                     '((task1 morning 10)
                       (task2 afternoon 10)))]
           [result (wco-solve problem)])
      (assert-true (wco-result? result))
      ;; Both preferences satisfied
      (assert-equal 0 (wco-result-cost result))))

  (define-test "conflicting tasks with preferences"
    ;; Two tasks, both prefer morning, but they conflict
    (let* ([problem (scheduling-problem
                     '(a b)
                     '(morning afternoon)
                     '((a . b))  ; a and b conflict
                     '((a morning 10)
                       (b morning 5)))]
           [result (wco-solve problem)])
      (assert-true (wco-result? result))
      ;; Task a gets morning (higher weight preference)
      (assert-true (wco-result-value result 'a:morning))
      ;; Task b gets afternoon (lower weight preference sacrificed)
      ;; Cost = 5 (b's morning preference unsatisfied)
      (assert-equal 5 (wco-result-cost result)))))

(run-all-tests)
