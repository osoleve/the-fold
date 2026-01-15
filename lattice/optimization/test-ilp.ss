;;; lattice/optimization/test-ilp.ss --- Tests for Integer Linear Programming
;;;
;;; Tests ILP solver with classic optimization problems.

(load "core/testing/test-framework.ss")
(load "lattice/optimization/ilp.ss")

;;; ============================================================
;;; Basic ILP Tests
;;; ============================================================

(test-group 'ilp-basic

  (define-test "LP relaxation returns non-integer, ILP returns integer"
    ;; Minimize x + y
    ;; s.t. 2x + 3y >= 7
    ;;      x, y >= 0, integer
    ;;
    ;; LP relaxation: x=0, y=7/3 ≈ 2.33 (z=2.33)
    ;; ILP optimal: x=2, y=1 (z=3) or x=0, y=3 (z=3)
    (let* ([c (vector 1 1 0 1000000)]  ; Big-M for artificial
           [A (matrix-from-lists '((2 3 -1 1)))]  ; 2x + 3y - s + a = 7
           [b (vector 7)]
           [ilp (make-ilp c A b '(0 1))]
           [result (ilp-solve ilp)])
      (assert-true (ilp-optimal? result))
      (let ([x (ilp-result-x result)])
        ;; Solution should be integer
        (assert-true (is-integer? (vector-ref x 0)))
        (assert-true (is-integer? (vector-ref x 1)))
        ;; Objective should be 3 (either (2,1) or (0,3))
        (assert-true (<= (ilp-result-z result) 3.001)))))

  (define-test "simple integer program"
    ;; Minimize -x - y (maximize x + y)
    ;; s.t. x + y <= 5
    ;;      x, y >= 0, integer
    ;;
    ;; Optimal: x=5, y=0 or x=0, y=5 etc. (any with x+y=5)
    (let* ([c (vector -1 -1 0)]  ; minimize -x - y
           [A (matrix-from-lists '((1 1 1)))]  ; x + y + s = 5
           [b (vector 5)]
           [ilp (make-ilp c A b '(0 1))]
           [result (ilp-solve ilp)])
      (assert-true (ilp-optimal? result))
      (let* ([x (ilp-result-x result)]
             [x0 (vector-ref x 0)]
             [x1 (vector-ref x 1)])
        (assert-true (is-integer? x0))
        (assert-true (is-integer? x1))
        ;; x + y should be 5
        (assert-equal 5 (+ (round x0) (round x1))))))

  (define-test "infeasible ILP"
    ;; x >= 3 and x <= 2, x integer
    ;; Infeasible
    (let* ([c (vector 1 0 0)]
           [A (matrix-from-lists '((1 -1 0)    ; x - s1 = 3 (x >= 3)
                                   (1 0 1)))]  ; x + s2 = 2 (x <= 2)
           [b (vector 3 2)]
           [ilp (make-ilp c A b '(0))]
           [result (ilp-solve ilp)])
      (assert-true (ilp-infeasible? result))))

  (define-test "already integer LP solution"
    ;; When LP relaxation is already integer, should return immediately
    (let* ([c (vector 1 1)]
           [A (matrix-from-lists '((1 0)
                                   (0 1)))]
           [b (vector 3 4)]
           [ilp (make-ilp c A b '(0 1))]
           [result (ilp-solve ilp)])
      (assert-true (ilp-optimal? result))
      (let ([x (ilp-result-x result)])
        (assert-equal 3 (round (vector-ref x 0)))
        (assert-equal 4 (round (vector-ref x 1)))))))

;;; ============================================================
;;; Knapsack Tests
;;; ============================================================

(test-group 'knapsack

  (define-test "simple knapsack"
    ;; Items: values=[60, 100, 120], weights=[10, 20, 30], capacity=50
    ;; Optimal: select items 1 and 2 (0-indexed: 1,2) for value 220
    (let* ([values (vector 60 100 120)]
           [weights (vector 10 20 30)]
           [capacity 50]
           [result (knapsack-solve values weights capacity)])
      (assert-false (eq? result 'infeasible))
      (let ([x (car result)]
            [val (cdr result)])
        ;; Should select items with combined value 220
        ;; Items 1,2 (indices 1,2): 100 + 120 = 220, weight = 20 + 30 = 50
        (assert-equal 220 val))))

  (define-test "knapsack with single item"
    ;; Single item that fits
    (let* ([values (vector 100)]
           [weights (vector 5)]
           [capacity 10]
           [result (knapsack-solve values weights capacity)])
      (assert-false (eq? result 'infeasible))
      (assert-equal 100 (cdr result))))

  (define-test "knapsack with no feasible solution"
    ;; All items too heavy
    (let* ([values (vector 100 200)]
           [weights (vector 20 30)]
           [capacity 10]
           [result (knapsack-solve values weights capacity)])
      ;; Should still be feasible (select nothing)
      (assert-false (eq? result 'infeasible))
      (assert-equal 0 (cdr result))))

  (define-test "knapsack greedy vs optimal"
    ;; Case where greedy (best value/weight ratio) is suboptimal
    ;; Items: v=[6, 10, 12], w=[1, 2, 3], cap=5
    ;; Greedy picks item 0 (ratio 6), then item 1 (ratio 5), val=16
    ;; Optimal picks items 1,2: val=22
    (let* ([values (vector 6 10 12)]
           [weights (vector 1 2 3)]
           [capacity 5]
           [result (knapsack-solve values weights capacity)])
      (assert-false (eq? result 'infeasible))
      (assert-equal 22 (cdr result)))))

;;; ============================================================
;;; Set Cover Tests
;;; ============================================================

(test-group 'set-cover

  (define-test "simple set cover"
    ;; Universe: {0, 1, 2}
    ;; Sets: S0 = {0, 1}, S1 = {1, 2}, S2 = {0, 2}
    ;; Costs: [1, 1, 1]
    ;; Any two sets cover all elements
    (let* ([coverage (matrix-from-lists '((1 0 1)    ; element 0: S0, S2
                                          (1 1 0)    ; element 1: S0, S1
                                          (0 1 1)))] ; element 2: S1, S2
           [costs (vector 1 1 1)]
           [result (set-cover-solve coverage costs)])
      (assert-false (eq? result 'infeasible))
      ;; Should select 2 sets
      (assert-equal 2 (length result))))

  (define-test "set cover with varying costs"
    ;; Universe: {0, 1}
    ;; Sets: S0 = {0}, S1 = {1}, S2 = {0, 1}
    ;; Costs: [1, 1, 3]
    ;; Optimal: select S0 and S1 (cost 2) not S2 (cost 3)
    (let* ([coverage (matrix-from-lists '((1 0 1)    ; element 0
                                          (0 1 1)))] ; element 1
           [costs (vector 1 1 3)]
           [result (set-cover-solve coverage costs)])
      (assert-false (eq? result 'infeasible))
      ;; Should not contain set 2 (too expensive)
      ;; Cost should be 2
      (let ([total-cost (apply + (map (lambda (i) (vector-ref costs i)) result))])
        (assert-equal 2 total-cost))))

  (define-test "set cover single set covers all"
    ;; Universe: {0, 1}
    ;; Sets: S0 = {0, 1}
    ;; Optimal: just S0
    (let* ([coverage (matrix-from-lists '((1)
                                          (1)))]
           [costs (vector 5)]
           [result (set-cover-solve coverage costs)])
      (assert-false (eq? result 'infeasible))
      (assert-equal '(0) result))))

;;; ============================================================
;;; Integrality Helper Tests
;;; ============================================================

(test-group 'integrality

  (define-test "is-integer? for integers"
    (assert-true (is-integer? 5))
    (assert-true (is-integer? 0))
    (assert-true (is-integer? -3)))

  (define-test "is-integer? for near-integers"
    (assert-true (is-integer? 4.9999999))
    (assert-true (is-integer? 5.0000001)))

  (define-test "is-integer? for non-integers"
    (assert-false (is-integer? 5.5))
    (assert-false (is-integer? 0.3))
    (assert-false (is-integer? 2.999)))

  (define-test "solution-is-integer?"
    (let ([x (vector 1 2 3.5 4)])
      (assert-true (solution-is-integer? x '(0 1 3)))
      (assert-false (solution-is-integer? x '(0 2)))))

  (define-test "find-fractional-var"
    (let ([x (vector 1 2.5 3 4.7)])
      (assert-equal 1 (find-fractional-var x '(0 1 2 3)))
      (assert-equal 3 (find-fractional-var x '(0 2 3)))
      (assert-equal #f (find-fractional-var x '(0 2))))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(display "\n=== Integer Linear Programming Tests ===\n\n")
(run-all-tests)
