;;; lattice/optimization/test-convergence.ss — Tests for Convergence Criteria
;;; Run with: scheme --script lattice/optimization/test-convergence.ss

(load "core/testing/test-framework.ss")
(load "lattice/optimization/convergence.ss")

;;; ============================================================
;;; Helpers
;;; ============================================================

(define (approx-equal? a b tol)
  (< (abs (- a b)) tol))

(define eps 1e-10)

;;; ============================================================
;;; Convergence Criteria Construction
;;; ============================================================

(test-group "convergence-criteria"
  (define-test "make-convergence-criteria"
    (let ([cc (make-convergence-criteria 1e-6 1e-8 1e-8 1000)])
      (assert-true (convergence-criteria? cc))))

  (define-test "accessors"
    (let ([cc (make-convergence-criteria 1e-3 1e-5 1e-7 500)])
      (assert-true (approx-equal? (cc-grad-tol cc) 1e-3 eps))
      (assert-true (approx-equal? (cc-f-tol cc) 1e-5 eps))
      (assert-true (approx-equal? (cc-x-tol cc) 1e-7 eps))
      (assert-equal 500 (cc-max-iter cc))))

  (define-test "default convergence exists"
    (assert-true (convergence-criteria? *default-convergence*))
    (assert-equal 1000 (cc-max-iter *default-convergence*)))

  (define-test "non-criteria fails predicate"
    (assert-false (convergence-criteria? '(not-a-criteria)))
    (assert-false (convergence-criteria? 42))
    (assert-false (convergence-criteria? '()))))

;;; ============================================================
;;; Convergence State
;;; ============================================================

(test-group "convergence-state"
  (define-test "make-convergence-state"
    (let ([cs (make-convergence-state 5 0.01 0.001 0.002 0.003)])
      (assert-true (convergence-state? cs))))

  (define-test "accessors"
    (let ([cs (make-convergence-state 10 3.14 0.5 0.1 0.2)])
      (assert-equal 10 (cs-iter cs))
      (assert-true (approx-equal? 3.14 (cs-f-val cs) eps))
      (assert-true (approx-equal? 0.5 (cs-grad-norm cs) eps))
      (assert-true (approx-equal? 0.1 (cs-f-change cs) eps))
      (assert-true (approx-equal? 0.2 (cs-x-change cs) eps))))

  (define-test "non-state fails predicate"
    (assert-false (convergence-state? '(not-a-state)))
    (assert-false (convergence-state? 42))))

;;; ============================================================
;;; Convergence Checking
;;; ============================================================

(test-group "converged?"
  (define cc (make-convergence-criteria 1e-6 1e-8 1e-8 100))

  (define-test "gradient converged"
    ;; grad-norm (1e-7) < grad-tol (1e-6)
    (let ([state (make-convergence-state 5 1.0 1e-7 0.5 0.5)])
      (assert-equal 'grad-converged (converged? cc state))))

  (define-test "f-value converged"
    ;; f-change (1e-9) < f-tol (1e-8), iter > 0
    (let ([state (make-convergence-state 5 1.0 0.01 1e-9 0.5)])
      (assert-equal 'f-converged (converged? cc state))))

  (define-test "x-value converged"
    ;; x-change (1e-9) < x-tol (1e-8), iter > 0, but f-change still large
    (let ([state (make-convergence-state 5 1.0 0.01 0.5 1e-9)])
      (assert-equal 'x-converged (converged? cc state))))

  (define-test "max iterations"
    ;; iter (100) >= max-iter (100), nothing else converged
    (let ([state (make-convergence-state 100 1.0 0.01 0.5 0.5)])
      (assert-equal 'max-iter (converged? cc state))))

  (define-test "not converged"
    ;; Everything above tolerance
    (let ([state (make-convergence-state 5 1.0 0.01 0.5 0.5)])
      (assert-false (converged? cc state))))

  (define-test "f-tol and x-tol require iter > 0"
    ;; At iter 0, even small f-change and x-change don't trigger
    (let ([state (make-convergence-state 0 1.0 0.01 1e-9 1e-9)])
      (assert-false (converged? cc state))))

  (define-test "grad convergence takes priority over f-convergence"
    ;; Both grad and f below tolerance — grad checked first
    (let ([state (make-convergence-state 5 1.0 1e-7 1e-9 0.5)])
      (assert-equal 'grad-converged (converged? cc state)))))

;;; ============================================================
;;; Vector Norms
;;; ============================================================

(test-group "vector-norms"
  (define-test "vec-norm of unit vectors"
    ;; ||(1,0,0)|| = 1
    (assert-true (approx-equal? 1.0 (vec-norm '(1 0 0)) 1e-10))
    ;; ||(0,1,0)|| = 1
    (assert-true (approx-equal? 1.0 (vec-norm '(0 1 0)) 1e-10)))

  (define-test "vec-norm of (3,4) is 5"
    (assert-true (approx-equal? 5.0 (vec-norm '(3 4)) 1e-10)))

  (define-test "vec-norm of empty is 0"
    (assert-equal 0 (vec-norm '())))

  (define-test "vec-norm of zeros is 0"
    (assert-equal 0 (vec-norm '(0 0 0))))

  (define-test "vec-norm handles large values"
    ;; Overflow protection via scaling
    (let ([norm (vec-norm '(1e200 1e200))])
      (assert-true (approx-equal? (* (sqrt 2) 1e200) norm (* 1e190 1.0)))))

  (define-test "vec-diff-norm"
    ;; ||(3,4) - (0,0)|| = 5
    (assert-true (approx-equal? 5.0 (vec-diff-norm '(3 4) '(0 0)) 1e-10))
    ;; ||(1,1) - (1,1)|| = 0
    (assert-true (approx-equal? 0.0 (vec-diff-norm '(1 1) '(1 1)) 1e-10)))

  (define-test "vec-inf-norm"
    ;; max(|3|, |-4|, |2|) = 4
    (assert-true (approx-equal? 4.0 (vec-inf-norm '(3 -4 2)) 1e-10))
    ;; Empty
    (assert-equal 0 (vec-inf-norm '()))))

;;; ============================================================
;;; Optimization Result
;;; ============================================================

(test-group "opt-result"
  (define-test "construction and accessors"
    (let ([r (make-opt-result '(1.0 2.0) 0.5 '(0.01 0.02) 42 'grad-converged)])
      (assert-true (opt-result? r))
      (assert-equal '(1.0 2.0) (opt-x r))
      (assert-true (approx-equal? 0.5 (opt-f r) eps))
      (assert-equal '(0.01 0.02) (opt-grad r))
      (assert-equal 42 (opt-iterations r))
      (assert-equal 'grad-converged (opt-converged r))))

  (define-test "non-result fails predicate"
    (assert-false (opt-result? '(not-a-result)))
    (assert-false (opt-result? 42))))

;;; ============================================================
;;; Convergence Monitoring
;;; ============================================================

(test-group "convergence-monitoring"
  (define-test "initial-convergence-state"
    (let ([cs (initial-convergence-state 10.0 '(3 4))])
      (assert-equal 0 (cs-iter cs))
      (assert-true (approx-equal? 10.0 (cs-f-val cs) eps))
      ;; grad-norm of (3,4) = 5
      (assert-true (approx-equal? 5.0 (cs-grad-norm cs) 1e-10))
      ;; Initial f-change and x-change are +inf.0
      (assert-true (infinite? (cs-f-change cs)))
      (assert-true (infinite? (cs-x-change cs)))))

  (define-test "update-convergence-state"
    (let* ([cs0 (initial-convergence-state 10.0 '(3 4))]
           [cs1 (update-convergence-state cs0 8.0 '(1 0) '(0 0) '(1 1))])
      ;; iter incremented
      (assert-equal 1 (cs-iter cs1))
      ;; new f-val
      (assert-true (approx-equal? 8.0 (cs-f-val cs1) eps))
      ;; grad-norm of (1,0) = 1
      (assert-true (approx-equal? 1.0 (cs-grad-norm cs1) 1e-10))
      ;; f-change = |8.0 - 10.0| = 2.0
      (assert-true (approx-equal? 2.0 (cs-f-change cs1) eps))
      ;; x-change = ||(1,1) - (0,0)|| = sqrt(2)
      (assert-true (approx-equal? (sqrt 2) (cs-x-change cs1) 1e-10))))

  (define-test "convergence-progress returns string"
    (let ([cs (make-convergence-state 5 1.23 0.456 0.01 0.02)])
      (assert-true (string? (convergence-progress cs))))))

;;; ============================================================
;;; Convenience Constructors
;;; ============================================================

(test-group "convenience-constructors"
  (define-test "loose-convergence"
    (let ([cc (loose-convergence)])
      (assert-true (convergence-criteria? cc))
      (assert-true (approx-equal? 1e-3 (cc-grad-tol cc) eps))
      (assert-equal 100 (cc-max-iter cc))))

  (define-test "tight-convergence"
    (let ([cc (tight-convergence)])
      (assert-true (convergence-criteria? cc))
      (assert-true (approx-equal? 1e-10 (cc-grad-tol cc) eps))
      (assert-equal 10000 (cc-max-iter cc))))

  (define-test "iter-only-convergence"
    (let ([cc (iter-only-convergence 50)])
      (assert-true (convergence-criteria? cc))
      ;; All tolerances are 0 — only max-iter matters
      (assert-equal 0 (cc-grad-tol cc))
      (assert-equal 0 (cc-f-tol cc))
      (assert-equal 0 (cc-x-tol cc))
      (assert-equal 50 (cc-max-iter cc))))

  (define-test "iter-only always converges at max-iter"
    (let* ([cc (iter-only-convergence 10)]
           ;; Large values everywhere — only iter should trigger
           [state (make-convergence-state 10 100.0 100.0 100.0 100.0)])
      (assert-equal 'max-iter (converged? cc state))))

  (define-test "iter-only does not converge early"
    (let* ([cc (iter-only-convergence 10)]
           ;; 0 tolerances means grad=0 triggers grad-converged
           ;; Actually: 0 < 0 is false, so grad won't trigger
           ;; But wait: if grad-norm = 0.0 and grad-tol = 0... (< 0 0) is #f
           [state (make-convergence-state 5 100.0 0.001 100.0 100.0)])
      (assert-false (converged? cc state)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests-and-exit)
