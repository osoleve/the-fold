;;; lattice/optimization/test-line-search.ss — Tests for Line Search Methods
;;; Run with: scheme --script lattice/optimization/test-line-search.ss

(load "core/testing/test-framework.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/optimization/line-search.ss")

;;; ============================================================
;;; Helpers
;;; ============================================================

(define (approx-equal? a b tol)
  (< (abs (- a b)) tol))

(define eps 1e-10)

;;; Test functions using traced operations (work with both plain and traced values)
;;; f(x,y) = x^2 + y^2, minimum at (0,0)
(define (sphere-2d x y)
  (traced-add (traced-sq x) (traced-sq y)))

;;; f(x) = x^2, minimum at 0
(define (quadratic-1d x)
  (traced-sq x))

;;; ============================================================
;;; Helper Functions
;;; ============================================================

(test-group "helpers"
  (define-test "dot-product basic"
    (assert-true (approx-equal? 32.0 (dot-product '(1 2 3) '(4 5 6)) eps)))

  (define-test "dot-product orthogonal"
    (assert-true (approx-equal? 0.0 (dot-product '(1 0) '(0 1)) eps)))

  (define-test "dot-product self"
    ;; (3,4) . (3,4) = 9 + 16 = 25
    (assert-true (approx-equal? 25.0 (dot-product '(3 4) '(3 4)) eps)))

  (define-test "dot-product empty"
    (assert-equal 0 (dot-product '() '())))

  (define-test "vec-add-scaled basic"
    ;; (1,2) + 2*(3,4) = (7,10)
    (let ([result (vec-add-scaled '(1 2) '(3 4) 2)])
      (assert-true (approx-equal? 7.0 (car result) eps))
      (assert-true (approx-equal? 10.0 (cadr result) eps))))

  (define-test "vec-add-scaled zero alpha"
    (let ([result (vec-add-scaled '(5 6) '(100 200) 0)])
      (assert-true (approx-equal? 5.0 (car result) eps))
      (assert-true (approx-equal? 6.0 (cadr result) eps))))

  (define-test "vec-add-scaled negative alpha"
    ;; (4,6) + (-1)*(1,2) = (3,4)
    (let ([result (vec-add-scaled '(4 6) '(1 2) -1)])
      (assert-true (approx-equal? 3.0 (car result) eps))
      (assert-true (approx-equal? 4.0 (cadr result) eps)))))

;;; ============================================================
;;; Armijo Condition
;;; ============================================================

(test-group "armijo-condition"
  (define-test "sufficient decrease satisfied"
    ;; f_new = 0.9, f_old = 1.0, alpha = 0.1, grad·d = -1.0, c1 = 1e-4
    ;; threshold = 1.0 + 1e-4 * 0.1 * (-1.0) = 0.99999
    ;; 0.9 <= 0.99999 → true
    (assert-true (armijo-condition? 0.9 1.0 0.1 -1.0 1e-4)))

  (define-test "insufficient decrease rejected"
    ;; f_new = 1.1 > 0.99999
    (assert-false (armijo-condition? 1.1 1.0 0.1 -1.0 1e-4)))

  (define-test "exact threshold boundary"
    ;; f_new = exactly at threshold
    (let ([threshold (+ 1.0 (* 1e-4 0.5 -2.0))])  ; 1.0 - 0.0001 = 0.9999
      (assert-true (armijo-condition? threshold 1.0 0.5 -2.0 1e-4))))

  (define-test "zero step always satisfies"
    ;; alpha = 0 → threshold = f_old, f_new = f_old → satisfied
    (assert-true (armijo-condition? 1.0 1.0 0 -1.0 1e-4))))

;;; ============================================================
;;; Armijo Backtracking
;;; ============================================================

(test-group "armijo-backtrack"
  (define-test "finds descent step for sphere"
    (let* ([x '(1.0 1.0)]
           [grad (gradient sphere-2d x)]
           [direction (map - grad)]  ; steepest descent
           [result (armijo-backtrack sphere-2d x grad direction)]
           [x-new (car result)]
           [alpha (cadr result)])
      ;; Step size should be positive
      (assert-true (> alpha 0))
      ;; New point should have lower function value
      (assert-true (< (apply sphere-2d x-new) (apply sphere-2d x)))))

  (define-test "non-descent direction returns zero step"
    ;; Direction = +gradient (ascent, not descent)
    (let* ([x '(1.0 1.0)]
           [grad (gradient sphere-2d x)]
           [direction grad]  ; ascent direction — grad·d > 0
           [result (armijo-backtrack-full sphere-2d x grad direction 1.0 1e-4 0.5 50)]
           [x-new (car result)]
           [alpha (cadr result)])
      ;; Should return zero step
      (assert-equal 0 alpha)
      ;; x unchanged
      (assert-equal x x-new)))

  (define-test "backtracking reduces step size"
    ;; With rho=0.5, large initial step should be reduced
    (let* ([x '(3.0 3.0)]
           [grad (gradient sphere-2d x)]
           [direction (map - grad)]
           ;; Start with alpha=100 (way too large), should backtrack
           [result (armijo-backtrack-full sphere-2d x grad direction 100.0 1e-4 0.5 50)]
           [alpha (cadr result)])
      ;; Alpha should be much smaller than initial
      (assert-true (< alpha 100.0))
      (assert-true (> alpha 0))))

  (define-test "respects max iterations"
    ;; Very strict c1 (0.99) + tiny rho should hit max-iters
    (let* ([x '(1.0 1.0)]
           [grad (gradient sphere-2d x)]
           [direction (map - grad)]
           [result (armijo-backtrack-full sphere-2d x grad direction 1.0 0.99 0.5 3)])
      ;; Should still return a result (not crash)
      (assert-true (pair? result))
      (assert-true (number? (cadr result))))))

;;; ============================================================
;;; Curvature Conditions
;;; ============================================================

(test-group "curvature-conditions"
  (define-test "weak curvature satisfied"
    ;; grad_new·d >= c2 * grad_old·d
    ;; grad_new·d = -0.1, grad_old·d = -1.0, c2 = 0.9
    ;; -0.1 >= 0.9 * (-1.0) = -0.9 → true
    (assert-true (curvature-condition? -0.1 -1.0 0.9)))

  (define-test "weak curvature not satisfied"
    ;; grad_new·d = -0.95, c2 * grad_old·d = -0.9
    ;; -0.95 >= -0.9 → false
    (assert-false (curvature-condition? -0.95 -1.0 0.9)))

  (define-test "strong curvature satisfied"
    ;; |grad_new·d| <= c2 * |grad_old·d|
    ;; |-0.1| <= 0.9 * |-1.0| → 0.1 <= 0.9 → true
    (assert-true (strong-curvature-condition? -0.1 -1.0 0.9)))

  (define-test "strong curvature not satisfied"
    ;; |-0.95| <= 0.9 * |-1.0| → 0.95 <= 0.9 → false
    (assert-false (strong-curvature-condition? -0.95 -1.0 0.9)))

  (define-test "strong curvature rejects positive gradient"
    ;; |0.95| <= 0.9 * |1.0| → 0.95 <= 0.9 → false
    ;; (strong curvature requires small magnitude, both signs)
    (assert-false (strong-curvature-condition? 0.95 1.0 0.9)))

  (define-test "zero gradient satisfies both"
    (assert-true (curvature-condition? 0 -1.0 0.9))
    (assert-true (strong-curvature-condition? 0 -1.0 0.9))))

;;; ============================================================
;;; Wolfe Line Search
;;; ============================================================

(test-group "wolfe-line-search"
  (define-test "finds valid step for sphere"
    (let* ([x '(2.0 2.0)]
           [grad (gradient sphere-2d x)]
           [direction (map - grad)]
           [result (wolfe-line-search sphere-2d x grad direction)]
           [x-new (car result)]
           [alpha (cadr result)])
      ;; Positive step
      (assert-true (> alpha 0))
      ;; Decrease in function value
      (assert-true (< (apply sphere-2d x-new) (apply sphere-2d x)))))

  (define-test "non-descent returns zero step"
    (let* ([x '(1.0 1.0)]
           [grad (gradient sphere-2d x)]
           [result (wolfe-line-search-full sphere-2d x grad grad 1.0 1e-4 0.9 10.0 50)]
           [alpha (cadr result)])
      (assert-equal 0 alpha)))

  (define-test "1D quadratic"
    (let* ([x '(5.0)]
           [grad (gradient quadratic-1d x)]
           [direction (map - grad)]
           [result (wolfe-line-search quadratic-1d x grad direction)]
           [x-new (car result)]
           [alpha (cadr result)])
      (assert-true (> alpha 0))
      ;; Should move closer to minimum at 0
      (assert-true (< (abs (car x-new)) (abs (car x)))))))

;;; ============================================================
;;; Exact Quadratic Step
;;; ============================================================

(test-group "exact-quadratic-step"
  (define-test "identity hessian"
    ;; f(x) = 0.5 * x^T I x, grad = x, direction = -grad
    ;; alpha = -(grad^T d) / (d^T I d) = ||x||^2 / ||x||^2 = 1
    (let* ([grad '(2.0 3.0)]
           [direction (map - grad)]
           [hessian (matrix-from-lists '((1.0 0.0) (0.0 1.0)))]
           [alpha (exact-quadratic-step grad direction hessian)])
      (assert-true (approx-equal? 1.0 alpha 1e-10))))

  (define-test "scaled hessian"
    ;; f(x) = 0.5 * x^T (2I) x, Hessian = 2I
    ;; alpha = ||grad||^2 / (2*||grad||^2) = 0.5
    (let* ([grad '(1.0 1.0)]
           [direction (map - grad)]
           [hessian (matrix-from-lists '((2.0 0.0) (0.0 2.0)))]
           [alpha (exact-quadratic-step grad direction hessian)])
      (assert-true (approx-equal? 0.5 alpha 1e-10))))

  (define-test "near-singular hessian returns 1.0"
    ;; d^T H d ≈ 0 → fallback to 1.0
    (let* ([grad '(1.0 0.0)]
           [direction '(0.0 1.0)]
           [hessian (make-matrix 2 2 0.0)]  ; zero matrix
           [alpha (exact-quadratic-step grad direction hessian)])
      (assert-true (approx-equal? 1.0 alpha 1e-10)))))

;;; ============================================================
;;; Initial Step Size
;;; ============================================================

(test-group "initial-step-size"
  (define-test "unit gradient and function"
    (let ([alpha (initial-step-size '(1 0 0) 1.0)])
      ;; min(1.0, |1.0| / (1.0 + eps)) ≈ 1.0
      (assert-true (approx-equal? 1.0 alpha 1e-6))))

  (define-test "large gradient scales down"
    ;; grad = (100, 0), f = 1.0
    ;; step = min(1.0, 1.0 / 100.0) = 0.01
    (let ([alpha (initial-step-size '(100 0) 1.0)])
      (assert-true (approx-equal? 0.01 alpha 1e-6))))

  (define-test "zero gradient returns 1.0"
    (let ([alpha (initial-step-size '(0 0 0) 5.0)])
      (assert-true (approx-equal? 1.0 alpha 1e-10))))

  (define-test "large function value gives larger step"
    ;; grad = (1, 0), f = 100.0
    ;; step = min(1.0, 100.0 / 1.0) = 1.0 (capped)
    (let ([alpha (initial-step-size '(1 0) 100.0)])
      (assert-true (approx-equal? 1.0 alpha 1e-6)))))

;;; ============================================================
;;; Constant Step
;;; ============================================================

(test-group "constant-step"
  (define-test "returns fixed alpha"
    (let* ([ls (constant-step 0.01)]
           [result (ls sphere-2d '(1.0 1.0) '(2.0 2.0) '(-2.0 -2.0))]
           [alpha (cadr result)])
      (assert-true (approx-equal? 0.01 alpha eps))))

  (define-test "applies step to direction"
    ;; x + alpha * d = (1,1) + 0.5 * (-2,-2) = (0,0)
    (let* ([ls (constant-step 0.5)]
           [result (ls sphere-2d '(1 1) '(2 2) '(-2 -2))]
           [x-new (car result)])
      (assert-true (approx-equal? 0.0 (car x-new) eps))
      (assert-true (approx-equal? 0.0 (cadr x-new) eps))))

  (define-test "zero step returns original point"
    (let* ([ls (constant-step 0)]
           [result (ls sphere-2d '(3 4) '(6 8) '(-6 -8))]
           [x-new (car result)])
      (assert-true (approx-equal? 3.0 (car x-new) eps))
      (assert-true (approx-equal? 4.0 (cadr x-new) eps)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests-and-exit)
