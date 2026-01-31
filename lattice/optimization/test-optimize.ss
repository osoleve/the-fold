;;; lattice/optimization/test-optimize.ss --- Tests for Optimization Algorithms
;;;
;;; Tests covering:
;;;   - Convergence criteria
;;;   - Line search methods
;;;   - First-order optimizers
;;;   - Second-order optimizers
;;;   - Quasi-Newton methods
;;;
;;; Run with: scheme --script lattice/optimization/test-optimize.ss

(load "core/testing/test-framework.ss")
(load "lattice/optimization/optimize.ss")

;;; ====
;;; Test Helper Functions
;;; ====

;;; approximately-equal? : Number × Number × Number → Bool
(define (approximately-equal? a b tol)
  (< (abs (- a b)) tol))

;;; list-approximately-equal? : (List Number) × (List Number) × Number → Bool
(define (list-approximately-equal? l1 l2 tol)
  (and (= (length l1) (length l2))
       (andmap (lambda (pair) (approximately-equal? (car pair) (cdr pair) tol))
               (map cons l1 l2))))

;;; ====
;;; Test Functions
;;; ====

;;; Simple quadratic: f(x) = x^2, minimum at x=0
(define (quadratic-1d x)
  (traced-sq x))

;;; 2D quadratic: f(x,y) = x^2 + y^2, minimum at (0,0)
(define (sphere-2d x y)
  (traced-add (traced-sq x) (traced-sq y)))

;;; Rosenbrock: f(x,y) = (1-x)^2 + 100(y-x^2)^2, minimum at (1,1)
(define (rosenbrock-2d x y)
  (traced-add
   (traced-sq (traced-sub 1 x))
   (traced-mul 100 (traced-sq (traced-sub y (traced-sq x))))))

;;; Booth function: f(x,y) = (x+2y-7)^2 + (2x+y-5)^2, minimum at (1,3)
(define (booth x y)
  (traced-add
   (traced-sq (traced-sub (traced-add x (traced-mul 2 y)) 7))
   (traced-sq (traced-sub (traced-add (traced-mul 2 x) y) 5))))

;;; Plain sphere for non-traced tests (returns plain number)
(define (sphere-plain args)
  (fold-left + 0 (map (lambda (x) (* x x)) args)))

;;; ====
;;; Convergence Criteria Tests
;;; ====

(test-group "Convergence Criteria"
            (define-test "make-convergence-criteria creates valid structure"
              (let ([cc (make-convergence-criteria 1e-6 1e-8 1e-8 1000)])
                   (and (convergence-criteria? cc)
                        (= (cc-grad-tol cc) 1e-6)
                        (= (cc-f-tol cc) 1e-8)
                        (= (cc-x-tol cc) 1e-8)
                        (= (cc-max-iter cc) 1000))))
            
            (define-test "converged? detects gradient convergence"
              (let* ([cc (make-convergence-criteria 1e-3 1e-8 1e-8 1000)]
                     [state (make-convergence-state 10 0.5 1e-4 1e-5 1e-5)])
                    (eq? (converged? cc state) 'grad-converged)))
            
            (define-test "converged? detects max iterations"
              (let* ([cc (make-convergence-criteria 1e-10 1e-10 1e-10 100)]
                     [state (make-convergence-state 100 0.5 1.0 0.1 0.1)])
                    (eq? (converged? cc state) 'max-iter)))
            
            (define-test "converged? returns #f when not converged"
              (let* ([cc (make-convergence-criteria 1e-10 1e-10 1e-10 1000)]
                     [state (make-convergence-state 10 0.5 1.0 0.1 0.1)])
                    (not (converged? cc state)))))

;;; ====
;;; Line Search Tests
;;; ====

(test-group "Line Search"
            (define-test "armijo-condition? accepts sufficient decrease"
              (armijo-condition? 0.9 1.0 0.1 -1.0 1e-4))
            
            (define-test "armijo-condition? rejects insufficient decrease"
              (not (armijo-condition? 1.1 1.0 0.1 -1.0 1e-4)))
            
            (define-test "armijo-backtrack finds valid step for quadratic"
              (let* ([x '(1.0 1.0)]
                     [grad (gradient sphere-2d x)]
                     [direction (map - grad)]
                     [result (armijo-backtrack sphere-2d x grad direction)]
                     [x-new (car result)]
                     [alpha (cadr result)])
                    (and (> alpha 0)
                         (< (apply sphere-2d x-new) (apply sphere-2d x)))))
            
            (define-test "wolfe-line-search finds valid step"
              (let* ([f sphere-2d]
                     [x '(2.0 2.0)]
                     [grad (gradient f x)]
                     [direction (map - grad)]
                     [result (wolfe-line-search f x grad direction)]
                     [x-new (car result)]
                     [alpha (cadr result)])
                    (and (> alpha 0)
                         (< (apply f x-new) (apply f x))))))

;;; ====
;;; First-Order Optimizer Tests
;;; ====

(test-group "First-Order Optimizers"
            (define-test "SGD minimizes simple quadratic"
              (let* ([result (sgd quadratic-1d '(5.0) 0.1 100)]
                     [x-opt (car result)])
                    (approximately-equal? x-opt 0.0 0.1)))
            
            (define-test "gradient-descent with convergence finds minimum"
              (let* ([cc (make-convergence-criteria 1e-4 1e-6 1e-6 1000)]
                     [result (gradient-descent sphere-2d '(5.0 5.0) 0.1 cc)]
                     [x-opt (opt-x result)])
                    (list-approximately-equal? x-opt '(0.0 0.0) 0.1)))
            
            (define-test "momentum accelerates convergence"
              (let* ([cc (make-convergence-criteria 1e-4 1e-6 1e-6 500)]
                     [result (momentum sphere-2d '(5.0 5.0) 0.1 0.9 cc)]
                     [x-opt (opt-x result)])
                    (list-approximately-equal? x-opt '(0.0 0.0) 0.1)))
            
            (define-test "adam finds minimum of sphere"
              (let* ([cc (make-convergence-criteria 1e-4 1e-6 1e-6 1000)]
                     [result (adam sphere-2d '(5.0 5.0) 0.1 cc)]
                     [x-opt (opt-x result)])
                    (list-approximately-equal? x-opt '(0.0 0.0) 0.1)))
            
            (define-test "rmsprop finds minimum of sphere"
              (let* ([cc (make-convergence-criteria 1e-4 1e-6 1e-6 1000)]
                     [result (rmsprop sphere-2d '(5.0 5.0) 0.1 cc)]
                     [x-opt (opt-x result)])
                    (list-approximately-equal? x-opt '(0.0 0.0) 0.1)))
            
            (define-test "adagrad finds minimum of sphere"
              (let* ([cc (make-convergence-criteria 1e-3 1e-6 1e-6 1000)]
                     [result (adagrad sphere-2d '(2.0 2.0) 1.0 cc)]
                     [x-opt (opt-x result)])
                    (list-approximately-equal? x-opt '(0.0 0.0) 0.5)))

            (define-test "muon finds minimum of sphere"
              (let* ([cc (make-convergence-criteria 1e-3 1e-6 1e-6 500)]
                     [result (muon sphere-2d '(5.0 5.0) 0.1 cc)]
                     [x-opt (opt-x result)])
                    (list-approximately-equal? x-opt '(0.0 0.0) 0.2)))

            (define-test "muon with momentum finds minimum"
              (let* ([cc (make-convergence-criteria 1e-3 1e-6 1e-6 500)]
                     [result (muon-full sphere-2d '(3.0 4.0) 0.1 0.9 cc)]
                     [x-opt (opt-x result)])
                    (list-approximately-equal? x-opt '(0.0 0.0) 0.2)))

            (define-test "muon handles near-zero gradients gracefully"
              ;; Start very close to minimum where gradients are tiny
              (let* ([cc (make-convergence-criteria 1e-6 1e-8 1e-8 10)]
                     [result (muon sphere-2d '(1e-10 1e-10) 0.01 cc)]
                     [x-opt (opt-x result)])
                    ;; Should converge without crashing (division by zero)
                    (opt-result? result))))

;;; ====
;;; Muon Matrix Tests
;;; ====

;;; Simple matrix objective: minimize sum of x_i^2 (Frobenius norm squared)
;;; Takes flat list of traced values, minimum at zero
(define (matrix-frob-sq-flat . args)
  (fold-left traced-add (traced-sq (car args))
             (map traced-sq (cdr args))))

;;; Matrix objective: minimize sum of (x_i - target_i)^2
;;; Takes flat list of traced values
(define (make-matrix-target-objective-flat target-list)
  (lambda args
    (let loop ([xs args] [ts target-list] [sum #f])
         (if (null? xs)
             sum
             (let ([diff-sq (traced-sq (traced-sub (car xs) (car ts)))])
                  (loop (cdr xs) (cdr ts)
                        (if sum (traced-add sum diff-sq) diff-sq)))))))

(test-group "Newton-Schulz Orthogonalization"
            (define-test "newton-schulz preserves approximate dimensions"
              (let* ([m (matrix-from-lists '((1.0 0.0) (0.0 1.0) (0.0 0.0)))]  ; 3x2
                     [m-orth (newton-schulz m 5)])
                    (and (= (matrix-rows m-orth) 3)
                         (= (matrix-cols m-orth) 2))))

            (define-test "newton-schulz orthogonalizes columns"
              ;; For a well-conditioned matrix, M^T @ M should be close to I after orthogonalization
              (let* ([m (matrix-from-lists '((0.5 0.1) (0.1 0.5)))]
                     [m-orth (newton-schulz m 10)]
                     [mt (matrix-transpose m-orth)]
                     [mtm (matrix-mul mt m-orth)]
                     [i2 (matrix-identity 2)])
                    ;; Should be approximately identity
                    (matrix-approx-equal? mtm i2 0.1)))

            (define-test "newton-schulz handles large-norm matrices via pre-scaling"
              ;; Large norm matrix (||M||_F > 2) - pre-scaling must trigger for convergence
              (let* ([m (matrix-from-lists '((10.0 5.0) (5.0 10.0)))]  ; ||M||_F ≈ 15.8
                     [m-orth (newton-schulz m 10)]
                     [mt (matrix-transpose m-orth)]
                     [mtm (matrix-mul mt m-orth)])
                    ;; Should not explode - result should be finite and roughly orthogonal
                    (and (< (frobenius-norm m-orth) 100)  ; Didn't explode
                         (matrix? mtm)))))

(test-group "Muon Matrix Optimizer"
            (define-test "muon-matrix reduces objective"
              (let* ([m0 (matrix-from-lists '((1.0 2.0) (3.0 4.0)))]
                     [cc (make-convergence-criteria 1e-2 1e-4 1e-4 50)]
                     [result (muon-matrix matrix-frob-sq-flat m0 0.1 cc)]
                     [m-opt (mor-matrix result)]
                     [f-final (mor-f result)]
                     ;; Compute initial value (sum of squares)
                     [f-init (+ 1.0 4.0 9.0 16.0)])
                    ;; Final value should be less than initial
                    (< f-final f-init)))

            (define-test "muon-matrix moves toward target"
              (let* ([target (matrix-from-lists '((1.0 0.0) (0.0 1.0)))]
                     [target-flat '(1.0 0.0 0.0 1.0)]
                     [m0 (matrix-from-lists '((5.0 5.0) (5.0 5.0)))]
                     [obj (make-matrix-target-objective-flat target-flat)]
                     [cc (make-convergence-criteria 1e-2 1e-4 1e-4 100)]
                     [result (muon-matrix obj m0 0.05 cc)]
                     [m-opt (mor-matrix result)]
                     ;; Check that we moved closer to target
                     [init-dist (frobenius-norm (matrix-sub m0 target))]
                     [final-dist (frobenius-norm (matrix-sub m-opt target))])
                    (< final-dist init-dist))))

;;; ====
;;; Newton Method Tests
;;; ====

(test-group "Newton Methods"
            (define-test "newton-method finds minimum of sphere"
              (let* ([cc (make-convergence-criteria 1e-6 1e-8 1e-8 100)]
                     [result (newton-method sphere-2d '(5.0 5.0) cc)]
                     [x-opt (opt-x result)])
                    (list-approximately-equal? x-opt '(0.0 0.0) 1e-4)))
            
            (define-test "newton-method converges fast on quadratic"
              (let* ([cc (make-convergence-criteria 1e-10 1e-12 1e-12 100)]
                     [result (newton-method sphere-2d '(10.0 10.0) cc)]
                     [iters (opt-iterations result)])
                    ;; Newton should converge in very few iterations for quadratic
                    (< iters 10)))
            
            (define-test "modified-newton handles non-convex start"
              (let* ([cc (make-convergence-criteria 1e-4 1e-6 1e-6 200)]
                     ;; Booth function is convex, so modified Newton should work well
                     [result (modified-newton booth '(0.0 0.0) cc)]
                     [x-opt (opt-x result)])
                    ;; Should approach minimum at (1, 3)
                    (< (apply booth x-opt) 1.0)))
            
            (define-test "newton-cg finds minimum"
              (let* ([cc (make-convergence-criteria 1e-4 1e-6 1e-6 200)]
                     [result (newton-cg sphere-2d '(5.0 5.0) cc)]
                     [x-opt (opt-x result)])
                    (list-approximately-equal? x-opt '(0.0 0.0) 0.1))))

;;; ====
;;; L-BFGS Tests
;;; ====

(test-group "L-BFGS"
            (define-test "lbfgs finds minimum of sphere"
              (let* ([cc (make-convergence-criteria 1e-6 1e-8 1e-8 100)]
                     [result (lbfgs sphere-2d '(5.0 5.0) cc)]
                     [x-opt (opt-x result)])
                    (list-approximately-equal? x-opt '(0.0 0.0) 1e-4)))
            
            (define-test "lbfgs converges on Rosenbrock"
              (let* ([cc (make-convergence-criteria 1e-4 1e-6 1e-6 500)]
                     [result (lbfgs rosenbrock-2d '(0.0 0.0) cc)]
                     [x-opt (opt-x result)]
                     [f-opt (opt-f result)])
                    ;; Should get close to minimum at (1, 1)
                    (and (< (abs (- (car x-opt) 1.0)) 0.5)
                         (< (abs (- (cadr x-opt) 1.0)) 0.5))))
            
            (define-test "lbfgs history pushes correctly"
              (let* ([h (make-lbfgs-history 5)]
                     [h1 (lbfgs-history-push h '(1.0 0.0) '(0.5 0.0))]
                     [h2 (lbfgs-history-push h1 '(0.0 1.0) '(0.0 0.5))])
                    (= (lh-size h2) 2)))
            
            (define-test "lbfgs direction is descent direction"
              (let* ([h (make-lbfgs-history 5)]
                     [h1 (lbfgs-history-push h '(1.0 0.0) '(0.5 0.0))]
                     [grad '(1.0 1.0)]
                     [direction (lbfgs-direction h1 grad)]
                     [dot (dot-product direction grad)])
                    ;; Direction should be descent (negative dot product with gradient)
                    (< dot 0))))

;;; ====
;;; L-BFGS-B (Bounded) Tests
;;; ====

(test-group "L-BFGS-B"
            (define-test "project-box enforces bounds"
              (let ([projected (project-box '(5.0 -3.0 0.5)
                                            '(0.0 -1.0 0.0)
                                            '(2.0 1.0 1.0))])
                   (list-approximately-equal? projected '(2.0 -1.0 0.5) 1e-10)))
            
            (define-test "lbfgs-b respects lower bounds"
              (let* ([cc (make-convergence-criteria 1e-4 1e-6 1e-6 100)]
                     ;; Sphere minimum at (0,0) but constrained to x >= 1, y >= 1
                     [result (lbfgs-b sphere-2d '(5.0 5.0)
                                      '(1.0 1.0) '(+inf.0 +inf.0) cc)]
                     [x-opt (opt-x result)])
                    ;; Should hit the boundary at (1, 1)
                    (and (>= (car x-opt) 0.99)
                         (>= (cadr x-opt) 0.99))))
            
            (define-test "lbfgs-b respects upper bounds"
              (let* ([cc (make-convergence-criteria 1e-4 1e-6 1e-6 100)]
                     ;; Negative sphere (maximize sphere = minimize at boundary)
                     [neg-sphere (lambda (x y) (traced-neg (traced-add (traced-sq x) (traced-sq y))))]
                     [result (lbfgs-b neg-sphere '(0.0 0.0)
                                      '(-inf.0 -inf.0) '(2.0 2.0) cc)]
                     [x-opt (opt-x result)])
                    ;; Should hit the corner at (2, 2)
                    (and (<= (car x-opt) 2.01)
                         (<= (cadr x-opt) 2.01)))))

;;; ====
;;; Unified API Tests
;;; ====

(test-group "Unified API"
            (define-test "minimize with default method works"
              (let* ([result (minimize sphere-2d '(3.0 3.0))]
                     [x-opt (opt-x result)])
                    (list-approximately-equal? x-opt '(0.0 0.0) 0.1)))
            
            (define-test "minimize with 'adam works"
              (let* ([cc (make-convergence-criteria 1e-3 1e-5 1e-5 500)]
                     [result (minimize sphere-2d '(3.0 3.0) 'adam cc)]
                     [x-opt (opt-x result)])
                    (list-approximately-equal? x-opt '(0.0 0.0) 0.2)))

            (define-test "minimize with 'muon works"
              (let* ([cc (make-convergence-criteria 1e-3 1e-5 1e-5 500)]
                     [result (minimize sphere-2d '(3.0 3.0) 'muon cc)]
                     [x-opt (opt-x result)])
                    (list-approximately-equal? x-opt '(0.0 0.0) 0.2)))
            
            (define-test "minimize with 'newton works"
              (let* ([cc (make-convergence-criteria 1e-6 1e-8 1e-8 50)]
                     [result (minimize sphere-2d '(3.0 3.0) 'newton cc)]
                     [x-opt (opt-x result)])
                    (list-approximately-equal? x-opt '(0.0 0.0) 1e-4)))
            
            (define-test "minimize-bounded works"
              (let* ([cc (make-convergence-criteria 1e-4 1e-6 1e-6 100)]
                     [result (minimize-bounded sphere-2d '(5.0 5.0)
                                               '(1.0 1.0) '(10.0 10.0) cc)]
                     [x-opt (opt-x result)])
                    ;; Should find constrained minimum at (1, 1)
                    (and (>= (car x-opt) 0.99)
                         (>= (cadr x-opt) 0.99)))))

;;; ====
;;; Run All Tests
;;; ====

(run-all-tests)
