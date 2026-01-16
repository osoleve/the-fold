;;; lattice/optimization/test-interval-global.ss — Tests for Interval Global Optimization
;;;
;;; Tests the branch-and-bound global optimizer with various test functions.

(load "core/testing/test-framework.ss")
(load "lattice/optimization/interval-global.ss")

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

;;; Check if a point is within a box
(define (point-in-box? point box)
  (and (= (length point) (length box))
       (let loop ([pts point] [ivs box])
         (or (null? pts)
             (and (<= (interval-lo (car ivs)) (car pts))
                  (<= (car pts) (interval-hi (car ivs)))
                  (loop (cdr pts) (cdr ivs)))))))

;;; Check if a point is within any of the candidate boxes
(define (point-in-boxes? point boxes)
  (exists (lambda (box) (point-in-box? point box)) boxes))

;;; Check if value is close to expected
(define (approx= a b tol)
  (< (abs (- a b)) tol))

;;; ============================================================================
;;; Test: 1D Quadratic (x²)
;;; ============================================================================

(test-group "1D Quadratic"

  (define-test "x² over [-5, 5] finds minimum near 0"
    (let* ([f-interval (lambda (box) (interval-sqr (car box)))]
           [box (list (interval -5 5))]
           [criteria (make-interval-convergence 1e-6 10000)]
           ;; Using new API: f-scalar is optional
           [result (interval-minimize f-interval box criteria)])
      ;; Global minimum is at x=0 with value 0
      (assert-true (< (ior-best-upper result) 1e-10))
      (let ([best-pt (ior-best-point result)])
        (assert-true (< (abs (car best-pt)) 1e-3)))))

  (define-test "x² converges with tight tolerance"
    (let* ([f-interval (lambda (box) (interval-sqr (car box)))]
           [box (list (interval -10 10))]
           [criteria (make-interval-convergence 1e-8 50000)]
           [result (interval-minimize f-interval box criteria)])
      (assert-true (< (ior-best-upper result) 1e-14))))

  (define-test "x² with explicit f-scalar (backward compat)"
    (let* ([f-interval (lambda (box) (interval-sqr (car box)))]
           [f-scalar (lambda (pt) (* (car pt) (car pt)))]
           [box (list (interval -5 5))]
           [criteria (make-interval-convergence 1e-6 10000)]
           ;; Old API still works with optional f-scalar
           [result (interval-minimize f-interval box criteria f-scalar)])
      (assert-true (< (ior-best-upper result) 1e-10))))
)

;;; ============================================================================
;;; Test: 1D Absolute Value (|x|)
;;; ============================================================================

(define (interval-abs-custom box)
  (interval-abs (car box)))

(define (scalar-abs-custom pt)
  (abs (car pt)))

(test-group "1D Absolute Value"

  (define-test "|x| over [-3, 3] finds minimum near 0"
    (let* ([box (list (interval -3 3))]
           [criteria (make-interval-convergence 1e-6 10000)]
           [result (interval-minimize interval-abs-custom box criteria)])
      (assert-true (< (ior-best-upper result) 1e-5))))

  (define-test "|x| over [1, 5] finds minimum at boundary"
    (let* ([box (list (interval 1 5))]
           [criteria (make-interval-convergence 1e-6 10000)]
           [result (interval-minimize interval-abs-custom box criteria)])
      ;; Minimum is at x=1 with value 1
      (assert-true (< (abs (- (ior-best-upper result) 1)) 1e-5))))
)

;;; ============================================================================
;;; Test: 2D Sphere Function
;;; ============================================================================

(test-group "2D Sphere"

  (define-test "Sphere over [-5,5]² finds minimum at origin"
    (let* ([box (list (interval -5 5) (interval -5 5))]
           [criteria (make-interval-convergence 1e-4 20000)]
           [result (interval-minimize interval-sphere box criteria)])
      (assert-true (< (ior-best-upper result) 1e-6))
      (let ([pt (ior-best-point result)])
        (assert-true (and (< (abs (car pt)) 1e-2)
                          (< (abs (cadr pt)) 1e-2))))))

  (define-test "Sphere with asymmetric domain"
    (let* ([box (list (interval -2 5) (interval -3 4))]
           [criteria (make-interval-convergence 1e-4 20000)]
           [result (interval-minimize interval-sphere box criteria)])
      ;; Global minimum is at origin with value 0
      ;; Tolerance slightly relaxed since asymmetric domain needs more bisections
      (assert-true (< (ior-best-upper result) 1e-5))))
)

;;; ============================================================================
;;; Test: 2D Rosenbrock Function
;;; ============================================================================

(test-group "2D Rosenbrock"

  (define-test "Rosenbrock over [-2,2]² finds minimum near (1,1)"
    (let* ([box (list (interval -2 2) (interval -2 2))]
           [criteria (make-interval-convergence 1e-2 50000)]
           [result (interval-minimize interval-rosenbrock box criteria)])
      ;; Global minimum at (1,1) with value 0
      (assert-true (< (ior-best-upper result) 1e-2))
      (let ([pt (ior-best-point result)])
        (assert-true (and (< (abs (- (car pt) 1)) 0.1)
                          (< (abs (- (cadr pt) 1)) 0.1))))))

  (define-test "Rosenbrock finds correct value at minimum"
    (let* ([box (list (interval 0.5 1.5) (interval 0.5 1.5))]
           [criteria (make-interval-convergence 1e-4 30000)]
           [result (interval-minimize interval-rosenbrock box criteria)])
      (assert-true (< (ior-best-upper result) 1e-4))))
)

;;; ============================================================================
;;; Test: Rastrigin Function (multimodal)
;;; ============================================================================

(test-group "Rastrigin (multimodal)"

  (define-test "1D Rastrigin finds global minimum near 0"
    (let* ([box (list (interval -5.12 5.12))]
           [criteria (make-interval-convergence 1e-3 30000)]
           ;; Use explicit f-scalar for Rastrigin since interval version is conservative
           [result (interval-minimize interval-rastrigin box criteria scalar-rastrigin)])
      ;; Global minimum at origin with value 0
      (let ([pt (ior-best-point result)])
        (assert-true (< (abs (car pt)) 0.5)))))

  (define-test "2D Rastrigin finds global minimum near origin"
    (let* ([box (list (interval -2 2) (interval -2 2))]
           [criteria (make-interval-convergence 1e-2 50000)]
           [result (interval-minimize interval-rastrigin box criteria scalar-rastrigin)])
      (let ([pt (ior-best-point result)])
        (assert-true (and (< (abs (car pt)) 0.5)
                          (< (abs (cadr pt)) 0.5))))))
)

;;; ============================================================================
;;; Test: Maximization
;;; ============================================================================

(define (interval-neg-sphere box)
  (interval-neg (interval-sphere box)))

(define (scalar-neg-sphere pt)
  (- (scalar-sphere pt)))

(test-group "Maximization"

  (define-test "Maximize -x² finds maximum at 0"
    (let* ([f-interval (lambda (box) (interval-neg (interval-sqr (car box))))]
           [box (list (interval -5 5))]
           [criteria (make-interval-convergence 1e-6 10000)]
           ;; New API: f-scalar optional for maximize too
           [result (interval-maximize f-interval box criteria)])
      ;; Maximum of -x² is 0 at x=0
      (assert-true (> (ior-best-upper result) -1e-10))))
)

;;; ============================================================================
;;; Test: Edge Cases
;;; ============================================================================

(test-group "Edge Cases"

  (define-test "Single point interval returns that point"
    (let* ([f-interval (lambda (box) (interval-sqr (car box)))]
           [box (list (interval 3 3))]  ; Point interval
           [criteria (make-interval-convergence 1e-6 100)]
           [result (interval-minimize f-interval box criteria)])
      (let ([pt (ior-best-point result)])
        (assert-true (approx= (car pt) 3 1e-10)))))

  (define-test "Constant function over box"
    (let* ([f-interval (lambda (box) (interval-singleton 42))]
           [box (list (interval -10 10))]
           [criteria (make-interval-convergence 1e-6 10000)]
           [result (interval-minimize f-interval box criteria)])
      (assert-true (approx= (ior-best-upper result) 42 1e-10))))

  (define-test "Max iterations terminates"
    (let* ([f-interval (lambda (box) (interval-sqr (car box)))]
           [box (list (interval -1000 1000))]
           [criteria (make-interval-convergence 1e-20 100)]  ; Very tight tol, few iters
           [result (interval-minimize f-interval box criteria)])
      (assert-equal 'max-iterations (ior-reason result))))
)

;;; ============================================================================
;;; Test: Result Structure
;;; ============================================================================

(test-group "Result Structure"

  (define-test "Result contains expected fields"
    (let* ([f-interval (lambda (box) (interval-sqr (car box)))]
           [box (list (interval -5 5))]
           [criteria (make-interval-convergence 1e-4 1000)]
           [result (interval-minimize f-interval box criteria)])
      (assert-true (interval-opt-result? result))
      (assert-true (list? (ior-candidates result)))
      (assert-true (number? (ior-best-upper result)))
      (assert-true (integer? (ior-iterations result)))
      (assert-true (symbol? (ior-reason result)))))

  (define-test "Candidates are within original box"
    (let* ([f-interval (lambda (box) (interval-sqr (car box)))]
           [original-box (list (interval -5 5))]
           [criteria (make-interval-convergence 1e-4 1000)]
           [result (interval-minimize f-interval original-box criteria)]
           [candidates (ior-candidates result)])
      (assert-true (for-all (lambda (cand)
                              (let ([iv (car cand)])
                                (and (>= (interval-lo iv) -5)
                                     (<= (interval-hi iv) 5))))
                            candidates))))

  (define-test "ior-solution-box returns hull of candidates"
    (let* ([f-interval (lambda (box) (interval-sqr (car box)))]
           [box (list (interval -5 5))]
           [criteria (make-interval-convergence 1e-4 1000)]
           [result (interval-minimize f-interval box criteria)]
           [sol-box (ior-solution-box result)])
      (assert-true (list? sol-box))
      (assert-true (interval? (car sol-box)))))
)

;;; ============================================================================
;;; Test: Enclosure Property (Gemini QA recommendation)
;;; ============================================================================
;;;
;;; The fundamental guarantee: the true global minimum is contained within
;;; one of the returned candidate boxes.

(test-group "Enclosure Property"

  (define-test "x² enclosure: origin in candidates"
    ;; True minimum at x=0
    (let* ([f-interval (lambda (box) (interval-sqr (car box)))]
           [box (list (interval -5 5))]
           [criteria (make-interval-convergence 1e-6 10000)]
           [result (interval-minimize f-interval box criteria)]
           [candidates (ior-candidates result)])
      ;; The point (0) must be in at least one candidate box
      (assert-true (point-in-boxes? '(0) candidates))))

  (define-test "Sphere enclosure: origin in candidates"
    ;; True minimum at (0, 0)
    (let* ([box (list (interval -5 5) (interval -5 5))]
           [criteria (make-interval-convergence 1e-4 20000)]
           [result (interval-minimize interval-sphere box criteria)]
           [candidates (ior-candidates result)])
      (assert-true (point-in-boxes? '(0 0) candidates))))

  (define-test "Rosenbrock enclosure: (1,1) in candidates"
    ;; True minimum at (1, 1)
    (let* ([box (list (interval -2 2) (interval -2 2))]
           [criteria (make-interval-convergence 1e-2 50000)]
           [result (interval-minimize interval-rosenbrock box criteria)]
           [candidates (ior-candidates result)])
      (assert-true (point-in-boxes? '(1 1) candidates))))

  (define-test "Solution box contains true minimum"
    ;; ior-solution-box should contain the true minimum
    (let* ([box (list (interval -5 5) (interval -5 5))]
           [criteria (make-interval-convergence 1e-4 20000)]
           [result (interval-minimize interval-sphere box criteria)]
           [sol-box (ior-solution-box result)])
      ;; Origin should be in the solution box
      (assert-true (point-in-box? '(0 0) sol-box))))
)

;;; ============================================================================
;;; Run Tests
;;; ============================================================================

(run-all-tests)
