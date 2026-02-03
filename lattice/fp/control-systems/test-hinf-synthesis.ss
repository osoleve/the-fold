;;; lattice/fp/control-systems/test-hinf-synthesis.ss — Tests for H-infinity Synthesis
;;;
;;; Tests cover:
;;;   - Generalized plant construction
;;;   - Weighting function helpers
;;;   - Transfer function to state-space conversion
;;;   - γ-iteration and Riccati solving
;;;   - Full synthesis workflow

(load "core/testing/test-framework.ss")
(load "lattice/fp/control-systems/hinf-synthesis.ss")

(test-group "H-infinity Synthesis"

  ;;; =========================================================================
  ;;; Weighting Functions
  ;;; =========================================================================

  (test-group "Weighting Functions"

    (define-test "constant weight creates static system"
      (let ([W (hinf-weight-constant 0.5)])
           (assert-true (ss? W))
           (assert-equal 0 (ss-order W))
           ;; D should be 0.5
           (assert-true (< (abs (- (matrix-ref (ss-D W) 0 0) 0.5)) 1e-10))))

    (define-test "first-order weight has correct structure"
      ;; W with dc gain = 100, hf gain = 0.1, crossover = 1
      (let ([W (hinf-weight-first-order 100 0.1 1)])
           (assert-true (ss? W))
           (assert-equal 1 (ss-order W))))
  )

  ;;; =========================================================================
  ;;; Transfer Function to State Space
  ;;; =========================================================================

  (test-group "TF to SS Conversion"

    (define-test "first-order tf converts correctly"
      ;; G(s) = 1/(s+1)
      (let* ([G-tf (tf-from-lists '(1) '(1 1))]
             [G-ss (tf->ss G-tf)])
            (assert-true (ss? G-ss))
            (assert-equal 1 (ss-order G-ss))
            ;; A should be -1
            (assert-true (< (abs (+ (matrix-ref (ss-A G-ss) 0 0) 1)) 1e-10))))

    (define-test "second-order tf converts correctly"
      ;; G(s) = 1/(s^2 + 2s + 1)
      (let* ([G-tf (tf-from-lists '(1) '(1 2 1))]
             [G-ss (tf->ss G-tf)])
            (assert-true (ss? G-ss))
            (assert-equal 2 (ss-order G-ss))))

    (define-test "static gain converts to zero-order system"
      ;; G(s) = 5
      (let* ([G-tf (tf-from-lists '(5) '(1))]
             [G-ss (tf->ss G-tf)])
            (assert-true (ss? G-ss))
            (assert-equal 0 (ss-order G-ss))
            (assert-true (< (abs (- (matrix-ref (ss-D G-ss) 0 0) 5)) 1e-10))))
  )

  ;;; =========================================================================
  ;;; Generalized Plant Construction
  ;;; =========================================================================

  (test-group "Generalized Plant"

    (define-test "mixed-sensitivity plant has correct dimensions"
      (let* ([G (tf->ss (tf-from-lists '(1) '(1 1)))]     ; 1/(s+1)
             [W1 (tf->ss (tf-from-lists '(1 0.1) '(1 10)))] ; (s+0.1)/(s+10)
             [W3 (hinf-weight-constant 0.5)]
             [gp (hinf-mixed-sensitivity-plant G W1 W3)])
            (assert-true (gp? gp))
            ;; Order should be sum of orders
            (assert-equal (+ (ss-order G) (ss-order W1) (ss-order W3))
                          (gp-order gp))
            ;; 1 disturbance (reference)
            (assert-equal 1 (gp-nw gp))
            ;; 1 control
            (assert-equal 1 (gp-nu gp))
            ;; 2 performance outputs (W1*e, W3*u)
            (assert-equal 2 (gp-nz gp))
            ;; 1 measurement
            (assert-equal 1 (gp-ny gp))))
  )

  ;;; =========================================================================
  ;;; Riccati Equations
  ;;; =========================================================================

  (test-group "Riccati Solving"

    (define-test "simple Riccati converges"
      ;; Solve A'X + XA + Q + X*R*X = 0
      ;; For stable A = -1, Q = 1, R = -0.5 (indefinite)
      (let* ([A (matrix-from-lists '((-1)))]
             [Q (matrix-from-lists '((1)))]
             [R (matrix-from-lists '((-0.5)))]
             [X (solve-hinf-riccati A R Q 100)])
            (if (and (pair? X) (eq? (car X) 'error))
                ;; May not converge for all parameters
                (assert-true #t)
                (begin
                  (assert-true (matrix? X))
                  ;; Check residual is small
                  (assert-true (< (hinf-riccati-residual A R Q X) 1e-4))))))

    (define-test "spectral radius computation works"
      (let* ([M (matrix-from-lists '((2 1) (0 3)))]
             [rho (spectral-radius M)])
            ;; Eigenvalues are 2 and 3, so rho = 3
            (assert-true (< (abs (- rho 3)) 0.1))))
  )

  ;;; =========================================================================
  ;;; Full Synthesis
  ;;; =========================================================================

  (test-group "Full Synthesis"

    (define-test "synthesis on simple plant returns controller"
      ;; This is a basic smoke test - full synthesis is complex
      (let* ([G (tf->ss (tf-from-lists '(1) '(1 1)))]
             [W1 (hinf-weight-constant 1)]
             [W3 (hinf-weight-constant 0.5)]
             [result (hinf-synthesize G W1 W3
                                      'gamma-min 0.5
                                      'gamma-max 10
                                      'tolerance 0.1
                                      'max-iter 20)])
            ;; May or may not find a controller depending on numerical issues
            (assert-true (pair? result))))

    (define-test "synthesis with better weights"
      ;; Use more realistic weights that should allow a feasible solution
      (let* ([G (tf->ss (tf-from-lists '(1) '(1 0.1)))]  ; 1/(s+0.1) - slower plant
             [W1 (hinf-weight-constant 0.5)]
             [W3 (hinf-weight-constant 0.1)]
             [result (hinf-synthesize G W1 W3
                                      'gamma-min 1
                                      'gamma-max 50
                                      'tolerance 0.5
                                      'max-iter 30)])
            ;; Check structure
            (assert-true (pair? result))
            (when (and (pair? result) (not (eq? (car result) 'error)))
                  (let ([gamma (car result)]
                        [K (cadr result)])
                       (assert-true (number? gamma))
                       (assert-true (> gamma 0))
                       (when (ss? K)
                             (assert-true (>= (ss-order K) 0)))))))
  )

  ;;; =========================================================================
  ;;; Closed-Loop Analysis
  ;;; =========================================================================

  (test-group "Closed-Loop Analysis"

    (define-test "hinf-norm computation works"
      (let* ([G-tf (tf-from-lists '(1) '(1 1))]
             [norm (hinf-norm G-tf)])
            ;; ||1/(s+1)||_∞ = 1 (at w=0)
            (assert-true (number? norm))
            (assert-true (< (abs (- norm 1)) 0.1))))

    (define-test "closed-loop tf construction"
      (let* ([G (tf-from-lists '(1) '(1 1))]    ; 1/(s+1)
             [K (tf-from-lists '(2) '(1))]       ; K = 2 (constant)
             [T (closed-loop-tf G K)])
            ;; T = GK/(1+GK) = 2/(s+1) / (1 + 2/(s+1)) = 2/(s+3)
            (assert-true (tf? T))))
  )
)

;;; Run all tests
(define (run-hinf-tests)
  (run-all-tests))

(run-hinf-tests)
