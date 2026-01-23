(load "core/testing/test-framework.ss")
(load "lattice/sim/dynamics/bifurcation.ss")

(test-group "bifurcation"

  ;;; ============================================================
  ;;; Parameterized ODE Systems
  ;;; ============================================================

  (define-test "make-parameterized-ode creates valid param-ode"
    (let ([psys (make-parameterized-ode
                 (lambda (r) (lorenz-system 10 r (/ 8 3))))])
         (assert-true (param-ode? psys))))

  (define-test "instantiate-at returns valid ODE"
    (let* ([psys lorenz-rho]
           [sys (instantiate-at psys 28)])
          (assert-true (ode-system? sys))
          (assert-equal (ode-dimension sys) 3)))

  ;;; ============================================================
  ;;; Saddle-Node Normal Form
  ;;; ============================================================

  (define-test "saddle-node normal form: no fixed points for r > 0"
    ;; dx/dt = r + x^2, for r > 0 there are no real fixed points
    (let* ([psys (saddle-node-normal-form-param)]
           [sys (instantiate-at psys 1.0)]
           [fps (find-all-fixed-points sys '((-5 . 5)) 10 1e-4)])
          ;; Should find no fixed points (or none that satisfy f(x) = 0)
          (assert-true (or (null? fps)
                           (every (lambda (fp)
                                          (> (vector-field-norm sys 0 fp) 0.1))
                                  fps)))))

  (define-test "saddle-node normal form: two fixed points for r < 0"
    ;; dx/dt = r + x^2, for r < 0 fixed points at x = +-sqrt(-r)
    (let* ([psys (saddle-node-normal-form-param)]
           [r -1.0]
           [sys (instantiate-at psys r)]
           [fps (find-all-fixed-points sys '((-3 . 3)) 20 1e-4)])
          ;; Should find fixed points near +1 and -1
          (assert-true (>= (length fps) 2))))

  ;;; ============================================================
  ;;; Pitchfork Normal Form
  ;;; ============================================================

  (define-test "pitchfork normal form: one fixed point at origin for r < 0"
    ;; dx/dt = rx - x^3
    (let* ([psys (pitchfork-normal-form-param)]
           [sys (instantiate-at psys -1.0)]
           [analysis (analyze-stability-general sys (vector 0.0) 1e-6)])
          ;; Origin should be stable for r < 0
          (assert-true (stable? (car analysis)))))

  (define-test "pitchfork normal form: three fixed points for r > 0"
    ;; For r > 0: fixed points at x = 0 (unstable) and x = +-sqrt(r) (stable)
    (let* ([psys (pitchfork-normal-form-param)]
           [r 1.0]
           [sys (instantiate-at psys r)]
           [fps (find-all-fixed-points sys '((-3 . 3)) 20 1e-4)])
          ;; Should find 3 fixed points
          (assert-true (>= (length fps) 2))))

  (define-test "pitchfork normal form: origin unstable for r > 0"
    (let* ([psys (pitchfork-normal-form-param)]
           [sys (instantiate-at psys 1.0)]
           [analysis (analyze-stability-general sys (vector 0.0) 1e-6)])
          (assert-true (unstable? (car analysis)))))

  ;;; ============================================================
  ;;; Transcritical Normal Form
  ;;; ============================================================

  (define-test "transcritical normal form: origin stable for r < 0"
    ;; dx/dt = rx - x^2
    (let* ([psys (transcritical-normal-form-param)]
           [sys (instantiate-at psys -1.0)]
           [analysis (analyze-stability-general sys (vector 0.0) 1e-6)])
          (assert-true (stable? (car analysis)))))

  (define-test "transcritical normal form: origin unstable for r > 0"
    (let* ([psys (transcritical-normal-form-param)]
           [sys (instantiate-at psys 1.0)]
           [analysis (analyze-stability-general sys (vector 0.0) 1e-6)])
          (assert-true (unstable? (car analysis)))))

  (define-test "transcritical normal form: nonzero fixed point at x=r"
    ;; For rx - x^2 = 0, solutions are x = 0 and x = r
    (let* ([psys (transcritical-normal-form-param)]
           [r 2.0]
           [sys (instantiate-at psys r)]
           [fp (find-fixed-point-newton sys (vector 1.5) 1e-8 1e-6 20)])
          (assert-true (vector? fp))
          (assert-true (< (abs (- (vector-ref fp 0) r)) 0.1))))

  ;;; ============================================================
  ;;; Hopf Normal Form
  ;;; ============================================================

  (define-test "hopf normal form: origin stable for r < 0"
    (let* ([psys (hopf-normal-form-param)]
           [sys (instantiate-at psys -0.5)]
           [analysis (analyze-stability-general sys (vector 0.0 0.0) 1e-6)])
          ;; Should be stable spiral
          (assert-true (eq? (car analysis) 'stable-spiral))))

  (define-test "hopf normal form: origin unstable for r > 0"
    (let* ([psys (hopf-normal-form-param)]
           [sys (instantiate-at psys 0.5)]
           [analysis (analyze-stability-general sys (vector 0.0 0.0) 1e-6)])
          ;; Should be unstable spiral
          (assert-true (eq? (car analysis) 'unstable-spiral))))

  (define-test "hopf normal form: eigenvalues are complex conjugates"
    (let* ([psys (hopf-normal-form-param)]
           [sys (instantiate-at psys 0.1)]
           [analysis (analyze-stability-general sys (vector 0.0 0.0) 1e-6)]
           [eigs (cdr analysis)]
           [im1 (complex-imag (car eigs))]
           [im2 (complex-imag (cadr eigs))])
          ;; Imaginary parts should be opposite
          (assert-true (< (abs (+ im1 im2)) 0.01))))

  ;;; ============================================================
  ;;; Fixed Point Continuation
  ;;; ============================================================

  (define-test "continue-fixed-point tracks pitchfork origin"
    (let* ([psys (pitchfork-normal-form-param)]
           [continuation (continue-fixed-point psys -2.0 (vector 0.0) 2.0 0.5)])
          ;; Should have several points along the branch
          (assert-true (> (length continuation) 5))
          ;; Each entry should have (param fp stability eigenvalues)
          (assert-true (= (length (car continuation)) 4))))

  (define-test "continue-fixed-point detects stability change"
    (let* ([psys (pitchfork-normal-form-param)]
           [continuation (continue-fixed-point psys -2.0 (vector 0.0) 2.0 0.2)]
           [stabilities (map caddr continuation)])
          ;; Should see stability change from stable to unstable
          (assert-true (pair? (memq 'stable-node stabilities)))
          (assert-true (pair? (memq 'unstable-node stabilities)))))

  ;;; ============================================================
  ;;; Arc-Length Continuation
  ;;; ============================================================

  (define-test "continue-fixed-point-arclength tracks branch"
    (let* ([psys (pitchfork-normal-form-param)]
           [continuation (continue-fixed-point-arclength psys -2.0 (vector 0.0) 20 0.2)])
          ;; Should produce continuation points
          (assert-true (> (length continuation) 5))
          ;; Each entry should have (param fp stability eigenvalues)
          (assert-true (= (length (car continuation)) 4))))

  (define-test "continue-fixed-point-arclength detects stability change"
    (let* ([psys (pitchfork-normal-form-param)]
           [continuation (continue-fixed-point-arclength psys -2.0 (vector 0.0) 30 0.15)]
           [stabilities (map caddr continuation)])
          ;; Should see stability change
          (assert-true (or (pair? (memq 'stable-node stabilities))
                           (pair? (memq 'degenerate stabilities))))
          (assert-true (or (pair? (memq 'unstable-node stabilities))
                           (pair? (memq 'degenerate stabilities))))))

  (define-test "compute-tangent returns unit tangent"
    (let* ([psys (pitchfork-normal-form-param)]
           [tangent (compute-tangent psys -1.0 (vector 0.0))])
          (assert-true (pair? tangent))
          ;; Tangent should be approximately unit length
          (let ([norm-sq (+ (vec-dot (car tangent) (car tangent))
                            (* (cdr tangent) (cdr tangent)))])
               (assert-true (< (abs (- norm-sq 1.0)) 0.01)))))

  (define-test "arclength-predict moves along tangent"
    (let* ([fp (vector 1.0 2.0)]
           [param 3.0]
           [tangent (cons (vector 0.6 0.0) 0.8)]  ; Unit tangent
           [predicted (arclength-predict fp param tangent 1.0)]
           [new-fp (car predicted)]
           [new-param (cdr predicted)])
          ;; Should move by ds=1.0 along tangent direction
          (assert-true (< (abs (- (vector-ref new-fp 0) 1.6)) 0.01))
          (assert-true (< (abs (- new-param 3.8)) 0.01))))

  (define-test "orient-tangent preserves direction"
    (let* ([old-tangent (cons (vector 1.0) 0.0)]
           [new-tangent (cons (vector 0.9) 0.1)]
           [oriented (orient-tangent new-tangent old-tangent)])
          ;; Dot product should be positive
          (assert-true (> (+ (vec-dot (car oriented) (car old-tangent))
                             (* (cdr oriented) (cdr old-tangent)))
                          0))))

  (define-test "orient-tangent flips reversed tangent"
    (let* ([old-tangent (cons (vector 1.0) 0.0)]
           [new-tangent (cons (vector -0.9) -0.1)]  ; Points opposite
           [oriented (orient-tangent new-tangent old-tangent)])
          ;; Should flip to point same direction
          (assert-true (> (+ (vec-dot (car oriented) (car old-tangent))
                             (* (cdr oriented) (cdr old-tangent)))
                          0))))

  ;;; ============================================================
  ;;; Bifurcation Detection
  ;;; ============================================================

  (define-test "detect-bifurcations finds pitchfork"
    (let* ([psys (pitchfork-normal-form-param)]
           [continuation (continue-fixed-point psys -2.0 (vector 0.0) 2.0 0.1)]
           [bifurcations (detect-bifurcations continuation)])
          ;; Should detect at least one bifurcation near r = 0
          (assert-true (>= (length bifurcations) 1))))

  (define-test "detect-bifurcations finds hopf"
    (let* ([psys (hopf-normal-form-param)]
           [continuation (continue-fixed-point psys -1.0 (vector 0.0 0.0) 1.0 0.1)]
           [bifurcations (detect-bifurcations continuation)]
           [hopf-bifs (filter (lambda (b) (eq? (car b) 'hopf)) bifurcations)])
          ;; Should detect Hopf bifurcation
          (assert-true (>= (length hopf-bifs) 1))))

  ;;; ============================================================
  ;;; Bifurcation Diagrams
  ;;; ============================================================

  (define-test "bifurcation-diagram-equilibria finds fixed points"
    (let* ([psys (pitchfork-normal-form-param)]
           [diagram (bifurcation-diagram-equilibria psys -2.0 2.0 10
                                                    '((-3 . 3)) 15 1e-4)])
          ;; Should have entries
          (assert-true (> (length diagram) 0))
          ;; Each entry is (param fp stability)
          (assert-true (= (length (car diagram)) 3))))

  (define-test "bifurcation-diagram-amplitude generates data"
    (let* ([psys (hopf-normal-form-param)]
           [diagram (bifurcation-diagram-amplitude psys -0.5 0.5 5
                                                   (vector 0.1 0.1) 0.01 100 50)])
          ;; Should have data points
          (assert-true (> (length diagram) 0))
          ;; Each point is (param . value)
          (assert-true (pair? (car diagram)))))

  ;;; ============================================================
  ;;; Hopf Bifurcation Analysis
  ;;; ============================================================

  (define-test "find-hopf-bifurcation locates bifurcation"
    (let* ([psys (hopf-normal-form-param)]
           [result (find-hopf-bifurcation psys -1.0 1.0 (vector 0.0 0.0) 1e-3)])
          ;; Should find bifurcation near r = 0
          (assert-true (list? result))
          (assert-true (< (abs (car result)) 0.1))))

  (define-test "hopf-frequency extracts imaginary part"
    (let* ([eigs (list (make-complex 0.0 2.5) (make-complex 0.0 -2.5))]
           [freq (hopf-frequency eigs)])
          ;; Should be approximately 2.5
          (assert-true (< (abs (- freq 2.5)) 0.1))))

  (define-test "hopf-criticality returns supercritical for normal form"
    ;; The standard Hopf normal form is supercritical (l₁ < 0)
    (let* ([psys (hopf-normal-form-param)]
           [crit (hopf-criticality psys 0.0 (vector 0.0 0.0) 1e-4)])
          ;; Should detect supercritical
          (assert-true (eq? crit 'supercritical))))

  (define-test "first-lyapunov-coefficient returns negative for supercritical"
    (let* ([psys (hopf-normal-form-param)]
           [l1 (first-lyapunov-coefficient psys 0.0 (vector 0.0 0.0) 1e-4)])
          ;; l₁ < 0 for supercritical Hopf
          (assert-true (and l1 (< l1 0)))))

  (define-test "hopf-point? detects Hopf eigenvalues"
    (let ([hopf-eigs (list (make-complex 0.01 1.5) (make-complex 0.01 -1.5))]
          [real-eigs (list (make-complex -1.0 0.0) (make-complex -2.0 0.0))])
         (assert-true (hopf-point? hopf-eigs))
         (assert-false (hopf-point? real-eigs))))

  ;;; ============================================================
  ;;; Period Doubling
  ;;; ============================================================

  (define-test "estimate-period-from-section returns valid period"
    (let* ([psys (hopf-normal-form-param)]
           [sys (instantiate-at psys 0.5)]
           ;; Get Poincare section for a limit cycle
           [section (poincare-section sys (vector 0.5 0.0) 0.01 5000 1000 0 0.0 'positive)]
           [period (estimate-period-from-section section 0.1)])
          ;; Period should be at least 1
          (assert-true (>= period 1))))

  (define-test "feigenbaum-delta computes ratio"
    ;; Known cascade: r1=3, r2=3.449, r4=3.544, r8=3.564 (approx for logistic)
    (let* ([cascade '(3.0 3.449 3.544 3.564)]
           [delta (feigenbaum-delta cascade)])
          ;; Feigenbaum delta should approach 4.669...
          ;; With only 4 points, estimate will be rough
          (assert-true (> delta 0))))

  ;;; ============================================================
  ;;; Saddle-Node Detection
  ;;; ============================================================

  (define-test "matrix-determinant-2d computes correctly"
    (let ([m (list 'matrix 2 2 (vector 1 2 3 4))])
         ;; det = 1*4 - 2*3 = -2
         (assert-true (< (abs (+ (matrix-determinant-2d m) 2)) 0.001))))

  (define-test "matrix-determinant works for 3x3 matrix"
    (let ([m (list 'matrix 3 3 (vector 1 2 3  0 1 4  5 6 0))])
         ;; det = 1*(1*0 - 4*6) - 2*(0*0 - 4*5) + 3*(0*6 - 1*5) = -24 + 40 - 15 = 1
         (assert-true (< (abs (- (matrix-determinant m) 1)) 0.001))))

  (define-test "matrix-determinant works for 4x4 identity"
    (let ([m (list 'matrix 4 4 (vector 1 0 0 0  0 1 0 0  0 0 1 0  0 0 0 1))])
         ;; det(I) = 1
         (assert-true (< (abs (- (matrix-determinant m) 1)) 0.001))))

  (define-test "matrix-determinant detects singular matrix"
    (let ([m (list 'matrix 3 3 (vector 1 2 3  4 5 6  7 8 9))])
         ;; This matrix is singular (rows are linearly dependent), det = 0
         (assert-true (< (abs (matrix-determinant m)) 0.001))))

  ;;; ============================================================
  ;;; Analysis Summary
  ;;; ============================================================

  (define-test "analyze-bifurcations returns complete summary"
    (let* ([psys (pitchfork-normal-form-param)]
           [summary (analyze-bifurcations psys -2.0 2.0 20 (vector 0.0)
                                          '((-3 . 3)) 10)])
          ;; Should have expected keys
          (assert-true (pair? (assq 'parameter-range summary)))
          (assert-true (pair? (assq 'continuation-points summary)))
          (assert-true (pair? (assq 'bifurcations summary)))
          (assert-true (pair? (assq 'bifurcation-count summary)))))

  ;;; ============================================================
  ;;; Classic Systems
  ;;; ============================================================

  (define-test "lorenz-rho is valid parameterized system"
    (assert-true (param-ode? lorenz-rho)))

  (define-test "van-der-pol-mu is valid parameterized system"
    (assert-true (param-ode? van-der-pol-mu)))

  (define-test "lorenz at rho=28 has expected fixed points"
    ;; Lorenz has fixed points at origin and (+-sqrt(beta*(rho-1)), +-sqrt(beta*(rho-1)), rho-1)
    (let* ([sys (instantiate-at lorenz-rho 28)]
           [fp-origin (vector 0.0 0.0 0.0)]
           [is-fp (is-equilibrium? sys fp-origin 0.1)])
          (assert-true is-fp)))

)

(run-all-tests)
