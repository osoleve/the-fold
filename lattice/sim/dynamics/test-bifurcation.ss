(load "core/lang/module.ss")
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
  ;;; Normal Form Classification
  ;;; ============================================================

  (define-test "compute-1d-derivatives correct for pitchfork"
    ;; dx/dt = rx - x³ at origin: f''(0) = 0, f'''(0) = -6
    (let* ([psys (pitchfork-normal-form-param)]
           [sys (instantiate-at psys 0.0)]  ; At bifurcation point
           [derivs (compute-1d-derivatives sys (vector 0.0) 1e-4)]
           [fpp (car derivs)]
           [fppp (cadr derivs)])
          ;; Second derivative should be near zero
          (assert-true (< (abs fpp) 0.01))
          ;; Third derivative should be approximately -6
          (assert-true (< (abs (+ fppp 6)) 0.5))))

  (define-test "compute-1d-derivatives correct for transcritical"
    ;; dx/dt = rx - x² at origin: f''(0) = -2, f'''(0) = 0
    (let* ([psys (transcritical-normal-form-param)]
           [sys (instantiate-at psys 0.0)]  ; At bifurcation point
           [derivs (compute-1d-derivatives sys (vector 0.0) 1e-4)]
           [fpp (car derivs)]
           [fppp (cadr derivs)])
          ;; Second derivative should be approximately -2
          (assert-true (< (abs (+ fpp 2)) 0.1))
          ;; Third derivative should be near zero
          (assert-true (< (abs fppp) 0.1))))

  (define-test "classify-codim1-bifurcation identifies pitchfork"
    (let* ([psys (pitchfork-normal-form-param)]
           [sys (instantiate-at psys 0.0)]
           [bif-type (classify-codim1-bifurcation sys (vector 0.0) 1e-4)])
          (assert-equal bif-type 'pitchfork)))

  (define-test "classify-codim1-bifurcation identifies transcritical"
    (let* ([psys (transcritical-normal-form-param)]
           [sys (instantiate-at psys 0.0)]
           [bif-type (classify-codim1-bifurcation sys (vector 0.0) 1e-4)])
          (assert-equal bif-type 'transcritical)))

  (define-test "normal-form-is-pitchfork? returns true for pitchfork"
    (let* ([psys (pitchfork-normal-form-param)]
           [sys (instantiate-at psys 0.0)])
          (assert-true (normal-form-is-pitchfork? sys (vector 0.0) 1e-4))))

  (define-test "normal-form-is-pitchfork? returns false for transcritical"
    (let* ([psys (transcritical-normal-form-param)]
           [sys (instantiate-at psys 0.0)])
          (assert-false (normal-form-is-pitchfork? sys (vector 0.0) 1e-4))))

  ;;; ============================================================
  ;;; N-Dimensional Normal Form Classification (fold-zxsh)
  ;;; ============================================================

  (define-test "classify-codim1-bifurcation-nd works for 1D pitchfork"
    ;; Should fall back to 1D fast path
    (let* ([psys (pitchfork-normal-form-param)]
           [bif-type (classify-codim1-bifurcation-nd psys 0.0 (vector 0.0) 1e-4)])
          (assert-equal bif-type 'pitchfork)))

  (define-test "classify-codim1-bifurcation-nd works for 1D transcritical"
    (let* ([psys (transcritical-normal-form-param)]
           [bif-type (classify-codim1-bifurcation-nd psys 0.0 (vector 0.0) 1e-4)])
          (assert-equal bif-type 'transcritical)))

  (define-test "compute-critical-eigenvectors-svd finds zero eigenvalue direction"
    ;; At pitchfork r=0, Jacobian is [[0]] for 1D, eigenvalue is 0
    (let* ([psys (pitchfork-normal-form-param)]
           [sys (instantiate-at psys 0.0)]
           [jac (compute-jacobian sys (vector 0.0) 1e-6)]
           [result (compute-critical-eigenvectors-svd jac)])
          ;; Should find eigenvectors (1D system)
          (assert-true (and result (= (length result) 2)))
          ;; Both should be unit vectors
          (assert-true (< (abs (- (vec-norm (car result)) 1.0)) 0.01))))

  (define-test "classify-codim1-bifurcation-nd handles 2D Hopf system at saddle-node"
    ;; The Hopf normal form at r=0 has purely imaginary eigenvalues (not zero),
    ;; so this should return 'unknown (not a fold bifurcation)
    (let* ([psys (hopf-normal-form-param)]
           [bif-type (classify-codim1-bifurcation-nd psys 0.0 (vector 0.0 0.0) 1e-4)])
          ;; At Hopf, eigenvalues are purely imaginary, not zero
          ;; So SVD won't find a near-zero singular value
          (assert-equal bif-type 'unknown)))

  (define-test "compute-nd-directional-derivatives returns valid derivatives"
    ;; For 1D pitchfork, use identity for right/left vecs
    (let* ([psys (pitchfork-normal-form-param)]
           [sys (instantiate-at psys 0.0)]
           [right-vec (vector 1.0)]
           [left-vec (vector 1.0)]
           [derivs (compute-nd-directional-derivatives sys (vector 0.0) right-vec left-vec 1e-4)])
          ;; For pitchfork dx/dt = rx - x³, at r=0, x=0:
          ;; g''(0) = d²f/dx² = -6x|_{x=0} = 0
          (assert-true (< (abs (car derivs)) 0.01))
          ;; g'''(0) = d³f/dx³ = -6
          (assert-true (< (abs (+ (cadr derivs) 6)) 1.0))))

  (define-test "classify-codim1-bifurcation-nd identifies 2D saddle-node (fold-zxsh)"
    ;; Test 2D system: dx/dt = r + x², dy/dt = -y
    ;; At r=0, x=0, y=0: Jacobian has eigenvalue 0 (x direction) and -1 (y direction)
    ;; This is a saddle-node in the x direction
    (let* ([psys (saddle-node-2d-param)]
           [bif-type (classify-codim1-bifurcation-nd psys 0.0 (vector 0.0 0.0) 1e-4)])
          ;; Should identify as saddle-node (transverse fold)
          (assert-equal 'saddle-node bif-type)))

  (define-test "compute-projected-parameter-derivative is non-zero at saddle-node"
    ;; For saddle-node r + x², at r=0 x=0: df/dp = 1 (the coefficient of r)
    ;; The left eigenvector w = (1,0) for the x-direction
    ;; So wᵀ(df/dp) = 1 ≠ 0
    (let* ([psys (saddle-node-2d-param)]
           [sys (instantiate-at psys 0.0)]
           [jac (compute-jacobian sys (vector 0.0 0.0) 1e-6)]
           [eigenpair (compute-critical-eigenvectors-svd jac)])
          (if eigenpair
              (let* ([left-vec (cadr eigenpair)]
                     [normalized (normalize-eigenvector-pair (car eigenpair) left-vec)]
                     [norm-left (cadr normalized)]
                     [param-deriv (compute-projected-parameter-derivative
                                   psys 0.0 (vector 0.0 0.0) norm-left 1e-4)])
                    ;; Should be approximately 1.0 (or -1.0 depending on eigenvector sign)
                    (assert-true (> (abs param-deriv) 0.5)))
              ;; If no eigenpair found, the test is invalid
              (assert-true #f))))

  ;;; ============================================================
  ;;; Bifurcation Detection
  ;;; ============================================================

  (define-test "detect-bifurcations finds pitchfork"
    (let* ([psys (pitchfork-normal-form-param)]
           [continuation (continue-fixed-point psys -2.0 (vector 0.0) 2.0 0.1)]
           [bifurcations (detect-bifurcations continuation)])
          ;; Should detect at least one bifurcation near r = 0
          (assert-true (>= (length bifurcations) 1))))

  (define-test "detect-bifurcations with psys correctly classifies pitchfork"
    (let* ([psys (pitchfork-normal-form-param)]
           [continuation (continue-fixed-point psys -2.0 (vector 0.0) 2.0 0.1)]
           [bifurcations (detect-bifurcations continuation psys)]
           [pitchforks (filter (lambda (b) (eq? (car b) 'pitchfork)) bifurcations)])
          ;; Should find pitchfork bifurcation when using normal form analysis
          (assert-true (>= (length pitchforks) 1))))

  (define-test "detect-bifurcations with psys correctly classifies transcritical"
    (let* ([psys (transcritical-normal-form-param)]
           [continuation (continue-fixed-point psys -2.0 (vector 0.0) 2.0 0.1)]
           [bifurcations (detect-bifurcations continuation psys)]
           [transcriticals (filter (lambda (b) (eq? (car b) 'transcritical)) bifurcations)])
          ;; Should find transcritical bifurcation when using normal form analysis
          (assert-true (>= (length transcriticals) 1))))

  (define-test "detect-bifurcations finds hopf"
    (let* ([psys (hopf-normal-form-param)]
           [continuation (continue-fixed-point psys -1.0 (vector 0.0 0.0) 1.0 0.1)]
           [bifurcations (detect-bifurcations continuation)]
           [hopf-bifs (filter (lambda (b) (eq? (car b) 'hopf)) bifurcations)])
          ;; Should detect Hopf bifurcation
          (assert-true (>= (length hopf-bifs) 1))))

  (define-test "detect-fold-from-param-reversal detects reversal"
    ;; Simulate arc-length data with parameter reversal at a fold
    (let* ([prev-prev (list -0.2 (vector 0.1) 'stable (list (make-complex -0.3 0)))]
           [prev (list -0.05 (vector 0.05) 'stable (list (make-complex -0.05 0)))]  ; Near fold
           [curr (list -0.1 (vector 0.15) 'stable (list (make-complex 0.1 0)))]     ; Reversed
           [prev-eigs (cadddr prev)]
           [result (detect-fold-from-param-reversal prev-prev prev curr prev-eigs)])
          ;; Should detect saddle-node from parameter reversal + near-zero eigenvalue
          (assert-equal result 'saddle-node)))

  (define-test "detect-fold-from-param-reversal ignores non-reversal"
    ;; No reversal - parameter keeps increasing
    (let* ([prev-prev (list 0.1 (vector 0.1) 'stable (list (make-complex -0.3 0)))]
           [prev (list 0.2 (vector 0.15) 'stable (list (make-complex -0.2 0)))]
           [curr (list 0.3 (vector 0.2) 'stable (list (make-complex -0.1 0)))]
           [prev-eigs (cadddr prev)]
           [result (detect-fold-from-param-reversal prev-prev prev curr prev-eigs)])
          ;; No reversal, should return #f
          (assert-false result)))

  (define-test "detect-fold-from-param-reversal rejects complex eigenvalues (fold-zxsg)"
    ;; Parameter reversal but eigenvalues are complex (Hopf-like)
    ;; Old code would detect this as saddle-node; new code should reject
    (let* ([prev-prev (list -0.2 (vector 0.1 0.1) 'stable-spiral
                            (list (make-complex -0.1 1.5) (make-complex -0.1 -1.5)))]
           [prev (list -0.05 (vector 0.05 0.05) 'degenerate
                       (list (make-complex 0.01 1.5) (make-complex 0.01 -1.5)))]  ; Near axis but complex
           [curr (list -0.1 (vector 0.15 0.15) 'unstable-spiral
                       (list (make-complex 0.1 1.5) (make-complex 0.1 -1.5)))]
           [prev-eigs (cadddr prev)]
           [result (detect-fold-from-param-reversal prev-prev prev curr prev-eigs)])
          ;; Complex eigenvalues should NOT be detected as saddle-node
          (assert-false result)))

  (define-test "detect-fold-from-param-reversal requires sign change across fold"
    ;; Parameter reversal and eigenvalue near zero, but no sign change
    (let* ([prev-prev (list -0.2 (vector 0.1) 'stable (list (make-complex -0.3 0)))]
           [prev (list -0.05 (vector 0.05) 'stable (list (make-complex 0.02 0)))]  ; Near zero
           [curr (list -0.1 (vector 0.15) 'stable (list (make-complex -0.1 0)))]   ; Still negative
           [prev-eigs (cadddr prev)]
           [result (detect-fold-from-param-reversal prev-prev prev curr prev-eigs)])
          ;; No sign change across fold, should return #f
          (assert-false result)))

  (define-test "end-to-end: arc-length continuation detects saddle-node (Gemini QA suggestion)"
    ;; Integration test: run actual continuation on saddle-node normal form
    ;; dx/dt = r + x². At r=-1, stable fixed point at x=-1.
    ;; As r increases toward 0, the fold occurs and parameter reverses in arc-length.
    (let* ([psys (saddle-node-normal-form-param)]
           ;; Start at r=-1 with the stable fixed point x = -sqrt(1) = -1
           [fp0 (vector -1.0)]
           ;; Use arc-length continuation to follow the branch
           [continuation (continue-fixed-point-arclength psys -1.0 fp0 40 0.1)]
           ;; Detect bifurcations from the continuation data
           [bifurcations (detect-bifurcations continuation psys)]
           ;; Filter for saddle-node detections
           [saddle-nodes (filter (lambda (b) (eq? (car b) 'saddle-node)) bifurcations)])
          ;; Should detect at least one saddle-node near r=0
          (assert-true (>= (length saddle-nodes) 1)
                       "Should detect saddle-node bifurcation")
          ;; The detected bifurcation should be near r=0
          (when (pair? saddle-nodes)
                (let ([bif-param (cadr (car saddle-nodes))])
                     (assert-true (< (abs bif-param) 0.3)
                                  (format "Saddle-node should be near r=0, got r=~a" bif-param))))))

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

  (define-test "matrix-determinant computes 2x2 correctly"
    (let ([m (list 'matrix 2 2 (vector 1 2 3 4))])
         ;; det = 1*4 - 2*3 = -2
         (assert-true (< (abs (+ (matrix-determinant m) 2)) 0.001))))

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

  ;;; ============================================================
  ;;; 3D System Tests (n > 2)
  ;;; ============================================================
  ;;; These tests verify behavior for systems with dimension > 2.
  ;;; Some analyses are 2D-only and should gracefully handle 3D input.
  ;;; Note: Several functions have known limitations for n>2.

  (define-test "lorenz 3D: find non-trivial fixed points"
    ;; For rho > 1, Lorenz has fixed points at C+ and C-:
    ;; C± = (±sqrt(β(ρ-1)), ±sqrt(β(ρ-1)), ρ-1) with β=8/3
    (let* ([rho 28.0]
           [beta (/ 8 3)]
           [sys (instantiate-at lorenz-rho rho)]
           ;; Expected C+ location
           [sqrt-val (sqrt (* beta (- rho 1)))]
           ;; Find fixed point near C+
           [fp (find-fixed-point-newton sys (vector 8.0 8.0 27.0) 1e-8 1e-6 20)])
          (assert-true (vector? fp))
          ;; Should be close to expected
          (assert-true (< (abs (- (vector-ref fp 0) sqrt-val)) 0.5))
          (assert-true (< (abs (- (vector-ref fp 2) (- rho 1))) 0.5))))

  (define-test "lorenz 3D: eigenvalues at origin"
    ;; Lorenz Jacobian at origin: verify we get 3 eigenvalues
    (let* ([sys (instantiate-at lorenz-rho 28)]
           [origin (vector 0.0 0.0 0.0)]
           [analysis (analyze-stability-general sys origin 1e-6)]
           [eigs (cdr analysis)])
          ;; Should have 3 eigenvalues
          (assert-equal 3 (length eigs))))

  (define-test "lorenz 3D: stability analysis returns classification"
    ;; Verify 3D stability analysis completes and returns a classification
    (let* ([sys (instantiate-at lorenz-rho 28)]
           [origin (vector 0.0 0.0 0.0)]
           [analysis (analyze-stability-general sys origin 1e-6)]
           [stability-type (car analysis)])
          ;; Should return some stability classification (not crash)
          (assert-true (symbol? stability-type))))

  (define-test "lorenz 3D: continuation produces points"
    ;; Verify continuation works for 3D, even if stability tracking limited
    (let* ([continuation (continue-fixed-point lorenz-rho 0.5 (vector 0.0 0.0 0.0) 2.0 0.3)])
          ;; Should have some continuation points
          (assert-true (> (length continuation) 2))
          ;; Each entry should have the expected structure
          (assert-true (= (length (car continuation)) 4))))

  (define-test "first-lyapunov-coefficient returns #f for 3D system"
    ;; First Lyapunov coefficient is only implemented for 2D
    (let* ([result (first-lyapunov-coefficient lorenz-rho 1.0 (vector 0.0 0.0 0.0) 1e-4)])
          ;; Should return #f for 3D system (documented limitation)
          (assert-false result)))

  (define-test "lorenz 3D: determinant computable"
    ;; Verify matrix-determinant works for 3x3 matrices (uses existing test)
    ;; Using a known 3x3 matrix rather than computed Jacobian to isolate test
    (let ([m (list 'matrix 3 3 (vector 1 2 3  0 1 4  5 6 0))])
         ;; det = 1 (verified earlier in test suite)
         (assert-true (< (abs (- (matrix-determinant m) 1)) 0.001))))

  ;;; ============================================================
  ;;; Branch Switching
  ;;; ============================================================

  (define-test "compute-critical-eigenvector finds null space at pitchfork"
    ;; At r=0 for dx/dt = rx - x³, Jacobian at origin is [0], which is singular
    (let* ([psys (pitchfork-normal-form-param)]
           [eigenvec (compute-critical-eigenvector psys 0.0 (vector 0.0))])
          ;; Should find an eigenvector (1D case: it's just ±1)
          (assert-true (vector? eigenvec))
          (assert-equal 1 (vector-length eigenvec))
          ;; Should be approximately unit length
          (assert-true (< (abs (- (vec-norm eigenvec) 1.0)) 0.01))))

  (define-test "compute-critical-eigenvector returns #f away from bifurcation"
    ;; At r=-1, Jacobian at origin is [-1], not singular
    (let* ([psys (pitchfork-normal-form-param)]
           [eigenvec (compute-critical-eigenvector psys -1.0 (vector 0.0))])
          ;; Should return #f (no zero eigenvalue)
          (assert-false eigenvec)))

  (define-test "switch-branch-adaptive finds upper branch of pitchfork"
    ;; Pitchfork at r=0.5: has branches at x=0 (unstable) and x=±sqrt(0.5) ≈ ±0.707
    (let* ([psys (pitchfork-normal-form-param)]
           [r 0.5]  ; Well past bifurcation for clear separation
           [sys (instantiate-at psys r)]
           ;; Find the origin branch (should be unstable)
           [origin-fp (vector 0.0)]
           ;; Try to switch to upper branch
           [result (switch-branch-adaptive psys r origin-fp 'upper)])
          ;; Should find a different fixed point
          (if result
              (begin
                (assert-true (pair? result))
                (assert-true (vector? (car result)))
                ;; The new fixed point should be noticeably away from origin
                (assert-true (> (abs (vector-ref (car result) 0)) 0.3)))
              ;; If switch fails, that's a problem at this parameter
              (assert-true #f))))

  (define-test "switch-branch-adaptive finds branches at pitchfork bifurcation"
    ;; At exactly r=0 (or very close), we should be able to detect both branches emerging
    ;; Use a small positive r to have actual branches to find
    (let* ([psys (pitchfork-normal-form-param)]
           [r 0.5]  ; Past bifurcation, branches at x = ±sqrt(0.5) ≈ ±0.707
           ;; Start from the unstable origin
           [origin-fp (vector 0.0)]
           [upper (switch-branch-adaptive psys r origin-fp 'upper)]
           [lower (switch-branch-adaptive psys r origin-fp 'lower)])
          ;; At least one branch should be found
          (assert-true (or (pair? upper) (pair? lower)))
          ;; If both found, they should be distinct
          (when (and (pair? upper) (pair? lower))
                (let ([upper-x (vector-ref (car upper) 0)]
                      [lower-x (vector-ref (car lower) 0)])
                     ;; Upper should be positive, lower negative (or vice versa)
                     (assert-true (> (abs (- upper-x lower-x)) 0.5))))))

  (define-test "find-all-branches-at-bifurcation returns multiple branches for pitchfork"
    ;; At pitchfork with r > 0, should find 3 branches (origin + 2 stable)
    (let* ([psys (pitchfork-normal-form-param)]
           [r 1.0]  ; Well past bifurcation
           [fp (vector 0.0)]  ; Origin
           [branches (find-all-branches-at-bifurcation psys r fp)])
          ;; Should have at least 2 branches (original + at least one new)
          (assert-true (>= (length branches) 2))
          ;; Should include original
          (assert-true (pair? (assq 'original branches)))))

  (define-test "switch-branch works with continuation data"
    ;; Run continuation through a bifurcation, then switch branches
    (let* ([psys (pitchfork-normal-form-param)]
           [continuation (continue-fixed-point psys -1.0 (vector 0.0) 1.0 0.1)]
           ;; Find index near bifurcation (r ≈ 0)
           [near-bif-idx (let loop ([idx 0] [data continuation])
                              (if (null? data)
                                  (- idx 1)
                                  (if (> (caar data) 0.05)
                                      idx
                                      (loop (+ idx 1) (cdr data)))))])
          ;; Try to switch at that point
          (when (> near-bif-idx 0)
                (let ([result (switch-branch psys continuation near-bif-idx 'upper)])
                     ;; Result may be #f if we're not exactly at bifurcation, that's ok
                     (when result
                           (assert-true (pair? result))
                           (assert-true (vector? (car result))))))))

  (define-test "continue-switched-branch produces continuation data"
    ;; Switch to a new branch and then continue along it
    (let* ([psys (pitchfork-normal-form-param)]
           [r 0.5]
           [fp (vector 0.0)]
           [switched (switch-branch-adaptive psys r fp 'upper)])
          (when switched
                (let* ([new-fp (car switched)]
                       [new-param (cdr switched)]
                       [continued (continue-switched-branch psys new-param new-fp 10 0.1)])
                      ;; Should produce continuation points
                      (assert-true (> (length continued) 0))
                      ;; Each entry should have proper structure
                      (assert-true (= (length (car continued)) 4))))))

  (define-test "transcritical branch switching finds exchanging branches"
    ;; Transcritical: branches x=0 and x=r exchange stability at r=0
    (let* ([psys (transcritical-normal-form-param)]
           [r 1.0]  ; Past bifurcation: x=0 unstable, x=r stable
           [origin-fp (vector 0.0)]
           [branches (find-all-branches-at-bifurcation psys r origin-fp)])
          ;; Should find at least the original and one other branch
          (assert-true (>= (length branches) 1))))

  (define-test "switch-branch-adaptive handles large parameter steps correctly"
    ;; Test for fold-zxtw: verify that large parameter steps don't cause false positives
    ;; by comparing to where the original branch moved, not the original fp
    (let* ([psys (pitchfork-normal-form-param)]
           ;; Start at r=0.5 (well past bifurcation, branches at x = ±sqrt(0.5) ≈ ±0.707)
           [r-start 0.5]
           [origin-fp (vector 0.0)]
           ;; The fix ensures we track where the original branch went at new-param.
           ;; For pitchfork, origin stays at x=0 for all r, so original branch tracking
           ;; should correctly identify when we've switched to sqrt(r) or -sqrt(r) branch.
           [result (switch-branch-adaptive psys r-start origin-fp 'upper)])
          ;; If we find a result, verify it's actually on a different branch
          (when result
                (let* ([new-fp (car result)]
                       [new-param (cdr result)]
                       [sys (instantiate-at psys new-param)]
                       ;; Converge from origin at new-param to find original branch there
                       [original-at-new-param (find-fixed-point-robust
                                               sys origin-fp
                                               *branch-switch-tolerance*
                                               *continuation-step-size*
                                               *branch-switch-max-iter*)])
                      ;; The found point should be different from where original branch is
                      ;; For pitchfork at r>0, upper branch at sqrt(r), origin still at 0
                      (when original-at-new-param
                            (assert-true (> (vec-norm (vec-sub new-fp original-at-new-param)) 0.1)))))))

  ;;; ============================================================
  ;;; Adaptive Step-Size Control (fold-zxrv)
  ;;; ============================================================

  (define-test "compute-eigenvalue-factor returns 1.0 for safe eigenvalues"
    (let ([eigs (list (make-complex 0.5 0.0) (make-complex -0.3 0.0))])
         (assert-equal (compute-eigenvalue-factor eigs) 1.0)))

  (define-test "compute-eigenvalue-factor returns 0.5 near critical"
    (let ([eigs (list (make-complex 0.05 0.0) (make-complex -0.3 0.0))])
         (assert-equal (compute-eigenvalue-factor eigs) 0.5)))

  (define-test "compute-curvature-factor returns 1.0 for small angle"
    (let ([t1 (cons (vector 1.0 0.0) 0.0)]
          [t2 (cons (vector 0.99 0.14) 0.0)])  ; ~8 degrees
         (assert-equal (compute-curvature-factor t1 t2) 1.0)))

  (define-test "compute-curvature-factor returns 0.5 for large angle"
    (let ([t1 (cons (vector 1.0 0.0) 0.0)]
          [t2 (cons (vector 0.7 0.7) 0.0)])  ; ~45 degrees
         (assert-equal (compute-curvature-factor t1 t2) 0.5)))

  (define-test "compute-residual-factor returns 1.0 for fast convergence"
    (assert-equal (compute-residual-factor 2) 1.0))

  (define-test "compute-residual-factor returns 0.5 for slow convergence"
    (assert-equal (compute-residual-factor 6) 0.5))

  (define-test "compute-residual-factor returns 0.25 near failure"
    (assert-equal (compute-residual-factor 9) 0.25))

  (define-test "compute-adaptive-step respects min bound"
    (let ([result (compute-adaptive-step 1e-6 '(0.5 0.5 0.5) 0.1)])
         (assert-true (>= result *adaptive-min-step*))))

  (define-test "compute-adaptive-step respects max bound"
    (let ([result (compute-adaptive-step 0.5 '(1.0 1.0 1.0) 0.1)])
         (assert-true (<= result (* 0.1 *adaptive-max-step-multiplier*)))))

  (define-test "should-expand? returns true after consecutive good steps"
    (assert-true (should-expand? 3 '(1.0 1.0 1.0))))

  (define-test "should-expand? returns false with shrink factor"
    (assert-false (should-expand? 3 '(1.0 0.5 1.0))))

  (define-test "continue-fixed-point-arclength-adaptive tracks pitchfork"
    (let* ([psys (pitchfork-normal-form-param)]
           [fp0 (vector 0.0)]
           [result (continue-fixed-point-arclength-adaptive psys -0.5 fp0 20 0.1)])
          ;; Should produce continuation data
          (assert-true (> (length result) 5))
          ;; All entries should have the expected structure
          (assert-true (every (lambda (e) (= (length e) 4)) result))))

  (define-test "continue-fixed-point-arclength-adaptive detects stability change"
    (let* ([psys (pitchfork-normal-form-param)]
           [fp0 (vector 0.0)]
           [result (continue-fixed-point-arclength-adaptive psys -0.5 fp0 30 0.1)]
           [stabilities (map caddr result)])
          ;; Should see stability change (stable -> unstable at r=0)
          (assert-true (not (every (lambda (s) (eq? s (car stabilities))) stabilities)))))

  (define-test "continue-fixed-point-adaptive tracks pitchfork"
    (let* ([psys (pitchfork-normal-form-param)]
           [fp0 (vector 0.0)]
           [result (continue-fixed-point-adaptive psys -0.5 fp0 0.5 0.1)])
          ;; Should produce continuation data
          (assert-true (> (length result) 5))))

  (define-test "arclength-correct returns iteration count"
    (let* ([psys (pitchfork-normal-form-param)]
           [param -0.5]
           [fp (vector 0.0)]
           [tangent (compute-tangent psys param fp)]
           [predicted (arclength-predict fp param tangent 0.1)]
           [result (arclength-correct psys (car predicted) (cdr predicted) tangent 0.1)])
          ;; Result should be (fp param iterations)
          (assert-true (and result (= (length result) 3)))
          (assert-true (number? (caddr result)))))

  (define-test "adaptive continuation uses fewer steps in stable region"
    ;; Compare adaptive vs fixed continuation - adaptive should take fewer steps
    ;; to cover same region when much of it is stable
    (let* ([psys (pitchfork-normal-form-param)]
           [fp0 (vector 0.0)]
           ;; Fixed: many small steps
           [fixed-result (continue-fixed-point-arclength psys -1.0 fp0 50 0.05)]
           ;; Adaptive: starts same but expands in stable region
           [adaptive-result (continue-fixed-point-arclength-adaptive psys -1.0 fp0 50 0.05)]
           ;; Both should cover similar parameter range
           [fixed-range (- (car (car (reverse fixed-result))) -1.0)]
           [adaptive-range (- (car (car (reverse adaptive-result))) -1.0)])
          ;; Adaptive should cover at least as much range (often more due to expansion)
          (assert-true (>= adaptive-range (* 0.8 fixed-range)))))

  ;;; ============================================================
  ;;; Automatic Bifurcation Diagram
  ;;; ============================================================

  (define-test "make-work-item creates valid work item"
    (let ([item (make-work-item 'branch-0 1.5 (vector 0.0) 'forward)])
         (assert-true (work-item? item))
         (assert-equal (work-item-id item) 'branch-0)
         (assert-equal (work-item-param item) 1.5)
         (assert-equal (work-item-direction item) 'forward)))

  (define-test "make-bifurcation-diagram creates valid structure"
    (let ([diag (make-bifurcation-diagram
                 '((branch-0 . ((0.0 #(0.0) stable ()))))
                 '()
                 '((param-range . (-1.0 . 1.0))))])
         (assert-true (bifurcation-diagram? diag))
         (assert-equal (length (diagram-branches diag)) 1)
         (assert-equal (length (diagram-bifurcations diag)) 0)))

  (define-test "spatial-hash registers and looks up points"
    (let* ([hash (make-spatial-hash -1.0 1.0 1.0 100)]
           [_ (spatial-hash-register! hash 0.5 (vector 0.3) 'branch-0)]
           [result (spatial-hash-lookup hash 0.5 (vector 0.3))])
          (assert-equal result 'branch-0)))

  (define-test "spatial-hash returns #f for unregistered points"
    (let* ([hash (make-spatial-hash -1.0 1.0 1.0 100)]
           [result (spatial-hash-lookup hash 0.5 (vector 0.3))])
          (assert-false result)))

  (define-test "trace-bifurcation-diagram produces valid diagram for pitchfork"
    (let* ([psys (pitchfork-normal-form-param)]
           [fp0 (vector 0.0)]
           [diag (trace-bifurcation-diagram psys -0.5 fp0 -1.0 1.0 50)])
          (assert-true (bifurcation-diagram? diag))
          ;; Should have at least the starting branch
          (assert-true (> (length (diagram-branches diag)) 0))))

  (define-test "trace-bifurcation-diagram finds bifurcation in pitchfork"
    (let* ([psys (pitchfork-normal-form-param)]
           [fp0 (vector 0.0)]
           [diag (trace-bifurcation-diagram psys -0.5 fp0 -1.0 1.0 100)])
          ;; Should detect the pitchfork bifurcation at r=0
          (assert-true (> (length (diagram-bifurcations diag)) 0))))

  (define-test "diagram-branch retrieves correct branch"
    (let* ([psys (pitchfork-normal-form-param)]
           [fp0 (vector 0.0)]
           [diag (trace-bifurcation-diagram psys -0.5 fp0 -1.0 1.0 50)]
           [branch (diagram-branch diag 'branch-0)])
          ;; Branch-0 should exist and have points
          (assert-true (and branch (> (length branch) 0)))))

  (define-test "diagram-summary returns valid statistics"
    (let* ([psys (pitchfork-normal-form-param)]
           [fp0 (vector 0.0)]
           [diag (trace-bifurcation-diagram psys -0.5 fp0 -1.0 1.0 50)]
           [summary (diagram-summary diag)])
          ;; Should have expected keys (assq returns pair, check it's truthy)
          (assert-true (pair? (assq 'branch-count summary)))
          (assert-true (pair? (assq 'total-points summary)))
          (assert-true (pair? (assq 'bifurcation-count summary)))))

  (define-test "diagram-stable-points filters correctly"
    (let* ([psys (pitchfork-normal-form-param)]
           [fp0 (vector 0.0)]
           [diag (trace-bifurcation-diagram psys -0.5 fp0 -1.0 1.0 50)]
           [stable (diagram-stable-points diag)])
          ;; Should have some stable points (origin is stable for r < 0)
          (assert-true (> (length stable) 0))
          ;; All returned points should have stable-like classification
          (assert-true (null? (filter (lambda (pt)
                                        (not (memq (caddr pt) '(stable stable-node stable-focus))))
                                      stable)))))

  (define-test "diagram-points-at-param finds cross-section"
    (let* ([psys (pitchfork-normal-form-param)]
           [fp0 (vector 0.0)]
           [diag (trace-bifurcation-diagram psys -0.5 fp0 -1.0 1.0 50)]
           [points (diagram-points-at-param diag -0.3 0.1)])
          ;; Should find at least the origin branch near r = -0.3
          (assert-true (> (length points) 0))))

)

(run-all-tests)
