;;; lattice/diffgeo/test-lie-groups.ss — Tests for Lie Groups and Algebras
;;; @test lie-groups

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/diffgeo/lie-groups.ss")

;;; ============================================================================
;;; Test Utilities
;;; ============================================================================

(define (test-approx name expected actual tolerance)
  (if (< (abs (- expected actual)) tolerance)
      (test-pass name)
      (test-fail name (format "Expected ~a, got ~a (diff: ~a)"
                              expected actual (abs (- expected actual))))))

(define (test-vec-approx name expected actual tolerance)
  (let ([n (vector-length expected)])
    (if (and (= n (vector-length actual))
             (let loop ([i 0])
               (or (= i n)
                   (and (< (abs (- (vector-ref expected i)
                                   (vector-ref actual i)))
                           tolerance)
                        (loop (+ i 1))))))
        (test-pass name)
        (test-fail name (format "Expected ~a, got ~a" expected actual)))))

(define (test-matrix-approx name expected actual tolerance)
  (if (matrix-approx-equal? expected actual tolerance)
      (test-pass name)
      (test-fail name (format "Matrices not approximately equal"))))

;;; ============================================================================
;;; SO(2) Tests
;;; ============================================================================

(test-group "SO(2) Rotation Group"

  (define-test "so2-identity is identity matrix"
    (let ([I (so2-matrix (so2-identity))])
      (assert-true (matrix-approx-equal? I (identity-matrix 2) 1e-10))))

  (define-test "so2 rotation by π/2"
    (let* ([g (make-so2 (/ *pi* 2))]
           [m (so2-matrix g)])
      ;; cos(π/2) ≈ 0, sin(π/2) ≈ 1
      (assert-true (< (abs (matrix-ref m 0 0)) 1e-10))
      (assert-true (< (abs (+ (matrix-ref m 0 1) 1)) 1e-10))
      (assert-true (< (abs (- (matrix-ref m 1 0) 1)) 1e-10))
      (assert-true (< (abs (matrix-ref m 1 1)) 1e-10))))

  (define-test "so2 angle extraction"
    (let* ([theta 0.7]
           [g (make-so2 theta)]
           [extracted (so2-angle g)])
      (assert-true (< (abs (- theta extracted)) 1e-10))))

  (define-test "so2 composition"
    (let* ([g1 (make-so2 0.3)]
           [g2 (make-so2 0.4)]
           [composed (so2-compose g1 g2)]
           [expected-angle (+ 0.3 0.4)])
      (assert-true (< (abs (- (so2-angle composed) expected-angle)) 1e-10))))

  (define-test "so2 inverse"
    (let* ([theta 0.5]
           [g (make-so2 theta)]
           [g-inv (so2-inverse g)]
           [composed (so2-compose g g-inv)])
      (assert-true (matrix-approx-equal? (so2-matrix composed)
                                         (identity-matrix 2)
                                         1e-10))))

  (define-test "so2 acts on vectors correctly"
    (let* ([g (make-so2 (/ *pi* 2))]
           [v (vector 1 0)]
           [result (so2-act g v)])
      ;; Rotating (1,0) by π/2 gives (0,1)
      (assert-true (< (abs (vector-ref result 0)) 1e-10))
      (assert-true (< (abs (- (vector-ref result 1) 1)) 1e-10))))

  (define-test "so2 exp/log round-trip"
    (let* ([omega 1.2]
           [xi (make-so2-alg omega)]
           [g (so2-exp xi)]
           [xi-back (so2-log g)])
      (assert-true (< (abs (- omega (so2-alg-omega xi-back))) 1e-10)))))

;;; ============================================================================
;;; SO(3) Tests
;;; ============================================================================

(test-group "SO(3) Rotation Group"

  (define-test "so3-identity is identity matrix"
    (let ([I (so3-matrix (so3-identity))])
      (assert-true (matrix-approx-equal? I (identity-matrix 3) 1e-10))))

  (define-test "so3-hat/vee round-trip"
    (let* ([v (vector 1 2 3)]
           [m (so3-hat v)]
           [v-back (so3-vee m)])
      (assert-true (< (vec-norm (vec-sub v v-back)) 1e-10))))

  (define-test "so3-hat produces skew-symmetric matrix"
    (let* ([v (vector 1 2 3)]
           [m (so3-hat v)])
      (assert-true (skew-symmetric? m))))

  (define-test "so3 rotation around z-axis by π/2"
    (let* ([axis (vector 0 0 1)]
           [theta (/ *pi* 2)]
           [g (make-so3 axis theta)]
           [v (vector 1 0 0)]
           [result (so3-act g v)])
      ;; Rotating (1,0,0) around z by π/2 gives (0,1,0)
      (assert-true (< (abs (vector-ref result 0)) 1e-10))
      (assert-true (< (abs (- (vector-ref result 1) 1)) 1e-10))
      (assert-true (< (abs (vector-ref result 2)) 1e-10))))

  (define-test "so3 rotation around x-axis"
    (let* ([axis (vector 1 0 0)]
           [theta (/ *pi* 2)]
           [g (make-so3 axis theta)]
           [v (vector 0 1 0)]
           [result (so3-act g v)])
      ;; Rotating (0,1,0) around x by π/2 gives (0,0,1)
      (assert-true (< (abs (vector-ref result 0)) 1e-10))
      (assert-true (< (abs (vector-ref result 1)) 1e-10))
      (assert-true (< (abs (- (vector-ref result 2) 1)) 1e-10))))

  (define-test "so3 inverse"
    (let* ([axis (vector 0.5773 0.5773 0.5773)]  ; Approx (1,1,1)/√3
           [theta 1.0]
           [g (make-so3 axis theta)]
           [g-inv (so3-inverse g)]
           [composed (so3-compose g g-inv)])
      (assert-true (matrix-approx-equal? (so3-matrix composed)
                                         (identity-matrix 3)
                                         1e-8))))

  (define-test "so3 exp/log round-trip (small angle)"
    (let* ([omega (vector 0.01 0.02 0.015)]
           [xi (make-so3-alg omega)]
           [g (so3-exp xi)]
           [xi-back (so3-log g)]
           [omega-back (so3-alg-vec xi-back)])
      (assert-true (< (vec-norm (vec-sub omega omega-back)) 1e-8))))

  (define-test "so3 exp/log round-trip (moderate angle)"
    (let* ([omega (vector 0.5 0.3 0.4)]
           [xi (make-so3-alg omega)]
           [g (so3-exp xi)]
           [xi-back (so3-log g)]
           [omega-back (so3-alg-vec xi-back)])
      (assert-true (< (vec-norm (vec-sub omega omega-back)) 1e-8))))

  (define-test "so3 exp/log round-trip (large angle)"
    (let* ([omega (vector 1.5 1.0 0.8)]
           [xi (make-so3-alg omega)]
           [g (so3-exp xi)]
           [xi-back (so3-log g)]
           [omega-back (so3-alg-vec xi-back)])
      (assert-true (< (vec-norm (vec-sub omega omega-back)) 1e-6))))

  (define-test "so3 axis-angle extraction"
    (let* ([axis (vector 0 1 0)]
           [theta 0.8]
           [g (make-so3 axis theta)]
           [result (so3-axis-angle g)]
           [axis-back (car result)]
           [theta-back (cdr result)])
      (assert-true (< (abs (- theta theta-back)) 1e-8))
      (assert-true (< (vec-norm (vec-sub axis axis-back)) 1e-8))))

  ;; Edge case: extremely small angle (exercises Taylor expansion in exp, linear approx in log)
  (define-test "so3 exp/log round-trip (tiny angle ~1e-11)"
    (let* ([omega (vector 1e-11 2e-11 1.5e-11)]
           [xi (make-so3-alg omega)]
           [g (so3-exp xi)]
           [xi-back (so3-log g)]
           [omega-back (so3-alg-vec xi-back)])
      ;; Should preserve tiny rotation, not snap to zero
      (assert-true (< (vec-norm (vec-sub omega omega-back)) 1e-13))))

  ;; Edge case: rotation by exactly π (exercises diagonal extraction in log)
  (define-test "so3 exp/log round-trip (theta = π, x-axis)"
    (let* ([axis (vector 1 0 0)]
           [theta *pi*]
           [g (make-so3 axis theta)]
           [xi-back (so3-log g)]
           [omega-back (so3-alg-vec xi-back)]
           [theta-back (vec-norm omega-back)])
      ;; Angle should be π (±small error)
      (assert-true (< (abs (- theta-back *pi*)) 1e-6))))

  ;; Edge case: rotation by π around arbitrary axis
  (define-test "so3 exp/log round-trip (theta = π, diagonal axis)"
    (let* ([axis (vec-scale (/ 1.0 (sqrt 3)) (vector 1 1 1))]
           [theta *pi*]
           [g (make-so3 axis theta)]
           [xi-back (so3-log g)]
           [omega-back (so3-alg-vec xi-back)]
           [theta-back (vec-norm omega-back)]
           [axis-back (vec-scale (/ 1.0 theta-back) omega-back)])
      ;; Angle should be π
      (assert-true (< (abs (- theta-back *pi*)) 1e-6))
      ;; Axis should match (or be negated - both valid for π rotation)
      (let ([dot (+ (* (vector-ref axis 0) (vector-ref axis-back 0))
                    (* (vector-ref axis 1) (vector-ref axis-back 1))
                    (* (vector-ref axis 2) (vector-ref axis-back 2)))])
        (assert-true (or (> dot 0.99) (< dot -0.99)))))))

;;; ============================================================================
;;; SE(2) Tests
;;; ============================================================================

(test-group "SE(2) Rigid Transformations"

  (define-test "se2-identity"
    (let ([g (se2-identity)])
      (assert-true (< (abs (so2-angle (se2-rotation g))) 1e-10))
      (assert-true (< (vec-norm (se2-translation g)) 1e-10))))

  (define-test "se2 composition (pure rotations)"
    (let* ([g1 (make-se2-from-angle 0.3 (vector 0 0))]
           [g2 (make-se2-from-angle 0.4 (vector 0 0))]
           [composed (se2-compose g1 g2)]
           [expected-angle (+ 0.3 0.4)])
      (assert-true (< (abs (- (so2-angle (se2-rotation composed)) expected-angle)) 1e-10))))

  (define-test "se2 composition (pure translations)"
    (let* ([g1 (make-se2-from-angle 0 (vector 1 2))]
           [g2 (make-se2-from-angle 0 (vector 3 4))]
           [composed (se2-compose g1 g2)]
           [t (se2-translation composed)])
      (assert-true (< (abs (- (vector-ref t 0) 4)) 1e-10))
      (assert-true (< (abs (- (vector-ref t 1) 6)) 1e-10))))

  (define-test "se2 inverse"
    (let* ([g (make-se2-from-angle 0.5 (vector 2 3))]
           [g-inv (se2-inverse g)]
           [composed (se2-compose g g-inv)])
      (assert-true (< (abs (so2-angle (se2-rotation composed))) 1e-10))
      (assert-true (< (vec-norm (se2-translation composed)) 1e-10))))

  (define-test "se2 acts on points correctly"
    (let* ([g (make-se2-from-angle (/ *pi* 2) (vector 1 0))]
           [p (vector 1 0)]
           [result (se2-act g p)])
      ;; Rotate (1,0) by π/2 → (0,1), then translate by (1,0) → (1,1)
      (assert-true (< (abs (- (vector-ref result 0) 1)) 1e-10))
      (assert-true (< (abs (- (vector-ref result 1) 1)) 1e-10))))

  (define-test "se2 exp/log round-trip (small values)"
    (let* ([xi (make-se2-alg 0.1 (vector 0.2 0.3))]
           [g (se2-exp xi)]
           [xi-back (se2-log g)])
      (assert-true (< (abs (- (se2-alg-omega xi) (se2-alg-omega xi-back))) 1e-8))
      (assert-true (< (vec-norm (vec-sub (se2-alg-v xi) (se2-alg-v xi-back))) 1e-8))))

  (define-test "se2 exp/log round-trip (moderate values)"
    (let* ([xi (make-se2-alg 1.0 (vector 2.0 1.5))]
           [g (se2-exp xi)]
           [xi-back (se2-log g)])
      (assert-true (< (abs (- (se2-alg-omega xi) (se2-alg-omega xi-back))) 1e-8))
      (assert-true (< (vec-norm (vec-sub (se2-alg-v xi) (se2-alg-v xi-back))) 1e-6)))))

;;; ============================================================================
;;; SE(3) Tests
;;; ============================================================================

(test-group "SE(3) Rigid Transformations"

  (define-test "se3-identity"
    (let ([g (se3-identity)])
      (assert-true (matrix-approx-equal? (so3-matrix (se3-rotation g))
                                         (identity-matrix 3)
                                         1e-10))
      (assert-true (< (vec-norm (se3-translation g)) 1e-10))))

  (define-test "se3 composition (pure rotations)"
    (let* ([R1 (make-so3 (vector 0 0 1) 0.3)]
           [R2 (make-so3 (vector 0 0 1) 0.4)]
           [g1 (make-se3 R1 (vector 0 0 0))]
           [g2 (make-se3 R2 (vector 0 0 0))]
           [composed (se3-compose g1 g2)]
           [R-composed (se3-rotation composed)]
           [R-expected (so3-compose R1 R2)])
      (assert-true (matrix-approx-equal? (so3-matrix R-composed)
                                         (so3-matrix R-expected)
                                         1e-10))))

  (define-test "se3 inverse"
    (let* ([R (make-so3 (vector 0.5773 0.5773 0.5773) 1.0)]
           [t (vector 1 2 3)]
           [g (make-se3 R t)]
           [g-inv (se3-inverse g)]
           [composed (se3-compose g g-inv)])
      (assert-true (matrix-approx-equal? (so3-matrix (se3-rotation composed))
                                         (identity-matrix 3)
                                         1e-8))
      (assert-true (< (vec-norm (se3-translation composed)) 1e-8))))

  (define-test "se3 acts on points correctly"
    (let* ([R (make-so3 (vector 0 0 1) (/ *pi* 2))]
           [t (vector 1 0 0)]
           [g (make-se3 R t)]
           [p (vector 1 0 0)]
           [result (se3-act g p)])
      ;; Rotate (1,0,0) around z by π/2 → (0,1,0), then translate by (1,0,0) → (1,1,0)
      (assert-true (< (abs (- (vector-ref result 0) 1)) 1e-10))
      (assert-true (< (abs (- (vector-ref result 1) 1)) 1e-10))
      (assert-true (< (abs (vector-ref result 2)) 1e-10))))

  (define-test "se3 exp/log round-trip"
    (let* ([omega (vector 0.3 0.2 0.1)]
           [v (vector 1.0 0.5 0.8)]
           [xi (make-se3-alg omega v)]
           [g (se3-exp xi)]
           [xi-back (se3-log g)])
      (assert-true (< (vec-norm (vec-sub omega (se3-alg-omega xi-back))) 1e-6))
      (assert-true (< (vec-norm (vec-sub v (se3-alg-v xi-back))) 1e-6)))))

;;; ============================================================================
;;; Adjoint Tests
;;; ============================================================================

(test-group "Adjoint Representation"

  (define-test "so3 adjoint is rotation"
    (let* ([R (make-so3 (vector 0 0 1) (/ *pi* 2))]
           [xi (make-so3-alg (vector 1 0 0))]
           [Ad-R (so3-adjoint R)]
           [result (Ad-R xi)]
           [expected-omega (so3-act R (vector 1 0 0))])
      (assert-true (< (vec-norm (vec-sub (so3-alg-vec result) expected-omega)) 1e-10)))))

;;; ============================================================================
;;; BCH Tests
;;; ============================================================================

(test-group "Baker-Campbell-Hausdorff"

  (define-test "so3-bch-2 for commuting elements"
    ;; Parallel axes commute, so BCH(X, Y) = X + Y
    (let* ([xi (make-so3-alg (vector 0 0 0.1))]
           [eta (make-so3-alg (vector 0 0 0.2))]
           [result (so3-bch-2 xi eta)]
           [expected (vector 0 0 0.3)])
      (assert-true (< (vec-norm (vec-sub (so3-alg-vec result) expected)) 1e-10))))

  (define-test "so3-bch-4 approximates exp composition (small angles)"
    ;; For small angles: exp(BCH(X,Y)) ≈ exp(X)·exp(Y)
    (let* ([xi (make-so3-alg (vector 0.05 0.03 0.02))]
           [eta (make-so3-alg (vector 0.02 0.04 0.03))]
           [bch-result (so3-bch-4 xi eta)]
           [R-bch (so3-exp bch-result)]
           [R-direct (so3-compose (so3-exp xi) (so3-exp eta))])
      (assert-true (matrix-approx-equal? (so3-matrix R-bch)
                                         (so3-matrix R-direct)
                                         1e-4)))))

;;; ============================================================================
;;; Interpolation Tests
;;; ============================================================================

(test-group "Interpolation"

  (define-test "so3-interpolate at t=0 returns g1"
    (let* ([g1 (make-so3 (vector 1 0 0) 0.5)]
           [g2 (make-so3 (vector 0 1 0) 1.0)]
           [result (so3-interpolate g1 g2 0)])
      (assert-true (matrix-approx-equal? (so3-matrix result)
                                         (so3-matrix g1)
                                         1e-10))))

  (define-test "so3-interpolate at t=1 returns g2"
    (let* ([g1 (make-so3 (vector 1 0 0) 0.5)]
           [g2 (make-so3 (vector 0 1 0) 1.0)]
           [result (so3-interpolate g1 g2 1)])
      (assert-true (matrix-approx-equal? (so3-matrix result)
                                         (so3-matrix g2)
                                         1e-8))))

  (define-test "so3-interpolate at t=0.5 is midpoint"
    (let* ([g1 (so3-identity)]
           [g2 (make-so3 (vector 0 0 1) 1.0)]
           [mid (so3-interpolate g1 g2 0.5)]
           [mid-angle (cdr (so3-axis-angle mid))])
      (assert-true (< (abs (- mid-angle 0.5)) 1e-8))))

  (define-test "se3-interpolate at endpoints"
    (let* ([R1 (make-so3 (vector 1 0 0) 0.3)]
           [R2 (make-so3 (vector 0 1 0) 0.6)]
           [g1 (make-se3 R1 (vector 0 0 0))]
           [g2 (make-se3 R2 (vector 1 2 3))]
           [at-0 (se3-interpolate g1 g2 0)]
           [at-1 (se3-interpolate g1 g2 1)])
      (assert-true (matrix-approx-equal? (so3-matrix (se3-rotation at-0))
                                         (so3-matrix R1)
                                         1e-10))
      (assert-true (< (vec-norm (vec-sub (se3-translation at-1) (vector 1 2 3))) 1e-6)))))

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(run-all-tests)
