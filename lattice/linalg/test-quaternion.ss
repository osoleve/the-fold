;;; core/linalg/test-quaternion.ss — Tests for Quaternion Library
;;;
;;; Run: scheme --script core/linalg/test-quaternion.ss

(load "core/test-framework.ss")
(load "lattice/linalg/quaternion.ss")

(set! *current-group* 'quaternion)

;;; Helper for approximate equality
(define (approx= a b)
  (< (abs (- a b)) 1e-9))

(define pi 3.141592653589793)

;;; ============================================================
;;; Construction Tests
;;; ============================================================

(define-test "quat creates quaternion"
  (let ([q (quat 1 2 3 4)])
       (assert-equal 1 (quat-w q))
       (assert-equal 2 (quat-x q))
       (assert-equal 3 (quat-y q))
       (assert-equal 4 (quat-z q))))

(define-test "quat-identity creates identity"
  (let ([q (quat-identity)])
       (assert-equal 1 (quat-w q))
       (assert-equal 0 (quat-x q))
       (assert-equal 0 (quat-y q))
       (assert-equal 0 (quat-z q))))

(define-test "quat-from-axis-angle creates rotation"
  (let ([q (quat-from-axis-angle (vec3 0 0 1) (/ pi 2))])
       ;; 90° around z-axis: w = cos(45°), z = sin(45°)
       (assert-true (approx= (cos (/ pi 4)) (quat-w q)))
       (assert-true (approx= 0 (quat-x q)))
       (assert-true (approx= 0 (quat-y q)))
       (assert-true (approx= (sin (/ pi 4)) (quat-z q)))))

(define-test "quat-from-euler creates rotation"
  (let ([q (quat-from-euler 0 0 (/ pi 2))])
       ;; 90° yaw = rotation around z
       (assert-true (approx= (cos (/ pi 4)) (quat-w q)))
       (assert-true (approx= (sin (/ pi 4)) (quat-z q)))))

(define-test "quat-from-two-vectors finds rotation"
  (let* ([v1 (vec3 1 0 0)]
         [v2 (vec3 0 1 0)]
         [q (quat-from-two-vectors v1 v2)]
         [rotated (quat-rotate-vec3 q v1)])
        (assert-true (vec3-nearly-equal? v2 rotated 1e-9))))

;;; ============================================================
;;; Predicate Tests
;;; ============================================================

(define-test "quat? identifies quaternions"
  (assert-true (quat? (quat 1 0 0 0)))
  (assert-false (quat? '(1 0 0 0)))
  (assert-false (quat? 42)))

(define-test "quat-identity? identifies identity"
  (assert-true (quat-identity? (quat-identity)))
  (assert-false (quat-identity? (quat 0 1 0 0))))

(define-test "quat-unit? identifies unit quaternions"
  (assert-true (quat-unit? (quat-identity)))
  (assert-true (quat-unit? (quat-from-axis-angle (vec3 1 0 0) 1)))
  (assert-false (quat-unit? (quat 2 0 0 0))))

;;; ============================================================
;;; Arithmetic Tests
;;; ============================================================

(define-test "quat-add adds quaternions"
  (let ([q (quat-add (quat 1 2 3 4) (quat 5 6 7 8))])
       (assert-equal (quat 6 8 10 12) q)))

(define-test "quat-sub subtracts quaternions"
  (let ([q (quat-sub (quat 5 6 7 8) (quat 1 2 3 4))])
       (assert-equal (quat 4 4 4 4) q)))

(define-test "quat-scale scales quaternion"
  (assert-equal (quat 2 4 6 8) (quat-scale (quat 1 2 3 4) 2)))

(define-test "quat-mul multiplies quaternions"
  ;; Identity * q = q
  (let* ([q (quat-from-axis-angle (vec3 1 0 0) 1)]
         [result (quat-mul (quat-identity) q)])
        (assert-true (quat-nearly-equal? q result 1e-9))))

(define-test "quat-mul composes rotations"
  ;; 90° around z + 90° around z = 180° around z
  (let* ([q (quat-from-axis-angle (vec3 0 0 1) (/ pi 2))]
         [q2 (quat-mul q q)]
         [expected (quat-from-axis-angle (vec3 0 0 1) pi)])
        (assert-true (quat-nearly-equal? q2 expected 1e-9))))

;;; ============================================================
;;; Normalization Tests
;;; ============================================================

(define-test "quat-magnitude computes length"
  (assert-true (approx= 1.0 (quat-magnitude (quat-identity))))
  (assert-true (approx= 2.0 (quat-magnitude (quat 2 0 0 0)))))

(define-test "quat-normalize creates unit quaternion"
  (let ([q (quat-normalize (quat 0 0 0 2))])
       (assert-true (approx= 1.0 (quat-magnitude q)))
       (assert-true (approx= 1.0 (quat-z q)))))

;;; ============================================================
;;; Conjugate and Inverse Tests
;;; ============================================================

(define-test "quat-conjugate negates vector part"
  (let ([q (quat-conjugate (quat 1 2 3 4))])
       (assert-equal 1 (quat-w q))
       (assert-equal -2 (quat-x q))
       (assert-equal -3 (quat-y q))
       (assert-equal -4 (quat-z q))))

(define-test "quat-inverse inverts rotation"
  (let* ([q (quat-from-axis-angle (vec3 0 0 1) 1)]
         [qi (quat-inverse q)]
         [result (quat-mul q qi)])
        (assert-true (quat-identity? result))))

;;; ============================================================
;;; Interpolation Tests
;;; ============================================================

(define-test "quat-slerp at endpoints"
  (let* ([q1 (quat-from-axis-angle (vec3 0 0 1) 0)]
         [q2 (quat-from-axis-angle (vec3 0 0 1) pi)])
        (assert-true (quat-nearly-equal? q1 (quat-slerp q1 q2 0) 1e-9))
        (assert-true (quat-nearly-equal? q2 (quat-slerp q1 q2 1) 1e-9))))

(define-test "quat-slerp at midpoint"
  (let* ([q1 (quat-from-axis-angle (vec3 0 0 1) 0)]
         [q2 (quat-from-axis-angle (vec3 0 0 1) pi)]
         [mid (quat-slerp q1 q2 0.5)]
         [expected (quat-from-axis-angle (vec3 0 0 1) (/ pi 2))])
        (assert-true (quat-nearly-equal? mid expected 1e-9))))

(define-test "quat-nlerp approximates slerp"
  (let* ([q1 (quat-from-axis-angle (vec3 0 0 1) 0)]
         [q2 (quat-from-axis-angle (vec3 0 0 1) 0.5)]
         [slerp-mid (quat-slerp q1 q2 0.5)]
         [nlerp-mid (quat-nlerp q1 q2 0.5)])
        ;; For small angles, nlerp ≈ slerp
        (assert-true (< (quat-angle slerp-mid nlerp-mid) 0.01))))

;;; ============================================================
;;; Rotation Tests
;;; ============================================================

(define-test "quat-rotate-vec3 rotates vector"
  (let* ([q (quat-from-axis-angle (vec3 0 0 1) (/ pi 2))]
         [v (vec3 1 0 0)]
         [rotated (quat-rotate-vec3 q v)])
        (assert-true (approx= 0 (vec3-x rotated)))
        (assert-true (approx= 1 (vec3-y rotated)))
        (assert-true (approx= 0 (vec3-z rotated)))))

(define-test "quat-rotate-vec3 preserves length"
  (let* ([q (quat-from-euler 0.5 0.7 0.3)]
         [v (vec3 1 2 3)]
         [rotated (quat-rotate-vec3 q v)])
        (assert-true (approx= (vec3-magnitude v) (vec3-magnitude rotated)))))

(define-test "identity rotation preserves vector"
  (let* ([v (vec3 1 2 3)]
         [rotated (quat-rotate-vec3 (quat-identity) v)])
        (assert-equal v rotated)))

(define-test "quat-rotate-vec3-inverse reverses rotation"
  (let* ([q (quat-from-euler 0.5 0.7 0.3)]
         [v (vec3 1 2 3)]
         [rotated (quat-rotate-vec3 q v)]
         [back (quat-rotate-vec3-inverse q rotated)])
        (assert-true (vec3-nearly-equal? v back 1e-9))))

;;; ============================================================
;;; Conversion Tests
;;; ============================================================

(define-test "quat-to-axis-angle round-trips"
  (let* ([axis (vec3-normalize (vec3 1 2 3))]
         [angle 1.5]
         [q (quat-from-axis-angle axis angle)]
         [result (quat-to-axis-angle q)]
         [result-axis (car result)]
         [result-angle (cadr result)])
        (assert-true (approx= angle result-angle))
        (assert-true (vec3-nearly-equal? axis result-axis 1e-9))))

(define-test "quat-to-euler round-trips"
  (let* ([roll 0.3]
         [pitch 0.5]
         [yaw 0.7]
         [q (quat-from-euler roll pitch yaw)]
         [result (quat-to-euler q)])
        (assert-true (approx= roll (car result)))
        (assert-true (approx= pitch (cadr result)))
        (assert-true (approx= yaw (caddr result)))))

(define-test "quat-from-euler matches composition"
  ;; Verify ZYX Euler conversion matches manual composition
  ;; ZYX means: first roll (X), then pitch (Y), then yaw (Z)
  (let* ([roll (/ pi 4)]   ; 45°
         [pitch (/ pi 6)]  ; 30°
         [yaw (/ pi 3)]    ; 60°
         [qe (quat-from-euler roll pitch yaw)]
         ;; Manually compose rotations: Z * Y * X (left-to-right application)
         [qx (quat-from-axis-angle (vec3 1 0 0) roll)]
         [qy (quat-from-axis-angle (vec3 0 1 0) pitch)]
         [qz (quat-from-axis-angle (vec3 0 0 1) yaw)]
         [qcomposed (quat-mul qz (quat-mul qy qx))]
         ;; Test on multiple vectors
         [v1 (vec3 1 0 0)]
         [v2 (vec3 0 1 0)]
         [v3 (vec3 1 2 3)]
         [r1e (quat-rotate-vec3 qe v1)]
         [r1c (quat-rotate-vec3 qcomposed v1)]
         [r2e (quat-rotate-vec3 qe v2)]
         [r2c (quat-rotate-vec3 qcomposed v2)]
         [r3e (quat-rotate-vec3 qe v3)]
         [r3c (quat-rotate-vec3 qcomposed v3)])
        ;; Euler formula should match manual composition
        (assert-true (vec3-nearly-equal? r1e r1c 1e-9))
        (assert-true (vec3-nearly-equal? r2e r2c 1e-9))
        (assert-true (vec3-nearly-equal? r3e r3c 1e-9))))

(define-test "quat-to-rotation-matrix creates valid rotation"
  (let* ([q (quat-from-axis-angle (vec3 0 0 1) (/ pi 2))]
         [m (quat-to-rotation-matrix q)]
         ;; Check that rotating (1,0,0) gives (0,1,0)
         [row0 (car m)]
         [row1 (cadr m)])
        ;; m[0][0] ≈ 0, m[1][0] ≈ 1 (first column)
        (assert-true (approx= 0 (car row0)))
        (assert-true (approx= 1 (car row1)))))

;;; ============================================================
;;; Angular Velocity Tests
;;; ============================================================

(define-test "quat-integrate rotates by angular velocity"
  (let* ([q (quat-identity)]
         [omega (vec3 0 0 pi)]  ; π rad/s around z
         [dt 1.0]
         [result (quat-integrate q omega dt)]
         [expected (quat-from-axis-angle (vec3 0 0 1) pi)])
        (assert-true (quat-nearly-equal? result expected 1e-9))))

(define-test "quat-angular-velocity recovers rotation rate"
  (let* ([q1 (quat-identity)]
         [q2 (quat-from-axis-angle (vec3 0 0 1) 1)]
         [dt 1.0]
         [omega (quat-angular-velocity q1 q2 dt)])
        (assert-true (approx= 0 (vec3-x omega)))
        (assert-true (approx= 0 (vec3-y omega)))
        (assert-true (approx= 1 (vec3-z omega)))))

;;; ============================================================
;;; Direction Tests
;;; ============================================================

(define-test "quat-forward returns rotated forward"
  (let* ([q (quat-identity)]
         [fwd (quat-forward q)])
        (assert-equal (vec3 0 0 1) fwd)))

(define-test "quat-right returns rotated right"
  (let* ([q (quat-identity)]
         [right (quat-right q)])
        (assert-equal (vec3 1 0 0) right)))

(define-test "quat-up returns rotated up"
  (let* ([q (quat-identity)]
         [up (quat-up q)])
        (assert-equal (vec3 0 1 0) up)))

;;; ============================================================
;;; Equality Tests
;;; ============================================================

(define-test "quat-equal? handles double cover"
  ;; q and -q represent the same rotation
  (let ([q (quat-from-axis-angle (vec3 0 0 1) 1)])
       (assert-true (quat-equal? q q))
       (assert-true (quat-equal? q (quat-neg q)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(display "Running Quaternion Tests...\n")
(display "==========================\n\n")
(run-tests 'quaternion)
