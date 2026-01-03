;;; core/linalg/test-vec3.ss — Tests for 3D Vector Library
;;;
;;; Run: scheme --script core/linalg/test-vec3.ss

(load "core/test-framework.ss")
(load "core/linalg/vec3.ss")

(set! *current-group* 'vec3)

;;; Helper for approximate equality
(define (approx= a b)
  (< (abs (- a b)) 1e-9))

(define pi 3.141592653589793)

;;; ============================================================
;;; Construction Tests
;;; ============================================================

(define-test "vec3 creates 3D vector"
  (let ([v (vec3 1 2 3)])
       (assert-equal 1 (vec3-x v))
       (assert-equal 2 (vec3-y v))
       (assert-equal 3 (vec3-z v))))

(define-test "vec3-zero creates zero vector"
  (let ([v (vec3-zero)])
       (assert-equal 0 (vec3-x v))
       (assert-equal 0 (vec3-y v))
       (assert-equal 0 (vec3-z v))))

(define-test "unit vectors are correct"
  (assert-equal (vec3 1 0 0) (vec3-unit-x))
  (assert-equal (vec3 0 1 0) (vec3-unit-y))
  (assert-equal (vec3 0 0 1) (vec3-unit-z)))

(define-test "vec3-from-spherical creates correct vector"
  (let ([v (vec3-from-spherical 1 (/ pi 2) 0)])
       (assert-true (approx= 1 (vec3-x v)))
       (assert-true (approx= 0 (vec3-y v)))
       (assert-true (approx= 0 (vec3-z v)))))

;;; ============================================================
;;; Predicate Tests
;;; ============================================================

(define-test "vec3? identifies 3D vectors"
  (assert-true (vec3? (vec3 1 2 3)))
  (assert-false (vec3? '(1 2 3)))
  (assert-false (vec3? 42)))

(define-test "vec3-zero? identifies zero vectors"
  (assert-true (vec3-zero? (vec3-zero)))
  (assert-true (vec3-zero? (vec3 0 0 0)))
  (assert-false (vec3-zero? (vec3 1 0 0))))

(define-test "vec3-unit? identifies unit vectors"
  (assert-true (vec3-unit? (vec3-unit-x)))
  (assert-true (vec3-unit? (vec3-normalize (vec3 1 1 1))))
  (assert-false (vec3-unit? (vec3 2 0 0))))

;;; ============================================================
;;; Arithmetic Tests
;;; ============================================================

(define-test "vec3-add adds vectors"
  (let ([v (vec3-add (vec3 1 2 3) (vec3 4 5 6))])
       (assert-equal (vec3 5 7 9) v)))

(define-test "vec3-sub subtracts vectors"
  (let ([v (vec3-sub (vec3 4 5 6) (vec3 1 2 3))])
       (assert-equal (vec3 3 3 3) v)))

(define-test "vec3-neg negates vector"
  (assert-equal (vec3 -1 -2 -3) (vec3-neg (vec3 1 2 3))))

(define-test "vec3-scale scales vector"
  (assert-equal (vec3 2 4 6) (vec3-scale (vec3 1 2 3) 2)))

(define-test "vec3-mul does component-wise multiplication"
  (assert-equal (vec3 4 10 18) (vec3-mul (vec3 1 2 3) (vec3 4 5 6))))

;;; ============================================================
;;; Product Tests
;;; ============================================================

(define-test "vec3-dot computes dot product"
  (assert-equal 32 (vec3-dot (vec3 1 2 3) (vec3 4 5 6))))

(define-test "vec3-dot of orthogonal vectors is zero"
  (assert-equal 0 (vec3-dot (vec3-unit-x) (vec3-unit-y))))

(define-test "vec3-cross computes cross product"
  ;; i × j = k
  (assert-equal (vec3 0 0 1) (vec3-cross (vec3-unit-x) (vec3-unit-y)))
  ;; j × k = i
  (assert-equal (vec3 1 0 0) (vec3-cross (vec3-unit-y) (vec3-unit-z)))
  ;; k × i = j
  (assert-equal (vec3 0 1 0) (vec3-cross (vec3-unit-z) (vec3-unit-x))))

(define-test "vec3-cross is anticommutative"
  (let* ([a (vec3 1 2 3)]
         [b (vec3 4 5 6)]
         [ab (vec3-cross a b)]
         [ba (vec3-cross b a)])
        (assert-equal ab (vec3-neg ba))))

(define-test "vec3-triple-scalar computes scalar triple product"
  ;; Volume of unit cube = 1
  (assert-equal 1 (vec3-triple-scalar (vec3-unit-x) (vec3-unit-y) (vec3-unit-z))))

;;; ============================================================
;;; Length and Distance Tests
;;; ============================================================

(define-test "vec3-magnitude computes length"
  (assert-equal 5 (vec3-magnitude (vec3 3 4 0)))
  (assert-true (approx= (sqrt 3) (vec3-magnitude (vec3-one)))))

(define-test "vec3-magnitude-sq computes squared length"
  (assert-equal 25 (vec3-magnitude-sq (vec3 3 4 0))))

(define-test "vec3-distance computes distance"
  (assert-equal 5 (vec3-distance (vec3 0 0 0) (vec3 3 4 0))))

;;; ============================================================
;;; Normalization Tests
;;; ============================================================

(define-test "vec3-normalize creates unit vector"
  (let ([v (vec3-normalize (vec3 3 4 0))])
       (assert-true (approx= 1.0 (vec3-magnitude v)))
       (assert-true (approx= 0.6 (vec3-x v)))
       (assert-true (approx= 0.8 (vec3-y v)))))

(define-test "vec3-normalize of zero returns zero"
  (assert-equal (vec3-zero) (vec3-normalize (vec3-zero))))

(define-test "vec3-set-magnitude scales to given length"
  (let ([v (vec3-set-magnitude (vec3 1 0 0) 5)])
       (assert-true (approx= 5 (vec3-magnitude v)))))

;;; ============================================================
;;; Interpolation Tests
;;; ============================================================

(define-test "vec3-lerp interpolates linearly"
  (let* ([a (vec3 0 0 0)]
         [b (vec3 10 10 10)]
         [mid (vec3-lerp a b 0.5)])
        (assert-true (vec3-nearly-equal? mid (vec3 5 5 5) 1e-9))))

(define-test "vec3-lerp at endpoints"
  (let ([a (vec3 1 2 3)]
        [b (vec3 4 5 6)])
       (assert-equal a (vec3-lerp a b 0))
       (assert-equal b (vec3-lerp a b 1))))

(define-test "vec3-slerp interpolates on sphere"
  (let* ([a (vec3-unit-x)]
         [b (vec3-unit-y)]
         [mid (vec3-slerp a b 0.5)])
        (assert-true (approx= 1.0 (vec3-magnitude mid)))
        (assert-true (approx= (vec3-x mid) (vec3-y mid)))))

;;; ============================================================
;;; Projection and Reflection Tests
;;; ============================================================

(define-test "vec3-project projects onto vector"
  (let ([proj (vec3-project (vec3 1 1 0) (vec3-unit-x))])
       (assert-equal (vec3 1 0 0) proj)))

(define-test "vec3-reject computes perpendicular component"
  (let ([rej (vec3-reject (vec3 1 1 0) (vec3-unit-x))])
       (assert-equal (vec3 0 1 0) rej)))

(define-test "vec3-reflect reflects across normal"
  (let* ([v (vec3 1 -1 0)]
         [n (vec3-unit-y)]
         [r (vec3-reflect v n)])
        (assert-true (approx= 1 (vec3-x r)))
        (assert-true (approx= 1 (vec3-y r)))))

;;; ============================================================
;;; Rotation Tests
;;; ============================================================

(define-test "vec3-rotate-x rotates around x-axis"
  (let ([v (vec3-rotate-x (vec3-unit-y) (/ pi 2))])
       (assert-true (approx= 0 (vec3-x v)))
       (assert-true (approx= 0 (vec3-y v)))
       (assert-true (approx= 1 (vec3-z v)))))

(define-test "vec3-rotate-y rotates around y-axis"
  (let ([v (vec3-rotate-y (vec3-unit-z) (/ pi 2))])
       (assert-true (approx= 1 (vec3-x v)))
       (assert-true (approx= 0 (vec3-y v)))
       (assert-true (approx= 0 (vec3-z v)))))

(define-test "vec3-rotate-z rotates around z-axis"
  (let ([v (vec3-rotate-z (vec3-unit-x) (/ pi 2))])
       (assert-true (approx= 0 (vec3-x v)))
       (assert-true (approx= 1 (vec3-y v)))
       (assert-true (approx= 0 (vec3-z v)))))

(define-test "vec3-rotate-axis rotates around arbitrary axis"
  (let ([v (vec3-rotate-axis (vec3-unit-x) (vec3-unit-z) (/ pi 2))])
       (assert-true (approx= 0 (vec3-x v)))
       (assert-true (approx= 1 (vec3-y v)))
       (assert-true (approx= 0 (vec3-z v)))))

;;; ============================================================
;;; Angle Tests
;;; ============================================================

(define-test "vec3-angle computes angle between vectors"
  (assert-true (approx= (/ pi 2) (vec3-angle (vec3-unit-x) (vec3-unit-y))))
  (assert-true (approx= 0 (vec3-angle (vec3-unit-x) (vec3-unit-x))))
  (assert-true (approx= pi (vec3-angle (vec3-unit-x) (vec3-neg (vec3-unit-x))))))

;;; ============================================================
;;; Utility Tests
;;; ============================================================

(define-test "vec3-min computes component-wise minimum"
  (assert-equal (vec3 1 2 3) (vec3-min (vec3 1 5 3) (vec3 4 2 6))))

(define-test "vec3-max computes component-wise maximum"
  (assert-equal (vec3 4 5 6) (vec3-max (vec3 1 5 3) (vec3 4 2 6))))

(define-test "vec3-abs takes absolute value"
  (assert-equal (vec3 1 2 3) (vec3-abs (vec3 -1 -2 -3))))

(define-test "vec3-clamp clamps components"
  (let ([v (vec3-clamp (vec3 -1 5 2) (vec3 0 0 0) (vec3 3 3 3))])
       (assert-equal (vec3 0 3 2) v)))

;;; ============================================================
;;; Coordinate Conversion Tests
;;; ============================================================

(define-test "vec3->spherical and back"
  (let* ([v (vec3 1 2 3)]
         [sph (vec3->spherical v)]
         [r (car sph)]
         [theta (cadr sph)]
         [phi (caddr sph)]
         [v2 (vec3-from-spherical r theta phi)])
        (assert-true (vec3-nearly-equal? v v2 1e-9))))

(define-test "vec3->cylindrical and back"
  (let* ([v (vec3 1 2 3)]
         [cyl (vec3->cylindrical v)]
         [r (car cyl)]
         [theta (cadr cyl)]
         [z (caddr cyl)]
         [v2 (vec3-from-cylindrical r theta z)])
        (assert-true (vec3-nearly-equal? v v2 1e-9))))

;;; ============================================================
;;; Basis Tests
;;; ============================================================

(define-test "vec3-orthonormal-basis creates orthonormal vectors"
  (let* ([basis (vec3-orthonormal-basis (vec3 1 1 1))]
         [t1 (car basis)]
         [t2 (cadr basis)]
         [n (caddr basis)])
        ;; All unit length
        (assert-true (approx= 1 (vec3-magnitude t1)))
        (assert-true (approx= 1 (vec3-magnitude t2)))
        (assert-true (approx= 1 (vec3-magnitude n)))
        ;; All orthogonal
        (assert-true (approx= 0 (vec3-dot t1 t2)))
        (assert-true (approx= 0 (vec3-dot t1 n)))
        (assert-true (approx= 0 (vec3-dot t2 n)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(display "Running Vec3 Tests...\n")
(display "=====================\n\n")
(run-tests 'vec3)
