;;; lattice/linalg/test-vec3-properties.ss — QuickCheck properties for 3D vectors

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'vec3)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define (approx= a b tol)
  (< (abs (- a b)) tol))

(define (vec3-approx=? a b tol)
  (vec3-nearly-equal? a b tol))

(define (nonzero-vec3? v)
  (not (vec3-zero? v)))

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-comp
  (gen-int-range -20 20))

(define gen-scalar
  (gen-int-range -10 10))

(define gen-nonzero-scalar
  (gen-bind (gen-int-range -10 10)
    (lambda (k)
      (if (= k 0) (gen-pure 1) (gen-pure k)))))

(define gen-t
  (gen-map (lambda (n) (/ n 100.0))
           (gen-int-range 0 100)))

(define gen-angle
  (gen-elements '(-3.141592653589793
                  -1.5707963267948966
                  -0.7853981633974483
                  0.0
                  0.7853981633974483
                  1.5707963267948966
                  3.141592653589793)))

(define gen-vec3-int
  (gen-bind gen-comp
    (lambda (x)
      (gen-bind gen-comp
        (lambda (y)
          (gen-map (lambda (z) (vec3 x y z))
                   gen-comp))))))

(define gen-vec3-pair
  (gen-pair gen-vec3-int gen-vec3-int))

(define gen-vec3-triple
  (gen-bind gen-vec3-int
    (lambda (a)
      (gen-bind gen-vec3-int
        (lambda (b)
          (gen-map (lambda (c) (list a b c))
                   gen-vec3-int))))))

(define gen-vec3-surface-args
  (gen-bind gen-vec3-int
    (lambda (a)
      (gen-bind gen-vec3-int
        (lambda (b)
          (gen-bind gen-vec3-int
            (lambda (c)
              (gen-bind gen-t
                (lambda (t)
                  (gen-bind gen-angle
                    (lambda (ang)
                      (gen-bind gen-nonzero-scalar
                        (lambda (k)
                          (gen-bind (gen-int-range 1 10)
                            (lambda (d)
                              (gen-pure (list a b c t ang k d))))))))))))))))
)

;;; ============================================================================
;;; Algebraic and product properties
;;; ============================================================================

(test-group vec3-algebra-properties

  (define-property "vec3-add is commutative"
    gen-vec3-pair
    (lambda (pair)
      (let ([a (car pair)]
            [b (cdr pair)])
        (vec3-equal? (vec3-add a b)
                     (vec3-add b a))))
    'tests 250)

  (define-property "vec3-add is associative"
    gen-vec3-triple
    (lambda (args)
      (let ([a (car args)]
            [b (cadr args)]
            [c (caddr args)])
        (vec3-equal? (vec3-add (vec3-add a b) c)
                     (vec3-add a (vec3-add b c)))))
    'tests 220)

  (define-property "(a + b) - b = a"
    gen-vec3-pair
    (lambda (pair)
      (let ([a (car pair)]
            [b (cdr pair)])
        (vec3-equal? (vec3-sub (vec3-add a b) b) a)))
    'tests 220)

  (define-property "scalar distributivity: (a+b)*k = a*k + b*k"
    (gen-bind gen-scalar
      (lambda (k)
        (gen-map (lambda (pair) (cons k pair))
                 gen-vec3-pair)))
    (lambda (args)
      (let* ([k (car args)]
             [a (car (cdr args))]
             [b (cdr (cdr args))])
        (vec3-equal? (vec3-scale (vec3-add a b) k)
                     (vec3-add (vec3-scale a k)
                               (vec3-scale b k)))))
    'tests 220)

  (define-property "dot product is commutative"
    gen-vec3-pair
    (lambda (pair)
      (let ([a (car pair)]
            [b (cdr pair)])
        (= (vec3-dot a b)
           (vec3-dot b a))))
    'tests 250)

  (define-property "cross product is anti-commutative"
    gen-vec3-pair
    (lambda (pair)
      (let* ([a (car pair)]
             [b (cdr pair)]
             [ab (vec3-cross a b)]
             [ba (vec3-cross b a)])
        (vec3-equal? ab (vec3-neg ba))))
    'tests 220)

  (define-property "cross is orthogonal to both inputs"
    gen-vec3-pair
    (lambda (pair)
      (let* ([a (car pair)]
             [b (cdr pair)]
             [c (vec3-cross a b)])
        (and (= 0 (vec3-dot c a))
             (= 0 (vec3-dot c b)))))
    'tests 220)

  (define-property "magnitude-squared equals dot(v, v)"
    gen-vec3-int
    (lambda (v)
      (= (vec3-magnitude-sq v)
         (vec3-dot v v)))
    'tests 250)
)

;;; ============================================================================
;;; Metric and geometric properties
;;; ============================================================================

(test-group vec3-geometry-properties

  (define-property "distance is symmetric"
    gen-vec3-pair
    (lambda (pair)
      (let ([a (car pair)]
            [b (cdr pair)])
        (approx= (vec3-distance a b)
                 (vec3-distance b a)
                 1e-12)))
    'tests 220)

  (define-property "normalize returns unit vector for non-zero vectors"
    gen-vec3-int
    (lambda (v)
      (if (nonzero-vec3? v)
          (approx= (vec3-magnitude (vec3-normalize v)) 1.0 1e-9)
          #t))
    'tests 220)

  (define-property "lerp endpoints: t=0 gives a and t=1 gives b"
    gen-vec3-pair
    (lambda (pair)
      (let ([a (car pair)]
            [b (cdr pair)])
        (and (vec3-equal? (vec3-lerp a b 0.0) a)
             (vec3-equal? (vec3-lerp a b 1.0) b))))
    'tests 220)

  (define-property "projection plus rejection reconstructs original vector"
    gen-vec3-pair
    (lambda (pair)
      (let* ([a (car pair)]
             [b (cdr pair)]
             [proj (vec3-project a b)]
             [rej (vec3-reject a b)])
        (vec3-approx=? (vec3-add proj rej) a 1e-9)))
    'tests 220)

  (define-property "rotation around X preserves magnitude"
    (gen-bind gen-vec3-int
      (lambda (v)
        (gen-map (lambda (ang) (cons v ang))
                 gen-angle)))
    (lambda (pair)
      (let* ([v (car pair)]
             [ang (cdr pair)]
             [rot (vec3-rotate-x v ang)])
        (approx= (vec3-magnitude rot)
                 (vec3-magnitude v)
                 1e-9)))
    'tests 200)

  (define-property "nlerp output is unit for non-opposite unit inputs"
    (gen-bind gen-vec3-int
      (lambda (a)
        (gen-bind gen-vec3-int
          (lambda (b)
            (gen-map (lambda (t) (list a b t))
                     gen-t)))))
    (lambda (args)
      (let* ([a0 (car args)]
             [b0 (cadr args)]
             [t (caddr args)]
             [a (vec3-normalize a0)]
             [b (vec3-normalize b0)]
             [sum (vec3-add a b)])
        (if (or (vec3-zero? a) (vec3-zero? b) (vec3-zero? sum))
            #t
            (approx= (vec3-magnitude (vec3-nlerp a b t))
                     1.0
                     1e-7))))
    'tests 180)

  (define-property "vec3 surface helper API returns consistent results"
    gen-vec3-surface-args
    (lambda (args)
      (let* ([a (car args)]
             [b (cadr args)]
             [c (caddr args)]
             [t (list-ref args 3)]
             [ang (list-ref args 4)]
             [k (list-ref args 5)]
             [d (list-ref args 6)]
             [safe-b (vec3 (if (= (vec3-x b) 0) 1 (vec3-x b))
                           (if (= (vec3-y b) 0) 1 (vec3-y b))
                           (if (= (vec3-z b) 0) 1 (vec3-z b)))]
             [ua (if (nonzero-vec3? a) (vec3-normalize a) (vec3-unit-x))]
             [ub (if (nonzero-vec3? b) (vec3-normalize b) (vec3-unit-y))]
             [lst (vec3->list a)]
             [a2 (list->vec3 lst)]
             [one (vec3-one)]
             [ux (vec3-unit-x)]
             [uy (vec3-unit-y)]
             [uz (vec3-unit-z)]
             [mul (vec3-mul a safe-b)]
             [div-back (vec3-div mul safe-b)]
             [scaled-inv (vec3-scale-inv (vec3-scale a k) k)]
             [absv (vec3-abs a)]
             [floorv (vec3-floor a)]
             [ceilv (vec3-ceil a)]
             [minv (vec3-min a b)]
             [maxv (vec3-max a b)]
             [clamped (vec3-clamp a (vec3 -2 -2 -2) (vec3 2 2 2))]
             [setmag (if (nonzero-vec3? a) (vec3-set-magnitude a d) (vec3-zero))]
             [clampmag (vec3-clamp-magnitude a 0 d)]
             [norm-or (vec3-normalize-or (vec3-zero) ux)]
             [rot-y (vec3-rotate-y a ang)]
             [rot-z (vec3-rotate-z a ang)]
             [rot-axis (vec3-rotate-axis a ua ang)]
             [slerped (vec3-slerp ua ub t)]
             [basis (vec3-orthonormal-basis ua)]
             [gs (vec3-gram-schmidt ua ub)]
             [refl (vec3-reflect a ua)]
             [refr (vec3-refract ua ub 0.8)]
             [triple-s (vec3-triple-scalar a b c)]
             [triple-s-def (vec3-dot a (vec3-cross b c))]
             [triple-v (vec3-triple-vector a b c)]
             [triple-v-def (vec3-sub (vec3-scale b (vec3-dot a c))
                                     (vec3-scale c (vec3-dot a b)))]
             [sph (vec3->spherical ua)]
             [cyl (vec3->cylindrical ua)]
             [back-sph (vec3-from-spherical (car sph) (cadr sph) (caddr sph))]
             [back-cyl (vec3-from-cylindrical (car cyl) (cadr cyl) (caddr cyl))])
        (and (vec3-equal? a a2)
             (= (vec3-ref a 0) (vec3-x a))
             (= (vec3-ref a 1) (vec3-y a))
             (vec3-equal? one (vec3 1 1 1))
             (= (vec3-x ux) 1) (= (vec3-y ux) 0)
             (= (vec3-y uy) 1)
             (= (vec3-z uz) 1)
             (approx= (vec3-length a) (vec3-magnitude a) 1e-12)
             (approx= (vec3-distance-sq a b)
                      (* (vec3-distance a b) (vec3-distance a b))
                      1e-8)
             (vec3-nearly-equal? div-back a 1e-9)
             (vec3-nearly-equal? scaled-inv a 1e-9)
             (<= (vec3-x floorv) (vec3-x a))
             (>= (vec3-y ceilv) (vec3-y a))
             (<= (vec3-x minv) (vec3-x maxv))
             (<= (vec3-y minv) (vec3-y maxv))
             (<= (vec3-z minv) (vec3-z maxv))
             (<= (vec3-x clamped) 2) (>= (vec3-x clamped) -2)
             (<= (vec3-y clamped) 2) (>= (vec3-y clamped) -2)
             (<= (vec3-z clamped) 2) (>= (vec3-z clamped) -2)
             (or (not (nonzero-vec3? a))
                 (approx= (vec3-magnitude setmag) d 1e-8))
             (<= (vec3-magnitude clampmag) (+ d 1e-8))
             (vec3-equal? norm-or ux)
             (approx= (vec3-magnitude rot-y) (vec3-magnitude a) 1e-8)
             (approx= (vec3-magnitude rot-z) (vec3-magnitude a) 1e-8)
             (approx= (vec3-magnitude rot-axis) (vec3-magnitude a) 1e-8)
             (vec3? slerped)
             (= (length basis) 3)
             (= (length gs) 2)
             (vec3? refl)
             (or (not refr) (vec3? refr))
             (approx= triple-s triple-s-def 1e-9)
             (vec3-nearly-equal? triple-v triple-v-def 1e-9)
             (vec3? absv)
             (approx= (vec3-angle ua ua) 0.0 1e-8)
             (number? (vec3-signed-angle ua ub ua))
             (vec3-nearly-equal? back-sph ua 1e-8)
             (vec3-nearly-equal? back-cyl ua 1e-8)
             (vec3-unit? ua))))
    'tests 200)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
