;;; lattice/linalg/test-vec2-properties.ss — QuickCheck properties for 2D vectors

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'vec2)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define (approx= a b tol)
  (< (abs (- a b)) tol))

(define (nonzero-vec2? v)
  (not (vec2-zero? v)))

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-comp
  (gen-int-range -25 25))

(define gen-scalar
  (gen-int-range -10 10))

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

(define gen-vec2-int
  (gen-bind gen-comp
    (lambda (x)
      (gen-map (lambda (y) (vec2 x y))
               gen-comp))))

(define gen-vec2-pair
  (gen-pair gen-vec2-int gen-vec2-int))

(define gen-vec2-triple
  (gen-bind gen-vec2-int
    (lambda (a)
      (gen-bind gen-vec2-int
        (lambda (b)
          (gen-map (lambda (c) (list a b c))
                   gen-vec2-int))))))

;;; ============================================================================
;;; Algebraic and metric properties
;;; ============================================================================

(test-group vec2-algebra-properties

  (define-property "vec2-add is commutative"
    gen-vec2-pair
    (lambda (pair)
      (let ([a (car pair)]
            [b (cdr pair)])
        (vec2-equal? (vec2-add a b)
                     (vec2-add b a))))
    'tests 250)

  (define-property "vec2-add is associative"
    gen-vec2-triple
    (lambda (args)
      (let ([a (car args)]
            [b (cadr args)]
            [c (caddr args)])
        (vec2-equal? (vec2-add (vec2-add a b) c)
                     (vec2-add a (vec2-add b c)))))
    'tests 220)

  (define-property "(a + b) - b = a"
    gen-vec2-pair
    (lambda (pair)
      (let ([a (car pair)]
            [b (cdr pair)])
        (vec2-equal? (vec2-sub (vec2-add a b) b) a)))
    'tests 220)

  (define-property "scalar distributivity: (a+b)*k = a*k + b*k"
    (gen-bind gen-scalar
      (lambda (k)
        (gen-map (lambda (pair) (cons k pair))
                 gen-vec2-pair)))
    (lambda (args)
      (let* ([k (car args)]
             [a (car (cdr args))]
             [b (cdr (cdr args))])
        (vec2-equal? (vec2-scale (vec2-add a b) k)
                     (vec2-add (vec2-scale a k)
                               (vec2-scale b k)))))
    'tests 220)

  (define-property "dot product is commutative"
    gen-vec2-pair
    (lambda (pair)
      (let ([a (car pair)]
            [b (cdr pair)])
        (= (vec2-dot a b)
           (vec2-dot b a))))
    'tests 250)

  (define-property "cross product is anti-commutative"
    gen-vec2-pair
    (lambda (pair)
      (let ([a (car pair)]
            [b (cdr pair)])
        (= (vec2-cross a b)
           (- (vec2-cross b a)))))
    'tests 250)

  (define-property "magnitude-squared equals dot(v,v)"
    gen-vec2-int
    (lambda (v)
      (= (vec2-magnitude-sq v)
         (vec2-dot v v)))
    'tests 250)

  (define-property "distance is symmetric"
    gen-vec2-pair
    (lambda (pair)
      (let ([a (car pair)]
            [b (cdr pair)])
        (approx= (vec2-distance a b)
                 (vec2-distance b a)
                 1e-12)))
    'tests 220)
)

;;; ============================================================================
;;; Geometry-specific properties
;;; ============================================================================

(test-group vec2-geometry-properties

  (define-property "normalize returns unit vector for non-zero vectors"
    gen-vec2-int
    (lambda (v)
      (if (nonzero-vec2? v)
          (approx= (vec2-magnitude (vec2-normalize v)) 1.0 1e-9)
          #t))
    'tests 220)

  (define-property "lerp endpoints: t=0 gives a and t=1 gives b"
    gen-vec2-pair
    (lambda (pair)
      (let ([a (car pair)]
            [b (cdr pair)])
        (and (vec2-equal? (vec2-lerp a b 0.0) a)
             (vec2-equal? (vec2-lerp a b 1.0) b))))
    'tests 220)

  (define-property "projection plus rejection reconstructs original vector"
    gen-vec2-pair
    (lambda (pair)
      (let* ([a (car pair)]
             [b (cdr pair)]
             [proj (vec2-project a b)]
             [rej (vec2-reject a b)])
        (vec2-nearly-equal? (vec2-add proj rej) a 1e-9)))
    'tests 220)

  (define-property "rotation preserves magnitude"
    (gen-bind gen-vec2-int
      (lambda (v)
        (gen-map (lambda (ang) (cons v ang))
                 gen-angle)))
    (lambda (pair)
      (let* ([v (car pair)]
             [ang (cdr pair)]
             [rot (vec2-rotate v ang)])
        (approx= (vec2-magnitude rot)
                 (vec2-magnitude v)
                 1e-9)))
    'tests 220)

  (define-property "rotate-90 and rotate-neg-90 are inverses"
    gen-vec2-int
    (lambda (v)
      (vec2-nearly-equal? (vec2-rotate-neg-90 (vec2-rotate-90 v)) v 1e-12))
    'tests 220)

  (define-property "angle-to is antisymmetric"
    gen-vec2-pair
    (lambda (pair)
      (let ([a (car pair)]
            [b (cdr pair)])
        (approx= (vec2-angle-to a b)
                 (- (vec2-angle-to b a))
                 1e-12)))
    'tests 220)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
