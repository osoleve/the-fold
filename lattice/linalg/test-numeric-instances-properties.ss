;;; lattice/linalg/test-numeric-instances-properties.ss — QuickCheck properties for numeric instances

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'vec)
(require 'matrix)
(require 'numeric-instances)

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define (approx= a b tol)
  (< (abs (- a b)) tol))

(define (finite-number? x)
  (and (number? x)
       (= x x)
       (not (= x +inf.0))
       (not (= x -inf.0))))

(define (vec-all-finite? v)
  (let loop ([xs (vec->list v)])
    (or (null? xs)
        (and (finite-number? (car xs))
             (loop (cdr xs))))))

(define (matrix-all-finite? m)
  (let loop-i ([i 0])
    (or (= i (matrix-rows m))
        (let loop-j ([j 0])
          (if (= j (matrix-cols m))
              (loop-i (+ i 1))
              (and (finite-number? (matrix-ref m i j))
                   (loop-j (+ j 1))))))))

(define gen-int-small
  (gen-int-range -20 20))

(define gen-int-nonzero
  (gen-bind (gen-int-range -20 20)
    (lambda (x)
      (if (= x 0)
          (gen-pure 1)
          (gen-pure x)))))

(define gen-same-length-pair
  (gen-bind (gen-int-range 0 20)
    (lambda (n)
      (gen-bind (gen-list-of n gen-int-small)
        (lambda (xs)
          (gen-map (lambda (ys) (cons xs ys))
                   (gen-list-of n gen-int-small)))))))

(define (gen-matrix rows cols)
  (gen-map matrix-from-lists
           (gen-list-of rows (gen-list-of cols gen-int-small))))

(define (gen-positive-matrix rows cols)
  (gen-map matrix-from-lists
           (gen-list-of rows (gen-list-of cols (gen-int-range 1 20))))
)

(define gen-same-shape-matrices
  (gen-bind (gen-int-range 0 4)
    (lambda (rows)
      (gen-bind (gen-int-range 0 4)
        (lambda (cols)
          (gen-bind (gen-matrix rows cols)
            (lambda (a)
              (gen-map (lambda (b) (cons a b))
                       (gen-matrix rows cols)))))))))

(define gen-same-shape-nonzero-matrices
  (gen-bind (gen-int-range 1 4)
    (lambda (rows)
      (gen-bind (gen-int-range 1 4)
        (lambda (cols)
          (gen-bind (gen-matrix rows cols)
            (lambda (a)
              (gen-map (lambda (b) (cons a b))
                       (gen-map matrix-from-lists
                                (gen-list-of rows
                                             (gen-list-of cols gen-int-nonzero)))))))))))

(define gen-positive-vector
  (gen-bind (gen-int-range 1 20)
    (lambda (n)
      (gen-list-of n (gen-int-range 1 20)))))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group numeric-instance-properties

  (define-property "vec+ matches vec-add"
    gen-same-length-pair
    (lambda (pair)
      (let* ([xs (car pair)]
             [ys (cdr pair)]
             [vx (vec-from-list xs)]
             [vy (vec-from-list ys)])
        (vec-equal? (vec+ vx vy)
                    (vec-add vx vy))))
    'tests 220)

  (define-property "vec-ap broadcasts a singleton function over values"
    (gen-bind (gen-int-range -8 8)
      (lambda (k)
        (gen-map (lambda (xs) (cons k xs))
                 (gen-list gen-int-small))))
    (lambda (args)
      (let* ([k (car args)]
             [xs (cdr args)]
             [vals (vec-from-list xs)]
             [applied (vec-ap (vec-pure (lambda (x) (+ x k))) vals)])
        (equal? (map (lambda (x) (+ x k)) xs)
                (vec->list applied))))
    'tests 200)

  (define-property "matrix-hadamard is element-wise multiplication"
    gen-same-shape-matrices
    (lambda (pair)
      (let* ([a (car pair)]
             [b (cdr pair)]
             [had (matrix-hadamard a b)]
             [manual (matrix-map2 * a b)])
        (matrix-equal? had manual)))
    'tests 180)

  (define-property "vec- matches vec-sub"
    gen-same-length-pair
    (lambda (pair)
      (let* ([vx (vec-from-list (car pair))]
             [vy (vec-from-list (cdr pair))])
        (vec-equal? (vec- vx vy) (vec-sub vx vy))))
    'tests 200)

  (define-property "vec* matches vec-mul"
    gen-same-length-pair
    (lambda (pair)
      (let* ([vx (vec-from-list (car pair))]
             [vy (vec-from-list (cdr pair))])
        (vec-equal? (vec* vx vy) (vec-mul vx vy))))
    'tests 200)

  (define-property "vec-from-integer fills vector with constant"
    (gen-bind (gen-int-range 0 20)
      (lambda (n)
        (gen-map (lambda (k) (list n k)) gen-int-small)))
    (lambda (args)
      (let* ([n (car args)]
             [k (cadr args)]
             [v (vec-from-integer n k)])
        (and (= (vec-length v) n)
             (equal? (make-list n k) (vec->list v))))
    )
    'tests 200)

  (define-property "vec/ and vec-recip satisfy elementwise identity"
    (gen-bind (gen-int-range 1 20)
      (lambda (n)
        (gen-bind (gen-list-of n gen-int-small)
          (lambda (xs)
            (gen-map (lambda (ys) (list xs ys))
                     (gen-list-of n gen-int-nonzero))))))
    (lambda (args)
      (let* ([vx (vec-from-list (car args))]
             [vy (vec-from-list (cadr args))]
             [quot (vec/ vx vy)]
             [reconstructed (vec* quot vy)])
        (vec-equal? reconstructed vx)))
    'tests 160)

  (define-property "vec-exp and vec-log approximately roundtrip on positive entries"
    gen-positive-vector
    (lambda (xs)
      (let* ([v (vec-from-list (map (lambda (x) (/ x 10.0)) xs))]
             [roundtrip (vec-log (vec-exp v))])
        (let loop ([a (vec->list v)] [b (vec->list roundtrip)])
          (or (null? a)
              (and (approx= (car a) (car b) 1e-10)
                   (loop (cdr a) (cdr b)))))))
    'tests 160)

  (define-property "scalar-vec+ matches explicit map"
    (gen-bind gen-int-small
      (lambda (k)
        (gen-map (lambda (xs) (list k xs))
                 (gen-list gen-int-small))))
    (lambda (args)
      (let* ([k (car args)]
             [xs (cadr args)]
             [v (vec-from-list xs)])
        (equal? (map (lambda (x) (+ k x)) xs)
                (vec->list (scalar-vec+ k v)))))
    'tests 180)

  (define-property "scalar-vec* matches vec-scale"
    (gen-bind gen-int-small
      (lambda (k)
        (gen-map (lambda (xs) (list k xs))
                 (gen-list gen-int-small))))
    (lambda (args)
      (let* ([k (car args)]
             [xs (cadr args)]
             [v (vec-from-list xs)])
        (vec-equal? (scalar-vec* k v)
                    (vec-scale k v))))
    'tests 180)

  (define-property "vec-fmap maps a fixed vector as expected"
    (gen-pure #t)
    (lambda (_)
      (let* ([v (vec-from-list '(1 2 3))]
             [mapped (vec-fmap (lambda (x) (+ x 3)) v)])
        (equal? '(4 5 6) (vec->list mapped))))
    'tests 20)

  (define-property "vec-pure creates singleton vector"
    gen-int-small
    (lambda (x)
      (let ([v (vec-pure x)])
        (and (= (vec-length v) 1)
             (= (vec-ref v 0) x))))
    'tests 120)

  (define-property "vec-lift2 with + matches vec+"
    gen-same-length-pair
    (lambda (pair)
      (let* ([vx (vec-from-list (car pair))]
             [vy (vec-from-list (cdr pair))])
        (vec-equal? (vec-lift2 + vx vy)
                    (vec+ vx vy))))
    'tests 180)

  (define-property "matrix- matches matrix-sub"
    gen-same-shape-matrices
    (lambda (pair)
      (let* ([a (car pair)]
             [b (cdr pair)])
        (matrix-equal? (matrix- a b)
                       (matrix-sub a b))))
    'tests 180)

  (define-property "matrix* alias matches matrix-hadamard"
    gen-same-shape-matrices
    (lambda (pair)
      (let* ([a (car pair)]
             [b (cdr pair)])
        (matrix-equal? (matrix* a b)
                       (matrix-hadamard a b))))
    'tests 180)

  (define-property "matrix-from-integer fills all entries"
    (gen-bind (gen-int-range 0 5)
      (lambda (rows)
        (gen-bind (gen-int-range 0 5)
          (lambda (cols)
            (gen-map (lambda (k) (list rows cols k)) gen-int-small)))))
    (lambda (args)
      (let* ([rows (car args)]
             [cols (cadr args)]
             [k (caddr args)]
             [m (matrix-from-integer rows cols k)])
        (and (= (matrix-rows m) rows)
             (= (matrix-cols m) cols)
             (let loop-i ([i 0])
               (or (= i rows)
                   (let loop-j ([j 0])
                     (if (= j cols)
                         (loop-i (+ i 1))
                         (and (= (matrix-ref m i j) k)
                              (loop-j (+ j 1)))))))))
    )
    'tests 160)

  (define-property "matrix/ and matrix* reconstruct numerator"
    gen-same-shape-nonzero-matrices
    (lambda (pair)
      (let* ([a (car pair)]
             [b (cdr pair)]
             [quot (matrix/ a b)]
             [reconstructed (matrix* quot b)])
        (matrix-equal? reconstructed a)))
    'tests 140)

  (define-property "matrix-exp and matrix-log approximately roundtrip on positive entries"
    (gen-bind (gen-int-range 1 4)
      (lambda (rows)
        (gen-bind (gen-int-range 1 4)
          (lambda (cols)
            (gen-positive-matrix rows cols)))))
    (lambda (m)
      (let ([roundtrip (matrix-log (matrix-exp m))])
        (let loop-i ([i 0])
          (or (= i (matrix-rows m))
              (let loop-j ([j 0])
                (if (= j (matrix-cols m))
                    (loop-i (+ i 1))
                    (and (approx= (matrix-ref m i j)
                                  (matrix-ref roundtrip i j)
                                  1e-10)
                         (loop-j (+ j 1)))))))))
    'tests 120)

  (define-property "scalar-matrix* matches matrix-scale"
    (gen-bind gen-int-small
      (lambda (k)
        (gen-bind (gen-int-range 0 4)
          (lambda (rows)
            (gen-bind (gen-int-range 0 4)
              (lambda (cols)
                (gen-map (lambda (m) (list k m))
                         (gen-matrix rows cols))))))))
    (lambda (args)
      (let* ([k (car args)]
             [m (cadr args)])
        (matrix-equal? (scalar-matrix* k m)
                       (matrix-scale k m))))
    'tests 160)

  (define-property "matrix-fmap equals matrix-map"
    (gen-bind (gen-int-range 0 4)
      (lambda (rows)
        (gen-bind (gen-int-range 0 4)
          (lambda (cols)
            (gen-matrix rows cols)))))
    (lambda (m)
      (matrix-equal? (matrix-fmap (lambda (x) (+ x 2)) m)
                     (matrix-map (lambda (x) (+ x 2)) m)))
    'tests 180)

  (define-property "matrix-pure fills with the provided value"
    (gen-bind (gen-int-range 0 5)
      (lambda (rows)
        (gen-bind (gen-int-range 0 5)
          (lambda (cols)
            (gen-map (lambda (x) (list rows cols x)) gen-int-small)))))
    (lambda (args)
      (let* ([rows (car args)]
             [cols (cadr args)]
             [x (caddr args)]
             [m (matrix-pure rows cols x)])
        (and (= (matrix-rows m) rows)
             (= (matrix-cols m) cols)
             (or (= (* rows cols) 0)
                 (= (matrix-ref m 0 0) x)))))
    'tests 120)

  (define-property "matrix-ap applies elementwise functions"
    (gen-bind (gen-int-range 0 4)
      (lambda (rows)
        (gen-bind (gen-int-range 0 4)
          (lambda (cols)
            (gen-bind (gen-matrix rows cols)
              (lambda (mx)
                (gen-map (lambda (k)
                           (list mx
                                 (matrix-from-lists
                                  (map (lambda (row)
                                         (map (lambda (_) (lambda (x) (+ x k))) row))
                                       (matrix->lists mx)))))
                         gen-int-small)))))))
    (lambda (args)
      (let* ([mx (car args)]
             [mf (cadr args)]
             [applied (matrix-ap mf mx)]
             [manual (matrix-map2 (lambda (f x) (f x)) mf mx)])
        (matrix-equal? applied manual)))
    'tests 120)

  (define-property "matrix-lift2 with + matches matrix+"
    gen-same-shape-matrices
    (lambda (pair)
      (let* ([a (car pair)]
             [b (cdr pair)])
        (matrix-equal? (matrix-lift2 + a b)
                       (matrix+ a b))))
    'tests 180)

  (define-property "numeric transcendental wrappers preserve shape and finiteness"
    (gen-pure #t)
    (lambda (_)
      (let* ([v-pos (vec-from-list '(1.5 2.5 3.5))]
             [v-unit (vec-from-list '(-0.8 0.2 0.6))]
             [v-acosh (vec-from-list '(1.2 2.0 3.0))]
             [v-pow (vec-from-list '(2.0 3.0 0.5))]
             [m-pos (matrix-from-lists '((1.5 2.5) (3.5 4.5)))]
             [m-unit (matrix-from-lists '((-0.8 0.2) (0.6 -0.4)))]
             [m-acosh (matrix-from-lists '((1.2 2.0) (3.0 4.0)))]
             [m-pow (matrix-from-lists '((2.0 0.5) (3.0 1.5)))]
             [v-absv (vec-abs v-unit)]
             [v-sign (vec-signum v-unit)]
             [v-pi (vec-pi 3)]
             [v-sqrtv (vec-sqrt v-pos)]
             [v-powv (vec-pow v-pos v-pow)]
             [v-sinv (vec-sin v-unit)]
             [v-cosv (vec-cos v-unit)]
             [v-tanv (vec-tan v-unit)]
             [v-asinv (vec-asin v-unit)]
             [v-acosv (vec-acos v-unit)]
             [v-atanv (vec-atan v-unit)]
             [v-sinhv (vec-sinh v-unit)]
             [v-coshv (vec-cosh v-unit)]
             [v-tanhv (vec-tanh v-unit)]
             [v-asinhv (vec-asinh v-unit)]
             [v-acoshv (vec-acosh v-acosh)]
             [v-atanhv (vec-atanh v-unit)]
             [m-neg (matrix-negate m-unit)]
             [m-absv (matrix-abs m-unit)]
             [m-sign (matrix-signum m-unit)]
             [m-recip (matrix-recip m-pos)]
             [m-pi (matrix-pi 2 2)]
             [m-sqrtv (matrix-sqrt m-pos)]
             [m-powv (matrix-pow m-pos m-pow)]
             [m-sinv (matrix-sin m-unit)]
             [m-cosv (matrix-cos m-unit)]
             [m-tanv (matrix-tan m-unit)]
             [m-asinv (matrix-asin m-unit)]
             [m-acosv (matrix-acos m-unit)]
             [m-atanv (matrix-atan m-unit)]
             [m-sinhv (matrix-sinh m-unit)]
             [m-coshv (matrix-cosh m-unit)]
             [m-tanhv (matrix-tanh m-unit)]
             [m-asinhv (matrix-asinh m-unit)]
             [m-acoshv (matrix-acosh m-acosh)]
             [m-atanhv (matrix-atanh m-unit)]
             [m-splus (scalar-matrix+ 2 m-unit)])
        (and (= (vec-length v-absv) 3)
             (= (vec-length v-sign) 3)
             (= (vec-length v-pi) 3)
             (vec-all-finite? v-sqrtv)
             (vec-all-finite? v-powv)
             (vec-all-finite? v-sinv)
             (vec-all-finite? v-cosv)
             (vec-all-finite? v-tanv)
             (vec-all-finite? v-asinv)
             (vec-all-finite? v-acosv)
             (vec-all-finite? v-atanv)
             (vec-all-finite? v-sinhv)
             (vec-all-finite? v-coshv)
             (vec-all-finite? v-tanhv)
             (vec-all-finite? v-asinhv)
             (vec-all-finite? v-acoshv)
             (vec-all-finite? v-atanhv)
             (= (matrix-rows m-neg) 2)
             (= (matrix-cols m-neg) 2)
             (matrix-all-finite? m-absv)
             (matrix-all-finite? m-sign)
             (matrix-all-finite? m-recip)
             (matrix-all-finite? m-pi)
             (matrix-all-finite? m-sqrtv)
             (matrix-all-finite? m-powv)
             (matrix-all-finite? m-sinv)
             (matrix-all-finite? m-cosv)
             (matrix-all-finite? m-tanv)
             (matrix-all-finite? m-asinv)
             (matrix-all-finite? m-acosv)
             (matrix-all-finite? m-atanv)
             (matrix-all-finite? m-sinhv)
             (matrix-all-finite? m-coshv)
             (matrix-all-finite? m-tanhv)
             (matrix-all-finite? m-asinhv)
             (matrix-all-finite? m-acoshv)
             (matrix-all-finite? m-atanhv)
             (matrix-all-finite? m-splus))))
    'tests 20)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
