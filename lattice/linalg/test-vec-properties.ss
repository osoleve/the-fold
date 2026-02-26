;;; lattice/linalg/test-vec-properties.ss — QuickCheck properties for generic vectors

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'vec)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define (error-result? x)
  (and (pair? x) (eq? (car x) 'error)))

(define (approx= a b tol)
  (< (abs (- a b)) tol))

(define (list->vec-int xs)
  (vec-from-list xs))

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-len
  (gen-int-range 0 20))

(define gen-nonempty-len
  (gen-int-range 1 20))

(define gen-int-small
  (gen-int-range -50 50))

(define gen-scalar
  (gen-int-range -10 10))

(define gen-int-list
  (gen-list gen-int-small))

(define gen-same-length-pair
  (gen-bind gen-len
    (lambda (n)
      (gen-bind (gen-list-of n gen-int-small)
        (lambda (xs)
          (gen-map (lambda (ys) (list xs ys))
                   (gen-list-of n gen-int-small)))))))

(define gen-same-length-triple
  (gen-bind gen-len
    (lambda (n)
      (gen-bind (gen-list-of n gen-int-small)
        (lambda (xs)
          (gen-bind (gen-list-of n gen-int-small)
            (lambda (ys)
              (gen-map (lambda (zs) (list xs ys zs))
                       (gen-list-of n gen-int-small)))))))))

(define gen-split-args
  (gen-bind gen-int-list
    (lambda (xs)
      (let ([n (length xs)])
        (gen-map (lambda (k) (cons xs k))
                 (gen-int-range 0 n))))))

(define gen-vec-surface-args
  (gen-bind gen-nonempty-len
    (lambda (n)
      (gen-bind (gen-list-of n gen-int-small)
        (lambda (xs)
          (gen-bind (gen-list-of n gen-int-small)
            (lambda (ys)
              (gen-bind (gen-int-range 0 (- n 1))
                (lambda (k)
                  (gen-map (lambda (p) (list xs ys k p))
                           (gen-int-range 0 100)))))))))))

;;; ============================================================================
;;; Representation and structure
;;; ============================================================================

(test-group vec-structure-properties

  (define-property "vec-from-list and vec->list roundtrip"
    gen-int-list
    (lambda (xs)
      (equal? xs (vec->list (vec-from-list xs))))
    'tests 250)

  (define-property "reverse is involutive"
    gen-int-list
    (lambda (xs)
      (let* ([v (list->vec-int xs)]
             [rr (vec-reverse (vec-reverse v))])
        (vec-equal? rr v)))
    'tests 220)

  (define-property "take/drop partition reconstructs the vector"
    gen-split-args
    (lambda (pair)
      (let* ([xs (car pair)]
             [k (cdr pair)]
             [v (list->vec-int xs)]
             [left (vec-take v k)]
             [right (vec-drop v k)])
        (and (not (error-result? left))
             (not (error-result? right))
             (vec-equal? (vec-append left right) v))))
    'tests 220)
)

;;; ============================================================================
;;; Algebraic properties
;;; ============================================================================

(test-group vec-algebra-properties

  (define-property "vec-add is commutative"
    gen-same-length-pair
    (lambda (args)
      (let* ([x (list->vec-int (car args))]
             [y (list->vec-int (cadr args))]
             [xy (vec-add x y)]
             [yx (vec-add y x)])
        (and (not (error-result? xy))
             (not (error-result? yx))
             (vec-equal? xy yx))))
    'tests 220)

  (define-property "vec-add is associative"
    gen-same-length-triple
    (lambda (args)
      (let* ([x (list->vec-int (car args))]
             [y (list->vec-int (cadr args))]
             [z (list->vec-int (caddr args))]
             [xy (vec-add x y)]
             [yz (vec-add y z)]
             [lhs (if (error-result? xy) xy (vec-add xy z))]
             [rhs (if (error-result? yz) yz (vec-add x yz))])
        (and (not (error-result? lhs))
             (not (error-result? rhs))
             (vec-equal? lhs rhs))))
    'tests 200)

  (define-property "(x + y) - y = x"
    gen-same-length-pair
    (lambda (args)
      (let* ([x (list->vec-int (car args))]
             [y (list->vec-int (cadr args))]
             [sum (vec-add x y)]
             [back (if (error-result? sum) sum (vec-sub sum y))])
        (and (not (error-result? back))
             (vec-equal? back x))))
    'tests 220)

  (define-property "scalar distributivity: k*(x+y) = k*x + k*y"
    (gen-bind gen-scalar
      (lambda (k)
        (gen-map (lambda (xy) (cons k xy))
                 gen-same-length-pair)))
    (lambda (args)
      (let* ([k (car args)]
             [x (list->vec-int (car (cdr args)))]
             [y (list->vec-int (cadr (cdr args)))]
             [sum (vec-add x y)]
             [lhs (if (error-result? sum) sum (vec-scale k sum))]
             [rhs (vec-add (vec-scale k x) (vec-scale k y))])
        (and (not (error-result? lhs))
             (not (error-result? rhs))
             (vec-equal? lhs rhs))))
    'tests 200)
)

;;; ============================================================================
;;; Dot products, norms, and distances
;;; ============================================================================

(test-group vec-metric-properties

  (define-property "dot product is commutative"
    gen-same-length-pair
    (lambda (args)
      (let* ([x (list->vec-int (car args))]
             [y (list->vec-int (cadr args))]
             [xy (vec-dot x y)]
             [yx (vec-dot y x)])
        (and (not (error-result? xy))
             (not (error-result? yx))
             (= xy yx))))
    'tests 220)

  (define-property "dot is linear in first argument"
    gen-same-length-triple
    (lambda (args)
      (let* ([x (list->vec-int (car args))]
             [y (list->vec-int (cadr args))]
             [z (list->vec-int (caddr args))]
             [x+y (vec-add x y)]
             [lhs (if (error-result? x+y) x+y (vec-dot x+y z))]
             [xz (vec-dot x z)]
             [yz (vec-dot y z)])
        (and (not (error-result? lhs))
             (not (error-result? xz))
             (not (error-result? yz))
             (= lhs (+ xz yz)))))
    'tests 200)

  (define-property "norm-squared equals dot(v,v)"
    gen-int-list
    (lambda (xs)
      (let* ([v (list->vec-int xs)]
             [n2 (vec-norm-squared v)]
             [dd (vec-dot v v)])
        (and (not (error-result? n2))
             (not (error-result? dd))
             (= n2 dd))))
    'tests 220)

  (define-property "distance is symmetric"
    gen-same-length-pair
    (lambda (args)
      (let* ([x (list->vec-int (car args))]
             [y (list->vec-int (cadr args))]
             [dxy (vec-distance x y)]
             [dyx (vec-distance y x)])
        (and (not (error-result? dxy))
             (not (error-result? dyx))
             (approx= dxy dyx 1e-12))))
    'tests 220)

  (define-property "L_inf norm is <= L1 norm on non-empty vectors"
    (gen-bind gen-nonempty-len
      (lambda (n)
        (gen-list-of n gen-int-small)))
    (lambda (xs)
      (let* ([v (list->vec-int xs)]
             [linf (vec-norm-linf v)]
             [l1 (vec-norm-l1 v)])
        (and (not (error-result? linf))
             (<= linf l1))))
    'tests 200)

  (define-property "vec surface helper API returns consistent results"
    gen-vec-surface-args
    (lambda (args)
      (let* ([xs (car args)]
             [ys (cadr args)]
             [k (caddr args)]
             [p (cadddr args)]
             [vx (list->vec-int xs)]
             [vy (list->vec-int ys)]
             [safe-y (vec-map (lambda (x) (if (= x 0) 1 x)) vy)]
             [zip (vec-zip-with + vx vy)]
             [add (vec-add vx vy)]
             [mul (vec-mul vx safe-y)]
             [div-back (vec-div mul safe-y)]
             [copy (vec-copy vx)]
             [first (vec-first vx)]
             [last (vec-last vx)]
             [empty? (vec-empty? (vec-zeros 0))]
             [mean (vec-mean vx)]
             [sum (vec-sum vx)]
             [sum-right (vec-fold-right (lambda (x acc) (+ x acc)) 0 vx)]
             [prod (vec-product vx)]
             [prod-list (fold-left * 1 xs)]
             [minv (vec-min vx)]
             [maxv (vec-max vx)]
             [imin (vec-argmin vx)]
             [imax (vec-argmax vx)]
             [negx (vec-negate vx)]
             [slice (vec-slice vx 0 (vec-length vx))]
             [lin (vec-linspace -2 2 (+ 2 k))]
             [ones (vec-ones (vec-length vx))]
             [zeros (vec-zeros (vec-length vx))]
             [rng (vec-range (vec-length vx))]
             [unit (vec-unit (vec-length vx) k)]
             [normed (vec-normalize vx)])
        (and (not (error-result? zip))
             (vec-equal? zip add)
             (vec-approx-equal? div-back vx 1e-10)
             (vec-equal? copy vx)
             (= first (car xs))
             (= last (car (reverse xs)))
             empty?
             (= sum-right sum)
             (= prod prod-list)
             (approx= mean (/ sum (vec-length vx)) 1e-12)
             (vec-equal? (vec-add vx negx) (vec-zeros (vec-length vx)))
             (= minv (vector-ref vx imin))
             (= maxv (vector-ref vx imax))
             (>= imin 0) (< imin (vec-length vx))
             (>= imax 0) (< imax (vec-length vx))
             (vec-equal? slice vx)
             (= (vec-ref lin 0) -2)
             (= (vec-ref lin (- (vec-length lin) 1)) 2)
             (= (vec-sum ones) (vec-length vx))
             (= (vec-sum zeros) 0)
             (= (vec-ref rng 0) 0)
             (= (vec-ref unit k) 1)
             (or (error-result? normed)
                 (approx= (vec-norm normed) 1.0 1e-9)))))
    'tests 220)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
