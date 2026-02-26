;;; lattice/linalg/test-matrix-properties.ss — QuickCheck properties for matrices

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'matrix)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define (approx= a b tol)
  (< (abs (- a b)) tol))

(define (matrix-error? x)
  (and (pair? x) (eq? (car x) 'error)))

(define (v+ a b)
  (let* ([n (vector-length a)]
         [out (make-vector n 0)])
    (do ([i 0 (+ i 1)])
        ((= i n) out)
        (vector-set! out i (+ (vector-ref a i) (vector-ref b i))))))

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-entry
  (gen-int-range -10 10))

(define gen-small-dim
  (gen-int-range 0 5))

(define gen-pos-dim
  (gen-int-range 1 5))

(define (gen-matrix rows cols)
  (gen-map matrix-from-lists
           (gen-list-of rows (gen-list-of cols gen-entry))))

(define gen-rect-lists
  (gen-bind gen-small-dim
    (lambda (rows)
      (gen-bind gen-small-dim
        (lambda (cols)
          (gen-list-of rows (gen-list-of cols gen-entry)))))))

(define gen-same-shape-pair
  (gen-bind gen-small-dim
    (lambda (rows)
      (gen-bind gen-small-dim
        (lambda (cols)
          (gen-bind (gen-matrix rows cols)
            (lambda (a)
              (gen-map (lambda (b) (cons a b))
                       (gen-matrix rows cols)))))))))

(define gen-same-shape-triple
  (gen-bind gen-small-dim
    (lambda (rows)
      (gen-bind gen-small-dim
        (lambda (cols)
          (gen-bind (gen-matrix rows cols)
            (lambda (a)
              (gen-bind (gen-matrix rows cols)
                (lambda (b)
                  (gen-map (lambda (c) (list a b c))
                           (gen-matrix rows cols)))))))))))

(define gen-mul-compatible-pair
  (gen-bind gen-pos-dim
    (lambda (m)
      (gen-bind gen-pos-dim
        (lambda (n)
          (gen-bind gen-pos-dim
            (lambda (p)
              (gen-bind (gen-matrix m n)
                (lambda (a)
                  (gen-map (lambda (b) (cons a b))
                           (gen-matrix n p)))))))))))

(define gen-mul-compatible-triple
  (gen-bind gen-pos-dim
    (lambda (m)
      (gen-bind gen-pos-dim
        (lambda (n)
          (gen-bind gen-pos-dim
            (lambda (p)
              (gen-bind gen-pos-dim
                (lambda (q)
                  (gen-bind (gen-matrix m n)
                    (lambda (a)
                      (gen-bind (gen-matrix n p)
                        (lambda (b)
                          (gen-map (lambda (c) (list a b c))
                                   (gen-matrix p q)))))))))))))))

(define gen-matrix-vector-compatible
  (gen-bind gen-pos-dim
    (lambda (rows)
      (gen-bind gen-pos-dim
        (lambda (cols)
          (gen-bind (gen-matrix rows cols)
            (lambda (a)
              (gen-bind (gen-list-of cols gen-entry)
                (lambda (xs)
                  (gen-map (lambda (ys)
                             (list a (list->vector xs) (list->vector ys)))
                           (gen-list-of cols gen-entry)))))))))))

;;; ============================================================================
;;; Structural properties
;;; ============================================================================

(test-group matrix-structure-properties

  (define-property "matrix-from-lists and matrix->lists roundtrip on rectangular input"
    gen-rect-lists
    (lambda (rows)
      (let ([m (matrix-from-lists rows)])
        (and (not (matrix-error? m))
             (equal? rows (matrix->lists m)))))
    'tests 220)

  (define-property "transpose is involutive"
    (gen-bind gen-small-dim
      (lambda (rows)
        (gen-bind gen-small-dim
          (lambda (cols)
            (gen-matrix rows cols)))))
    (lambda (a)
      (matrix-equal? (matrix-transpose (matrix-transpose a)) a))
    'tests 220)

  (define-property "transpose swaps shape"
    (gen-bind gen-small-dim
      (lambda (rows)
        (gen-bind gen-small-dim
          (lambda (cols)
            (gen-matrix rows cols)))))
    (lambda (a)
      (let ([at (matrix-transpose a)])
        (and (= (matrix-rows at) (matrix-cols a))
             (= (matrix-cols at) (matrix-rows a)))))
    'tests 220)
)

;;; ============================================================================
;;; Algebraic properties
;;; ============================================================================

(test-group matrix-algebra-properties

  (define-property "matrix-add is commutative"
    gen-same-shape-pair
    (lambda (pair)
      (let ([a (car pair)]
            [b (cdr pair)])
        (matrix-equal? (matrix-add a b)
                       (matrix-add b a))))
    'tests 220)

  (define-property "matrix-add is associative"
    gen-same-shape-triple
    (lambda (args)
      (let ([a (car args)]
            [b (cadr args)]
            [c (caddr args)])
        (matrix-equal? (matrix-add (matrix-add a b) c)
                       (matrix-add a (matrix-add b c)))))
    'tests 200)

  (define-property "(A + B) - B = A"
    gen-same-shape-pair
    (lambda (pair)
      (let ([a (car pair)]
            [b (cdr pair)])
        (matrix-equal? (matrix-sub (matrix-add a b) b) a)))
    'tests 220)

  (define-property "A * (B + C) = A*B + A*C"
    (gen-bind gen-pos-dim
      (lambda (m)
        (gen-bind gen-pos-dim
          (lambda (n)
            (gen-bind gen-pos-dim
              (lambda (p)
                (gen-bind (gen-matrix m n)
                  (lambda (a)
                    (gen-bind (gen-matrix n p)
                      (lambda (b)
                        (gen-map (lambda (c) (list a b c))
                                 (gen-matrix n p))))))))))))
    (lambda (args)
      (let* ([a (car args)]
             [b (cadr args)]
             [c (caddr args)]
             [b+c (matrix-add b c)]
             [lhs (matrix-mul a b+c)]
             [ab (matrix-mul a b)]
             [ac (matrix-mul a c)]
             [rhs (matrix-add ab ac)])
        (and (not (matrix-error? lhs))
             (not (matrix-error? rhs))
             (matrix-equal? lhs rhs))))
    'tests 180)

  (define-property "matrix multiplication is associative"
    gen-mul-compatible-triple
    (lambda (args)
      (let* ([a (car args)]
             [b (cadr args)]
             [c (caddr args)]
             [ab (matrix-mul a b)]
             [bc (matrix-mul b c)]
             [lhs (if (matrix-error? ab) ab (matrix-mul ab c))]
             [rhs (if (matrix-error? bc) bc (matrix-mul a bc))])
        (and (not (matrix-error? lhs))
             (not (matrix-error? rhs))
             (matrix-equal? lhs rhs))))
    'tests 150)

  (define-property "identity is multiplicative neutral element on square matrices"
    (gen-bind (gen-int-range 0 5)
      (lambda (n)
        (gen-matrix n n)))
    (lambda (a)
      (let* ([n (matrix-rows a)]
             [i (matrix-identity n)]
             [left (matrix-mul i a)]
             [right (matrix-mul a i)])
        (and (matrix-equal? left a)
             (matrix-equal? right a))))
    'tests 200)
)

;;; ============================================================================
;;; Numeric and operator consistency
;;; ============================================================================

(test-group matrix-numeric-properties

  (define-property "trace is linear"
    (gen-bind (gen-int-range 0 5)
      (lambda (n)
        (gen-bind (gen-matrix n n)
          (lambda (a)
            (gen-map (lambda (b) (cons a b))
                     (gen-matrix n n))))))
    (lambda (pair)
      (let ([a (car pair)]
            [b (cdr pair)])
        (= (matrix-trace (matrix-add a b))
           (+ (matrix-trace a) (matrix-trace b)))))
    'tests 200)

  (define-property "Frobenius norm matches sum-of-squares definition"
    (gen-bind gen-small-dim
      (lambda (rows)
        (gen-bind gen-small-dim
          (lambda (cols)
            (gen-matrix rows cols)))))
    (lambda (a)
      (let* ([lhs (* (frobenius-norm a) (frobenius-norm a))]
             [rhs (matrix-fold (lambda (acc x) (+ acc (* x x))) 0 a)])
        (approx= lhs rhs 1e-9)))
    'tests 200)

  (define-property "matrix-vec-mul is linear in the vector argument"
    gen-matrix-vector-compatible
    (lambda (args)
      (let* ([a (car args)]
             [x (cadr args)]
             [y (caddr args)]
             [x+y (v+ x y)]
             [lhs (matrix-vec-mul a x+y)]
             [ax (matrix-vec-mul a x)]
             [ay (matrix-vec-mul a y)]
             [rhs (if (or (matrix-error? ax) (matrix-error? ay))
                      '(error internal)
                      (v+ ax ay))])
        (and (not (matrix-error? lhs))
             (not (matrix-error? rhs))
             (equal? lhs rhs))))
    'tests 180)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
