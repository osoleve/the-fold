;;; lattice/linalg/test-dep-linalg-properties.ss — QuickCheck properties for dep-linalg

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'vec)
(require 'matrix)
(require 'dep-linalg)

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-int-small
  (gen-int-range -25 25))

(define gen-int-list
  (gen-list gen-int-small))

(define gen-nonempty-int-list
  (gen-bind (gen-int-range 1 20)
    (lambda (n)
      (gen-list-of n gen-int-small))))

(define gen-small-dim
  (gen-int-range 0 4))

(define (gen-matrix rows cols)
  (gen-map matrix-from-lists
           (gen-list-of rows (gen-list-of cols gen-int-small))))

(define gen-vector-with-index
  (gen-bind (gen-int-range 1 20)
    (lambda (n)
      (gen-bind (gen-list-of n gen-int-small)
        (lambda (xs)
          (gen-map (lambda (i) (list xs i))
                   (gen-int-range 0 (- n 1))))))))

(define gen-matrix-with-index
  (gen-bind (gen-int-range 1 5)
    (lambda (rows)
      (gen-bind (gen-int-range 1 5)
        (lambda (cols)
          (gen-bind (gen-matrix rows cols)
            (lambda (m)
              (gen-bind (gen-int-range 0 (- rows 1))
                (lambda (i)
                  (gen-map (lambda (j) (list rows cols m i j))
                           (gen-int-range 0 (- cols 1))))))))))))

(define gen-compatible-matrix-pair
  (gen-bind (gen-int-range 1 4)
    (lambda (m)
      (gen-bind (gen-int-range 1 4)
        (lambda (n)
          (gen-bind (gen-int-range 1 4)
            (lambda (p)
              (gen-bind (gen-matrix m n)
                (lambda (a)
                  (gen-map (lambda (b) (list m n p a b))
                           (gen-matrix n p)))))))))))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group dep-linalg-properties

  (define-property "vec-append-typed matches append at runtime"
    (gen-pair gen-int-list gen-int-list)
    (lambda (pair)
      (let* ([xs (car pair)]
             [ys (cdr pair)]
             [v1 (vec-from-list xs)]
             [v2 (vec-from-list ys)]
             [result (vec-append-typed (length xs) (length ys) 'Int v1 v2)])
        (equal? (append xs ys)
                (vec->list result))))
    'tests 220)

  (define-property "vec-map-typed preserves mapped list semantics"
    gen-int-list
    (lambda (xs)
      (let* ([v (vec-from-list xs)]
             [mapped (vec-map-typed (length xs) 'Int 'Int (lambda (x) (+ x 1)) v)])
        (equal? (map (lambda (x) (+ x 1)) xs)
                (vec->list mapped))))
    'tests 220)

  (define-property "matrix-add-typed matches matrix-add"
    (gen-bind gen-small-dim
      (lambda (rows)
        (gen-bind gen-small-dim
          (lambda (cols)
            (gen-bind (gen-matrix rows cols)
              (lambda (a)
                (gen-map (lambda (b) (list rows cols a b))
                         (gen-matrix rows cols))))))))
    (lambda (args)
      (let* ([rows (car args)]
             [cols (cadr args)]
             [a (caddr args)]
             [b (cadddr args)]
             [typed (matrix-add-typed rows cols 'Int a b)]
             [plain (matrix-add a b)])
        (matrix-equal? typed plain)))
    'tests 180)

  (define-property "vec-zip-typed matches pairwise zip"
    (gen-bind gen-int-list
      (lambda (xs)
        (gen-bind (gen-list-of (length xs) gen-int-small)
          (lambda (ys)
            (gen-pure (list xs ys))))))
    (lambda (args)
      (let* ([xs (car args)]
             [ys (cadr args)]
             [vx (vec-from-list xs)]
             [vy (vec-from-list ys)]
             [zipped (vec-zip-typed (length xs) 'Int 'Int vx vy)])
        (equal? (map (lambda (x y) (cons x y)) xs ys)
                (vec->list zipped))))
    'tests 200)

  (define-property "vec-head-typed returns first element"
    gen-nonempty-int-list
    (lambda (xs)
      (let* ([v (vec-from-list xs)]
             [head (vec-head-typed (- (length xs) 1) 'Int v)])
        (= head (car xs))))
    'tests 220)

  (define-property "vec-tail-typed drops first element"
    gen-nonempty-int-list
    (lambda (xs)
      (let* ([v (vec-from-list xs)]
             [tail (vec-tail-typed (- (length xs) 1) 'Int v)])
        (equal? (cdr xs) (vec->list tail))))
    'tests 220)

  (define-property "vec-fold-typed matches fold-left"
    gen-int-list
    (lambda (xs)
      (let* ([v (vec-from-list xs)]
             [typed (vec-fold-typed (length xs) 'Int 'Int + 0 v)]
             [plain (fold-left + 0 xs)])
        (= typed plain)))
    'tests 220)

  (define-property "matrix-mul-typed matches matrix-mul"
    gen-compatible-matrix-pair
    (lambda (args)
      (let* ([m (car args)]
             [n (cadr args)]
             [p (caddr args)]
             [a (cadddr args)]
             [b (list-ref args 4)]
             [typed (matrix-mul-typed m n p 'Int a b)]
             [plain (matrix-mul a b)])
        (matrix-equal? typed plain)))
    'tests 180)

  (define-property "matrix-transpose-typed matches matrix-transpose"
    (gen-bind gen-small-dim
      (lambda (rows)
        (gen-bind gen-small-dim
          (lambda (cols)
            (gen-map (lambda (m) (list rows cols m))
                     (gen-matrix rows cols))))))
    (lambda (args)
      (let* ([rows (car args)]
             [cols (cadr args)]
             [m (caddr args)]
             [typed (matrix-transpose-typed rows cols 'Int m)]
             [plain (matrix-transpose m)])
        (matrix-equal? typed plain)))
    'tests 200)

  (define-property "matrix-scale-typed matches matrix-scale"
    (gen-bind gen-small-dim
      (lambda (rows)
        (gen-bind gen-small-dim
          (lambda (cols)
            (gen-bind gen-int-small
              (lambda (k)
                (gen-map (lambda (m) (list rows cols k m))
                         (gen-matrix rows cols))))))))
    (lambda (args)
      (let* ([rows (car args)]
             [cols (cadr args)]
             [k (caddr args)]
             [m (cadddr args)]
             [typed (matrix-scale-typed rows cols 'Int k m)]
             [plain (matrix-scale k m)])
        (matrix-equal? typed plain)))
    'tests 200)

  (define-property "vec-ref-safe matches vec-ref"
    gen-vector-with-index
    (lambda (args)
      (let* ([xs (car args)]
             [i (cadr args)]
             [v (vec-from-list xs)])
        (= (vec-ref-safe (length xs) 'Int i #t v)
           (vec-ref v i))))
    'tests 220)

  (define-property "matrix-ref-safe matches matrix-ref"
    gen-matrix-with-index
    (lambda (args)
      (let* ([rows (car args)]
             [cols (cadr args)]
             [m (caddr args)]
             [i (cadddr args)]
             [j (list-ref args 4)])
        (= (matrix-ref-safe rows cols 'Int i j #t #t m)
           (matrix-ref m i j))))
    'tests 220)

  (define-property "vec-nil-typed creates empty vector"
    (gen-pure #t)
    (lambda (_)
      (= (vec-length (vec-nil-typed 'Int)) 0))
    'tests 40)

  (define-property "vec-cons-typed prepends an element"
    gen-int-list
    (lambda (xs)
      (let* ([x 42]
             [v (vec-from-list xs)]
             [out (vec-cons-typed (length xs) 'Int x v)])
        (equal? (cons x xs) (vec->list out))))
    'tests 200)

  (define-property "make-dep-linalg-ctx returns exported dependent type table"
    (gen-pure #t)
    (lambda (_)
      (equal? (make-dep-linalg-ctx) dep-linalg-types))
    'tests 30)

  (define-property "extend-ctx-with-dep-linalg prefixes dep-linalg entries"
    gen-int-list
    (lambda (xs)
      (let* ([ctx (map (lambda (x) (cons 'dummy x)) xs)]
             [extended (extend-ctx-with-dep-linalg ctx)])
        (and (>= (length extended) (length dep-linalg-types))
             (equal? dep-linalg-types
                     (take (length dep-linalg-types) extended)))))
    'tests 80)

  (define-property "diff-primal returns underlying function"
    gen-int-small
    (lambda (x)
      (let* ([d `((primal . ,(lambda (z) (+ z 1)))
                  (gradient . ,(lambda (_z) 1.0)))]
             [f (diff-primal 'A 'B d)])
        (= (f x) (+ x 1))))
    'tests 120)

  (define-property "diff-grad and diff-scalar use gradient entry"
    gen-int-small
    (lambda (x)
      (let* ([d `((primal . ,(lambda (z) (* z z)))
                  (gradient . ,(lambda (z) (* 2 z))))])
        (and (= (diff-grad 1 'Int d x) (* 2 x))
             (= (diff-scalar d x) (* 2 x)))))
    'tests 120)

  (define-property "diff-jacobian and diff-hessian delegate to provided handlers"
    gen-int-small
    (lambda (x)
      (let* ([j (matrix-from-lists '((1 2) (3 4)))]
             [h (matrix-from-lists '((5 6) (7 8)))]
             [d `((primal . ,(lambda (z) z))
                  (jacobian . ,(lambda (_x) j))
                  (hessian . ,(lambda (_x) h)))])
        (and (matrix-equal? (diff-jacobian 2 2 'Int d x) j)
             (matrix-equal? (diff-hessian 2 'Int d x) h))))
    'tests 60)

  (define-property "diff-compose returns composed differentiable wrapper entries"
    gen-int-small
    (lambda (x)
      (let* ([df `((primal . ,(lambda (z) (* 2 z)))
                   (gradient . ,(lambda (_z) 2.0)))]
             [dg `((primal . ,(lambda (z) (+ z 3)))
                   (gradient . ,(lambda (_z) 1.0)))]
             [dc (diff-compose 'A 'B 'C dg df)])
        (and (assq 'primal dc)
             (assq 'gradient dc))))
    'tests 100)

  (define-property "diff-lift preserves primal behavior"
    gen-int-small
    (lambda (x)
      (let* ([d (diff-lift 'Int 'Int (lambda (z) (+ z 7)))]
             [p (cdr (assq 'primal d))])
        (= (p x) (+ x 7))))
    'tests 120)

  (define-property "diff-jvp and diff-vjp delegate to provided handlers"
    gen-int-small
    (lambda (x)
      (let* ([d `((primal . ,(lambda (z) z))
                  (jvp . ,(lambda (_pt tangent) tangent))
                  (vjp . ,(lambda (_pt cotangent) cotangent)))]
             [v (vec-from-list (list x (+ x 1)))]
             [w (vec-from-list (list (+ x 2) (+ x 3)))])
        (and (vec-equal? (diff-jvp 2 2 'Int d v v) v)
             (vec-equal? (diff-vjp 2 2 'Int d v w) w))))
    'tests 80)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
