;;; lattice/fp/test-combinators-properties.ss — QuickCheck properties for combinators
;;;
;;; Tests fundamental combinator properties:
;;; 1. Identity:      id x == x
;;; 2. Composition:   (compose f g) x == f (g x)
;;; 3. Constancy:     const k x == k
;;; 4. Flip:          flip f x y == f y x
;;; 5. Pipe/Compose:  pipe = compose (reversed order)
;;; 6. Curry/Uncurry: curry/uncurry roundtrip

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'combinators)

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-int-small
  (gen-int-range -100 100))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group combinators-identity-laws

  ;; id x == x
  (define-property "identity returns its argument"
    gen-int-small
    (lambda (x)
      (equal? (id x) x))
    'tests 100)

  ;; identity is an alias for id
  (define-property "identity is alias for id"
    gen-int-small
    (lambda (x)
      (equal? (identity x) (id x)))
    'tests 100)
)

(test-group combinators-constant-laws

  ;; const k x == k
  (define-property "const returns first argument"
    (gen-bind gen-int-small
      (lambda (k)
        (gen-map (lambda (x) (cons k x)) gen-int-small)))
    (lambda (kx)
      (let ([k (car kx)]
            [x (cdr kx)])
        (equal? ((const k) x) k)))
    'tests 100)
)

(test-group combinators-flip-laws

  ;; flip f x y == f y x
  (define-property "flip reverses argument order"
    (gen-bind gen-int-small
      (lambda (x)
        (gen-map (lambda (y) (cons x y)) gen-int-small)))
    (lambda (xy)
      (let ([x (car xy)]
            [y (cdr xy)])
        ;; Use subtraction to verify order: (- x y) != (- y x)
        (equal? ((flip -) x y) (- y x))))
    'tests 100)
)

(test-group combinators-composition-laws

  ;; compose f g x == f (g x)
  (define-property "compose applies right-to-left"
    gen-int-small
    (lambda (x)
      (let* ([f (lambda (y) (* y 2))]
             [g (lambda (y) (+ y 1))]
             [composed (compose f g)]
             [lhs (composed x)]
             [rhs (f (g x))])
        (equal? lhs rhs)))
    'tests 100)

  ;; compose2 is compose alias
  (define-property "compose2 is alias for compose"
    gen-int-small
    (lambda (x)
      (let* ([f (lambda (y) (* y 2))]
             [g (lambda (y) (+ y 1))])
        (equal? ((compose2 f g) x) ((compose f g) x))))
    'tests 100)

  ;; pipe applies left-to-right (reverse of compose)
  (define-property "pipe applies left-to-right"
    gen-int-small
    (lambda (x)
      (let* ([f (lambda (y) (* y 2))]
             [g (lambda (y) (+ y 1))]
             [piped (pipe f g)]
             ;; pipe f g x = g (f x)
             [lhs (piped x)]
             [rhs (g (f x))])
        (equal? lhs rhs)))
    'tests 100)

  ;; pipe2 is pipe alias
  (define-property "pipe2 is alias for pipe"
    gen-int-small
    (lambda (x)
      (let* ([f (lambda (y) (* y 2))]
             [g (lambda (y) (+ y 1))])
        (equal? ((pipe2 f g) x) ((pipe f g) x))))
    'tests 100)

  ;; >>> is pipe alias
  (define-property ">>> is alias for pipe"
    gen-int-small
    (lambda (x)
      (let* ([f (lambda (y) (* y 2))]
             [g (lambda (y) (+ y 1))])
        (equal? ((>>> f g) x) ((pipe f g) x))))
    'tests 100)

  ;; <<< is compose alias
  (define-property "<<< is alias for compose"
    gen-int-small
    (lambda (x)
      (let* ([f (lambda (y) (* y 2))]
             [g (lambda (y) (+ y 1))])
        (equal? ((<<< f g) x) ((compose f g) x))))
    'tests 100)

  ;; compose is associative: (compose f (compose g h)) == (compose (compose f g) h)
  (define-property "compose is associative"
    gen-int-small
    (lambda (x)
      (let* ([f (lambda (y) (* y 2))]
             [g (lambda (y) (+ y 1))]
             [h (lambda (y) (- y 5))]
             [lhs ((compose f (compose g h)) x)]
             [rhs ((compose (compose f g) h) x)])
        (equal? lhs rhs)))
    'tests 100)

  ;; id is identity for compose: (compose id f) == f
  (define-property "id is right identity for compose"
    gen-int-small
    (lambda (x)
      (let* ([f (lambda (y) (+ y 10))]
             [lhs ((compose f id) x)]
             [rhs (f x)])
        (equal? lhs rhs)))
    'tests 100)
)

(test-group combinators-curry-laws

  ;; curry2/uncurry2 roundtrip
  ;; curry2: (a, b) -> c becomes a -> b -> c
  ;; uncurry2: (a -> b -> c) becomes (a, b) -> c where (a, b) is a cons pair
  (define-property "curry2/uncurry2 roundtrip"
    (gen-bind gen-int-small
      (lambda (x)
        (gen-map (lambda (y) (cons x y)) gen-int-small)))
    (lambda (xy)
      (let ([x (car xy)]
            [y (cdr xy)]
            [f (lambda (a b) (+ a b))])
        ;; uncurry2 takes a curried function and a pair
        (equal? ((uncurry2 (curry2 f)) xy) (f x y))))
    'tests 100)

  ;; curry3/uncurry3 roundtrip
  ;; curry3: (a, b, c) -> d becomes a -> b -> c -> d
  ;; uncurry3: (a -> b -> c -> d) becomes (a, b, c) -> d where (a, b, c) is a list
  (define-property "curry3/uncurry3 roundtrip"
    (gen-bind gen-int-small
      (lambda (x)
        (gen-bind gen-int-small
          (lambda (y)
            (gen-map (lambda (z) (list x y z)) gen-int-small)))))
    (lambda (xyz)
      (let ([x (car xyz)]
            [y (cadr xyz)]
            [z (caddr xyz)]
            [f (lambda (a b c) (+ a b c))])
        ;; uncurry3 takes a curried function and a list of 3 elements
        (equal? ((uncurry3 (curry3 f)) xyz) (f x y z))))
    'tests 100)

  ;; curried function application
  (define-property "curry2 produces unary function"
    gen-int-small
    (lambda (x)
      (let* ([f (lambda (a b) (* a b))]
             [curried (curry2 f)]
             ;; (curry2 f) x returns a function
             [result ((curried x) 5)])
        (equal? result (* x 5))))
    'tests 100)
)

(test-group combinators-partial-laws

  ;; partial fixes first argument
  (define-property "partial fixes first arguments"
    (gen-bind gen-int-small
      (lambda (a)
        (gen-map (lambda (b) (cons a b)) gen-int-small)))
    (lambda (ab)
      (let ([a (car ab)]
            [b (cdr ab)]
            [f (lambda (x y) (+ x y))])
        (equal? ((partial f a) b) (f a b))))
    'tests 100)

  ;; partial2 fixes first two arguments
  (define-property "partial2 fixes first two arguments"
    gen-int-small
    (lambda (c)
      (let* ([a 3]
             [b 4]
             [f (lambda (x y z) (+ x y z))]
             [result ((partial2 f a b) c)])
        (equal? result (+ a b c))))
    'tests 100)

  ;; rpartial fixes last argument
  (define-property "rpartial fixes last argument"
    (gen-bind gen-int-small
      (lambda (a)
        (gen-map (lambda (b) (cons a b)) gen-int-small)))
    (lambda (ab)
      (let ([a (car ab)]
            [b (cdr ab)]
            [f (lambda (x y) (- x y))])
        ;; (rpartial f b) fixes y=b, so ((rpartial f b) a) = f a b = a - b
        (equal? ((rpartial f b) a) (f a b))))
    'tests 100)
)

(test-group combinators-higher-order-laws

  ;; apply-fn applies function to single argument
  (define-property "apply-fn applies function"
    (gen-pure #t)
    (lambda (_)
      (let ([f (lambda (x) (+ x 1))])
        (equal? (apply-fn f 3) 4)))
    'tests 50)

  ;; on applies binary op to transformed values
  (define-property "on applies binary op to transformed values"
    gen-int-small
    (lambda (x)
      ;; ((on + abs) x (- x)) = (+ (abs x) (abs (- x)))
      ;; This should equal 2*|x|
      (let ([result ((on + abs) x (- x))])
        (equal? result (* 2 (abs x)))))
    'tests 100)

  ;; both applies function to both elements of a pair
  ;; both: (a -> b) -> (a, a) -> (b, b)
  (define-property "both applies function to both elements of pair"
    (gen-bind gen-int-small
      (lambda (x)
        (gen-map (lambda (y) (cons x y)) gen-int-small)))
    (lambda (pair)
      (let ([result ((both (lambda (n) (* n 2))) pair)])
        ;; both returns (cons (f (car pair)) (f (cdr pair)))
        (and (equal? (car result) (* (car pair) 2))
             (equal? (cdr result) (* (cdr pair) 2)))))
    'tests 100)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
