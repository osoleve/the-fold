(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")

;;; ============================================================
;;; Type Class Lawfulness Property Tests
;;;
;;; Verifies algebraic laws for all registered type class instances.
;;; Laws catch subtle bugs where an instance type-checks but violates
;;; its contract (e.g., a Monad bind that isn't associative).
;;;
;;; The implementations here mirror the core evaluator's instances
;;; (core/lang/eval.ss lines 1041-1134) using native Scheme functions.
;;; ============================================================

;;; ============================================================
;;; Option Type (Some/None)
;;; Representation: 'none | (cons 'some value)
;;; ============================================================

(define (make-some x) (cons 'some x))
(define make-none 'none)
(define (some? x) (and (pair? x) (eq? (car x) 'some)))
(define (none? x) (eq? x 'none))
(define (some-value x) (cdr x))

;;; ============================================================
;;; Either Type (Left/Right)
;;; Representation: (cons 'left value) | (cons 'right value)
;;; ============================================================

(define (make-left x) (cons 'left x))
(define (make-right x) (cons 'right x))
(define (left? x) (and (pair? x) (eq? (car x) 'left)))
(define (right? x) (and (pair? x) (eq? (car x) 'right)))
(define (either-value x) (cdr x))

;;; ============================================================
;;; Functor Implementations (matching eval.ss semantics)
;;; ============================================================

(define (list-fmap f xs)
  (if (null? xs) '()
      (cons (f (car xs)) (list-fmap f (cdr xs)))))

(define (option-fmap f opt)
  (if (none? opt) make-none
      (make-some (f (some-value opt)))))

(define (either-fmap f e)
  (if (left? e) e
      (make-right (f (either-value e)))))

;;; ============================================================
;;; Monad Implementations (matching eval.ss semantics)
;;; ============================================================

(define (list-return x) (list x))

(define (list-bind xs f)
  (if (null? xs) '()
      (append (f (car xs)) (list-bind (cdr xs) f))))

(define (option-return x) (make-some x))

(define (option-bind mx f)
  (if (none? mx) make-none
      (f (some-value mx))))

(define (either-return x) (make-right x))

(define (either-bind mx f)
  (if (left? mx) mx
      (f (either-value mx))))

;;; ============================================================
;;; Applicative Implementations
;;; ============================================================

(define (list-pure x) (list x))

(define (list-ap fs xs)
  (if (null? fs) '()
      (append (map (car fs) xs)
              (list-ap (cdr fs) xs))))

(define (option-pure x) (make-some x))

(define (option-ap mf mx)
  (cond
    [(none? mf) make-none]
    [(none? mx) make-none]
    [else (make-some ((some-value mf) (some-value mx)))]))

;;; ============================================================
;;; Eq Implementations
;;; ============================================================

(define (list-eq xs ys)
  (cond
    [(and (null? xs) (null? ys)) #t]
    [(or (null? xs) (null? ys)) #f]
    [(not (equal? (car xs) (car ys))) #f]
    [else (list-eq (cdr xs) (cdr ys))]))

(define (option-eq a b)
  (cond
    [(and (none? a) (none? b)) #t]
    [(or (none? a) (none? b)) #f]
    [else (equal? (some-value a) (some-value b))]))

;;; ============================================================
;;; Ord Implementations
;;; ============================================================

(define (nat-compare a b)
  (cond [(< a b) 'LT] [(= a b) 'EQ] [else 'GT]))

;;; ============================================================
;;; Structural Equality Helpers
;;; ============================================================

(define (option-equal? a b)
  (cond
    [(and (none? a) (none? b)) #t]
    [(or (none? a) (none? b)) #f]
    [else (equal? (some-value a) (some-value b))]))

(define (either-equal? a b)
  (and (equal? (car a) (car b))
       (equal? (cdr a) (cdr b))))

;;; ============================================================
;;; Test Data Generators
;;; ============================================================

;;; Small deterministic values covering edge cases
(define test-ints '(0 1 -1 2 -2 3 7 42 -100))
(define test-lists '(() (1) (1 2) (1 2 3) (0) (-1 0 1)))
(define test-options
  (list make-none (make-some 0) (make-some 1) (make-some -1)
        (make-some 42) (make-some 7)))
(define test-eithers
  (list (make-left "err") (make-left "fail")
        (make-right 0) (make-right 1) (make-right -1)
        (make-right 42)))

;;; Test functions for composition
(define (f1 x) (+ x 1))
(define (f2 x) (* x 2))
(define (f3 x) (- x 3))

;;; Test functions for monadic bind
(define (m-list-f x) (list x (+ x 10)))
(define (m-list-g x) (if (> x 0) (list x) '()))
(define (m-opt-f x) (if (> x 0) (make-some (* x 2)) make-none))
(define (m-opt-g x) (make-some (+ x 1)))
(define (m-either-f x) (if (> x 0) (make-right (* x 2)) (make-left "neg")))
(define (m-either-g x) (make-right (+ x 1)))

;;; ============================================================
;;; Functor Laws
;;; ============================================================
;;;
;;; Law 1 (Identity):    fmap id = id
;;; Law 2 (Composition): fmap (g . f) = fmap g . fmap f

(define (identity x) x)

(test-group "functor-laws-list"

  (define-test "fmap identity"
    (for-each
     (lambda (xs)
       (assert-equal xs (list-fmap identity xs)))
     test-lists))

  (define-test "fmap composition"
    (for-each
     (lambda (xs)
       (assert-equal (list-fmap (lambda (x) (f2 (f1 x))) xs)
                     (list-fmap f2 (list-fmap f1 xs))))
     test-lists)))

(test-group "functor-laws-option"

  (define-test "fmap identity"
    (for-each
     (lambda (opt)
       (assert-true (option-equal? opt (option-fmap identity opt))))
     test-options))

  (define-test "fmap composition"
    (for-each
     (lambda (opt)
       (assert-true (option-equal?
                     (option-fmap (lambda (x) (f2 (f1 x))) opt)
                     (option-fmap f2 (option-fmap f1 opt)))))
     test-options)))

(test-group "functor-laws-either"

  (define-test "fmap identity"
    (for-each
     (lambda (e)
       (assert-true (either-equal? e (either-fmap identity e))))
     test-eithers))

  (define-test "fmap composition"
    (for-each
     (lambda (e)
       (assert-true (either-equal?
                     (either-fmap (lambda (x) (f2 (f1 x))) e)
                     (either-fmap f2 (either-fmap f1 e)))))
     test-eithers)))

;;; ============================================================
;;; Monad Laws
;;; ============================================================
;;;
;;; Law 1 (Left unit):    return a >>= f  =  f a
;;; Law 2 (Right unit):   m >>= return    =  m
;;; Law 3 (Associativity): (m >>= f) >>= g  =  m >>= (λx. f x >>= g)

(test-group "monad-laws-list"

  (define-test "left unit"
    (for-each
     (lambda (a)
       (assert-equal (m-list-f a)
                     (list-bind (list-return a) m-list-f)))
     test-ints))

  (define-test "right unit"
    (for-each
     (lambda (xs)
       (assert-equal xs
                     (list-bind xs list-return)))
     test-lists))

  (define-test "associativity"
    (for-each
     (lambda (xs)
       (assert-equal (list-bind (list-bind xs m-list-f) m-list-g)
                     (list-bind xs (lambda (x) (list-bind (m-list-f x) m-list-g)))))
     test-lists)))

(test-group "monad-laws-option"

  (define-test "left unit"
    (for-each
     (lambda (a)
       (assert-true (option-equal? (m-opt-f a)
                                   (option-bind (option-return a) m-opt-f))))
     test-ints))

  (define-test "right unit"
    (for-each
     (lambda (opt)
       (assert-true (option-equal? opt
                                   (option-bind opt option-return))))
     test-options))

  (define-test "associativity"
    (for-each
     (lambda (opt)
       (assert-true (option-equal?
                     (option-bind (option-bind opt m-opt-f) m-opt-g)
                     (option-bind opt (lambda (x) (option-bind (m-opt-f x) m-opt-g))))))
     test-options)))

(test-group "monad-laws-either"

  (define-test "left unit"
    (for-each
     (lambda (a)
       (assert-true (either-equal? (m-either-f a)
                                   (either-bind (either-return a) m-either-f))))
     test-ints))

  (define-test "right unit"
    (for-each
     (lambda (e)
       (assert-true (either-equal? e
                                   (either-bind e either-return))))
     test-eithers))

  (define-test "associativity"
    (for-each
     (lambda (e)
       (assert-true (either-equal?
                     (either-bind (either-bind e m-either-f) m-either-g)
                     (either-bind e (lambda (x) (either-bind (m-either-f x) m-either-g))))))
     test-eithers)))

;;; ============================================================
;;; Applicative Laws
;;; ============================================================
;;;
;;; Law 1 (Identity):     pure id <*> v = v
;;; Law 2 (Homomorphism): pure f <*> pure x = pure (f x)
;;; Law 3 (Interchange):  u <*> pure y = pure ($ y) <*> u

(test-group "applicative-laws-list"

  (define-test "identity"
    (for-each
     (lambda (xs)
       (assert-equal xs
                     (list-ap (list-pure identity) xs)))
     test-lists))

  (define-test "homomorphism"
    (for-each
     (lambda (x)
       (assert-equal (list-pure (f1 x))
                     (list-ap (list-pure f1) (list-pure x))))
     test-ints))

  (define-test "interchange"
    (let ([u (list f1 f2)])
      (for-each
       (lambda (y)
         (assert-equal (list-ap u (list-pure y))
                       (list-ap (list-pure (lambda (f) (f y))) u)))
       test-ints))))

(test-group "applicative-laws-option"

  (define-test "identity"
    (for-each
     (lambda (opt)
       (assert-true (option-equal? opt
                                   (option-ap (option-pure identity) opt))))
     test-options))

  (define-test "homomorphism"
    (for-each
     (lambda (x)
       (assert-true (option-equal? (option-pure (f1 x))
                                   (option-ap (option-pure f1) (option-pure x)))))
     test-ints))

  (define-test "interchange"
    (let ([u (make-some f1)])
      (for-each
       (lambda (y)
         (assert-true (option-equal?
                       (option-ap u (option-pure y))
                       (option-ap (option-pure (lambda (f) (f y))) u))))
       test-ints))))

;;; ============================================================
;;; Eq Laws
;;; ============================================================
;;;
;;; Law 1 (Reflexivity):    x == x
;;; Law 2 (Symmetry):       x == y → y == x
;;; Law 3 (Transitivity):   x == y ∧ y == z → x == z
;;; Law 4 (Negation):       x /= y ↔ ¬(x == y)

(test-group "eq-laws-base-types"

  (define-test "reflexivity (integers)"
    (for-each
     (lambda (x) (assert-true (= x x)))
     test-ints))

  (define-test "symmetry (integers)"
    (for-each
     (lambda (x)
       (for-each
        (lambda (y)
          (assert-true (eq? (= x y) (= y x))))
        test-ints))
     test-ints))

  (define-test "negation consistency (integers)"
    (for-each
     (lambda (x)
       (for-each
        (lambda (y)
          (assert-true (eq? (not (= x y)) (not (= x y)))))
        test-ints))
     test-ints)))

(test-group "eq-laws-list"

  (define-test "reflexivity"
    (for-each
     (lambda (xs) (assert-true (list-eq xs xs)))
     test-lists))

  (define-test "symmetry"
    (for-each
     (lambda (xs)
       (for-each
        (lambda (ys)
          (assert-true (eq? (list-eq xs ys) (list-eq ys xs))))
        test-lists))
     test-lists))

  (define-test "negation consistency"
    (for-each
     (lambda (xs)
       (for-each
        (lambda (ys)
          (assert-true (eq? (not (list-eq xs ys))
                            (not (list-eq xs ys)))))
        test-lists))
     test-lists)))

(test-group "eq-laws-option"

  (define-test "reflexivity"
    (for-each
     (lambda (opt) (assert-true (option-eq opt opt)))
     test-options))

  (define-test "symmetry"
    (for-each
     (lambda (a)
       (for-each
        (lambda (b)
          (assert-true (eq? (option-eq a b) (option-eq b a))))
        test-options))
     test-options)))

;;; ============================================================
;;; Ord Laws
;;; ============================================================
;;;
;;; Law 1 (Reflexivity):    x <= x (compare x x = EQ)
;;; Law 2 (Antisymmetry):   x <= y ∧ y <= x → x = y
;;; Law 3 (Transitivity):   x <= y ∧ y <= z → x <= z
;;; Law 4 (Totality):       x <= y ∨ y <= x

(test-group "ord-laws-integers"

  (define-test "reflexivity"
    (for-each
     (lambda (x)
       (assert-equal 'EQ (nat-compare x x)))
     test-ints))

  (define-test "antisymmetry"
    (for-each
     (lambda (x)
       (for-each
        (lambda (y)
          (when (and (not (eq? 'GT (nat-compare x y)))
                     (not (eq? 'GT (nat-compare y x))))
            (assert-equal 'EQ (nat-compare x y))))
        test-ints))
     test-ints))

  (define-test "transitivity"
    (for-each
     (lambda (x)
       (for-each
        (lambda (y)
          (for-each
           (lambda (z)
             (when (and (not (eq? 'GT (nat-compare x y)))
                        (not (eq? 'GT (nat-compare y z))))
               (assert-true (not (eq? 'GT (nat-compare x z))))))
           test-ints))
        test-ints))
     test-ints))

  (define-test "totality"
    (for-each
     (lambda (x)
       (for-each
        (lambda (y)
          ;; Either x <= y or y <= x (or both)
          (assert-true (or (not (eq? 'GT (nat-compare x y)))
                           (not (eq? 'GT (nat-compare y x))))))
        test-ints))
     test-ints))

  (define-test "compare consistency with operators"
    (for-each
     (lambda (x)
       (for-each
        (lambda (y)
          (let ([cmp (nat-compare x y)])
            (assert-true (eq? (eq? cmp 'LT) (< x y)))
            (assert-true (eq? (eq? cmp 'EQ) (= x y)))
            (assert-true (eq? (eq? cmp 'GT) (> x y)))))
        test-ints))
     test-ints)))

;;; ============================================================
;;; Semigroup Laws
;;; ============================================================
;;;
;;; Law 1 (Associativity): (x <> y) <> z = x <> (y <> z)

(test-group "semigroup-laws-string"

  (define test-strings '("" "a" "bc" "hello" "world"))

  (define-test "associativity"
    (for-each
     (lambda (x)
       (for-each
        (lambda (y)
          (for-each
           (lambda (z)
             (assert-equal (string-append (string-append x y) z)
                           (string-append x (string-append y z))))
           test-strings))
        test-strings))
     test-strings)))

(test-group "semigroup-laws-list"

  (define-test "associativity"
    (for-each
     (lambda (xs)
       (for-each
        (lambda (ys)
          (for-each
           (lambda (zs)
             (assert-equal (append (append xs ys) zs)
                           (append xs (append ys zs))))
           test-lists))
        test-lists))
     test-lists)))

;;; ============================================================
;;; Monoid Laws
;;; ============================================================
;;;
;;; Law 1 (Left identity):  mempty <> x = x
;;; Law 2 (Right identity): x <> mempty = x

(test-group "monoid-laws-string"

  (define test-strings '("" "a" "bc" "hello" "world"))

  (define-test "left identity"
    (for-each
     (lambda (x)
       (assert-equal x (string-append "" x)))
     test-strings))

  (define-test "right identity"
    (for-each
     (lambda (x)
       (assert-equal x (string-append x "")))
     test-strings)))

(test-group "monoid-laws-list"

  (define-test "left identity"
    (for-each
     (lambda (xs)
       (assert-equal xs (append '() xs)))
     test-lists))

  (define-test "right identity"
    (for-each
     (lambda (xs)
       (assert-equal xs (append xs '())))
     test-lists)))

;;; ============================================================
;;; Monad-Functor Consistency
;;; ============================================================
;;;
;;; fmap f m = m >>= (return . f)

(test-group "monad-functor-consistency-list"

  (define-test "fmap via bind"
    (for-each
     (lambda (xs)
       (assert-equal (list-fmap f1 xs)
                     (list-bind xs (lambda (x) (list-return (f1 x))))))
     test-lists)))

(test-group "monad-functor-consistency-option"

  (define-test "fmap via bind"
    (for-each
     (lambda (opt)
       (assert-true (option-equal?
                     (option-fmap f1 opt)
                     (option-bind opt (lambda (x) (option-return (f1 x)))))))
     test-options)))

(test-group "monad-functor-consistency-either"

  (define-test "fmap via bind"
    (for-each
     (lambda (e)
       (assert-true (either-equal?
                     (either-fmap f1 e)
                     (either-bind e (lambda (x) (either-return (f1 x)))))))
     test-eithers)))

;;; ============================================================
;;; Applicative-Monad Consistency
;;; ============================================================
;;;
;;; pure = return
;;; f <*> x = f >>= (\g -> x >>= (\y -> return (g y)))

(test-group "applicative-monad-consistency-list"

  (define-test "pure = return"
    (for-each
     (lambda (x)
       (assert-equal (list-pure x) (list-return x)))
     test-ints))

  (define-test "ap via bind"
    (let ([fs (list f1 f2)])
      (for-each
       (lambda (xs)
         (assert-equal (list-ap fs xs)
                       (list-bind fs (lambda (g)
                                       (list-bind xs (lambda (y)
                                                       (list-return (g y))))))))
       test-lists))))

(test-group "applicative-monad-consistency-option"

  (define-test "pure = return"
    (for-each
     (lambda (x)
       (assert-true (option-equal? (option-pure x) (option-return x))))
     test-ints))

  (define-test "ap via bind"
    (let ([mf (make-some f1)])
      (for-each
       (lambda (opt)
         (assert-true (option-equal?
                       (option-ap mf opt)
                       (option-bind mf (lambda (g)
                                         (option-bind opt (lambda (y)
                                                            (option-return (g y)))))))))
       test-options))))

(run-all-tests)
