;;; lattice/category/test-state-store-adjunction.ss — Tests for State/Store Adjunction

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/category/state-store-adjunction.ss")

;;; ====
;;; Product Functor Tests
;;; ====

(test-group "Product Functor"

  (define-test "product functor maps first component"
    (let* ([pair (cons 5 'state)]
           [f (lambda (x) (* x 2))]
           [result ((functor-fmap product-functor) f pair)])
      (assert-equal (cons 10 'state) result)))

  (define-test "product functor preserves state"
    (let* ([pair (cons "hello" 42)]
           [f string-length]
           [result ((functor-fmap product-functor) f pair)])
      (assert-equal 5 (car result))
      (assert-equal 42 (cdr result)))))

;;; ====
;;; Exponential Functor Tests
;;; ====

(test-group "Exponential Functor"

  (define-test "exponential functor post-composes"
    (let* ([g (lambda (s) (+ s 10))]  ; S → a
           [f (lambda (x) (* x 2))]   ; a → b
           [result ((functor-fmap exponential-functor) f g)])
      ;; result should be f ∘ g
      (assert-equal 30 (result 5))))  ; (5 + 10) * 2 = 30

  (define-test "exponential functor identity law"
    (let* ([g (lambda (s) (+ s 1))]
           [result ((functor-fmap exponential-functor) id g)])
      (assert-equal 6 (result 5))
      (assert-equal 6 (g 5)))))

;;; ====
;;; Adjunction Tests
;;; ====

(test-group "State/Store Adjunction"

  (define-test "adjunction is well-formed"
    (assert-true (adjunction? adj-state-store))
    (assert-equal 'state-store (adjunction-name adj-state-store)))

  (define-test "unit sends value to constant state function"
    (let* ([η (nat-transform-component (make-state-store-unit))]
           [result (η 42)])
      ;; result should be λs. (42, s)
      (assert-equal (cons 42 'any-state) (result 'any-state))
      (assert-equal (cons 42 100) (result 100))))

  (define-test "counit extracts value at position"
    (let* ([ε (nat-transform-component (make-state-store-counit))]
           [accessor (lambda (s) (* s s))]  ; squares
           [store (cons accessor 5)]
           [result (ε store)])
      ;; ε(accessor, 5) = accessor(5) = 25
      (assert-equal 25 result))))

;;; ====
;;; Derived State Monad Tests
;;; ====

(test-group "State Monad from Adjunction"

  (define-test "return wraps value with identity state"
    (let* ([m (state-from-adjunction-return 42)]
           [result (m 'initial-state)])
      (assert-equal 42 (car result))
      (assert-equal 'initial-state (cdr result))))

  (define-test "bind sequences state transformations"
    (let* ([m1 (lambda (s) (cons s (+ s 1)))]  ; get state, increment
           [f (lambda (x) (lambda (s) (cons (* x 2) s)))]  ; double, keep state
           [m2 (state-from-adjunction-bind m1 f)]
           [result (m2 10)])
      ;; m1 at s=10: (10, 11)
      ;; f(10) at s=11: (20, 11)
      (assert-equal 20 (car result))
      (assert-equal 11 (cdr result))))

  (define-test "state monad left identity law"
    ;; return a >>= f = f a
    (let* ([a 5]
           [f (lambda (x) (lambda (s) (cons (* x s) (+ s 1))))]
           [lhs (state-from-adjunction-bind (state-from-adjunction-return a) f)]
           [rhs (f a)]
           [s 10])
      (assert-equal (rhs s) (lhs s))))

  (define-test "state monad right identity law"
    ;; m >>= return = m
    (let* ([m (lambda (s) (cons (+ s 1) (* s 2)))]
           [lhs (state-from-adjunction-bind m state-from-adjunction-return)]
           [s 5])
      (assert-equal (m s) (lhs s)))))

;;; ====
;;; Derived Store Comonad Tests
;;; ====

(test-group "Store Comonad from Adjunction"

  (define-test "extract gets value at position"
    (let* ([accessor (lambda (x) (* x x))]
           [store (cons accessor 4)]
           [result (store-from-adjunction-extract store)])
      (assert-equal 16 result)))  ; 4² = 16

  (define-test "duplicate creates nested store"
    (let* ([accessor (lambda (x) (+ x 10))]
           [store (cons accessor 5)]
           [duped (store-from-adjunction-duplicate store)])
      ;; duped = (λs'. (accessor, s'), 5)
      (let* ([outer-accessor (car duped)]
             [outer-pos (cdr duped)]
             [inner-store (outer-accessor 3)])  ; Get store at position 3
        (assert-equal 5 outer-pos)
        (assert-equal accessor (car inner-store))
        (assert-equal 3 (cdr inner-store)))))

  (define-test "extend applies contextual function"
    (let* ([accessor (lambda (x) (* x 2))]
           [store (cons accessor 5)]
           ;; f: sum of current value and value at position+1
           [f (lambda (st)
                (+ ((car st) (cdr st))           ; current value
                   ((car st) (+ (cdr st) 1))))]  ; next value
           [extended (store-from-adjunction-extend f store)])
      ;; At position 5: accessor(5) + accessor(6) = 10 + 12 = 22
      (assert-equal 22 ((car extended) 5))
      ;; Position preserved
      (assert-equal 5 (cdr extended))))

  (define-test "store comonad law 1: extend extract = id"
    (let* ([accessor (lambda (x) (+ x 100))]
           [store (cons accessor 7)]
           [extended (store-from-adjunction-extend
                      store-from-adjunction-extract
                      store)])
      ;; Should get back equivalent store
      (assert-equal (cdr store) (cdr extended))
      (assert-equal ((car store) 7) ((car extended) 7))))

  (define-test "store comonad law 2: extract . extend f = f"
    (let* ([accessor (lambda (x) (* x x))]
           [store (cons accessor 4)]
           [f (lambda (st) (+ 100 ((car st) (cdr st))))]
           [extended (store-from-adjunction-extend f store)]
           [result (store-from-adjunction-extract extended)])
      (assert-equal (f store) result))))

;;; ====
;;; Curry/Uncurry via Adjunction
;;; ====

(test-group "Curry/Uncurry"

  (define-test "curry via adjunction"
    (let* ([uncurried (lambda (pair) (+ (car pair) (cdr pair)))]
           [curried (curry-via-adjunction uncurried)])
      ;; curried 3 should give λs. (3 + s)
      (assert-equal 8 ((curried 3) 5))))

  (define-test "uncurry via adjunction"
    (let* ([curried (lambda (a) (lambda (s) (cons (+ a s) (* a s))))]
           [uncurried (uncurry-via-adjunction curried)])
      (assert-equal (cons 8 15) (uncurried (cons 3 5))))))

;;; ====
;;; Verification Tests
;;; ====

(test-group "Verification"

  (define-test "derived return matches standard"
    (assert-true (verify-state-return-matches 42 'test-state))
    (assert-true (verify-state-return-matches "hello" 100)))

  (define-test "derived extract matches standard"
    (let ([accessor (lambda (x) (+ x 1))])
      (assert-true (verify-store-extract-matches accessor 10)))))

;;; ====
;;; Triangle Identity Tests
;;; ====

(test-group "Triangle Identities"

  ;; Left triangle: ε_{F(a)} ∘ F(η_a) = id_{F(a)}
  ;; For an element (a, s) in F(A) = A × S, the identity says:
  ;;   ε ∘ F(η)((a, s)) = (a, s)
  ;; Since F(η)(a, s) = (η(a), s) = ((λs'. (a, s')), s)
  ;; And ε((λs'. (a, s')), s) = (λs'. (a, s'))(s) = (a, s)
  ;; So the round-trip recovers the original pair.

  (define-test "left triangle identity with numbers"
    (assert-true (verify-state-store-triangle-left (cons 42 100))))

  (define-test "left triangle identity with symbols"
    (assert-true (verify-state-store-triangle-left (cons 'value 'state))))

  (define-test "left triangle identity with complex state"
    (assert-true (verify-state-store-triangle-left (cons "hello" '(a b c)))))

  ;; Right triangle: G(ε) ∘ η_{G(a)} = id_{G(a)}
  ;; For a function func : S → A in G(A) = S → A, the identity says:
  ;;   (G(ε) ∘ η)(func) = func
  ;; Since η(func) = λs. (func, s)
  ;; And G(ε)(η(func)) = ε ∘ η(func) = λs. ε((func, s)) = λs. func(s) = func
  ;; So the round-trip recovers the original function (extensionally).

  (define-test "right triangle identity - doubling function"
    (let ([func (lambda (s) (* s 2))])
      (assert-true (verify-state-store-triangle-right func 5))
      (assert-true (verify-state-store-triangle-right func 10))
      (assert-true (verify-state-store-triangle-right func 0))))

  (define-test "right triangle identity - constant function"
    (let ([func (lambda (s) 'constant)])
      (assert-true (verify-state-store-triangle-right func 'any))
      (assert-true (verify-state-store-triangle-right func 42))))

  (define-test "right triangle identity - polynomial function"
    (let ([func (lambda (s) (+ (* s s) s 1))])  ; s² + s + 1
      (assert-true (verify-state-store-triangle-right func 0))   ; = 1
      (assert-true (verify-state-store-triangle-right func 3))   ; = 13
      (assert-true (verify-state-store-triangle-right func -1)))) ; = 1

  (define-test "right triangle identity - list state"
    (let ([func (lambda (s) (length s))])
      (assert-true (verify-state-store-triangle-right func '()))
      (assert-true (verify-state-store-triangle-right func '(a b c))))))

;;; ====
;;; Run Tests
;;; ====

(run-all-tests)
