;;; lattice/fp/category/test-monad-derivation.ss — Monad Derivation Tests
;;;
;;; Tests for monad derivation from adjunctions, including:
;;;   - MonadOps record operations
;;;   - List monad derived from free monoid adjunction
;;;   - Reader adjunction and derived monad
;;;   - Monad law verification (left identity, right identity, associativity)
;;;   - Functor law verification (identity, composition)
;;;   - Comparison with hand-coded monad operations
;;;
;;; Run from project root: scheme --script lattice/fp/category/test-monad-derivation.ss

(load "lattice/fp/category/monad-derivation.ss")

;;; ====
;;; Test Helpers
;;; ====

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (test name expected actual)
  (set! test-count (+ test-count 1))
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (begin
        (set! pass-count (+ pass-count 1))
        (display "PASS"))
      (begin
        (set! fail-count (+ fail-count 1))
        (display "FAIL\n    expected: ")
        (display expected)
        (display "\n    got: ")
        (display actual)))
  (newline))

(define (test-true name actual)
  (test name #t (if actual #t #f)))

(define (test-false name actual)
  (test name #f actual))

(define (section name)
  (newline)
  (display "=== ")
  (display name)
  (display " ===")
  (newline))

;;; ====
;;; Test: MonadOps Record
;;; ====

(section "MonadOps Record")

(test-true "monad-ops? recognizes MonadOps"
           (monad-ops? monad-list-derived))

(test-false "monad-ops? rejects non-MonadOps"
            (monad-ops? '(not a monad)))

(test "monad-ops-name extracts name"
      'monad-free-list
      (monad-ops-name monad-list-derived))

(test-true "monad-ops-return is procedure"
           (procedure? (monad-ops-return monad-list-derived)))

(test-true "monad-ops-fmap is procedure"
           (procedure? (monad-ops-fmap monad-list-derived)))

(test-true "monad-ops-join is procedure"
           (procedure? (monad-ops-join monad-list-derived)))

(test-true "monad-ops-bind is procedure"
           (procedure? (monad-ops-bind monad-list-derived)))

;;; ====
;;; Test: List Monad from Free Monoid Adjunction
;;; ====

(section "List Monad (Derived from adj-free-list)")

;; Get operations
(let ([return (monad-ops-return monad-list-derived)]
      [fmap (monad-ops-fmap monad-list-derived)]
      [join (monad-ops-join monad-list-derived)]
      [bind (monad-ops-bind monad-list-derived)])

  ;; return should create singleton list
  (test "return 42 = '(42)"
        '(42)
        (return 42))

  (test "return 'a = '(a)"
        '(a)
        (return 'a))

  ;; fmap should map over list
  (test "fmap (+ 1) '(1 2 3) = '(2 3 4)"
        '(2 3 4)
        (fmap (lambda (x) (+ x 1)) '(1 2 3)))

  (test "fmap identity '(a b c) = '(a b c)"
        '(a b c)
        (fmap id '(a b c)))

  (test "fmap on empty list = '()"
        '()
        (fmap (lambda (x) (* x 2)) '()))

  ;; join should flatten nested lists
  (test "join '((1 2) (3 4)) = '(1 2 3 4)"
        '(1 2 3 4)
        (join '((1 2) (3 4))))

  (test "join '(() (1) () (2 3)) = '(1 2 3)"
        '(1 2 3)
        (join '(() (1) () (2 3))))

  (test "join '(()) = '()"
        '()
        (join '(())))

  ;; bind should be equivalent to concatMap
  (test "bind '(1 2 3) (x -> [x, x]) = '(1 1 2 2 3 3)"
        '(1 1 2 2 3 3)
        (bind '(1 2 3) (lambda (x) (list x x))))

  (test "bind '(1 2) (x -> [x, x+10]) = '(1 11 2 12)"
        '(1 11 2 12)
        (bind '(1 2) (lambda (x) (list x (+ x 10)))))

  (test "bind '() f = '()"
        '()
        (bind '() (lambda (x) (list x x)))))

;;; ====
;;; Test: Monad Laws for List Monad
;;; ====

(section "Monad Laws (List Monad)")

(let ([return (monad-ops-return monad-list-derived)]
      [bind (monad-ops-bind monad-list-derived)])

  ;; Left identity: bind (return a) f = f a
  (let ([f (lambda (x) (list x x x))])
    (test "Left identity: bind (return 5) f = f 5"
          (f 5)
          (bind (return 5) f))

    (test "Left identity: bind (return 'a) f = f 'a"
          (f 'a)
          (bind (return 'a) f)))

  ;; Right identity: bind m return = m
  (test "Right identity: bind '(1 2 3) return = '(1 2 3)"
        '(1 2 3)
        (bind '(1 2 3) return))

  (test "Right identity: bind '() return = '()"
        '()
        (bind '() return))

  ;; Associativity: bind (bind m f) g = bind m (λx. bind (f x) g)
  (let ([m '(1 2)]
        [f (lambda (x) (list x (+ x 10)))]
        [g (lambda (y) (list y y))])

    (let ([lhs (bind (bind m f) g)]
          [rhs (bind m (lambda (x) (bind (f x) g)))])
      (test "Associativity: bind (bind m f) g = bind m (λx. bind (f x) g)"
            lhs
            rhs))))

;; Use verification functions
(test-true "verify-left-identity (list)"
           (verify-left-identity monad-list-derived
                                 5
                                 (lambda (x) (list x x))))

(test-true "verify-right-identity (list)"
           (verify-right-identity monad-list-derived '(1 2 3)))

(test-true "verify-associativity (list)"
           (verify-associativity monad-list-derived
                                 '(1 2)
                                 (lambda (x) (list x (+ x 1)))
                                 (lambda (y) (list y y))))

(test-true "verify-monad-laws (list, comprehensive)"
           (verify-monad-laws monad-list-derived
                              10
                              '(1 2 3)
                              (lambda (x) (list x x))
                              (lambda (y) (list y (+ y 1)))))

;;; ====
;;; Test: Functor Laws for List Monad
;;; ====

(section "Functor Laws (List Monad)")

(test-true "Functor identity: fmap id = id"
           (verify-functor-identity monad-list-derived '(1 2 3)))

(test-true "Functor identity on empty"
           (verify-functor-identity monad-list-derived '()))

(test-true "Functor composition: fmap (g . f) = fmap g . fmap f"
           (verify-functor-composition monad-list-derived
                                       (lambda (x) (* x 2))
                                       (lambda (x) (+ x 1))
                                       '(1 2 3)))

;;; ====
;;; Test: Comparison with Hand-coded List Monad
;;; ====

(section "Comparison with Hand-coded List Monad")

;; Hand-coded list monad operations
(define (list-return x) (list x))
(define (list-fmap f xs) (map f xs))
(define (list-join xss) (apply append xss))
(define (list-bind xs f) (list-join (list-fmap f xs)))

;; Get derived operations
(let ([d-return (monad-ops-return monad-list-derived)]
      [d-fmap (monad-ops-fmap monad-list-derived)]
      [d-join (monad-ops-join monad-list-derived)]
      [d-bind (monad-ops-bind monad-list-derived)])

  ;; Test equivalence
  (test "return equivalence"
        (list-return 42)
        (d-return 42))

  (test "fmap equivalence"
        (list-fmap (lambda (x) (* x 2)) '(1 2 3))
        (d-fmap (lambda (x) (* x 2)) '(1 2 3)))

  (test "join equivalence"
        (list-join '((a b) (c d) (e)))
        (d-join '((a b) (c d) (e))))

  (test "bind equivalence"
        (list-bind '(1 2 3) (lambda (x) (list x (- x))))
        (d-bind '(1 2 3) (lambda (x) (list x (- x))))))

;;; ====
;;; Test: Reader Adjunction
;;; ====

(section "Reader Adjunction")

(test-true "make-reader-adjunction creates adjunction"
           (adjunction? adj-reader-example))

(test "reader adjunction name"
      'reader-env
      (adjunction-name adj-reader-example))

(test-true "reader adjunction has left functor"
           (functor? (adjunction-left adj-reader-example)))

(test-true "reader adjunction has right functor"
           (functor? (adjunction-right adj-reader-example)))

(test-true "reader adjunction has unit"
           (nat-transform? (adjunction-unit adj-reader-example)))

(test-true "reader adjunction has counit"
           (nat-transform? (adjunction-counit adj-reader-example)))

;;; ====
;;; Test: Reader Monad (Derived)
;;; ====

(section "Reader Monad (Derived)")

(test-true "monad-reader-derived is MonadOps"
           (monad-ops? monad-reader-derived))

(test "reader monad name"
      'monad-reader-env
      (monad-ops-name monad-reader-derived))

;; The derived monad is State-like: M A = E -> (A, E)
(let ([return (monad-ops-return monad-reader-derived)]
      [fmap (monad-ops-fmap monad-reader-derived)]
      [bind (monad-ops-bind monad-reader-derived)])

  ;; return a = λe. (a, e)
  (let ([m (return 42)])
    (test "return 42 with env 'x produces (42 . x)"
          (cons 42 'x)
          (m 'x))

    (test "return 42 with env 100 produces (42 . 100)"
          (cons 42 100)
          (m 100)))

  ;; fmap applies to the value part
  (let ([m (return 10)])
    (let ([mapped (fmap (lambda (x) (* x 2)) m)])
      (test "fmap (* 2) on return 10, env 'e"
            (cons 20 'e)
            (mapped 'e)))))

;;; ====
;;; Test: State-like Operations
;;; ====

(section "State-like Operations")

(let ([return (monad-ops-return monad-reader-derived)])
  (let ([m (return 'value)])

    (test "run-state returns (value . state)"
          (cons 'value 'initial)
          (run-state m 'initial))

    (test "eval-state returns value"
          'value
          (eval-state m 'initial))

    (test "exec-state returns state"
          'initial
          (exec-state m 'initial))))

;;; ====
;;; Test: Reader Triangle Identities
;;; ====

(section "Reader Triangle Identities")

;; These tests verify the adjunction structure is correct
(let* ([F (adjunction-left adj-reader-example)]
       [G (adjunction-right adj-reader-example)]
       [η (adjunction-unit adj-reader-example)]
       [ε (adjunction-counit adj-reader-example)]
       [η-comp (nat-transform-component η)]
       [ε-comp (nat-transform-component ε)]
       [F-fmap (functor-fmap F)]
       [G-fmap (functor-fmap G)])

  ;; Left triangle: ε_{F(A)} ∘ F(η_A) = id_{F(A)}
  ;; For val = (42 . env) in F(A):
  ;; F(η)(val) = F(η)((42 . env)) where F(f)(a,e) = (f(a), e)
  ;; η(42) = λe. (42, e)
  ;; F(η)(42, env) = (η(42), env) = (λe.(42,e), env)
  ;; ε((λe.(42,e), env)) = (λe.(42,e))(env) = (42, env)
  ;; This should equal original (42, env)
  (let* ([val (cons 42 'my-env)]
         [step1 (F-fmap η-comp val)]  ; Apply F(η) to val
         [result (ε-comp step1)])      ; Apply ε to result
    (test "Left triangle identity"
          val
          result))

  ;; Right triangle: G(ε) ∘ η_{G(B)} = id_{G(B)}
  ;; For val = (λe. e) in G(B) (identity function):
  ;; η(val) = λe'. (val, e') = λe'. ((λe.e), e')
  ;; G(ε)(η(val)) = G(ε)(λe'.(val, e'))
  ;; G(f)(g) = λe. f(g(e))
  ;; G(ε)(η(val))(e) = ε(η(val)(e)) = ε((val, e)) = val(e) = e
  (let* ([val (lambda (e) e)]  ; Identity function in G(B)
         [step1 (η-comp val)]   ; Apply η
         [result-fn (G-fmap ε-comp step1)])  ; Apply G(ε)
    ;; result-fn should behave like val
    (test "Right triangle identity (test at env=100)"
          (val 100)
          (result-fn 100))
    (test "Right triangle identity (test at env='foo)"
          (val 'foo)
          (result-fn 'foo))))

;;; ====
;;; Test: Display Functions
;;; ====

(section "Display Functions")

(test-true "monad-ops->string produces string"
           (string? (monad-ops->string monad-list-derived)))

(test "monad-ops->string for list monad"
      "MonadOps<monad-free-list>"
      (monad-ops->string monad-list-derived))

(test "monad-ops->string for reader monad"
      "MonadOps<monad-reader-env>"
      (monad-ops->string monad-reader-derived))

;;; ====
;;; Test: Edge Cases
;;; ====

(section "Edge Cases")

;; Empty list operations
(let ([return (monad-ops-return monad-list-derived)]
      [fmap (monad-ops-fmap monad-list-derived)]
      [join (monad-ops-join monad-list-derived)]
      [bind (monad-ops-bind monad-list-derived)])

  (test "fmap on empty list"
        '()
        (fmap (lambda (x) x) '()))

  (test "join on empty list of lists"
        '()
        (join '()))

  (test "join on list of empty lists"
        '()
        (join '(() () ())))

  (test "bind with function returning empty"
        '()
        (bind '(1 2 3) (lambda (x) '()))))

;; Nested structure - join only flattens one level at a time
(let ([join (monad-ops-join monad-list-derived)])
  ;; '(((1 2)) ((3 4))) -> join -> '((1 2) (3 4)) -> join -> '(1 2 3 4)
  (test "double join flattens two levels"
        '(1 2 3 4)
        (join (join '(((1 2)) ((3 4))))))

  ;; Single join on nested list
  (test "single join on '((1 2) (3 4))"
        '(1 2 3 4)
        (join '((1 2) (3 4)))))

;;; ====
;;; Summary
;;; ====

(newline)
(display "========================================")
(newline)
(display "Test Summary: ")
(display pass-count)
(display " passed, ")
(display fail-count)
(display " failed, ")
(display test-count)
(display " total")
(newline)

(if (= fail-count 0)
    (display "All tests passed!")
    (display "Some tests failed!"))
(newline)
