;;; lattice/fp/category/test-kleisli.ss — Kleisli Category Tests
;;;
;;; Tests for Kleisli category construction and common monads:
;;;   - Kleisli category structure and operations
;;;   - Category law verification (identity, associativity)
;;;   - Maybe monad and its Kleisli category
;;;   - Either monad and its Kleisli category
;;;   - Reader monad and its Kleisli category
;;;   - Kleisli combinators (sequence, pipe, replicate)
;;;
;;; Run from project root: scheme --script lattice/fp/category/test-kleisli.ss

(load "lattice/fp/category/kleisli.ss")
(load "lattice/fp/category/common-monads.ss")

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
;;; Test: Kleisli Category Construction
;;; ====

(section "Kleisli Category Construction")

(test-true "kleisli-category? recognizes Kleisli"
           (kleisli-category? kc-list))

(test-false "kleisli-category? rejects non-Kleisli"
            (kleisli-category? '(not a kleisli)))

(test "kleisli-category-name extracts name"
      'monad-free-list
      (kleisli-category-name kc-list))

(test-true "kleisli-category-monad returns MonadOps"
           (monad-ops? (kleisli-category-monad kc-list)))

(test-true "kleisli-identity returns procedure"
           (procedure? (kleisli-identity kc-list)))

(test-true "kleisli-compose returns procedure"
           (procedure? (kleisli-compose kc-list)))

;;; ====
;;; Test: Kleisli Category for List Monad
;;; ====

(section "Kleisli Category for List Monad")

;; Define some Kleisli arrows for List
;; f : Int -> [Int]  (duplicate)
(define (list-duplicate x) (list x x))

;; g : Int -> [Int]  (add 10)
(define (list-add10 x) (list (+ x 10)))

;; h : Int -> [Int]  (negate and identity)
(define (list-negate-id x) (list (- x) x))

;; Test identity: return
(let ([id (kleisli-id kc-list)])
  (test "kleisli-id returns singleton"
        '(42)
        (id 42)))

;; Test composition: f >=> g
(let* ([fg (kleisli->>> kc-list list-duplicate list-add10)])
  ;; list-duplicate 5 = '(5 5)
  ;; then bind with list-add10: '(15 15)
  (test "kleisli->>> composition"
        '(15 15)
        (fg 5)))

;; Test fish operator (same as >>>)
(let* ([fg (fish kc-list list-duplicate list-add10)])
  (test "fish operator"
        '(15 15)
        (fg 5)))

;; Test kleisli-lift
(let* ([lifted (kleisli-lift kc-list (lambda (x) (* x 2)))])
  (test "kleisli-lift creates Kleisli arrow"
        '(10)
        (lifted 5)))

;;; ====
;;; Test: Kleisli Laws for List Monad
;;; ====

(section "Kleisli Laws for List Monad")

;; Left identity: return >=> f = f
(test-true "Kleisli left identity"
           (verify-kleisli-left-identity kc-list list-duplicate 5))

;; Right identity: f >=> return = f
(test-true "Kleisli right identity"
           (verify-kleisli-right-identity kc-list list-duplicate 5))

;; Associativity: (f >=> g) >=> h = f >=> (g >=> h)
(test-true "Kleisli associativity"
           (verify-kleisli-associativity kc-list
                                         list-duplicate
                                         list-add10
                                         list-negate-id
                                         3))

;; All laws together
(test-true "Kleisli laws (all)"
           (verify-kleisli-laws kc-list
                                list-duplicate
                                list-add10
                                list-negate-id
                                3))

;;; ====
;;; Test: Kleisli Combinators
;;; ====

(section "Kleisli Combinators")

;; kleisli-sequence
(let* ([arrows (list list-duplicate list-add10)]
       [seq (kleisli-sequence kc-list arrows)])
  (test "kleisli-sequence chains arrows"
        '(15 15)
        (seq 5)))

(let* ([seq-empty (kleisli-sequence kc-list '())])
  (test "kleisli-sequence empty = identity"
        '(7)
        (seq-empty 7)))

;; kleisli-pipe
(test "kleisli-pipe feeds through arrows"
      '(15 15)
      (kleisli-pipe kc-list 5 (list list-duplicate list-add10)))

;; kleisli-replicate
(let* ([rep3 (kleisli-replicate kc-list 3 list-duplicate)])
  ;; duplicate 3 times: x -> (x x) -> (x x x x) -> (x x x x x x x x)
  (test "kleisli-replicate 3 times"
        8
        (length (rep3 1))))

(let* ([rep0 (kleisli-replicate kc-list 0 list-duplicate)])
  (test "kleisli-replicate 0 = identity"
        '(42)
        (rep0 42)))

;;; ====
;;; Test: Maybe Monad
;;; ====

(section "Maybe Monad")

(test "maybe-return wraps value"
      (maybe-just 42)
      (maybe-return 42))

(test-true "maybe-just? recognizes Just"
           (maybe-just? (maybe-just 42)))

(test-true "maybe-nothing? recognizes Nothing"
           (maybe-nothing? maybe-nothing))

(test "maybe-from-just extracts value"
      42
      (maybe-from-just (maybe-just 42)))

;; Key test: Just #f is distinct from Nothing
(test-true "Just #f is distinct from Nothing"
           (maybe-just? (maybe-just #f)))

(test-false "Just #f is not Nothing"
            (maybe-nothing? (maybe-just #f)))

(test "maybe-bind on Just"
      (maybe-just 20)
      (maybe-bind (maybe-just 10) (lambda (x) (maybe-just (* x 2)))))

(test "maybe-bind on Nothing"
      maybe-nothing
      (maybe-bind maybe-nothing (lambda (x) (maybe-just (* x 2)))))

(test "maybe-bind propagates Nothing"
      maybe-nothing
      (maybe-bind (maybe-just 10) (lambda (x) maybe-nothing)))

(test "maybe-fmap on Just"
      (maybe-just 20)
      (maybe-fmap (lambda (x) (* x 2)) (maybe-just 10)))

(test "maybe-fmap on Nothing"
      maybe-nothing
      (maybe-fmap (lambda (x) (* x 2)) maybe-nothing))

(test "maybe-from-default with Just"
      42
      (maybe-from-default 0 (maybe-just 42)))

(test "maybe-from-default with Nothing"
      0
      (maybe-from-default 0 maybe-nothing))

;; Maybe monad laws
(test-true "Maybe left identity"
           (verify-left-identity monad-maybe 5 (lambda (x) (maybe-just (* x 2)))))

(test-true "Maybe right identity"
           (verify-right-identity monad-maybe (maybe-just 42)))

(test-true "Maybe associativity"
           (verify-associativity monad-maybe
                                 (maybe-just 10)
                                 (lambda (x) (maybe-just (+ x 1)))
                                 (lambda (y) (maybe-just (* y 2)))))

;;; ====
;;; Test: Kleisli Category for Maybe
;;; ====

(section "Kleisli Category for Maybe")

(define kc-maybe (make-kleisli-category monad-maybe))

;; Safe division (Kleisli arrow: Int -> Maybe Int)
(define (safe-div x)
  (if (= x 0) maybe-nothing (maybe-just (/ 10 x))))

;; Safe square root (Kleisli arrow: Num -> Maybe Num)
(define (safe-sqrt x)
  (if (< x 0) maybe-nothing (maybe-just (sqrt x))))

(test-true "kc-maybe is Kleisli category"
           (kleisli-category? kc-maybe))

;; Test composition with short-circuit
(let* ([div-then-sqrt (kleisli->>> kc-maybe safe-div safe-sqrt)])
  (test "kc-maybe composition (success)"
        (maybe-just (sqrt 2))  ; 10/5 = 2, sqrt(2)
        (div-then-sqrt 5))

  (test "kc-maybe composition (fail first)"
        maybe-nothing
        (div-then-sqrt 0))

  (test "kc-maybe composition (fail second)"
        maybe-nothing
        (div-then-sqrt -1)))  ; 10/-1 = -10, sqrt(-10) = nothing

;; Kleisli laws for Maybe
(test-true "Maybe Kleisli left identity"
           (verify-kleisli-left-identity kc-maybe safe-div 5))

(test-true "Maybe Kleisli right identity"
           (verify-kleisli-right-identity kc-maybe safe-div 5))

(test-true "Maybe Kleisli associativity"
           (verify-kleisli-associativity kc-maybe
                                         safe-div
                                         safe-sqrt
                                         (lambda (x) (maybe-just (+ x 1)))
                                         4))

;;; ====
;;; Test: Either Monad
;;; ====

(section "Either Monad")

(test-true "make-right creates Right"
           (either-right? (make-right 42)))

(test-true "make-left creates Left"
           (either-left? (make-left 'error)))

(test "either-value extracts Right value"
      42
      (either-value (make-right 42)))

(test "either-value extracts Left error"
      'error
      (either-value (make-left 'error)))

(test "either-bind on Right"
      (make-right 20)
      (either-bind (make-right 10) (lambda (x) (make-right (* x 2)))))

(test "either-bind on Left propagates"
      (make-left 'oops)
      (either-bind (make-left 'oops) (lambda (x) (make-right (* x 2)))))

(test "either-fmap on Right"
      (make-right 20)
      (either-fmap (lambda (x) (* x 2)) (make-right 10)))

(test "either-fmap on Left propagates"
      (make-left 'oops)
      (either-fmap (lambda (x) (* x 2)) (make-left 'oops)))

(test "either-fold on Right"
      'success
      (either-fold (lambda (e) 'failure)
                   (lambda (v) 'success)
                   (make-right 42)))

(test "either-fold on Left"
      'failure
      (either-fold (lambda (e) 'failure)
                   (lambda (v) 'success)
                   (make-left 'oops)))

;; Either monad laws
(test-true "Either left identity"
           (verify-left-identity monad-either
                                 5
                                 (lambda (x) (make-right (* x 2)))))

(test-true "Either right identity"
           (verify-right-identity monad-either (make-right 42)))

(test-true "Either associativity"
           (verify-associativity monad-either
                                 (make-right 10)
                                 (lambda (x) (make-right (+ x 1)))
                                 (lambda (y) (make-right (* y 2)))))

;;; ====
;;; Test: Kleisli Category for Either
;;; ====

(section "Kleisli Category for Either")

(define kc-either (make-kleisli-category monad-either))

;; Checked operations
(define (checked-div x)
  (if (= x 0)
      (make-left 'division-by-zero)
      (make-right (/ 10 x))))

(define (checked-sqrt x)
  (if (< x 0)
      (make-left 'negative-sqrt)
      (make-right (sqrt x))))

(test-true "kc-either is Kleisli category"
           (kleisli-category? kc-either))

;; Test composition with error tracking
(let* ([div-then-sqrt (kleisli->>> kc-either checked-div checked-sqrt)])
  (test "kc-either composition (success)"
        (make-right (sqrt 2))
        (div-then-sqrt 5))

  (test "kc-either composition (div by zero)"
        (make-left 'division-by-zero)
        (div-then-sqrt 0))

  (test "kc-either composition (negative sqrt)"
        (make-left 'negative-sqrt)
        (div-then-sqrt -1)))

;;; ====
;;; Test: Reader Monad
;;; ====

(section "Reader Monad")

;; Run with environment 100
(define test-env 100)

(test "reader-return ignores environment"
      42
      (run-reader (reader-return 42) test-env))

(test "reader-ask reads environment"
      100
      (run-reader reader-ask test-env))

(test "reader-asks applies projection"
      200
      (run-reader (reader-asks (lambda (e) (* e 2))) test-env))

(test "reader-bind threads environment"
      200
      (run-reader (reader-bind reader-ask
                               (lambda (e) (reader-return (* e 2))))
                  test-env))

(test "reader-local modifies environment"
      10
      (run-reader (reader-local (lambda (e) (/ e 10)) reader-ask)
                  test-env))

;; Reader monad laws (need custom equality)
(define (reader-eq-at env r1 r2)
  (equal? (run-reader r1 env) (run-reader r2 env)))

(test-true "Reader left identity (at env=100)"
           (verify-left-identity-with-eq monad-reader
                                         5
                                         (lambda (x) (reader-return (* x 2)))
                                         (lambda (r1 r2) (reader-eq-at 100 r1 r2))))

(test-true "Reader right identity (at env=100)"
           (verify-right-identity-with-eq monad-reader
                                          (reader-return 42)
                                          (lambda (r1 r2) (reader-eq-at 100 r1 r2))))

;;; ====
;;; Test: Display Functions
;;; ====

(section "Display Functions")

(test-true "kleisli-category->string produces string"
           (string? (kleisli-category->string kc-list)))

(test "kleisli-category->string for list"
      "Kleisli<monad-free-list>"
      (kleisli-category->string kc-list))

(test "kleisli-category->string for maybe"
      "Kleisli<maybe>"
      (kleisli-category->string kc-maybe))

;;; ====
;;; Test: Edge Cases
;;; ====

(section "Edge Cases")

;; Empty list in Kleisli
(test "Kleisli arrow returning empty list"
      '()
      ((kleisli->>> kc-list (lambda (x) '()) list-duplicate) 5))

;; Chain of failures in Maybe
(let* ([fail-chain (kleisli->>> kc-maybe
                                 (lambda (x) maybe-nothing)
                                 (lambda (x) (maybe-just (* x 2))))])
  (test "Maybe failure short-circuits"
        maybe-nothing
        (fail-chain 5)))

;; Complex Either chain
(let* ([pipeline (kleisli-sequence kc-either
                                   (list checked-div
                                         checked-sqrt
                                         (lambda (x) (make-right (+ x 1)))))])
  (test "Either pipeline success"
        #t
        (either-right? (pipeline 4))))  ; 10/4 = 2.5, sqrt = 1.58..., +1

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
