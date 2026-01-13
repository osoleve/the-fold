;;; Test harness for normalize.ss and expand.ss

(load "core/blocks/normalize.ss")
(load "core/blocks/expand.ss")
(load "core/blocks/cas.ss")

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (display "✓")
      (begin
       (display "✗
    expected: ")
       (display expected)
       (display "
    got: ")
       (display actual)))
  (newline))

;;; Test 1: Identity function
(display "Test 1: Identity function
")
(test "normalize (fn (x) x)"
      '(fn (dv 0))
      (normalize '(fn (x) x)))

(test "normalize (fn (y) y)"
      '(fn (dv 0))
      (normalize '(fn (y) y)))

(test "same normalized form"
      (normalize '(fn (x) x))
      (normalize '(fn (y) y)))

;;; Test 2: Nested binders
(display "
Test 2: Nested binders
")
(test "outer variable"
      '(fn (fn (dv 1)))
      (normalize '(fn (x) (fn (y) x))))

(test "inner variable"
      '(fn (fn (dv 0)))
      (normalize '(fn (x) (fn (y) y))))

;;; Test 3: Let bindings
(display "
Test 3: Let bindings
")
(test "simple let"
      '(let (42) (dv 0))
      (normalize '(let ((x 42)) x)))

;;; Test 4: Application
(display "
Test 4: Application
")
(test "apply inner to outer"
      '(fn (fn ((dv 1) (dv 0))))
      (normalize '(fn (f) (fn (x) (f x)))))

;;; Test 5: Free variables preserved
(display "
Test 5: Free variables preserved
")
(test "free var"
      '(fn (+ (dv 0) 1))
      (normalize '(fn (x) (+ x 1))))

;;; Test 6: Expansion
(display "
Test 6: Expansion
")
(test "expand identity"
      '(fn (a) a)
      (expand '(fn (dv 0)) '(a)))

(test "expand nested"
      '(fn (a) (fn (b) a))
      (expand '(fn (fn (dv 1))) '(a b)))

;;; Test 7: Round-trip
(display "
Test 7: Round-trip
")
(define orig '(fn (f) (fn (x) (f (f x)))))
(define normalized (normalize orig))
(define expanded (expand normalized '(g y)))
(define renormalized (normalize expanded))
(test "normalize = renormalize"
      normalized
      renormalized)

;;; Test 8: Complex expression
(display "
Test 8: Complex expression
")
(define complex-expr
  '(let ((compose (fn (f) (fn (g) (fn (x) (f (g x)))))))
    (compose inc inc)))
(define norm-complex (normalize complex-expr))
(display "  normalized: ") (display norm-complex) (newline)

;;; ============================================================
;;; Algebraic Normalization Tests
;;; ============================================================

(display "
Test 9: Algebraic normalization - Commutativity
")
(test "addition sorted"
      (normalize-algebraic '(+ a b))
      (normalize-algebraic '(+ b a)))

(test "multiplication sorted"
      (normalize-algebraic '(* x y))
      (normalize-algebraic '(* y x)))

(test "nested commutative"
      (normalize-algebraic '(+ (* a b) (* c d)))
      (normalize-algebraic '(+ (* d c) (* b a))))

(display "
Test 10: Algebraic normalization - Associativity
")
(let ([e1 (normalize-algebraic '(+ (+ a b) c))]
      [e2 (normalize-algebraic '(+ a (+ b c)))])
  (test "flatten addition" e1 e2)
  (display "  flattened: ") (display e1) (newline))

(let ([e1 (normalize-algebraic '(append (append a b) c))]
      [e2 (normalize-algebraic '(append a (append b c)))])
  (test "append flattened (same)" e1 e2))

(display "
Test 11: Short-circuit operators NOT commutative
")
(test "and preserved order 1"
      '(and a b)
      (normalize-algebraic '(and a b)))
(test "and preserved order 2"
      '(and b a)
      (normalize-algebraic '(and b a)))
(test "and different"
      #f
      (equal? (normalize-algebraic '(and a b))
              (normalize-algebraic '(and b a))))

(display "
Test 12: Parallel let* bindings
")
;; Independent bindings should be sorted
(let ([e1 (normalize-algebraic '(let* ((b 2) (a 1)) (+ a b)))]
      [e2 (normalize-algebraic '(let* ((a 1) (b 2)) (+ a b)))])
  (test "independent bindings sorted" e1 e2))

;; Dependent bindings respect dependency
(let ([result (normalize-algebraic '(let* ((b (+ a 1)) (a 1)) (+ a b)))])
  (let ([first-var (caar (cadr result))])
    (test "dependent binding: a comes first" 'a first-var)))

(display "
Test 13: Pure begin expressions sorted
")
(let ([e1 (normalize-algebraic '(begin (+ 2 1) (+ 1 0)))]
      [e2 (normalize-algebraic '(begin (+ 1 0) (+ 2 1)))])
  (test "pure begin sorted" e1 e2))

(display "
Test 14: Impure begin preserved
")
(let ([e1 (normalize-algebraic '(begin (set! x 1) (set! y 2)))]
      [e2 (normalize-algebraic '(begin (set! y 2) (set! x 1)))])
  (test "impure begin NOT equal"
        #f
        (equal? e1 e2)))

(display "
Test 15: Full normalization (algebraic + α)
")
(let ([e1 (normalize-full '(fn (x) (+ x 1)))]
      [e2 (normalize-full '(fn (y) (+ 1 y)))])
  (test "commutative + α equiv" e1 e2))

(let ([e1 (normalize-full '(fn (a) (fn (b) (* b a))))]
      [e2 (normalize-full '(fn (x) (fn (y) (* x y))))])
  (test "nested fn commutative" e1 e2))

(display "
Test 16: Purity analysis
")
(test "number pure" #t (expr-pure? 42))
(test "quoted pure" #t (expr-pure? '(quote (a b c))))
(test "lambda pure" #t (expr-pure? '(fn (x) x)))
(test "pure primitive" #t (expr-pure? '(+ 1 2)))
(test "set! impure" #f (expr-pure? '(set! x 1)))
(test "display impure" #f (expr-pure? '(display "hi")))
(test "unknown fn impure" #f (expr-pure? '(my-function 1 2)))

(display "
Test 17: Canonical ordering
")
(test "numbers < symbols"
      #t
      (canonical<? 1 'a))
(test "symbols alphabetical"
      #t
      (canonical<? 'a 'b))
(test "de Bruijn by index"
      #t
      (canonical<? '(dv 0) '(dv 1)))

(display "
Test 18: Hash version bytes
")
(let ([h1 (hash-sexpr 'test '(+ 1 2))]
      [h2 (hash-sexpr-algebraic 'test '(+ 1 2))])
  (test "alpha hash version 0x00" #x00 (address-version-byte h1))
  (test "algebraic hash version 0x01" #x01 (address-version-byte h2))
  (test "different hashes" #f (equal? h1 h2)))

(display "
Test 19: Commutative expressions same algebraic hash
")
(let ([h1 (hash-sexpr-algebraic 'expr '(+ a b))]
      [h2 (hash-sexpr-algebraic 'expr '(+ b a))])
  (test "commutative same hash" h1 h2))

(let ([h1 (hash-sexpr-algebraic 'expr '(+ (+ a b) c))]
      [h2 (hash-sexpr-algebraic 'expr '(+ a (+ b c)))])
  (test "associative same hash" h1 h2))

(display "
✓ All tests complete.
")
