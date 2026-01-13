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

;;; ============================================================
;;; Version 2 Normalization Tests
;;; ============================================================

(display "
Test 20: η-reduction
")
;; Simple eta reduction: (fn (x) (f x)) → f
(let ([reduced (eta-reduce '(fn (x) (f x)))])
  (test "eta-reduce (fn (x) (f x))" 'f reduced))

;; No reduction when x appears multiple times
(let ([reduced (eta-reduce '(fn (x) (g x x)))])
  (test "no eta when x appears twice" '(fn (x) (g x x)) reduced))

;; No reduction when x appears in function position
(let ([reduced (eta-reduce '(fn (x) (x y)))])
  (test "no eta when x is operator" '(fn (x) (x y)) reduced))

;; Nested eta reduction
(let ([reduced (eta-reduce '(fn (y) (fn (x) (f x))))])
  (test "nested eta" '(fn (y) f) reduced))

(display "
Test 21: Identity element elimination
")
(test "plus zero identity"
      'x
      (eliminate-identities '(+ x 0)))

(test "times one identity"
      'x
      (eliminate-identities '(* x 1)))

(test "multiple zeros"
      '(+ a b)
      (eliminate-identities '(+ a 0 b 0)))

(test "all zeros"
      0
      (eliminate-identities '(+ 0 0 0)))

(display "
Test 22: Absorbing element elimination
")
(test "times zero absorbs"
      0
      (eliminate-identities '(* x 0 y)))

;; Note: (+ (* a 0) b) first reduces (* a 0) → 0, then (+ 0 b) → b
;; This is correct: + has no absorbing element, only identity
(test "nested with inner absorb"
      'b
      (eliminate-identities '(+ (* a 0) b)))

(test "all terms absorb"
      0
      (eliminate-identities '(+ (* a 0) (* b 0))))

(display "
Test 23: Polynomial canonicalization
")
;; Same polynomial, different order
(let ([p1 (poly-canonicalize '(+ (* a b) (* c d)))]
      [p2 (poly-canonicalize '(+ (* d c) (* b a)))])
  (test "poly same regardless of order" p1 p2))

;; Collect like terms: (+ x x) → (* 2 x)
(let ([result (poly-canonicalize '(+ x x))])
  (test "collect like terms" '(* 2 x) result))

;; Simple addition stays simple
(let ([result (poly-canonicalize '(+ a b))])
  (test "simple addition canonical" '(+ a b) result))

;; Multiplication stays canonical
(let ([result (poly-canonicalize '(* a b c))])
  (test "multiplication canonical" '(* a b c) result))

;; Constant folding
(let ([result (poly-canonicalize '(+ 1 2 3))])
  (test "constant folding" 6 result))

(display "
Test 24: Combined v2 normalization
")
;; Test normalize-v2 combines η-reduction with identity elimination
;; Note: η-reduction requires body to be exactly (f x), not (+ 0 (f x))
(let ([result (normalize-v2-no-hashcons '(fn (x) (f x)))])
  (test "v2 applies eta" 'f result))

;; Test identity elimination in v2
(let ([result (normalize-v2-no-hashcons '(+ 0 (f x)))])
  (test "v2 eliminates identity" '(f x) result))

;; Test algebraic + α + identity
(let ([e1 (normalize-v2-no-hashcons '(+ a 0 b))]
      [e2 (normalize-v2-no-hashcons '(+ b a))])
  (test "v2 identity + commutative" e1 e2))

(display "
Test 25: Hash-consing
")
;; Reset stats
(hash-cons-reset!)

;; Hash-cons some expressions
(let* ([e1 (hash-cons '(+ a b))]
       [e2 (hash-cons '(+ a b))]
       [e3 (hash-cons '(+ a b))])
  ;; They should be eq? (same pointer)
  (test "hash-cons deduplicates" #t (eq? e1 e2))
  (test "hash-cons consistent" #t (eq? e2 e3)))

;; Check stats show hits
(let ([stats (hash-cons-stats)])
  (test "hash-cons has hits" #t (> (car stats) 0)))

(display "
Test 26: Version 0x02 hashing
")
(let ([h1 (hash-sexpr-v2 'test '(+ 1 2))]
      [h2 (hash-sexpr-algebraic 'test '(+ 1 2))]
      [h3 (hash-sexpr 'test '(+ 1 2))])
  (test "v2 hash version 0x02" #x02 (address-version-byte h1))
  (test "v2 different from v1" #f (equal? h1 h2))
  (test "v2 different from v0" #f (equal? h1 h3)))

;; Test v2 semantic equivalence
(let ([h1 (hash-sexpr-v2 'expr '(+ x 0))]
      [h2 (hash-sexpr-v2 'expr 'x)])
  ;; With identity elimination, these should hash the same
  (test "v2 identity equivalence" h1 h2))

(let ([h1 (hash-sexpr-v2 'expr '(+ x x))]
      [h2 (hash-sexpr-v2 'expr '(* 2 x))])
  ;; With polynomial canonicalization, these should hash the same
  (test "v2 polynomial equivalence" h1 h2))

(display "
Test 27: Float handling in poly-canon
")
;; Floats should NOT be canonicalized (precision issues)
(let ([result (arithmetic-expr? '(+ 1.5 x))])
  (test "floats excluded from poly-canon" #f result))

(let ([result (arithmetic-expr? '(+ 1 x))])
  (test "integers included in poly-canon" #t result))

(display "
✓ All tests complete.
")
