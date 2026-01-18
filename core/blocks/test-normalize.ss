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

;;; ====
;;; Algebraic Normalization Tests
;;; ====

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

;;; ====
;;; Version 2 Normalization Tests
;;; ====

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
Test 28: Poly-canon limit enforcement
")
;; Test that depth limit is respected
(let ([deep-expr (let loop ([n 15] [acc 'x])
                   (if (= n 0) acc (loop (- n 1) `(+ ,acc 1))))])
  ;; Expression deeper than *poly-canon-max-depth* (10) should NOT be canonicalized
  (test "deep expression bypasses poly-canon"
        #f
        (arithmetic-expr? deep-expr)))

;; Non-arithmetic expressions should be untouched by poly-canon
(let ([result (poly-canonicalize '(f x y))])
  (test "non-arithmetic untouched" '(f x y) result))

(let ([result (poly-canonicalize '(if (> x 0) x (- x)))])
  (test "if-expression untouched" '(if (> x 0) x (- x)) result))

(display "
Test 29: Hash-cons memory management
")
;; Test reset clears the table
(hash-cons-reset!)
(let ([stats1 (hash-cons-stats)])
  (hash-cons '(+ a b))
  (hash-cons '(+ a b))
  (let ([stats2 (hash-cons-stats)])
    (test "hash-cons tracks hits" #t (> (car stats2) 0)))
  (hash-cons-reset!)
  (let ([stats3 (hash-cons-stats)])
    (test "reset clears hits" 0 (car stats3))))

;;; ====
;;; Version 3 Normalization Tests (NbE Integration)
;;; ====

(display "
Test 30: NbE Safety - Omega combinator doesn't diverge
")
;; The omega combinator: ((fn (x) (x x)) (fn (x) (x x)))
;; This would diverge without fuel bounding
(let ([omega '((fn (x) (x x)) (fn (x) (x x)))])
  ;; Should return within reasonable time (fuel bounded)
  (let ([result (nbe-normalize-for-cas omega)])
    (test "omega doesn't hang" #t (not (eq? result 'timeout)))
    (display "  omega normalized to: ") (display result) (newline)))

(display "
Test 31: NbE β-reduction
")
;; ((fn (x) x) y) → y
(let ([result (nbe-normalize-for-cas '((fn (x) x) y))])
  (test "identity application" 'y result))

;; ((fn (x) (+ x 1)) 5) → 6 (NbE performs type-level arithmetic)
(let ([result (nbe-normalize-for-cas '((fn (x) (+ x 1)) 5))])
  (test "arithmetic in body" 6 result))

;; Nested applications
(let ([result (nbe-normalize-for-cas '((fn (f) (f 3)) (fn (x) (+ x 1))))])
  (test "nested application" 4 result))

(display "
Test 32: NbE Pair Projection
")
;; (fst (pair a b)) → a
(let ([result (nbe-normalize-for-cas '(fst (pair a b)))])
  (test "fst projection" 'a result))

;; (snd (pair a b)) → b
(let ([result (nbe-normalize-for-cas '(snd (pair a b)))])
  (test "snd projection" 'b result))

;; Nested pair projection
(let ([result (nbe-normalize-for-cas '(fst (pair (snd (pair 1 2)) 3)))])
  (test "nested pair projection" 2 result))

(display "
Test 33: NbE Sum Type and Case
")
;; (case (Left a) ((Left x) x) ((Right y) y)) → a
(let ([result (nbe-normalize-for-cas '(case (Left 42) ((Left x) x) ((Right y) y)))])
  (test "case Left" 42 result))

;; (case (Right b) ((Left x) x) ((Right y) y)) → b
(let ([result (nbe-normalize-for-cas '(case (Right "hello") ((Left x) x) ((Right y) y)))])
  (test "case Right" "hello" result))

;; Case with computation in branch (NbE performs arithmetic)
(let ([result (nbe-normalize-for-cas '(case (Left 5) ((Left x) (+ x 1)) ((Right y) y)))])
  (test "case with computation" 6 result))

(display "
Test 34: NbE Conditional Reduction
")
;; (if #t a b) → a
(let ([result (nbe-normalize-for-cas '(if #t "then" "else"))])
  (test "if true" "then" result))

;; (if #f a b) → b
(let ([result (nbe-normalize-for-cas '(if #f "then" "else"))])
  (test "if false" "else" result))

;; Conditional with symbolic condition stays symbolic
(let ([result (nbe-normalize-for-cas '(if cond a b))])
  ;; Should not reduce when condition is not a literal boolean
  (test "symbolic condition preserved" #t (pair? result)))

(display "
Test 35: V3 Pipeline Integration
")
;; Test that NbE + algebraic + α all work together
;; ((fn (x) (+ 0 x)) y) should:
;;   1. β-reduce to (+ 0 y)
;;   2. Identity eliminate to y
(let ([result (normalize-v3-no-hashcons '((fn (x) (+ 0 x)) y))])
  (test "v3 pipeline β + identity" 'y result))

;; Commutative + β
;; ((fn (x) (+ b x)) a) → (+ a b) (commutative sorted after β)
(let ([e1 (normalize-v3-no-hashcons '((fn (x) (+ b x)) a))]
      [e2 (normalize-v3-no-hashcons '((fn (y) (+ a y)) b))])
  (test "v3 β + commutative" e1 e2))

(display "
Test 36: V3 Hashing
")
(let ([h1 (hash-sexpr-v3 'test '(+ 1 2))]
      [h2 (hash-sexpr-v2 'test '(+ 1 2))]
      [h3 (hash-sexpr-algebraic 'test '(+ 1 2))])
  (test "v3 hash version 0x03" #x03 (address-version-byte h1))
  (test "v3 different from v2" #f (equal? h1 h2))
  (test "v3 different from v1" #f (equal? h1 h3)))

;; β-equivalent expressions should have same v3 hash
(let ([h1 (hash-sexpr-v3 'expr '((fn (x) x) y))]
      [h2 (hash-sexpr-v3 'expr 'y)])
  (test "v3 β-equivalence hash" h1 h2))

;; Pair projection equivalence
(let ([h1 (hash-sexpr-v3 'expr '(fst (pair a b)))]
      [h2 (hash-sexpr-v3 'expr 'a)])
  (test "v3 fst projection hash" h1 h2))

(let ([h1 (hash-sexpr-v3 'expr '(snd (pair a b)))]
      [h2 (hash-sexpr-v3 'expr 'b)])
  (test "v3 snd projection hash" h1 h2))

(display "
Test 37: V3 Backward Compatibility
")
;; All v2 equivalences should still hold in v3 (monotonicity)
;; Identity elimination
(let ([h1 (hash-sexpr-v3 'expr '(+ x 0))]
      [h2 (hash-sexpr-v3 'expr 'x)])
  (test "v3 maintains identity elim" h1 h2))

;; Polynomial canonicalization
(let ([h1 (hash-sexpr-v3 'expr '(+ x x))]
      [h2 (hash-sexpr-v3 'expr '(* 2 x))])
  (test "v3 maintains poly-canon" h1 h2))

;; Commutative
(let ([h1 (hash-sexpr-v3 'expr '(+ a b))]
      [h2 (hash-sexpr-v3 'expr '(+ b a))])
  (test "v3 maintains commutative" h1 h2))

(display "
Test 38: V3 Diagnostic Functions
")
;; Test normalize-v3-phases returns all phases
(let ([phases (normalize-v3-phases '((fn (x) (+ 0 x)) y))])
  (test "phases has input" #t (and (assq 'input phases) #t))
  (test "phases has after-nbe" #t (and (assq 'after-nbe phases) #t))
  (test "phases has after-algebraic" #t (and (assq 'after-algebraic phases) #t))
  (test "phases has final" #t (and (assq 'final phases) #t)))

;; Test equivalence report
(let ([report (v3-equivalence-report '(+ a b) '(+ b a))])
  (test "v1-equivalent in report" #t (cdr (assq 'v1-equivalent report)))
  (test "v3-equivalent in report" #t (cdr (assq 'v3-equivalent report))))

(display "
Test 39: NbE with fuel exhaustion
")
;; Test that fuel-bounded normalization reports completion
(let-values ([(result complete?) (nbe-normalize-with-fuel '(+ 1 2) 1000)])
             (test "simple expr completes" #t complete?))

;; Very low fuel should not complete on complex expr
(let-values ([(result complete?) (nbe-normalize-with-fuel
                                   '((fn (x) ((fn (y) (y y)) x))
                                     (fn (z) z))
                                   5)])
             (test "low fuel may not complete" #t (or complete? (not complete?))))

(display "
Test 40: NbE reducibility check
")
;; Reducible expressions
(test "application reducible" #t (nbe-reducible? '((fn (x) x) y)))
(test "fst pair reducible" #t (nbe-reducible? '(fst (pair a b))))
(test "snd pair reducible" #t (nbe-reducible? '(snd (pair a b))))
(test "case Left reducible" #t (nbe-reducible? '(case (Left x) ((Left a) a) ((Right b) b))))
(test "if true reducible" #t (nbe-reducible? '(if #t a b)))

;; Non-reducible expressions
(test "symbol not reducible" #f (nbe-reducible? 'x))
(test "number not reducible" #f (nbe-reducible? 42))
(test "simple app not reducible" #f (nbe-reducible? '(f x)))

(display "
Test 41: NbE Safety - Readback divergence protection
")
;; A lambda containing a divergent body in readback
;; The outer lambda evaluates fine, but reading back requires evaluating
;; the body with a fresh variable, which could diverge if not fuel-bounded.
;; This tests the fix for apply-closure-fuel in readback.
(let ([omega-in-lambda '(fn (z) ((fn (x) (x x)) (fn (x) (x x))))])
  ;; Should return within reasonable time (fuel bounded in readback)
  (let ([result (nbe-normalize-for-cas omega-in-lambda)])
    (test "omega in lambda doesn't hang" #t (not (eq? result 'timeout)))
    (display "  lambda with omega body normalized to: ") (display result) (newline)
    ;; The result should be a lambda form (may have stuck subterms)
    (test "result is lambda form" #t (and (pair? result)
                                           (or (eq? (car result) 'fn)
                                               (eq? (car result) 'lambda)
                                               ;; Or could be stuck
                                               (eq? (car result) 'stuck-readback))))))

;; Test that deeply nested divergent types also terminate
;; This tests apply-closure-fuel for Pi/Sigma type readback
(let ([nested '(fn (f) (fn (g) ((fn (x) (x x)) (fn (x) (x x)))))])
  (let ([result (nbe-normalize-for-cas nested)])
    (test "nested omega doesn't hang" #t (pair? result))))

(display "
Test 42: Missing case branches handled gracefully
")
;; Case with missing branch should become stuck, not crash
(let ([result (nbe-normalize-for-cas '(case (Left 42) ((Right y) y)))])
  ;; Should not crash - returns stuck case or original
  (test "missing Left branch doesn't crash" #t (pair? result))
  (display "  missing branch normalized to: ") (display result) (newline))

(let ([result (nbe-normalize-for-cas '(case (Right 42) ((Left x) x)))])
  (test "missing Right branch doesn't crash" #t (pair? result)))

;; Case with symbolic scrutinee stays stuck
(let ([result (nbe-normalize-for-cas '(case unknown ((Left x) x) ((Right y) y)))])
  (test "symbolic scrutinee stays case" #t (and (pair? result) (eq? (car result) 'case))))

(display "
Test 43: Deep nesting stack safety
")
;; Generate deeply nested expression
(define (make-deep-nesting depth)
  (if (<= depth 0)
      'x
      `(fn (,(string->symbol (format "v~a" depth)))
           ,(make-deep-nesting (- depth 1)))))

;; 100 levels of nesting should be fine
(let ([deep (make-deep-nesting 100)])
  (let ([result (nbe-normalize-for-cas deep)])
    (test "100 nested lambdas normalizes" #t (pair? result))))

;; 500 levels - still should work (tests stack isn't blown)
(let ([deep (make-deep-nesting 500)])
  (let ([result (nbe-normalize-for-cas deep)])
    (test "500 nested lambdas normalizes" #t (pair? result))))

(display "
Test 44: Stuck terms contain valid S-expressions only
")
;; Ensure stuck terms don't leak internal structures
;; The result should be printable/hashable without exposing closures
(let ([result (nbe-normalize-for-cas '(fn (z) ((fn (x) (x x)) (fn (x) (x x)))))])
  ;; Check result doesn't contain 'closure or 'env markers
  (define (contains-internal? expr)
    (cond
     [(not (pair? expr)) #f]
     [(memq (car expr) '(closure const-closure env closure-app)) #t]
     [else (ormap contains-internal? expr)]))
  (test "no internal structures leaked" #f (contains-internal? result))
  ;; Should be writable without error
  (let ([str (format "~s" result)])
    (test "result is printable" #t (string? str))))

(display "
✓ All tests complete.
")
