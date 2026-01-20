;;; lattice/fp/category/test-free-algebra.ss — Free Algebra Tests
;;;
;;; Tests for generalized Free ⊣ Forgetful adjunctions:
;;;   - Signature construction and operations
;;;   - Algebra construction and evaluation
;;;   - Term representation and functorial mapping
;;;   - Normalization via rewrite rules
;;;   - Triangle identities for various signatures
;;;   - Monad derivation from free adjunctions
;;;
;;; Run from project root: scheme --script lattice/fp/category/test-free-algebra.ss

(load "lattice/fp/category/free-algebra.ss")
(load "lattice/fp/category/monad-derivation.ss")

;;; ====
;;; Test Helpers
;;; ====

(define test-passed 0)
(define test-failed 0)

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (begin
        (set! test-passed (+ test-passed 1))
        (display "ok"))
      (begin
        (set! test-failed (+ test-failed 1))
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
  (display name)
  (newline))

;;; ====
;;; Test: Signature Basics
;;; ====

(section "Signature Basics")

(test-true "signature? on sig-magma"
           (signature? sig-magma))

(test "signature-name of sig-magma"
      'magma
      (signature-name sig-magma))

(test "signature-operations of sig-magma"
      '((* . 2))
      (signature-operations sig-magma))

(test "signature-laws of sig-magma (empty)"
      '()
      (signature-laws sig-magma))

(test-true "signature? on sig-monoid"
           (signature? sig-monoid))

(test "signature-name of sig-monoid"
      'monoid
      (signature-name sig-monoid))

(test "monoid has 2 operations"
      2
      (length (signature-operations sig-monoid)))

(test "monoid has 3 laws"
      3
      (length (signature-laws sig-monoid)))

(test "signature-op-arity: e is 0-arity"
      0
      (signature-op-arity sig-monoid 'e))

(test "signature-op-arity: * is 2-arity"
      2
      (signature-op-arity sig-monoid '*))

(test-true "signature-has-op?: monoid has e"
           (signature-has-op? sig-monoid 'e))

(test-true "signature-has-op?: monoid has *"
           (signature-has-op? sig-monoid '*))

(test-false "signature-has-op?: monoid lacks inv"
            (signature-has-op? sig-monoid 'inv))

(test-true "signature? on sig-group"
           (signature? sig-group))

(test "group has 3 operations"
      3
      (length (signature-operations sig-group)))

(test-true "signature-has-op?: group has inv"
           (signature-has-op? sig-group 'inv))

;;; ====
;;; Test: Algebra Basics
;;; ====

(section "Algebra Basics")

(test-true "algebra? on alg-list-monoid"
           (algebra? alg-list-monoid))

(test "algebra-carrier of list monoid"
      'list
      (algebra-carrier alg-list-monoid))

(test-true "algebra-signature returns monoid sig"
           (eq? (algebra-signature alg-list-monoid) sig-monoid))

;; Test operation lookup
(test-true "algebra-op returns procedure for e"
           (procedure? (algebra-op alg-list-monoid 'e)))

(test-true "algebra-op returns procedure for *"
           (procedure? (algebra-op alg-list-monoid '*)))

;; Test list monoid operations
(test "list monoid identity"
      '()
      ((algebra-op alg-list-monoid 'e)))

(test "list monoid *"
      '(1 2 3 4)
      ((algebra-op alg-list-monoid '*) '(1 2) '(3 4)))

;; Test sum monoid
(test "sum monoid identity"
      0
      ((algebra-op alg-sum-monoid 'e)))

(test "sum monoid *"
      15
      ((algebra-op alg-sum-monoid '*) 10 5))

;; Test integer group
(test "integer group inv"
      -5
      ((algebra-op alg-integer-group 'inv) 5))

;;; ====
;;; Test: Algebra Validation
;;; ====

(section "Algebra Validation")

;; Helper to check if substring exists
(define (string-contains s sub)
  (let loop ([i 0])
    (cond
      [(> (+ i (string-length sub)) (string-length s)) #f]
      [(string=? (substring s i (+ i (string-length sub))) sub) #t]
      [else (loop (+ i 1))])))

;; Valid algebras pass validation
(test-true "algebra-valid? on list monoid"
           (algebra-valid? alg-list-monoid))

(test-true "algebra-valid? on integer group"
           (algebra-valid? alg-integer-group))

;; validate-algebra returns (ok alg) for valid algebras
(let ([result (validate-algebra alg-list-monoid)])
  (test "validate-algebra returns ok for valid algebra"
        'ok
        (car result)))

;; Missing operations are caught
(let* ([incomplete-ops `((e . ,(lambda () '())))]  ; Missing * operation
       [bad-alg (make-algebra sig-monoid 'list incomplete-ops)]
       [result (validate-algebra bad-alg)])
  (test "validate-algebra catches missing operations"
        'err
        (car result))
  (test-true "error message mentions missing op"
             (string-contains (cadr result) "*")))

;; Non-procedure implementations are caught
(let* ([bad-ops `((e . ,(lambda () '()))
                  (* . "not-a-procedure"))]  ; Not a procedure!
       [bad-alg (make-algebra sig-monoid 'list bad-ops)]
       [result (validate-algebra bad-alg)])
  (test "validate-algebra catches non-procedure"
        'err
        (car result)))

;; Extra operations produce ok with warning
(let* ([extra-ops `((e . ,(lambda () '()))
                    (* . ,append)
                    (bonus . ,(lambda (x) x)))]  ; Extra op
       [alg (make-algebra sig-monoid 'list extra-ops)]
       [result (validate-algebra alg)])
  (test "validate-algebra ok with extra ops"
        'ok
        (car result))
  (test "extra ops returned as warning"
        '(bonus)
        (caddr result)))

;;; ====
;;; Test: Algebra Homomorphisms
;;; ====

(section "Algebra Homomorphisms")

;; Define a homomorphism from sum-monoid to product-monoid via exp
;; exp(0) = 1 (identity maps to identity)
;; exp(a + b) = exp(a) * exp(b) (preserves operation)
(define exp-hom
  (make-algebra-hom alg-sum-monoid alg-product-monoid exp))

(test-true "algebra-hom? on exp-hom"
           (algebra-hom? exp-hom))

(test "algebra-hom-source"
      alg-sum-monoid
      (algebra-hom-source exp-hom))

(test "algebra-hom-target"
      alg-product-monoid
      (algebra-hom-target exp-hom))

(test-true "algebra-hom-function is procedure"
           (procedure? (algebra-hom-function exp-hom)))

;; Test applying homomorphism
(test "algebra-hom-apply on 0"
      1  ; exp(0) = 1 (exact integer in Chez)
      (algebra-hom-apply exp-hom 0))

(test "algebra-hom-apply on 1"
      (exp 1)
      (algebra-hom-apply exp-hom 1))

;; Test identity homomorphism
(let ([id-hom (identity-algebra-hom alg-sum-monoid)])
  (test-true "identity-algebra-hom is algebra-hom"
             (algebra-hom? id-hom))
  (test "identity hom preserves value"
        42
        (algebra-hom-apply id-hom 42)))

;; Test homomorphism verification
;; exp : (Z,+,0) → (R,*,1) is a homomorphism
;; Format: ((arity . ((arg1 arg2 ...) (arg1 arg2 ...) ...)))
(test-true "verify-homomorphism: exp is homomorphism"
           (verify-homomorphism exp-hom
                                `((2 . ((1 2) (3 4) (0 5))))))

;; Non-homomorphism: square function (a+b)² ≠ a² + b²
(let ([bad-hom (make-algebra-hom alg-sum-monoid alg-sum-monoid
                                  (lambda (x) (* x x)))])
  (test-false "verify-homomorphism: square is NOT homomorphism"
              (verify-homomorphism bad-hom
                                   `((2 . ((2 3)))))))

;; Test composition
(define double-hom
  (make-algebra-hom alg-sum-monoid alg-sum-monoid
                    (lambda (x) (* 2 x))))

(let ([composed (compose-algebra-hom double-hom double-hom)])
  (test-true "compose-algebra-hom produces algebra-hom"
             (algebra-hom? composed))
  (test "composed hom quadruples"
        20
        (algebra-hom-apply composed 5)))

;;; ====
;;; Test: Term Representation
;;; ====

(section "Term Representation")

(define g1 (make-gen 'a))
(define g2 (make-gen 'b))

(test-true "gen? on generator"
           (gen? g1))

(test "gen-value extracts value"
      'a
      (gen-value g1))

(test-false "gen? on non-generator"
            (gen? '(* a b)))

(test-true "term-op? recognizes 0-arity op"
           (term-op? sig-monoid 'e))

(test-true "term-op? recognizes operation application"
           (term-op? sig-monoid '(* a b)))

(test-false "term-op? rejects unknown op"
            (term-op? sig-monoid '(unknown a b)))

(test-true "term? on generator"
           (term? sig-monoid g1))

(test-true "term? on 0-arity op"
           (term? sig-monoid 'e))

(test-true "term? on binary op"
           (term? sig-monoid '(* (gen a) (gen b))))

;;; ====
;;; Test: Functorial Mapping (free-fmap)
;;; ====

(section "Functorial Mapping")

;; Map over a single generator
(test "free-fmap on generator"
      (make-gen 10)
      (free-fmap sig-monoid (lambda (x) (* x 2)) (make-gen 5)))

;; Map over 0-arity constant
(test "free-fmap preserves 0-arity op"
      'e
      (free-fmap sig-monoid (lambda (x) 'changed) 'e))

;; Map over binary operation
(test "free-fmap over (* gen gen)"
      '(* (gen 2) (gen 4))
      (free-fmap sig-monoid (lambda (x) (* x 2)) '(* (gen 1) (gen 2))))

;; Nested operations
(test "free-fmap over nested ops"
      '(* (gen 10) (* (gen 20) (gen 30)))
      (free-fmap sig-monoid (lambda (x) (* x 10))
                 '(* (gen 1) (* (gen 2) (gen 3)))))

;; Mixed: operation with generators and constants
(test "free-fmap leaves constants unchanged"
      '(* e (gen 10))
      (free-fmap sig-monoid (lambda (x) (* x 2)) '(* e (gen 5))))

;;; ====
;;; Test: Term Evaluation
;;; ====

(section "Term Evaluation")

;; Evaluate generator
(test "eval-term: generator"
      '(1 2 3)
      (eval-term alg-list-monoid (make-gen '(1 2 3))))

;; Evaluate 0-arity constant
(test "eval-term: identity"
      '()
      (eval-term alg-list-monoid 'e))

;; Evaluate binary operation on generators
(test "eval-term: (* gen gen)"
      '(1 2 3 4)
      (eval-term alg-list-monoid
                 '(* (gen (1 2)) (gen (3 4)))))

;; Nested evaluation
(test "eval-term: nested ops"
      '(1 2 3 4 5 6)
      (eval-term alg-list-monoid
                 '(* (gen (1 2)) (* (gen (3 4)) (gen (5 6))))))

;; With identity
(test "eval-term: with identity"
      '(1 2 3)
      (eval-term alg-list-monoid
                 '(* e (gen (1 2 3)))))

;; Sum monoid evaluation
(test "eval-term: sum monoid"
      15
      (eval-term alg-sum-monoid
                 '(* (gen 5) (* (gen 7) (gen 3)))))

;; Group inverse
(test "eval-term: group inverse"
      0
      (eval-term alg-integer-group
                 '(* (gen 5) (inv (gen 5)))))

;;; ====
;;; Test: Term Normalization
;;; ====

(section "Term Normalization")

;; Left identity: (* e x) → x
(test "normalize-term: left identity"
      '(gen a)
      (normalize-term sig-monoid '(* e (gen a))))

;; Right identity: (* x e) → x
(test "normalize-term: right identity"
      '(gen b)
      (normalize-term sig-monoid '(* (gen b) e)))

;; Associativity: (* (* x y) z) → (* x (* y z))
(test "normalize-term: associativity"
      '(* (gen a) (* (gen b) (gen c)))
      (normalize-term sig-monoid '(* (* (gen a) (gen b)) (gen c))))

;; Combined: (* e (* (* a b) c)) → (* a (* b c))
(let ([normalized (normalize-term sig-monoid '(* e (* (* (gen a) (gen b)) (gen c))))])
  ;; After left-id: (* (* (gen a) (gen b)) (gen c))
  ;; After assoc: (* (gen a) (* (gen b) (gen c)))
  (test "normalize-term: combined laws"
        '(* (gen a) (* (gen b) (gen c)))
        normalized))

;; Group: inverse of inverse
(test "normalize-term: double inverse"
      '(gen x)
      (normalize-term sig-group '(inv (inv (gen x)))))

;; Group: inverse of identity
(test "normalize-term: inv e = e"
      'e
      (normalize-term sig-group '(inv e)))

;; Magma: no normalization (no laws)
(test "normalize-term: magma unchanged"
      '(* (* (gen a) (gen b)) (gen c))
      (normalize-term sig-magma '(* (* (gen a) (gen b)) (gen c))))

;;; ====
;;; Test: Adjunction Construction
;;; ====

(section "Adjunction Construction")

(test-true "adj-free-monoid is an adjunction"
           (adjunction? adj-free-monoid))

(test "adjunction-name of adj-free-monoid"
      'free-monoid
      (adjunction-name adj-free-monoid))

(test-true "adjunction-left is a functor"
           (functor? (adjunction-left adj-free-monoid)))

(test-true "adjunction-right is a functor"
           (functor? (adjunction-right adj-free-monoid)))

(test-true "adjunction-unit is nat-transform"
           (nat-transform? (adjunction-unit adj-free-monoid)))

(test-true "adjunction-counit is nat-transform"
           (nat-transform? (adjunction-counit adj-free-monoid)))

;; Check functor names
(test "free functor name"
      'Free-monoid
      (functor-name (adjunction-left adj-free-monoid)))

(test "forgetful functor name"
      'Forget-monoid
      (functor-name (adjunction-right adj-free-monoid)))

;;; ====
;;; Test: Triangle Identities
;;; ====

(section "Triangle Identities")

;; For the generalized free adjunction, triangle identities work on terms.
;; Left triangle: ε_{F(A)} . F(η_A) = id_{F(A)}
;; For a term t, F(η)(t) wraps generators in gen, then ε unwraps them.

;; Test with a simple generator term
(let* ([term (make-gen 'x)]
       [F (adjunction-left adj-free-monoid)]
       [eta (adjunction-unit adj-free-monoid)]
       [eps (adjunction-counit adj-free-monoid)]
       [F-fmap (functor-fmap F)]
       [eta-comp (nat-transform-component eta)]
       [eps-comp (nat-transform-component eps)]
       ;; Step 1: F(η)(term) = fmap make-gen over term
       [step1 (F-fmap eta-comp term)]
       ;; Step 2: ε(step1)
       [result (eps-comp step1)])
  ;; For (gen x), F(η) gives (gen (gen x)), ε gives (gen x)
  ;; Wait, that's not quite right. Let me trace through:
  ;; term = (gen x)
  ;; F(η)(term) = free-fmap make-gen (gen x) = (gen (gen x))
  ;; ε((gen (gen x))) should give (gen x)... but our eps-comp just extracts gen-value
  ;; eps-comp on (gen (gen x)) returns (gen x) ✓
  (test "left triangle on generator term"
        term
        result))

;; Right triangle: G(ε_B) . η_{G(B)} = id_{G(B)}
;; For a value v in G(B), η(v) = (gen v), then G(ε)(gen v) = v
(let* ([val 'test-value]
       [G (adjunction-right adj-free-monoid)]
       [eta (adjunction-unit adj-free-monoid)]
       [eps (adjunction-counit adj-free-monoid)]
       [G-fmap (functor-fmap G)]
       [eta-comp (nat-transform-component eta)]
       [eps-comp (nat-transform-component eps)]
       ;; Step 1: η(val) = (gen val)
       [step1 (eta-comp val)]
       ;; Step 2: G(ε)(step1) = ε applied via G's fmap
       [result (G-fmap eps-comp step1)])
  ;; η(val) = (gen val)
  ;; G(ε)((gen val)) = eps-comp((gen val)) = val ✓
  (test "right triangle on value"
        val
        result))

;; Additional triangle tests with different signatures
(test-true "adj-free-magma is an adjunction"
           (adjunction? adj-free-magma))

(test-true "adj-free-group is an adjunction"
           (adjunction? adj-free-group))

;; Complex term triangle identities (QA-identified gap)
;; These test that the counit correctly handles nested operations, not just generators.
;; The bug was: counit only extracted top-level generators, breaking identity for complex terms.

(let* ([term '(* (gen a) (gen b))]  ; Binary operation with generators
       [F (adjunction-left adj-free-monoid)]
       [eta (adjunction-unit adj-free-monoid)]
       [eps (adjunction-counit adj-free-monoid)]
       [F-fmap (functor-fmap F)]
       [eta-comp (nat-transform-component eta)]
       [eps-comp (nat-transform-component eps)]
       ;; F(η)(term) = free-fmap make-gen term
       ;; = (* (gen (gen a)) (gen (gen b)))
       [step1 (F-fmap eta-comp term)]
       ;; ε(step1) should recursively extract generators
       ;; = (* (gen a) (gen b)) = original term
       [result (eps-comp step1)])
  (test "left triangle on binary op term"
        term
        result))

(let* ([term '(* (gen a) (* (gen b) (gen c)))]  ; Nested operations
       [F (adjunction-left adj-free-monoid)]
       [eta (adjunction-unit adj-free-monoid)]
       [eps (adjunction-counit adj-free-monoid)]
       [F-fmap (functor-fmap F)]
       [eta-comp (nat-transform-component eta)]
       [eps-comp (nat-transform-component eps)]
       [step1 (F-fmap eta-comp term)]
       [result (eps-comp step1)])
  (test "left triangle on nested ops"
        term
        result))

(let* ([term '(* e (gen x))]  ; Mixed: 0-arity and generator
       [F (adjunction-left adj-free-monoid)]
       [eta (adjunction-unit adj-free-monoid)]
       [eps (adjunction-counit adj-free-monoid)]
       [F-fmap (functor-fmap F)]
       [eta-comp (nat-transform-component eta)]
       [eps-comp (nat-transform-component eps)]
       [step1 (F-fmap eta-comp term)]
       [result (eps-comp step1)])
  (test "left triangle on term with constant"
        term
        result))

;; Test with group signature (has inv operation)
(let* ([term '(* (gen x) (inv (gen y)))]
       [F (adjunction-left adj-free-group)]
       [eta (adjunction-unit adj-free-group)]
       [eps (adjunction-counit adj-free-group)]
       [F-fmap (functor-fmap F)]
       [eta-comp (nat-transform-component eta)]
       [eps-comp (nat-transform-component eps)]
       [step1 (F-fmap eta-comp term)]
       [result (eps-comp step1)])
  (test "left triangle on group term with inv"
        term
        result))

;;; ====
;;; Test: Monad Derivation
;;; ====

(section "Monad Derivation from Free Adjunction")

(define monad-free-monoid
  (monad-from-adjunction adj-free-monoid))

(test-true "monad-from-adjunction produces MonadOps"
           (monad-ops? monad-free-monoid))

(test "monad name"
      'monad-free-monoid
      (monad-ops-name monad-free-monoid))

;; Test monad operations
(let ([return (monad-ops-return monad-free-monoid)]
      [fmap (monad-ops-fmap monad-free-monoid)]
      [join (monad-ops-join monad-free-monoid)]
      [bind (monad-ops-bind monad-free-monoid)])

  ;; return wraps in generator
  (test "monad return"
        (make-gen 42)
        (return 42))

  ;; fmap maps over generators
  (test "monad fmap"
        (make-gen 84)
        (fmap (lambda (x) (* x 2)) (make-gen 42)))

  ;; join flattens nested generators
  ;; join((gen (gen x))) should give (gen x)
  ;; Actually, join = G(ε), which for our adjunction means applying ε via G's fmap
  ;; G-fmap ε-comp (gen (gen x)) = ε-comp((gen x)) via G's identity-like fmap
  ;; = (gen x) via eps-comp extracting from outer gen... hmm
  ;; Let me check: our G-fmap is (lambda (f x) (f x)), so G(ε)(val) = ε(val)
  ;; join((gen (gen x))) = ε((gen (gen x)))
  ;;                     = gen-value((gen (gen x))) = (gen x) if outer is gen
  ;; But (gen (gen x)) has outer = gen, so gen-value = (gen x) ✓
  (test "monad join on nested"
        (make-gen 'x)
        (join (make-gen (make-gen 'x))))

  ;; bind m f = join (fmap f m)
  (test "monad bind"
        (make-gen 84)
        (bind (make-gen 42) (lambda (x) (make-gen (* x 2))))))

;;; ====
;;; Test: eval-in-algebra
;;; ====

(section "Evaluation in Algebra Context")

;; Build a term and evaluate with normalization
(test "eval-in-algebra: with normalization"
      '(1 2 3 4 5)
      (eval-in-algebra alg-list-monoid
                       '(* e (* (gen (1 2)) (* (gen (3 4)) (gen (5)))))))

;; Create evaluator for specific algebra
(define eval-sum (make-algebra-evaluator alg-sum-monoid))

(test "make-algebra-evaluator works"
      15
      (eval-sum '(* (gen 5) (* (gen 7) (gen 3)))))

;; Normalization simplifies before evaluation
(test "normalized evaluation: (* e (gen 10))"
      10
      (eval-sum '(* e (gen 10))))

;;; ====
;;; Test: Display Functions
;;; ====

(section "Display Functions")

(test-true "signature->string returns string"
           (string? (signature->string sig-monoid)))

(test-true "algebra->string returns string"
           (string? (algebra->string alg-list-monoid)))

(test-true "term->string returns string"
           (string? (term->string sig-monoid (make-gen 'x))))

;;; ====
;;; Summary
;;; ====

(newline)
(display "==========================================")
(newline)
(display (format "Free Algebra Tests: ~a passed, ~a failed"
                 test-passed test-failed))
(newline)

(when (> test-failed 0)
  (exit 1))
