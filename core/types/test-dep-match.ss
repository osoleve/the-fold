;;; core/types/test-dep-match.ss — Tests for Dependent Pattern Matching
;;;
;;; Run from project root: scheme --script core/types/test-dep-match.ss

(load "core/types/dep-match.ss")

(define *tests-passed* 0)
(define *tests-failed* 0)

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (begin
       (set! *tests-passed* (+ *tests-passed* 1))
       (display "✓"))
      (begin
       (set! *tests-failed* (+ *tests-failed* 1))
       (display "✗")
       (newline)
       (display "    expected: ")
       (write expected)
       (newline)
       (display "    got: ")
       (write actual)))
  (newline))

(define (test-section name)
  (newline)
  (display name)
  (newline))

;;; ============================================================
;;; Pattern Parsing Tests
;;; ============================================================
(test-section "Pattern Parsing")

;; Variable pattern
(let ([p (parse-dep-pattern 'x)])
     (test "var pattern kind" 'var (dep-pattern-kind p))
     (test "var pattern data" 'x (dep-pattern-data p)))

;; Wildcard pattern
(let ([p (parse-dep-pattern '_)])
     (test "wildcard pattern kind" 'wildcard (dep-pattern-kind p))
     (test "wildcard pattern data" #f (dep-pattern-data p)))

;; Dot pattern
(let ([p (parse-dep-pattern (list (string->symbol ".") 'zero))])
     (test "dot pattern kind" 'dot (dep-pattern-kind p))
     (test "dot pattern data" 'zero (dep-pattern-data p)))

;; Constructor pattern: nil
(let ([p (parse-dep-pattern '(nil))])
     (test "nil pattern kind" 'ctor (dep-pattern-kind p))
     (test "nil pattern ctor name" 'nil (dep-pattern-ctor-name p))
     (test "nil pattern no subpats" '() (dep-pattern-subpatterns p)))

;; Constructor pattern: (cons x xs)
(let ([p (parse-dep-pattern '(cons x xs))])
     (test "cons pattern kind" 'ctor (dep-pattern-kind p))
     (test "cons pattern ctor name" 'cons (dep-pattern-ctor-name p))
     (test "cons pattern has 2 subpats" 2 (length (dep-pattern-subpatterns p))))

;; Nested constructor pattern: (cons x (cons y nil))
(let ([p (parse-dep-pattern '(cons x (cons y (nil))))])
     (test "nested pattern kind" 'ctor (dep-pattern-kind p))
     (let ([subpats (dep-pattern-subpatterns p)])
          (test "nested: first is var" 'var (dep-pattern-kind (car subpats)))
          (test "nested: second is ctor" 'ctor (dep-pattern-kind (cadr subpats)))))

;;; ============================================================
;;; Pattern Predicate Tests
;;; ============================================================
(test-section "Pattern Predicates")

(test "dep-pattern-var?" #t (dep-pattern-var? (parse-dep-pattern 'x)))
(test "not dep-pattern-var?" #f (dep-pattern-var? (parse-dep-pattern '(nil))))
(test "dep-pattern-ctor?" #t (dep-pattern-ctor? (parse-dep-pattern '(cons x xs))))
(test "dep-pattern-wildcard?" #t (dep-pattern-wildcard? (parse-dep-pattern '_)))
(test "dep-pattern-dot?" #t (dep-pattern-dot? (parse-dep-pattern (list (string->symbol ".") 'zero))))

;;; ============================================================
;;; Type Refinement Tests
;;; ============================================================
(test-section "Type Refinement")

;; Empty refinement
(let ([r (empty-refinement)])
     (test "empty refinement subst" '() (type-refinement-substitution r))
     (test "empty refinement constraints" '() (type-refinement-constraints r)))

;; Add to refinement
(let* ([r (empty-refinement)]
       [r (refinement-add r 'n 'zero)])
      (test "refinement-add: subst has entry" #t
            (pair? (assq 'n (type-refinement-substitution r)))))

;; Apply refinement
(let* ([r (empty-refinement)]
       [r (refinement-add r 'n '(succ m))]
       [type '(Vec n A)])
      (test "apply-refinement" '(Vec (succ m) A) (apply-refinement r type)))

;; Merge refinements
(let* ([r1 (refinement-add (empty-refinement) 'a 'Int)]
       [r2 (refinement-add (empty-refinement) 'b 'Bool)]
       [merged (merge-refinements r1 r2)])
      (test "merged has a" #t (pair? (assq 'a (type-refinement-substitution merged))))
      (test "merged has b" #t (pair? (assq 'b (type-refinement-substitution merged)))))

;;; ============================================================
;;; Type Unification Tests
;;; ============================================================
(test-section "Type Unification")

;; Equal types
(test "unify equal types" '() (unify-types 'Int 'Int))
(test "unify equal compound" '() (unify-types '(List Int) '(List Int)))

;; Type variable on left
(let ([result (unify-types 'a 'Int)])
     (test "unify a with Int" #t (pair? result))
     (test "a maps to Int" 'Int (cdr (car result))))

;; Type variable on right
(let ([result (unify-types 'Bool 'b)])
     (test "unify Bool with b" #t (pair? result))
     (test "b maps to Bool" 'Bool (cdr (car result))))

;; Compound unification
(let ([result (unify-types '(Vec n a) '(Vec (succ m) Int))])
     (test "unify Vec types" #t (list? result))
     (test "n ~ (succ m)" '(succ m) (cdr (assq 'n result)))
     (test "a ~ Int" 'Int (cdr (assq 'a result))))

;; Mismatch
(test "unify mismatch" 'mismatch (unify-types '(List a) '(Vec n b)))
(test "unify arity mismatch" 'mismatch (unify-types '(Pair a) '(Pair a b c)))

;; Occurs check
(test "occurs check" 'mismatch (unify-types 'a '(List a)))

;;; ============================================================
;;; Constructor Type Accessors
;;; ============================================================
(test-section "Constructor Type Accessors")

;; Simple function type
(test "get-ctor-return-type: simple" 'R (get-ctor-return-type '(-> A B R)))

;; Pi type
(let ([type '(Π ((x : A)) B)])
     (test "get-ctor-return-type: pi" 'B (get-ctor-return-type type)))

;; Nested Pi
(let ([type '(Π ((x : A)) (Π ((y : B)) C))])
     (test "get-ctor-return-type: nested pi" 'C (get-ctor-return-type type)))

;; Forall type
(let ([type '(∀ (a) (-> a a))])
     (test "get-ctor-return-type: forall" 'a (get-ctor-return-type type)))

;; Param types
(test "get-ctor-param-types: function" '(A B) (get-ctor-param-types '(-> A B R)))
(test "get-ctor-param-types: pi" '(A) (get-ctor-param-types '(Π ((x : A)) B)))

;;; ============================================================
;;; Coverage Checking Tests
;;; ============================================================
(test-section "Coverage Checking")

;; With data declaration
(let* ([data (t-data-simple 'Bool '((true . Bool) (false . Bool)))]
       [pats (list (parse-dep-pattern '(true))
                   (parse-dep-pattern '(false)))])
      (test "coverage: complete" '(ok complete) (coverage-check pats 'Bool data)))

;; Missing pattern
(let* ([data (t-data-simple 'Bool '((true . Bool) (false . Bool)))]
       [pats (list (parse-dep-pattern '(true)))])
      (let ([result (coverage-check pats 'Bool data)])
           (test "coverage: missing" 'error (car result))))

;; Wildcard covers all
(let* ([data (t-data-simple 'Bool '((true . Bool) (false . Bool)))]
       [pats (list (parse-dep-pattern '_))])
      (test "coverage: wildcard" '(ok complete) (coverage-check pats 'Bool data)))

;; Variable covers all
(let* ([data (t-data-simple 'Bool '((true . Bool) (false . Bool)))]
       [pats (list (parse-dep-pattern 'b))])
      (test "coverage: var" '(ok complete) (coverage-check pats 'Bool data)))

;;; ============================================================
;;; Impossible Case Detection
;;; ============================================================
(test-section "Impossible Case Detection")

;; For these tests, we need a Vec-like declaration
(let ([vec-data (t-data 'Vec '((n : Nat) A)
                        '((nil . (Vec zero A))
                          (cons . (Π ((x : A)) (Π ((m : Nat)) (Π ((xs : (Vec m A))) (Vec (succ m) A)))))))])
     ;; cons pattern against Vec 0 A should be impossible
     (let ([cons-pat (parse-dep-pattern '(cons x xs))])
          (test "impossible: cons vs Vec 0" #t
                (is-impossible-case? cons-pat '(Vec zero A) vec-data)))
     
     ;; nil pattern against Vec 0 A should be possible
     (let ([nil-pat (parse-dep-pattern '(nil))])
          (test "possible: nil vs Vec 0" #f
                (is-impossible-case? nil-pat '(Vec zero A) vec-data))))

;;; ============================================================
;;; Pattern Matching with Refinement
;;; ============================================================
(test-section "Pattern Matching with Refinement")

;; Variable pattern
(let* ([data #f]
       [pat (parse-dep-pattern 'x)]
       [result (match-pattern-with-refinement pat 'Int data)])
      (test "var match: success" 'ok (car result))
      (test "var match: binding" 'Int (cdr (car (cadr result)))))

;; Wildcard pattern
(let* ([data #f]
       [pat (parse-dep-pattern '_)]
       [result (match-pattern-with-refinement pat 'Int data)])
      (test "wildcard match: success" 'ok (car result))
      (test "wildcard match: no bindings" '() (cadr result)))

;;; ============================================================
;;; Case Tree Tests
;;; ============================================================
(test-section "Case Tree")

(let ([leaf (make-leaf '(+ x 1))])
     (test "leaf is leaf" #t (case-tree-leaf? leaf))
     (test "leaf data" '(+ x 1) (case-tree-data leaf)))

(let ([split (make-split 'e '((true . #f) (false . #f)))])
     (test "split is split" #t (case-tree-split? split))
     (test "split scrutinee" 'e (car (case-tree-data split))))

;;; ============================================================
;;; Pretty Printing
;;; ============================================================
(test-section "Pretty Printing")

(test "pp: var" "x" (dep-pattern->string (parse-dep-pattern 'x)))
(test "pp: wildcard" "_" (dep-pattern->string (parse-dep-pattern '_)))
(test "pp: dot" "(. zero)" (dep-pattern->string (parse-dep-pattern (list (string->symbol ".") 'zero))))
(test "pp: nil" "nil" (dep-pattern->string (parse-dep-pattern '(nil))))
(test "pp: cons" "(cons x xs)" (dep-pattern->string (parse-dep-pattern '(cons x xs))))

(test "pp: empty refinement" "(no refinements)" (refinement->string (empty-refinement)))
(let ([ref (refinement-add (empty-refinement) 'n 'zero)])
     (test "pp: refinement with n" #t
           (string? (refinement->string ref))))

;;; ============================================================
;;; Standard Declarations
;;; ============================================================
(test-section "Standard Indexed Types")

(test "Vec-decl is data" #t (data-type? Vec-decl))
(test "Vec-decl name" 'Vec (data-name Vec-decl))
(test "Vec-decl has nil" #t (pair? (assq 'nil (data-constructors Vec-decl))))
(test "Vec-decl has cons" #t (pair? (assq 'cons (data-constructors Vec-decl))))

(test "Fin-decl is data" #t (data-type? Fin-decl))
(test "Fin-decl name" 'Fin (data-name Fin-decl))
(test "Fin-decl has fzero" #t (pair? (assq 'fzero (data-constructors Fin-decl))))
(test "Fin-decl has fsucc" #t (pair? (assq 'fsucc (data-constructors Fin-decl))))

;;; ============================================================
;;; Summary
;;; ============================================================
(newline)
(display "=================================")
(newline)
(display "Tests passed: ")
(display *tests-passed*)
(newline)
(display "Tests failed: ")
(display *tests-failed*)
(newline)

(when (> *tests-failed* 0)
      (exit 1))
