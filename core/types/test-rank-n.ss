;;; core/types/test-rank-n.ss — Tests for Rank-N Polymorphism
;;;
;;; Run from project root: scheme --script core/types/test-rank-n.ss

(load "core/types/rank-n.ss")

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

;;; Helper: string-contains?
(define (string-contains? str substr)
  (let* ([str-len (string-length str)]
         [sub-len (string-length substr)])
        (let loop ([i 0])
             (cond
              [(> (+ i sub-len) str-len) #f]
              [(string=? (substring str i (+ i sub-len)) substr) #t]
              [else (loop (+ i 1))]))))

;;; ============================================================
;;; Type Rank Calculation Tests
;;; ============================================================
(test-section "Type Rank Calculation")

;; Rank 0: monomorphic types
(test "Int is rank 0" 0 (type-rank 'Int))
(test "Bool is rank 0" 0 (type-rank 'Bool))
(test "String is rank 0" 0 (type-rank 'String))
(test "Int → Bool is rank 0" 0 (type-rank '(-> Int Bool)))
(test "Int → Int → Int is rank 0" 0 (type-rank '(-> Int Int Int)))
(test "List Int is rank 0" 0 (type-rank '(List Int)))
(test "Int × Bool is rank 0" 0 (type-rank '(× Int Bool)))

;; Rank 1: top-level quantification
(test "∀a. a is rank 1" 1 (type-rank '(∀ (a) a)))
(test "∀a. a → a is rank 1" 1 (type-rank '(∀ (a) (-> a a))))
(test "∀a b. a → b → a is rank 1" 1 (type-rank '(∀ (a b) (-> a b a))))
(test "∀a. a → List a is rank 1" 1 (type-rank '(∀ (a) (-> a (List a)))))
(test "∀a. List a → a is rank 1" 1 (type-rank '(∀ (a) (-> (List a) a))))

;; Rank 2: quantifier in argument position
(test "(∀a. a → a) → Int is rank 2" 2 (type-rank '(-> (∀ (a) (-> a a)) Int)))
(test "runST type is rank 2" 2 (type-rank '(-> (∀ (s) (ST s a)) a)))
(test "Two rank-1 args is rank 2" 2 (type-rank '(-> (∀ (a) a) (∀ (b) b) Int)))

;; Quantifier in positive (return) position stays rank 1
(test "Int → (∀a. a → a) is rank 1" 1 (type-rank '(-> Int (∀ (a) (-> a a)))))

;; Rank 3 and higher
(test "((∀a. a → a) → Int) → Bool is rank 3" 3
      (type-rank '(-> (-> (∀ (a) (-> a a)) Int) Bool)))
(test "Deeply nested quantifiers is rank 3" 3
      (type-rank '(-> (∀ (a) (-> (∀ (b) (-> b a)) a)) Int)))

;;; ============================================================
;;; Rank Predicates
;;; ============================================================
(test-section "Rank Predicates")

(test "Int is monomorphic" #t (monomorphic? 'Int))
(test "Int → Bool is monomorphic" #t (monomorphic? '(-> Int Bool)))
(test "∀a. a is not monomorphic" #f (monomorphic? '(∀ (a) a)))

(test "∀a. a → a is rank-1" #t (rank-1? '(∀ (a) (-> a a))))
(test "Int is not rank-1" #f (rank-1? 'Int))
(test "(∀a. a) → Int is not rank-1" #f (rank-1? '(-> (∀ (a) a) Int)))

(test "(∀a. a) → Int is rank-N" #t (rank-n? '(-> (∀ (a) a) Int)))
(test "∀a. a is not rank-N" #f (rank-n? '(∀ (a) a)))

;;; ============================================================
;;; Subsumption Tests
;;; ============================================================
(test-section "Subsumption")

;; Reflexivity
(test "Int <: Int" #t (subsumes 'Int 'Int))
(test "Bool <: Bool" #t (subsumes 'Bool 'Bool))
(test "(→ Int Bool) <: (→ Int Bool)" #t (subsumes '(-> Int Bool) '(-> Int Bool)))
(test "∀a. a <: ∀a. a" #t (subsumes '(∀ (a) a) '(∀ (a) a)))

;; Instantiation (left)
(test "∀a. a <: Int" #t (subsumes '(∀ (a) a) 'Int))
(test "∀a. a → a <: Int → Int" #t (subsumes '(∀ (a) (-> a a)) '(-> Int Int)))
(test "∀a b. a → b <: Int → Bool" #t (subsumes '(∀ (a b) (-> a b)) '(-> Int Bool)))

;; Generalization (right) - should fail
(test "Int → Int does not subsume ∀a. a → a" #f
      (subsumes '(-> Int Int) '(∀ (a) (-> a a))))

;; Function contravariance
(test "(∀a. a → a) <: (Int → Int)" #t
      (subsumes '(∀ (a) (-> a a)) '(-> Int Int)))
;; Note: (∀a. a) → Bool does NOT subsume Int → Bool because
;; contravariance requires Int <: (∀a. a), but Int is LESS general.
;; The subsumption goes the other way: Int → Bool <: (∀a. a) → Bool
(test "Int → Bool <: (∀a. a) → Bool" #t
      (subsumes '(-> Int Bool) '(-> (∀ (a) a) Bool)))

;; Higher-rank subsumption
(test "(∀a. a → a) → Int does not subsume (Int → Int) → Int" #f
      (subsumes '(-> (∀ (a) (-> a a)) Int) '(-> (-> Int Int) Int)))
(test "(Int → Int) → Int subsumes (∀a. a → a) → Int" #t
      (subsumes '(-> (-> Int Int) Int) '(-> (∀ (a) (-> a a)) Int)))

;;; ============================================================
;;; Deep Instantiation Tests
;;; ============================================================
(test-section "Deep Instantiation")

(let ([result (deep-instantiate '(∀ (a) (-> a a)))])
     (test "deep-instantiate removes top-level ∀" #f
           (and (pair? result) (eq? (car result) '∀)))
     (test "deep-instantiate gives function type" #t
           (function-type? result)))

(test "deep-instantiate preserves Int" 'Int (deep-instantiate 'Int))
(test "deep-instantiate preserves →" '(-> Int Bool) (deep-instantiate '(-> Int Bool)))

;;; ============================================================
;;; Deep Skolemization Tests
;;; ============================================================
(test-section "Deep Skolemization")

(let* ([result (deep-skolemize '(∀ (a) (-> a a)))]
       [type (car result)]
       [skolems (cadr result)])
      (test "deep-skolemize introduces skolems" #t (> (length skolems) 0))
      (test "deep-skolemize gives function type" #t (function-type? type))
      ;; The param and return should be the same skolem
      (test "param and return are same skolem"
            (function-param-types type)
            (list (function-return-type type))))

;;; ============================================================
;;; Impredicative Unification Tests
;;; ============================================================
(test-section "Impredicative Unification")

(test "Int unifies with Int" 'ok (car (impredicative-unify 'Int 'Int)))
(test "Int doesn't unify with Bool" 'error (car (impredicative-unify 'Int 'Bool)))

(let ([result (impredicative-unify 'a 'Int)])
     (test "a unifies with Int" 'ok (car result))
     (test "a maps to Int" 'Int (cdr (assq 'a (cadr result)))))

(let ([result (impredicative-unify 'a '(∀ (b) (-> b b)))])
     (test "a unifies with polymorphic type (impredicative)" 'ok (car result))
     (test "a is in substitution" #t (if (assq 'a (cadr result)) #t #f)))

(test "occurs check fails" 'error (car (impredicative-unify 'a '(List a))))

(test "∀a. a → a unifies with ∀b. b → b" 'ok
      (car (impredicative-unify '(∀ (a) (-> a a)) '(∀ (b) (-> b b)))))

;;; ============================================================
;;; Annotation Detection Tests
;;; ============================================================
(test-section "Annotation Detection")

(test "Int doesn't need annotation" #f (requires-annotation 'Int))
(test "Rank-1 doesn't need annotation" #f (requires-annotation '(∀ (a) (-> a a))))
(test "Rank-2 needs annotation" #t (requires-annotation '(-> (∀ (a) a) Int)))

(test "Can infer Int" #t (can-infer? 'Int))
(test "Can infer rank-1" #t (can-infer? '(∀ (a) (-> a a))))
(test "Cannot infer rank-2" #f (can-infer? '(-> (∀ (a) a) Int)))

;;; ============================================================
;;; Pretty Printing Tests
;;; ============================================================
(test-section "Pretty Printing")

(test "rank-n-type->string Int" "Int" (rank-n-type->string 'Int))
(test "rank-n-type->string Bool" "Bool" (rank-n-type->string 'Bool))
(test "rank-n-type->string ∀a. a" "∀a. a" (rank-n-type->string '(∀ (a) a)))
(test "rank-n-type->string ∀a b. a → b" "∀a b. (a → b)"
      (rank-n-type->string '(∀ (a b) (-> a b))))

(let ([result (rank-n-type->string '(-> (∀ (a) (-> a a)) Int))])
     (test "higher-rank parenthesizes forall in arg" #t
           (string-contains? result "(∀")))

;;; ============================================================
;;; Example Type Patterns
;;; ============================================================
(test-section "Example Types")

(test "runST is rank 2" 2 (type-rank type-runST))
(test "runST is higher-rank" #t (rank-n? type-runST))

(let ([lens-type (type-lens 's 't 'a 'b)])
     (test "Lens is polymorphic" #t (is-polymorphic? lens-type))
     (test "Lens type is rank 1" 1 (type-rank lens-type)))

(test "id is rank 1" 1 (type-rank type-id))
(test "Can infer id type" #t (can-infer? type-id))

(let ([pair-type (type-church-pair 'a 'b)])
     (test "Church pair constructor is rank 1" 1 (type-rank pair-type)))

;;; ============================================================
;;; Capture-Avoiding Substitution Tests
;;; ============================================================
(test-section "Capture-Avoiding Substitution")

;; Test that [b/a] in (∀b. a → b) renames b to avoid capture
;; Without capture-avoidance: (∀b. b → b) - WRONG (a was captured)
;; With capture-avoidance: (∀b$N. b → b$N) - correct
(let* ([subst (list (cons 'a 'b))]
       [type '(∀ (b) (-> a b))]
       [result (apply-subst-rankn subst type)])
      ;; The result should NOT be (∀ (b) (-> b b))
      ;; It should rename b to avoid capture
      (test "capture-avoiding: b renamed" #f (equal? result '(∀ (b) (-> b b))))
      ;; The body should have the substitution applied
      (test "capture-avoiding: a becomes b" #t
            (let ([body (caddr result)])
                 ;; First arg position should be b (from subst)
                 (eq? (cadr body) 'b))))

;; Test that no renaming happens when not needed
(let* ([subst (list (cons 'a 'c))]
       [type '(∀ (b) (-> a b))]
       [result (apply-subst-rankn subst type)])
      (test "no capture: b unchanged" '(b) (cadr result))
      (test "no capture: a becomes c" '(-> c b) (caddr result)))

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
