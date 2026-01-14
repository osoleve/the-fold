;;; core/types/test-rank-n-infer.ss — Tests for Rank-N Type Inference
;;;
;;; Run from project root: scheme --script core/types/test-rank-n-infer.ss

(load "core/types/rank-n-infer.ss")

(define *tests-passed* 0)
(define *tests-failed* 0)

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (begin
       (set! *tests-passed* (+ *tests-passed* 1))
       (display "[pass]"))
      (begin
       (set! *tests-failed* (+ *tests-failed* 1))
       (display "[FAIL]")
       (newline)
       (display "    expected: ")
       (write expected)
       (newline)
       (display "    got: ")
       (write actual)))
  (newline))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

(define (test-ok name result)
  (test name 'ok (car result)))

(define (test-error name result)
  (test name 'error (car result)))

(define (test-section name)
  (newline)
  (display name)
  (newline))

;;; ====
;;; Forall Type Predicates Tests
;;; ====
(test-section "Forall Type Predicates")

(test-true "forall-type? on (forall (a) a)"
           (forall-type? '(forall (a) a)))

(test-true "forall-type? on unicode forall"
           (forall-type? '(∀ (a) a)))

(test-false "forall-type? on function"
            (forall-type? '(-> Int Bool)))

(test-true "forall-well-formed? on valid"
           (forall-well-formed? '(forall (a) a)))

(test-false "forall-well-formed? on empty vars"
            (forall-well-formed? '(forall () a)))

;;; ====
;;; Forall Accessors Tests
;;; ====
(test-section "Forall Accessors")

(test "forall-vars simple" '(a) (forall-vars '(forall (a) a)))
(test "forall-vars multiple" '(a b) (forall-vars '(forall (a b) (-> a b))))
(test "forall-vars with kinds" '(a) (forall-vars '(forall ((a : *)) a)))

(test "forall-var-kinds simple" '((a . Type)) (forall-var-kinds '(forall (a) a)))
(test "forall-var-kinds with kind" '((a . *)) (forall-var-kinds '(forall ((a : *)) a)))

(test "forall-body" '(-> a a) (forall-body '(forall (a) (-> a a))))

;;; ====
;;; Instantiation Tests
;;; ====
(test-section "Instantiation")

(test "rank-n-instantiate simple"
      'Int
      (rank-n-instantiate '(forall (a) a) '(Int)))

(test "rank-n-instantiate function"
      '(-> Int Int)
      (rank-n-instantiate '(forall (a) (-> a a)) '(Int)))

(test "rank-n-instantiate multiple vars"
      '(-> Int Bool)
      (rank-n-instantiate '(forall (a b) (-> a b)) '(Int Bool)))

;;; ====
;;; Generalization Tests
;;; ====
(test-section "Generalization")

;; Free variables in type but not in context should be generalized
(let ([result (rank-n-generalize '(-> a a) '())])
     (test-true "generalize creates forall"
                (forall-type? result))
     (test "generalize captures free vars"
           '(a)
           (forall-vars result)))

;; Variable in context should NOT be generalized
(let ([result (rank-n-generalize '(-> a b) '((x . a)))])
     (test-true "generalize with context creates forall"
                (forall-type? result))
     (test "generalize skips context vars"
           '(b)  ; Only b should be generalized, not a
           (forall-vars result)))

;; Monomorphic type should not be generalized
(test "no generalization for monomorphic"
      '(-> Int Bool)
      (rank-n-generalize '(-> Int Bool) '()))

;;; ====
;;; Subsumption Tests (using rank-n.ss infrastructure)
;;; ====
(test-section "Subsumption")

(test-true "same types subsume" (rank-n-subsumes? 'Int 'Int '()))
(test-true "forall instantiation subsumes" (rank-n-subsumes? '(∀ (a) a) 'Int '()))
(test-true "forall function subsumes" (rank-n-subsumes? '(∀ (a) (-> a a)) '(-> Int Int) '()))
(test-false "mono doesn't subsume poly" (rank-n-subsumes? '(-> Int Int) '(∀ (a) (-> a a)) '()))

;;; ====
;;; Type Synthesis Tests
;;; ====
(test-section "Type Synthesis")

;; Literals
(let ([result (rank-n-typeof 42)])
     (test-true "literal Int synthesis" (not (pair? result)))
     (test "literal is Int" 'Int result))

(let ([result (rank-n-typeof #t)])
     (test "literal is Bool" 'Bool result))

(let ([result (rank-n-typeof "hello")])
     (test "literal is String" 'String result))

;; Variables
(let ([result (rank-n-infer-synth 'x '((x . Int)))])
     (test-ok "variable lookup succeeds" result)
     (test "variable type correct" 'Int (cadr result)))

(let ([result (rank-n-infer-synth 'y '())])
     (test-error "unbound variable fails" result))

;; Lambda
(let ([result (rank-n-typeof '(fn (x) x))])
     (test-true "lambda synthesis creates forall"
                (forall-type? result)))

;; Application
(let* ([env '((f . (-> Int Bool)) (x . Int))]
       [result (rank-n-infer-synth '(f x) env)])
      (test-ok "application succeeds" result)
      (test "application result" 'Bool (cadr result)))

;; Let
(let ([result (rank-n-typeof '(let ((x 42)) x))])
     (test "let simple" 'Int result))

;; Let with generalization
(let ([result (rank-n-typeof '(let ((id (fn (x) x))) (id 42)))])
     (test "let with poly uses" 'Int result))

;; If
(let ([result (rank-n-typeof '(if #t 1 2))])
     (test "if expression" 'Int result))

;;; ====
;;; Type Checking Tests
;;; ====
(test-section "Type Checking")

;; Check literal against type
(test-true "check 42 : Int" (rank-n-typecheck 42 'Int))

;; Check lambda against function type
(test-true "check identity against Int → Int"
           (rank-n-typecheck '(fn (x) x) '(-> Int Int)))

;; Check against polymorphic type
(test-true "check identity against ∀a. a → a"
           (rank-n-typecheck '(fn (x) x) '(∀ (a) (-> a a))))

;; Type mismatch should fail
(let ([result (rank-n-typecheck '(fn (x) #t) '(-> Int Int))])
     (test-false "type mismatch fails" (eq? result #t)))

;;; ====
;;; Higher-Rank Type Checking Tests
;;; ====
(test-section "Higher-Rank Type Checking")

;; runST-style: function taking polymorphic argument
(test-true "accept poly arg (annotated)"
           (rank-n-typecheck
            '(fn ((f : (∀ (a) (-> a a)))) (f 42))
            '(-> (∀ (a) (-> a a)) Int)))

;; Church-encoded pair selector
(let* ([pair-type '(∀ (r) (-> (-> Int Int r) r))]
       [result (rank-n-typecheck
                '(fn ((p : (∀ (r) (-> (-> Int Int r) r))))
                  (p (fn (x y) x)))
                `(-> ,pair-type Int))])
      (test-true "church pair fst" result))

;;; ====
;;; Annotation Tests
;;; ====
(test-section "Type Annotations")

(let ([result (rank-n-typeof '(: 42 Int))])
     (test "annotation preserves type" 'Int result))

(let ([result (rank-n-typeof '(: (fn (x) x) (∀ (a) (-> a a))))])
     (test-true "annotation with forall"
                (forall-type? result)))

;;; ====
;;; Edge Cases
;;; ====
(test-section "Edge Cases")

;; Nested let with shadowing
(let ([result (rank-n-typeof '(let ((x 1)) (let ((x #t)) x)))])
     (test "nested let shadowing" 'Bool result))

;; Multiple bindings in let
(let ([result (rank-n-typeof '(let ((x 1) (y 2)) x))])
     (test "let multiple bindings" 'Int result))

;; Quoted data
(let ([result (rank-n-typeof ''foo)])
     (test "quoted symbol" 'Symbol result))

;;; ====
;;; True Rank-N Tests (Quick Look / Impredicative)
;;; ====
(test-section "True Rank-N Inference")

;; runST-style: function taking polymorphic argument (WITHOUT annotation!)
;; This is the classic test case for Rank-N: apply-id expects ∀a. a→a
(let* ([env '((apply-id . (-> (∀ (a) (-> a a)) Int)))]
       [result (rank-n-infer-synth '(apply-id (fn (x) x)) env)])
      (test-ok "runST-style without annotation" result)
      (test "runST-style result type" 'Int (cadr result)))

;; Polymorphic let binding used at multiple types
;; id is generalized and can be used at both Bool and Int
(let ([result (rank-n-typeof
               '(let ((id (fn (x) x)))
                 (if (id #t) (id 1) (id 2))))])
     (test "poly let at multiple types" 'Int result))

;; Higher-rank with variable as polymorphic argument
(let* ([env '((apply-poly . (-> (∀ (a) (-> a a)) Int))
              (my-id . (∀ (b) (-> b b))))]
       [result (rank-n-infer-synth '(apply-poly my-id) env)])
      (test-ok "poly var as higher-rank arg" result)
      (test "poly var result type" 'Int (cadr result)))

;; Quick Look: delay instantiation for lambda but not for monomorphic
(let* ([env '((mono-apply . (-> (-> Int Int) Int)))]
       [result (rank-n-infer-synth '(mono-apply (fn (x) x)) env)])
      (test-ok "mono apply with lambda" result))

;;; ====
;;; Skolem Escape Prevention Tests
;;; ====
(test-section "Skolem Escape Prevention")

;; This tests that we properly reject unsound programs where
;; skolem variables would escape their scope.

;; Type mismatch: (fn (x) x) has type ∀a. a→a, not ∀a. a
;; When checking against (∀ (a) a), the identity function should fail
(let* ([env '((needs-value . (-> (∀ (a) a) Int)))]
       [result (rank-n-infer-synth '(needs-value (fn (x) x)) env)])
      ;; This should fail: identity is a→a, not a value of type a
      (test-error "identity is not a value" result))

;;; ====
;;; Summary
;;; ====
(newline)
(display "====")
(newline)
(display "Tests passed: ")
(display *tests-passed*)
(newline)
(display "Tests failed: ")
(display *tests-failed*)
(newline)

(when (> *tests-failed* 0)
      (exit 1))
