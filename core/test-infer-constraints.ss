;;; Test harness for infer.ss constraint integration

(load "core/block.ss")
(load "core/types.ss")
(load "core/kinds.ss")
(load "core/infer.ss")
(load "core/resolve.ss")

(define passed 0)
(define failed 0)

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (begin
       (set! passed (+ passed 1))
       (display "ok"))
      (begin
       (set! failed (+ failed 1))
       (display "FAIL\n    expected: ")
       (write expected)
       (display "\n    got: ")
       (write actual)))
  (newline))

(define (test-ok name result)
  (display "  ")
  (display name)
  (display ": ")
  (if (and (pair? result) (eq? (car result) 'ok))
      (begin
       (set! passed (+ passed 1))
       (display "ok"))
      (begin
       (set! failed (+ failed 1))
       (display "FAIL\n    expected ok, got: ")
       (write result)))
  (newline))

(define (test-err name result)
  (display "  ")
  (display name)
  (display ": ")
  (if (and (pair? result) (eq? (car result) 'error))
      (begin
       (set! passed (+ passed 1))
       (display "ok (correctly failed)"))
      (begin
       (set! failed (+ failed 1))
       (display "FAIL\n    expected error, got: ")
       (write result)))
  (newline))

(define (section name)
  (newline)
  (display "=== ")
  (display name)
  (display " ===")
  (newline))

;;; ============================================================
;;; Test instantiate-constrained
;;; ============================================================

(section "instantiate-constrained")

;; Simple polymorphic type without constraints
(let ([result (instantiate-constrained '(∀ (a) a))])
     (test "polymorphic no constraints - type is symbol" #t (symbol? (car result)))
     (test "polymorphic no constraints - no constraints" '() (cadr result)))

;; Constrained type: (=> (Functor f) (-> ...))
(let* ([fmap-type '(∀ (f a b)
                    (=> ((Functor f))
                        (-> (-> a b) (@ f a) (@ f b))))]
       [result (instantiate-constrained fmap-type)])
      (test "fmap type - underlying is function" '-> (car (car result)))
      (test "fmap type - one constraint" 1 (length (cadr result)))
      (test "fmap type - constraint class" 'Functor (caar (cadr result))))

;; Non-polymorphic constrained type
(let ([result (instantiate-constrained '(=> ((Eq a)) (-> a a Bool)))])
     (test "constrained but not forall - function" '-> (car (car result)))
     (test "constrained but not forall - one constraint" 1 (length (cadr result))))

;;; ============================================================
;;; Test apply-subst-to-constraints
;;; ============================================================

(section "apply-subst-to-constraints")

(let* ([constraints '((Functor f) (Eq a))]
       [subst '((f . List) (a . Int))]
       [result (apply-subst-to-constraints subst constraints)])
      (test "subst constraint 1 class" 'Functor (caar result))
      (test "subst constraint 1 type" 'List (cadar result))
      (test "subst constraint 2 class" 'Eq (caadr result))
      (test "subst constraint 2 type" 'Int (cadadr result)))

;;; ============================================================
;;; Test solve-constraints
;;; ============================================================

(section "solve-constraints")

;; Empty constraints
(test-ok "empty constraints" (solve-constraints '() standard-instances))

;; Single satisfiable constraint
(test-ok "Functor List" (solve-constraints '((Functor List)) standard-instances))

;; Multiple satisfiable constraints
(test-ok "multiple constraints"
         (solve-constraints '((Functor List) (Monad Option)) standard-instances))

;; Unsatisfiable constraint
(test-err "no instance" (solve-constraints '((Functor Int)) standard-instances))

;; Parametric constraint
(test-ok "parametric - Functor (Either String)"
         (solve-constraints '((Functor (@ Either String))) standard-instances))

;;; ============================================================
;;; Test make-class-env
;;; ============================================================

(section "make-class-env")

(let ([env (make-class-env standard-classes)])
     (test "class env has fmap" #t (if (assq 'fmap env) #t #f))
     (test "class env has pure" #t (if (assq 'pure env) #t #f))
     (test "class env has >>=" #t (if (assq '>>= env) #t #f))
     (test "class env has ==" #t (if (assq '== env) #t #f))
     (test "class env has show" #t (if (assq 'show env) #t #f)))

;;; ============================================================
;;; Test get-standard-class-tenv
;;; ============================================================

(section "get-standard-class-tenv")

(let ([tenv (get-standard-class-tenv)])
     (test "standard-class-tenv has fmap" #t (if (assq 'fmap tenv) #t #f))
     (test "standard-class-tenv has <>" #t (if (assq '<> tenv) #t #f)))

;;; ============================================================
;;; Test typeof-constrained
;;; ============================================================

(section "typeof-constrained")

;; Simple expression (no constraints)
(test-ok "simple literal" (typeof-constrained 42 standard-instances))
(test-ok "boolean" (typeof-constrained #t standard-instances))

;; Lambda
(test-ok "lambda" (typeof-constrained '(fn (x) x) standard-instances))

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(display "======================")
(newline)
(display "Passed: ")
(display passed)
(newline)
(display "Failed: ")
(display failed)
(newline)

(if (= failed 0)
    (display "\nAll constraint integration tests passed.\n")
    (display "\nSome tests failed!\n"))
