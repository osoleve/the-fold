(load "core/testing/test-framework.ss")
(load "boundary/lsp/capabilities.ss")

(doc 'module 'lsp/test-hover-inference)
(doc 'description "Test real type inference for LSP hover. Tests the integration of core/types/infer.ss with LSP hover.")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(display "Testing hover-inference integration\n")
(display "====\n\n")

;;; ====
;;; Helper
;;; ====

(define (string-contains? str substr)
  (let ([str-len (string-length str)]
        [sub-len (string-length substr)])
    (let loop ([i 0])
      (cond
        [(> (+ i sub-len) str-len) #f]
        [(string=? (substring str i (+ i sub-len)) substr) #t]
        [else (loop (+ i 1))]))))

;;; ====
;;; Type Inference Loading Tests
;;; ====

(test-group type-inference-loading
  (define-test infer-ss-loaded
    (assert-true (if *infer-available* #t #f)))

  (define-test types-ss-loaded
    (assert-true (if (top-level-bound? 'type->string) #t #f))))

;;; ====
;;; Extract Definitions Tests
;;; ====

(test-group extract-definitions
  (define-test simple-define-count
    (let ([defs (parse-definitions "(define x 42)")])
      (assert-equal 1 (length defs))))

  (define-test simple-define-has-x
    (let ([defs (parse-definitions "(define x 42)")])
      (assert-true (if (assq 'x defs) #t #f))))

  (define-test function-define-count
    (let ([defs (parse-definitions "(define (f x) (+ x 1))")])
      (assert-equal 1 (length defs))))

  (define-test function-define-has-f
    (let ([defs (parse-definitions "(define (f x) (+ x 1))")])
      (assert-true (if (assq 'f defs) #t #f))))

  (define-test multiple-defines
    (let ([defs (parse-definitions "(define a 1)\n(define b 2)\n(define c 3)")])
      (assert-equal 3 (length defs)))))

;;; ====
;;; Build Type Environment Tests
;;; ====

(test-group build-type-environment
  (define-test x-in-env
    (let* ([defs (parse-definitions "(define x 42)")]
           [env (build-tenv-from-defs defs)])
      (assert-true (if (tenv-lookup env 'x) #t #f))))

  (define-test x-is-int
    (let* ([defs (parse-definitions "(define x 42)")]
           [env (build-tenv-from-defs defs)])
      (assert-equal 'Int (tenv-lookup env 'x))))

  (define-test id-in-env
    (let* ([defs (parse-definitions "(define id (fn (x) x))")]
           [env (build-tenv-from-defs defs)]
           [id-type (tenv-lookup env 'id)])
      (assert-true (if id-type #t #f))))

  (define-test id-is-polymorphic-or-function
    (let* ([defs (parse-definitions "(define id (fn (x) x))")]
           [env (build-tenv-from-defs defs)]
           [id-type (tenv-lookup env 'id)])
      (assert-true (and (pair? id-type)
                        (or (eq? (car id-type) '->)
                            (eq? (car id-type) '∀)))))))

;;; ====
;;; Try Infer Type Tests
;;; ====

(test-group try-infer-type
  ;; Simple constant
  (define-test infer-x-42
    (let ([type (try-infer-type "x" "(define x 42)")])
      (assert-equal "Int" type)))

  ;; Identity function
  (define-test infer-identity-function
    (let ([type (try-infer-type "id" "(define id (fn (x) x))")])
      (assert-true (if type #t #f))))

  (define-test identity-is-polymorphic
    (let ([type (try-infer-type "id" "(define id (fn (x) x))")])
      (assert-true (if (and type (string-contains? type "→")) #t #f))))

  ;; Boolean
  (define-test infer-flag-bool
    (let ([type (try-infer-type "flag" "(define flag #t)")])
      (assert-equal "Bool" type)))

  ;; String
  (define-test infer-msg-string
    (let ([type (try-infer-type "msg" "(define msg \"hello\")")])
      (assert-equal "String" type)))

  ;; Function using another definition
  (define-test infer-double-function
    (let ([type (try-infer-type "double" "(define x 5)\n(define double (fn (n) (prim 'add n n)))")])
      (assert-true (if type #t #f))))

  (define-test double-takes-int
    (let ([type (try-infer-type "double" "(define x 5)\n(define double (fn (n) (prim 'add n n)))")])
      (assert-true (if (and type (string-contains? type "Int")) #t #f)))))

;;; ====
;;; Fallback Behavior Tests
;;; ====

(test-group fallback-behavior
  ;; get-type-string still works for primitives
  (define-test primitive-plus-type
    (assert-equal "(Int → Int → Int)" (primitive-type "+")))

  (define-test primitive-map-type
    (assert-equal "((α → β) → (List α) → (List β))" (primitive-type "map")))

  ;; Unknown symbol returns #f
  (define-test unknown-symbol
    (assert-false (try-infer-type "nonexistent" "(define x 1)"))))

;;; ====
;;; Run Tests
;;; ====

(print-summary)
(when (> *tests-failed* 0)
  (exit 1))
