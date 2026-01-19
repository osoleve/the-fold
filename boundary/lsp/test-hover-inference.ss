;;; Test real type inference for LSP hover
;;;
;;; Tests the integration of core/types/infer.ss with LSP hover.

(load "boundary/lsp/capabilities.ss")

;;; ====
;;; Test Helpers
;;; ====

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

(define (test-true name actual)
  (test name #t (if actual #t #f)))

(define (test-false name actual)
  (test name #f actual))

(define (section name)
  (newline)
  (display name)
  (newline))

;;; string-contains? : String × String → Boolean
(define (string-contains? str substr)
  (let ([str-len (string-length str)]
        [sub-len (string-length substr)])
       (let loop ([i 0])
            (cond
             [(> (+ i sub-len) str-len) #f]
             [(string=? (substring str i (+ i sub-len)) substr) #t]
             [else (loop (+ i 1))]))))

;;; ====
;;; Test: Type Inference Available
;;; ====

(section "Type Inference Loading")

(test-true "infer.ss loaded" *infer-available*)
(test-true "types.ss loaded" (top-level-bound? 'type->string))

;;; ====
;;; Test: Extract Definitions
;;; ====

(section "Extract Definitions")

(let ([defs (parse-definitions "(define x 42)")])
  (test "simple define count" 1 (length defs))
  (test-true "has x binding" (assq 'x defs)))

(let ([defs (parse-definitions "(define (f x) (+ x 1))")])
  (test "function define count" 1 (length defs))
  (test-true "has f binding" (assq 'f defs)))

(let ([defs (parse-definitions "(define a 1)\n(define b 2)\n(define c 3)")])
  (test "multiple defines" 3 (length defs)))

;;; ====
;;; Test: Build Type Environment
;;; ====

(section "Build Type Environment")

(let* ([defs (parse-definitions "(define x 42)")]
       [env (build-tenv-from-defs defs)])
  (test-true "x in env" (tenv-lookup env 'x))
  (test "x is Int" 'Int (tenv-lookup env 'x)))

(let* ([defs (parse-definitions "(define id (fn (x) x))")]
       [env (build-tenv-from-defs defs)]
       [id-type (tenv-lookup env 'id)])
  (test-true "id in env" id-type)
  ;; id should be polymorphic
  (test-true "id is polymorphic or function"
             (and (pair? id-type)
                  (or (eq? (car id-type) '->)
                      (eq? (car id-type) '∀)))))

;;; ====
;;; Test: Try Infer Type
;;; ====

(section "Try Infer Type")

;; Simple constant
(let ([type (try-infer-type "x" "(define x 42)")])
  (test "infer x=42" "Int" type))

;; Identity function
(let ([type (try-infer-type "id" "(define id (fn (x) x))")])
  (test-true "infer identity function" type)
  (test-true "identity is polymorphic" (and type (string-contains? type "→"))))

;; Boolean
(let ([type (try-infer-type "flag" "(define flag #t)")])
  (test "infer flag=#t" "Bool" type))

;; String
(let ([type (try-infer-type "msg" "(define msg \"hello\")")])
  (test "infer msg=string" "String" type))

;; Function using another definition
(let ([type (try-infer-type "double" "(define x 5)\n(define double (fn (n) (prim 'add n n)))")])
  (test-true "infer double function" type)
  (test-true "double takes Int" (and type (string-contains? type "Int"))))

;;; ====
;;; Test: Fallback Behavior
;;; ====

(section "Fallback Behavior")

;; get-type-string still works for primitives
(test "primitive + type" "(Int → Int → Int)" (primitive-type "+"))
(test "primitive map type" "((α → β) → (List α) → (List β))" (primitive-type "map"))

;; Unknown symbol returns #f
(test-false "unknown symbol" (try-infer-type "nonexistent" "(define x 1)"))

;;; ====
;;; Summary
;;; ====

(newline)
(display "Hover type inference tests complete.")
(newline)
