;;; Test harness for core/annotate.ss — Type Annotations

(load "core/block.ss")
(load "core/types/types.ss")
(load "core/types/kinds.ss")
(load "core/types/infer.ss")
(load "core/types/annotate.ss")

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

(define (test-ok name result)
  (display "  ")
  (display name)
  (display ": ")
  (if (ann? result)
      (display "✓")
      (begin
       (display "✗
    expected annotated expr, got: ")
       (display result)))
  (newline))

(define (test-type name expected-type result)
  (display "  ")
  (display name)
  (display ": ")
  (if (ann? result)
      (if (type=? expected-type (ann-type result))
          (display "✓")
          (begin
           (display "✗
    expected type: ")
           (display (type->string expected-type))
           (display "
    got type: ")
           (display (type->string (ann-type result)))))
      (begin
       (display "✗ not annotated: ")
       (display result)))
  (newline))

(define (test-error name result)
  (display "  ")
  (display name)
  (display ": ")
  (if (and (pair? result) (eq? (car result) 'error))
      (display "✓")
      (begin
       (display "✗
    expected error, got: ")
       (display result)))
  (newline))

(define (test-section name)
  (newline)
  (display name)
  (newline))

;;; ============================================================
;;; Basic Annotations
;;; ============================================================
(test-section "Basic Annotations")

(let ([result (annotate-expr 42)])
     (test-ok "integer annotates" result)
     (test-type "integer is Int" 'Int result))

(let ([result (annotate-expr #t)])
     (test-type "true is Bool" 'Bool result))

(let ([result (annotate-expr "hello")])
     (test-type "string is String" 'String result))

(let ([result (annotate-expr '(quote foo))])
     (test-type "quoted symbol is Symbol" 'Symbol result))

;;; ============================================================
;;; Lambda Annotations
;;; ============================================================
(test-section "Lambda Annotations")

;; Simple identity
(let ([result (annotate-expr '(fn (x) x))])
     (test-ok "identity annotates" result)
     ;; Type should be a function
     (test "identity is function" '-> (if (pair? (ann-type result))
                                          (car (ann-type result))
                                          'not-fn)))

;; Lambda with known type from use
(let ([result (annotate-expr '(fn (x) (prim 'neg x)))])
     (test-type "neg fn is Int->Int" '(-> Int Int) result))

;;; ============================================================
;;; Application Annotations
;;; ============================================================
(test-section "Application Annotations")

(let ([result (annotate-expr '((fn (x) x) 42))])
     (test-type "apply id to int" 'Int result))

(let ([result (annotate-expr '((fn (x) (prim 'add x 1)) 5))])
     (test-type "apply add fn" 'Int result))

;;; ============================================================
;;; Let Annotations
;;; ============================================================
(test-section "Let Annotations")

(let ([result (annotate-expr '(let ((x 42)) x))])
     (test-type "simple let" 'Int result))

(let ([result (annotate-expr '(let ((x 1) (y 2)) (prim 'add x y)))])
     (test-type "multi-binding let" 'Int result))

;; Let-polymorphism
(let ([result (annotate-expr '(let ((id (fn (x) x))) (id 42)))])
     (test-type "let-polymorphism" 'Int result))

;;; ============================================================
;;; If Annotations
;;; ============================================================
(test-section "If Annotations")

(let ([result (annotate-expr '(if #t 1 2))])
     (test-type "if branches are Int" 'Int result))

(let ([result (annotate-expr '(if (prim 'lt? 1 2) 'yes 'no))])
     (test-type "if with symbols" 'Symbol result))

;;; ============================================================
;;; Prim Annotations
;;; ============================================================
(test-section "Prim Annotations")

(let ([result (annotate-expr '(prim 'add 1 2))])
     (test-type "add is Int" 'Int result))

(let ([result (annotate-expr '(prim 'lt? 1 2))])
     (test-type "lt? is Bool" 'Bool result))

(let ([result (annotate-expr '(prim 'neg 42))])
     (test-type "neg is Int" 'Int result))

;;; ============================================================
;;; Case Annotations
;;; ============================================================
(test-section "Case Annotations")

;; Simple case with one clause
(let ([result (annotate-expr '(fn (blk) (case blk ((Foo x) 42))))])
     (test-ok "case annotates" result)
     (test "case result is function" '-> (if (pair? (ann-type result))
                                             (car (ann-type result))
                                             'not-fn)))

;; Case with multiple clauses
(let ([result (annotate-expr '(fn (blk)
                               (case blk
                                     ((True) 1)
                                     ((False) 0))))])
     (test-ok "multi-clause case" result))

;; Case using bound variable
(let ([result (annotate-expr '(fn (blk)
                               (case blk
                                     ((Wrap ref) ref))))])
     (test-ok "case binds ref" result))

;; Case with different result types (should unify)
(let ([result (annotate-expr '(fn (blk)
                               (case blk
                                     ((A) (prim 'add 1 2))
                                     ((B x) (prim 'neg 3)))))])
     (test-ok "case unifies branches" result))

;;; ============================================================
;;; Type Errors
;;; ============================================================
(test-section "Type Errors")

(test-error "unbound variable" (annotate-expr 'undefined))

;;; ============================================================
;;; Strip Annotations
;;; ============================================================
(test-section "Strip Annotations")

(let* ([result (annotate-expr '42)]
       [stripped (strip-ann result)])
      (test "strip literal" 42 stripped))

(let* ([result (annotate-expr '(prim 'add 1 2))]
       [stripped (strip-ann result)])
      (test "strip preserves structure" 'prim (car stripped)))

;;; ============================================================
;;; Pretty Print
;;; ============================================================
(test-section "Pretty Print (visual inspection)")

(display "
--- Annotated 42 ---
")
(show-annotated 42)

(display "
--- Annotated (fn (x) x) ---
")
(show-annotated '(fn (x) x))

(display "
--- Annotated (fn (x) (prim 'add x 1)) ---
")
(show-annotated '(fn (x) (prim 'add x 1)))

(display "
--- Annotated (let ((x 1)) (prim 'mul x 2)) ---
")
(show-annotated '(let ((x 1)) (prim 'mul x 2)))

(display "
--- Annotated (if #t 1 2) ---
")
(show-annotated '(if #t 1 2))

(display "
--- Annotated case expression ---
")
(show-annotated '(fn (blk) (case blk ((True) 1) ((False) 0))))

(display "
--- Annotated complex expression ---
")
(show-annotated '(let ((double (fn (x) (prim 'mul x 2))))
                  (double 21)))

(newline)
(display "✓ All annotation tests complete.
")
