;;; shell/tools/typed-holes.ss — Type-Driven Development with Holes
;;;
;;; Agda/Idris-inspired workflow for The Fold:
;;;   (?) or (? name) — placeholder expressions
;;;   (hole-type expr) — show what type is needed
;;;   (hole-fits expr) — show available bindings that fit
;;;   (hole-refine expr) — narrow holes with partial info
;;;   (case-split expr var) — split on sum type constructors
;;;
;;; Example:
;;;   (let ((xs (list 1 2 3)))
;;;     (map (?) xs))
;;;
;;;   Hole has type: (-> Int a)
;;;   Available fits:
;;;     - (fn (x) ...) where x : Int
;;;     - square : (-> Int Int)
;;;     - show : (-> Int String)
;;;
;;; This is Shell code: provides REPL integration.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - infer.ss
;;;   - types.ss
;;;   - index.ss

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/infer.ss")
(load "core/lang/index.ss")

;;; ============================================================
;;; Hole Detection
;;; ============================================================

;;; hole? : Any → Boolean
;;; Is this a hole expression?
(define (hole? x)
  (or (eq? x '?)
      (and (pair? x) (eq? (car x) '?) (= (length x) 2))))

;;; hole-name : Expr → Symbol | #f
;;; Get the name of a named hole, or #f for anonymous.
(define (hole-name h)
  (if (and (pair? h) (eq? (car h) '?))
      (cadr h)
      #f))

;;; find-holes : Expr → (List (Path × Hole))
;;; Find all holes in an expression with their paths.
(define (find-holes expr)
  (find-holes-aux expr '()))

(define (find-holes-aux expr path)
  (cond
   [(hole? expr)
    (list (cons (reverse path) expr))]
   [(not (pair? expr))
    '()]
   [else
    (let loop ([items (cdr expr)] [idx 1] [results '()])
         (if (null? items)
             results
             (loop (cdr items)
                   (+ idx 1)
                   (append results
                           (find-holes-aux (car items)
                                           (cons idx path))))))]))

;;; ============================================================
;;; Type Context Building
;;; ============================================================

;;; expr-context : Expr × Path → TEnv
;;; Build the type environment visible at a hole location.
(define (expr-context expr path)
  (expr-context-aux expr path '()))

(define (expr-context-aux expr path env)
  (cond
   [(null? path) env]
   [(not (pair? expr)) env]
   [(eq? (car expr) 'let)
    ;; (let ((x e1) ...) body)
    (let* ([bindings (cadr expr)]
           [body (caddr expr)]
           [idx (car path)]
           [new-env (fold-left
                     (lambda (e b)
                             (let ([var (car b)]
                                   [init (cadr b)]
                                   [result (infer init e)])
                                  (if (eq? (car result) 'ok)
                                      (tenv-extend e var (cadr result))
                                      e)))
                     env
                     bindings)])
          (if (= idx 2)
              ;; In body
              (expr-context-aux body (cdr path) new-env)
              ;; In binding
              (expr-context-aux (list-ref (list-ref bindings (- idx 1)) 1)
                                (cdr path) env)))]
   [(eq? (car expr) 'fn)
    ;; (fn (x y) body)
    (let* ([params (cadr expr)]
           [body (caddr expr)]
           [param-types (map (lambda (_) (fresh-tvar)) params)]
           [new-env (tenv-extend* env (map cons params param-types))])
          (expr-context-aux body (cdr path) new-env))]
   [else
    (let* ([idx (car path)]
           [subexpr (list-ref expr idx)])
          (expr-context-aux subexpr (cdr path) env))]))

;;; ============================================================
;;; Hole Type Inference
;;; ============================================================

;;; infer-hole-type : Expr × Path → Type | Error
;;; Infer what type a hole at the given path should have.
(define (infer-hole-type expr path)
  (let* ([ctx (expr-context expr path)]
         [hole-expr (path-get expr path)]
         ;; Find the parent expression and position
         [parent-path (if (null? path) '() (reverse (cdr (reverse path))))]
         [position (if (null? path) 0 (car (reverse path)))]
         [parent (path-get expr parent-path)])
        (infer-hole-from-context parent position ctx)))

;;; path-get : Expr × Path → Expr
(define (path-get expr path)
  (if (null? path)
      expr
      (path-get (list-ref expr (car path)) (cdr path))))

;;; infer-hole-from-context : Expr × Nat × TEnv → Type | Error
(define (infer-hole-from-context parent position ctx)
  (cond
   ;; Application: (f ? arg...)
   [(and (pair? parent) (not (special-form? (car parent))))
    (let* ([fn-expr (car parent)]
           [fn-result (infer fn-expr ctx)])
          (if (not (eq? (car fn-result) 'ok))
              fn-result
              (let ([fn-type (cadr fn-result)])
                   (if (function-type? fn-type)
                       ;; Get the type of the parameter at this position
                       (let ([param-types (function-param-types fn-type)])
                            (if (< (- position 1) (length param-types))
                                (list-ref param-types (- position 1))
                                `(error position-out-of-range ,position)))
                       ;; Function type not known, return fresh var
                       (fresh-tvar)))))]
   
   ;; Let binding: (let ((x ?)) body)
   [(and (pair? parent) (eq? (car parent) 'let))
    ;; Hole is in the init, type is inferred from usage
    (fresh-tvar)]
   
   ;; If branches: (if test ? ?)
   [(and (pair? parent) (eq? (car parent) 'if))
    (cond
     [(= position 1) 'Bool]  ;; Test must be Bool
     [else (fresh-tvar)])]   ;; Branches can be anything
   
   ;; Default: unknown
   [else (fresh-tvar)]))

;;; ============================================================
;;; Finding Available Fits
;;; ============================================================

;;; find-fits : Type × TEnv → (List (Symbol × Type))
;;; Find bindings in scope that can fill a hole of the given type.
(define (find-fits target-type env)
  (let ([direct-fits (find-direct-fits target-type env)]
        [func-fits (if (function-type? target-type)
                       '()  ;; Lambda templates handled separately
                       (find-function-fits target-type env))])
       (append direct-fits func-fits)))

;;; find-direct-fits : Type × TEnv → (List (Symbol × Type))
;;; Find bindings whose type unifies with the target.
(define (find-direct-fits target env)
  (filter-map
   (lambda (binding)
           (let* ([name (car binding)]
                  [type (cdr binding)]
                  [inst-type (instantiate type)]
                  [result (unify target inst-type)])
                 (if (eq? (car result) 'ok)
                     (cons name type)
                     #f)))
   env))

;;; find-function-fits : Type × TEnv → (List (Symbol × Type))
;;; Find functions that could produce the target type when applied.
(define (find-function-fits target env)
  (filter-map
   (lambda (binding)
           (let* ([name (car binding)]
                  [type (cdr binding)]
                  [inst-type (instantiate type)])
                 (if (function-type? inst-type)
                     (let* ([ret-type (function-return-type inst-type)]
                            [result (unify target ret-type)])
                           (if (eq? (car result) 'ok)
                               (cons name type)
                               #f))
                     #f)))
   env))

;;; ============================================================
;;; REPL Commands
;;; ============================================================

;;; hole-type : Expr → Void
;;; Show what type each hole in the expression should have.
(define (hole-type expr)
  (reset-fresh!)
  (let ([holes (find-holes expr)])
       (display "\n")
       (if (null? holes)
           (display "  No holes found in expression.\n")
           (for-each
            (lambda (hole-info)
                    (let* ([path (car hole-info)]
                           [hole (cdr hole-info)]
                           [hole-n (hole-name hole)]
                           [type (infer-hole-type expr path)])
                          (if hole-n
                              (printf "  Hole ~a at ~a : ~a\n"
                                      hole-n path (type->string type))
                              (printf "  Hole at ~a : ~a\n"
                                      path (type->string type)))))
            holes))
       (display "\n")))

;;; hole-fits : Expr → Void
;;; Show available fits for each hole in the expression.
(define (hole-fits expr)
  (reset-fresh!)
  (let ([holes (find-holes expr)])
       (display "\n")
       (if (null? holes)
           (display "  No holes found in expression.\n")
           (for-each
            (lambda (hole-info)
                    (let* ([path (car hole-info)]
                           [hole (cdr hole-info)]
                           [hole-n (hole-name hole)]
                           [env (expr-context expr path)]
                           ;; Add standard bindings
                           [full-env (append standard-env env)]
                           [type (infer-hole-type expr path)]
                           [fits (if (symbol? type)
                                     (find-fits type full-env)
                                     '())])
                          (if hole-n
                              (printf "  Hole ~a : ~a\n" hole-n (type->string type))
                              (printf "  Hole : ~a\n" (type->string type)))
                          (if (null? fits)
                              (display "    No direct fits found.\n")
                              (for-each
                               (lambda (fit)
                                       (printf "    - ~a : ~a\n"
                                               (car fit) (type->string (cdr fit))))
                               (take-upto 10 fits)))
                          ;; Suggest lambda for function holes
                          (when (function-type? type)
                                (let ([params (function-param-types type)])
                                     (printf "    - (fn (~a) ...)\n"
                                             (join-strings " "
                                                           (map (lambda (i)
                                                                        (string-append "x"
                                                                                       (number->string i)))
                                                                (iota (length params)))))))))
            holes))
       (display "\n")))

;;; hole-refine : Expr × Type → Void
;;; Show what the expression looks like with holes narrowed to the given type.
(define (hole-refine expr expected-type)
  (reset-fresh!)
  (let ([result (check expr expected-type empty-tenv)])
       (display "\n")
       (if (eq? (car result) 'ok)
           (begin
            (display "  Expression type-checks against ")
            (display (type->string expected-type))
            (display "\n"))
           (begin
            (display "  Type error: ")
            (display (format-type-error result))
            (display "\n")))
       (display "\n")))

;;; case-split : Expr × Symbol → Void
;;; Generate case patterns for a sum type variable.
(define (case-split type var)
  (display "\n")
  (cond
   [(not (pair? type))
    (printf "  ~a is not a sum type.\n" type)]
   [(eq? (car type) '+)
    (display "  Case patterns:\n")
    (for-each
     (lambda (variant)
             (let ([tag (car variant)]
                   [fields (cdr variant)])
                  (if (null? fields)
                      (printf "    [~a ...]\n" tag)
                      (printf "    [(~a ~a) ...]\n"
                              tag
                              (join-strings " "
                                            (map (lambda (i)
                                                         (string-append
                                                          (string-downcase (symbol->string tag))
                                                          (number->string i)))
                                                 (iota (length fields))))))))
     (cdr type))]
   [(eq? (car type) 'List)
    (display "  Case patterns:\n")
    (display "    [() ...]\n")
    (display "    [(cons x xs) ...]\n")]
   [(eq? type 'Bool)
    (display "  Case patterns:\n")
    (display "    [#t ...]\n")
    (display "    [#f ...]\n")]
   [(eq? type 'Nat)
    (display "  Case patterns:\n")
    (display "    [0 ...]\n")
    (display "    [(succ n) ...]\n")]
   [else
    (printf "  Don't know how to split ~a\n" (type->string type))])
  (display "\n"))

;;; ============================================================
;;; Standard Environment
;;; ============================================================

;;; Common functions available in the standard environment
(define standard-env
  `((id . (∀ (a) (-> a a)))
    (const . (∀ (a b) (-> a b a)))
    (compose . (∀ (a b c) (-> (-> b c) (-> a b) (-> a c))))
    (flip . (∀ (a b c) (-> (-> a b c) (-> b a c))))
    (+ . (-> Int Int Int))
    (- . (-> Int Int Int))
    (* . (-> Int Int Int))
    (/ . (-> Int Int Int))
    (< . (-> Int Int Bool))
    (> . (-> Int Int Bool))
    (= . (-> Int Int Bool))
    (not . (-> Bool Bool))
    (and . (-> Bool Bool Bool))
    (or . (-> Bool Bool Bool))
    (car . (∀ (a) (-> (List a) a)))
    (cdr . (∀ (a) (-> (List a) (List a))))
    (cons . (∀ (a) (-> a (List a) (List a))))
    (null? . (∀ (a) (-> (List a) Bool)))
    (map . (∀ (a b) (-> (-> a b) (List a) (List b))))
    (filter . (∀ (a) (-> (-> a Bool) (List a) (List a))))
    (foldl . (∀ (a b) (-> (-> b a b) b (List a) b)))
    (foldr . (∀ (a b) (-> (-> a b b) b (List a) b)))
    (length . (∀ (a) (-> (List a) Int)))
    (append . (∀ (a) (-> (List a) (List a) (List a))))
    (reverse . (∀ (a) (-> (List a) (List a))))
    (take . (∀ (a) (-> Int (List a) (List a))))
    (drop . (∀ (a) (-> Int (List a) (List a))))
    (show . (∀ (a) (-> a String)))
    (square . (-> Int Int))
    (double . (-> Int Int))
    (even? . (-> Int Bool))
    (odd? . (-> Int Bool))
    (zero? . (-> Int Bool))
    (positive? . (-> Int Bool))
    (negative? . (-> Int Bool))
    (fst . (∀ (a b) (-> (× a b) a)))
    (snd . (∀ (a b) (-> (× a b) b)))))

;;; ============================================================
;;; Helper Functions
;;; ============================================================

;;; take-upto : Nat × (List a) → (List a)
(define (take-upto n lst)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take-upto (- n 1) (cdr lst)))))

;;; iota : Nat → (List Nat)
(define (iota n)
  (let loop ([i 0] [acc '()])
       (if (>= i n)
           (reverse acc)
           (loop (+ i 1) (cons i acc)))))

;;; join-strings : String × (List String) → String
(define (join-strings sep strs)
  (if (null? strs)
      ""
      (fold-left (lambda (acc s) (string-append acc sep s))
                 (car strs)
                 (cdr strs))))

;;; filter-map : (a → b | #f) × (List a) → (List b)
(define (filter-map f lst)
  (let loop ([items lst] [result '()])
       (if (null? items)
           (reverse result)
           (let ([mapped (f (car items))])
                (if mapped
                    (loop (cdr items) (cons mapped result))
                    (loop (cdr items) result))))))

;;; ============================================================
;;; Usage Help
;;; ============================================================

(define (hole-help)
  (display "\n")
  (display "  Type-Driven Development with Holes\n")
  (display "  ────────────────────────────────────────────\n")
  (display "  (hole-type expr)        - Show types for holes in expr\n")
  (display "  (hole-fits expr)        - Show available fits for holes\n")
  (display "  (hole-refine expr type) - Check expr against expected type\n")
  (display "  (case-split type var)   - Generate case patterns for type\n")
  (display "\n")
  (display "  Hole syntax:\n")
  (display "    ?           - Anonymous hole\n")
  (display "    (? name)    - Named hole\n")
  (display "\n")
  (display "  Example:\n")
  (display "    (hole-fits '(let ((xs '(1 2 3))) (map ? xs)))\n")
  (display "    ;; Shows functions of type (-> Int a)\n")
  (display "\n"))

(display "Typed holes loaded. Use (hole-help) for usage.\n")
