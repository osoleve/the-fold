;;; core/blocks/normalize.ss — S-expression α-normalization via de Bruijn indices
;;; @module normalize
;;; @requires prelude
;;;
;;; Converts named variables to positional indices, ensuring that
;;; α-equivalent expressions produce identical canonical forms.
;;;
;;; (fn (x) x)           → (fn (dv 0))
;;; (fn (x) (fn (y) x))  → (fn (fn (dv 1)))
;;; (fn (x) (fn (y) y))  → (fn (fn (dv 0)))
;;;
;;; Binder forms recognized:
;;;   (fn (var ...) body)        - single or multi-argument function
;;;   (let ((var val) ...) body) - single or multi-binding let (parallel)
;;;   (fix (f) body)             - recursive binding
;;;
;;; (dv n) represents a de Bruijn variable with index n.
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "core/base/prelude.ss")

;;; ============================================================
;;; Environment (prefixed to avoid collision with eval.ss)
;;; ============================================================

;;; An environment is a list of symbols, with the innermost binding first.
;;; Index 0 refers to (car env), index 1 to (cadr env), etc.

;;; norm-env-empty : Env
;;; The empty environment with no bindings.
(define norm-env-empty '())

;;; norm-env-extend : Env × Symbol → Env
;;; Extend environment with a new symbol binding.
(define (norm-env-extend env sym)
  (cons sym env))

;;; norm-env-lookup : Env × Symbol → (Option Nat)
;;; Returns the de Bruijn index if found, #f otherwise.
(define (norm-env-lookup env sym)
  (let loop ([e env] [i 0])
       (cond
        [(null? e) #f]
        [(eq? (car e) sym) i]
        [else (loop (cdr e) (+ i 1))])))

;;; ============================================================
;;; Normalization
;;; ============================================================

;;; normalize : S-expr → S-expr
;;; Convert an expression to de Bruijn form.
(define (normalize expr)
  (normalize-with-env expr norm-env-empty))

;;; normalize-with-env : S-expr × Env → S-expr
;;; Convert expression to de Bruijn form using given environment.
(define (normalize-with-env expr env)
  (cond
   ;; Symbols: look up in environment
   [(symbol? expr)
    (let ([idx (norm-env-lookup env expr)])
         (if idx
             `(dv ,idx)      ; Bound variable → de Bruijn index
             expr))]          ; Free variable → keep as symbol
   
   ;; Non-list atoms pass through
   [(not (pair? expr)) expr]
   
   ;; (fn (var ...) body) → (fn normalized-body)
   ;; Handles both single and multi-argument functions
   [(and (eq? (car expr) 'fn)
         (pair? (cdr expr))
         (list? (cadr expr)))
    (let* ([params (cadr expr)]
           [body (caddr expr)]
           [new-env (fold-left norm-env-extend env params)])
          `(fn ,(normalize-with-env body new-env)))]
   
   ;; (let ((var val) ...) body) → nested single-binding lets
   ;; Handles both single and multi-binding lets
   [(and (eq? (car expr) 'let)
         (pair? (cdr expr))
         (list? (cadr expr)))
    (let ([bindings (cadr expr)]
          [body (caddr expr)])
         (let loop ([bindings bindings]
                    [current-env env])
              (if (null? bindings)
                  (normalize-with-env body current-env)
                  (let* ([binding (car bindings)]
                         [var (car binding)]
                         [val (cadr binding)]
                         [norm-val (normalize-with-env val env)])
                        `(let (,norm-val)
                          ,(loop (cdr bindings)
                                 (norm-env-extend current-env var)))))))]
   
   ;; (fix (f) body) → (fix normalized-body)
   ;; The recursive name is bound in the body
   [(and (eq? (car expr) 'fix)
         (pair? (cdr expr))
         (pair? (cadr expr))
         (symbol? (caadr expr)))
    (let* ([f (caadr expr)]
           [body (caddr expr)]
           [new-env (norm-env-extend env f)])
          `(fix ,(normalize-with-env body new-env)))]
   
   ;; (quote datum) → (quote datum) unchanged
   [(eq? (car expr) 'quote) expr]
   
   ;; General list: normalize each element
   [else
    (map (lambda (e) (normalize-with-env e env)) expr)]))

;;; ============================================================
;;; Free Variables
;;; ============================================================

;;; free-vars : S-expr → (List Symbol)
;;; Collect free variables in the expression (before normalization).
(define (free-vars expr)
  (free-vars-with-env expr norm-env-empty))

;;; free-vars-with-env : S-expr × Env → (List Symbol)
;;; Collect free variables using given environment.
(define (free-vars-with-env expr env)
  (cond
   [(symbol? expr)
    (if (norm-env-lookup env expr)
        '()
        (list expr))]
   [(not (pair? expr)) '()]
   [(eq? (car expr) 'quote) '()]
   [(eq? (car expr) 'fn)
    (let* ([params (cadr expr)]
           [body (caddr expr)]
           [new-env (fold-left norm-env-extend env params)])
          (free-vars-with-env body new-env))]
   [(eq? (car expr) 'let)
    (let ([bindings (cadr expr)]
          [body (caddr expr)])
         (let loop ([bindings bindings]
                    [current-env env]
                    [free-in-vals '()])
              (if (null? bindings)
                  (append free-in-vals
                          (free-vars-with-env body current-env))
                  (let* ([binding (car bindings)]
                         [var (car binding)]
                         [val (cadr binding)]
                         [free-in-val (free-vars-with-env val env)])
                        (loop (cdr bindings)
                              (norm-env-extend current-env var)
                              (append free-in-vals free-in-val))))))]
   [(eq? (car expr) 'fix)
    (let* ([f (caadr expr)]
           [body (caddr expr)])
          (free-vars-with-env body (norm-env-extend env f)))]
   [else
    (apply append (map (lambda (e) (free-vars-with-env e env)) expr))]))

;;; Note: unique is provided by prelude.ss
