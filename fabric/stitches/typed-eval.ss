;;; fabric/stitches/typed-eval.ss — Type-Checked Evaluation
;;;
;;; The marriage of types and computation.
;;;
;;; This module integrates type inference with evaluation:
;;;   1. Type-check expressions before evaluation
;;;   2. Reject ill-typed programs at compile time
;;;   3. Evaluate well-typed programs with confidence
;;;   4. Return typed values for introspection
;;;
;;; The key principle: Well-typed programs don't go wrong.
;;;   - Type errors are caught before execution
;;;   - Runtime type errors become impossible
;;;   - Values carry their types for inspection
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - block.ss
;;;   - prim.ss
;;;   - eval.ss
;;;   - types.ss
;;;   - kinds.ss
;;;   - infer.ss
;;;   - annotate.ss

(load "prelude.ss")
(load "block.ss")
(load "prim.ss")
(load "eval.ss")
(load "types.ss")
(load "kinds.ss")
(load "infer.ss")
(load "annotate.ss")

;;; ============================================================
;;; Typed Values
;;; ============================================================

;;; A typed value pairs a value with its type.
;;; (typed Type Value)

(define (typed type value)
  `(typed ,type ,value))

(define (typed? v)
  (and (pair? v) (eq? (car v) 'typed)))

(define (typed-type tv)
  (if (typed? tv) (cadr tv) '?))

(define (typed-value tv)
  (if (typed? tv)
      (typed-value (caddr tv))  ; Recursively unwrap nested typed values
      tv))

;;; ============================================================
;;; Type-Check Then Evaluate
;;; ============================================================

;;; typecheck-eval : Expr × Fuel → (ok TypedValue) | (error ...)
;;;
;;; The main entry point for typed evaluation:
;;;   1. Run type inference
;;;   2. If type error → return the error (no evaluation)
;;;   3. If well-typed → evaluate and return typed result
;;;
;;; This ensures that only well-typed programs execute.

(define (typecheck-eval expr fuel)
  (reset-fresh!)
  (let ([type-result (typeof expr)])
       (cond
        ;; Type error — reject before evaluation
        [(and (pair? type-result) (eq? (car type-result) 'error))
         type-result]
        ;; Well-typed — evaluate
        [else
         (let ([eval-result (run expr fuel)])
              (cond
               [(eq? (car eval-result) 'ok)
                `(ok ,(typed type-result (cadr eval-result)))]
               [else eval-result]))])))

;;; typecheck-eval-env : Expr × TypeEnv × ValueEnv × Fuel → Result
;;;
;;; Type-check and evaluate with custom environments.
;;; TypeEnv maps vars to types, ValueEnv maps vars to values.

(define (typecheck-eval-env expr tenv venv fuel)
  (reset-fresh!)
  (let ([type-result (infer expr tenv)])
       (cond
        [(and (pair? type-result) (eq? (car type-result) 'error))
         type-result]
        [(eq? (car type-result) 'ok)
         (let* ([inferred-type (cadr type-result)]
                [subst (caddr type-result)]
                [final-type (generalize '() (apply-subst subst inferred-type))]
                [eval-result (eval-with-env expr venv fuel)])
               (cond
                [(eq? (car eval-result) 'ok)
                 `(ok ,(typed final-type (cadr eval-result)))]
                [else eval-result]))]
        [else type-result])))

;;; ============================================================
;;; Annotated Evaluation
;;; ============================================================

;;; eval-annotated : AnnotatedExpr × Env × Fuel → (ok TypedValue Fuel) | ...
;;;
;;; Evaluate an already-annotated expression.
;;; The types are already computed; we just evaluate and preserve them.

(define (eval-annotated e env fuel)
  (if (out-of-fuel? fuel)
      `(suspended ,e ,env)
      (if (ann? e)
          (let* ([type (ann-type e)]
                 [inner (ann-expr e)]
                 [result (eval-annotated-inner inner env fuel)])
                (cond
                 [(eq? (car result) 'ok)
                  ;; Extract raw value before wrapping to avoid double-typing
                  (let ([raw-val (typed-value (cadr result))])
                       `(ok ,(typed type raw-val) ,(caddr result)))]
                 [else result]))
          ;; Not annotated — fall back to regular evaluation
          (eval-expr e env fuel))))

(define (eval-annotated-inner expr env fuel)
  (cond
   ;; Literals
   [(or (number? expr) (string? expr) (boolean? expr) (null? expr))
    `(ok ,expr ,fuel)]
   
   ;; Variable
   [(symbol? expr)
    (let ([result (env-lookup env expr)])
         (if (eq? (car result) 'ok)
             `(ok ,(typed-value (cadr result)) ,(- fuel 1))
             result))]
   
   [(not (pair? expr))
    `(error invalid-annotated-expr ,expr)]
   
   ;; Quote
   [(eq? (car expr) 'quote)
    `(ok ,(cadr expr) ,(- fuel 1))]
   
   ;; Lambda with annotated params
   [(eq? (car expr) 'fn)
    (let* ([ann-params (cadr expr)]
           [body (caddr expr)]
           ;; Extract param names from annotations
           [params (map (lambda (p)
                                (if (ann? p) (ann-expr p) p))
                        ann-params)])
          `(ok ,(make-closure params body env) ,(- fuel 1)))]
   
   ;; Let with annotated bindings
   [(eq? (car expr) 'let)
    (eval-annotated-let (cadr expr) (caddr expr) env fuel)]
   
   ;; Fix
   [(eq? (car expr) 'fix)
    (let ([name (cadr expr)]
          [fn-expr (caddr expr)])
         (eval-annotated-fix name fn-expr env fuel))]
   
   ;; If
   [(eq? (car expr) 'if)
    (eval-annotated-if (cadr expr) (caddr expr) (cadddr expr) env fuel)]
   
   ;; Case
   [(eq? (car expr) 'case)
    (eval-annotated-case (cadr expr) (cddr expr) env fuel)]
   
   ;; Prim
   [(eq? (car expr) 'prim)
    (eval-annotated-prim (cadr expr) (cddr expr) env fuel)]
   
   ;; Application
   [else
    (eval-annotated-app (car expr) (cdr expr) env fuel)]))

;;; ============================================================
;;; Annotated Case Evaluation
;;; ============================================================

;;; (case scrutinee ((Tag vars...) body) ...)
;;; Pattern matching on block tags with type preservation

(define (eval-annotated-case scrutinee clauses env fuel)
  (if (out-of-fuel? fuel)
      `(suspended (case ,scrutinee ,@clauses) ,env)
      (let ([scrut-result (eval-annotated scrutinee env (- fuel 1))])
           (cond
            [(eq? (car scrut-result) 'ok)
             (let ([val (typed-value (cadr scrut-result))]
                   [remaining (caddr scrut-result)])
                  (if (block? val)
                      (match-annotated-clauses val clauses env remaining)
                      `(error case-requires-block ,val)))]
            [(eq? (car scrut-result) 'suspended)
             `(suspended (case ,(cadr scrut-result) ,@clauses)
               ,(caddr scrut-result))]
            [else scrut-result]))))

(define (match-annotated-clauses block clauses env fuel)
  (if (null? clauses)
      `(error no-matching-clause ,(block-tag block))
      (let* ([clause (car clauses)]
             [pattern (car clause)]
             [body (cadr clause)]
             ;; Pattern may have annotated variables
             [tag (if (ann? (car pattern)) (ann-expr (car pattern)) (car pattern))]
             [vars (map (lambda (v) (if (ann? v) (ann-expr v) v))
                        (cdr pattern))])
            (if (eq? tag (block-tag block))
                ;; Match! Bind refs to vars
                (let ([refs (block-refs-list block)])
                     (if (= (length vars) (length refs))
                         (eval-annotated body (env-extend* env vars refs) fuel)
                         `(error pattern-arity-mismatch ,tag)))
                ;; No match, try next clause
                (match-annotated-clauses block (cdr clauses) env fuel)))))

;;; ============================================================
;;; Annotated Let Evaluation
;;; ============================================================

(define (eval-annotated-let bindings body env fuel)
  (if (out-of-fuel? fuel)
      `(suspended (let ,bindings ,body) ,env)
      (eval-annotated-let-bindings bindings body env '() fuel)))

(define (eval-annotated-let-bindings bindings body env acc fuel)
  (if (null? bindings)
      (eval-annotated body (env-extend* env (map car acc) (map cdr acc)) fuel)
      (let* ([binding (car bindings)]
             [name (car binding)]
             [init (cadr binding)]
             [result (eval-annotated init env fuel)])
            (cond
             [(eq? (car result) 'ok)
              (eval-annotated-let-bindings
               (cdr bindings) body env
               (cons (cons name (cadr result)) acc)
               (caddr result))]
             [else result]))))

;;; ============================================================
;;; Annotated Fix Evaluation
;;; ============================================================

(define (eval-annotated-fix name fn-expr env fuel)
  (if (out-of-fuel? fuel)
      `(suspended (fix ,name ,fn-expr) ,env)
      (let ([fn-result (eval-annotated fn-expr env fuel)])
           (if (eq? (car fn-result) 'ok)
               (let* ([fn-val (typed-value (cadr fn-result))]
                      [remaining (caddr fn-result)])
                     (if (closure? fn-val)
                         (let* ([params (closure-params fn-val)]
                                [body (closure-body fn-val)]
                                [closure-env (closure-env fn-val)]
                                ;; Create recursive environment
                                [rec-env (env-extend closure-env name 'placeholder)]
                                [rec-closure (make-closure params body rec-env)])
                               (set-cdr! (car rec-env) rec-closure)
                               `(ok ,rec-closure ,remaining))
                         `(error fix-requires-fn ,fn-val)))
               fn-result))))

;;; ============================================================
;;; Annotated If Evaluation
;;; ============================================================

(define (eval-annotated-if test then-expr else-expr env fuel)
  (if (out-of-fuel? fuel)
      `(suspended (if ,test ,then-expr ,else-expr) ,env)
      (let ([test-result (eval-annotated test env (- fuel 1))])
           (cond
            [(eq? (car test-result) 'ok)
             (let ([test-val (typed-value (cadr test-result))]
                   [remaining (caddr test-result)])
                  (if test-val
                      (eval-annotated then-expr env remaining)
                      (eval-annotated else-expr env remaining)))]
            [else test-result]))))

;;; ============================================================
;;; Annotated Prim Evaluation
;;; ============================================================

(define (eval-annotated-prim op args env fuel)
  (if (out-of-fuel? fuel)
      `(suspended (prim ,op ,@args) ,env)
      (eval-annotated-prim-args op args env '() (- fuel 1))))

(define (eval-annotated-prim-args op remaining env acc fuel)
  (if (null? remaining)
      (let ([op-sym (if (and (pair? op) (eq? (car op) 'quote))
                        (cadr op)
                        op)]
            [arg-vals (map typed-value (reverse acc))])
           `(ok ,(apply prim (cons op-sym arg-vals)) ,fuel))
      (let ([result (eval-annotated (car remaining) env fuel)])
           (cond
            [(eq? (car result) 'ok)
             (eval-annotated-prim-args op (cdr remaining) env
                                       (cons (cadr result) acc)
                                       (caddr result))]
            [else result]))))

;;; ============================================================
;;; Annotated Application Evaluation
;;; ============================================================

(define (eval-annotated-app fn args env fuel)
  (if (out-of-fuel? fuel)
      `(suspended (,fn ,@args) ,env)
      (let ([fn-result (eval-annotated fn env (- fuel 1))])
           (cond
            [(eq? (car fn-result) 'ok)
             (let ([fn-val (typed-value (cadr fn-result))]
                   [remaining (caddr fn-result)])
                  (if (closure? fn-val)
                      (eval-annotated-call-args fn-val args env '() remaining)
                      `(error not-a-function ,fn-val)))]
            [else fn-result]))))

(define (eval-annotated-call-args closure remaining env acc fuel)
  (if (null? remaining)
      (let* ([params (closure-params closure)]
             [body (closure-body closure)]
             [closure-env (closure-env closure)]
             [arg-vals (map typed-value (reverse acc))])
            (if (= (length params) (length arg-vals))
                (eval-annotated body (env-extend* closure-env params arg-vals) fuel)
                `(error arity-mismatch (expected ,(length params))
                  (got ,(length arg-vals)))))
      (let ([result (eval-annotated (car remaining) env fuel)])
           (cond
            [(eq? (car result) 'ok)
             (eval-annotated-call-args closure (cdr remaining) env
                                       (cons (cadr result) acc)
                                       (caddr result))]
            [else result]))))

;;; ============================================================
;;; Full Pipeline: Annotate → Type-Check → Evaluate
;;; ============================================================

;;; typed-run : Expr × Fuel → (ok TypedValue) | (error ...)
;;;
;;; The complete typed evaluation pipeline:
;;;   1. Annotate expression with types
;;;   2. If annotation fails (type error) → return error
;;;   3. Evaluate annotated expression
;;;   4. Return typed result

(define (typed-run expr fuel)
  (reset-fresh!)
  (let ([ann-result (annotate expr empty-tenv)])
       (cond
        ;; Type error during annotation
        [(and (pair? ann-result) (eq? (car ann-result) 'error))
         ann-result]
        ;; Successfully annotated
        [(eq? (car ann-result) 'ok)
         (let* ([annotated-e (finalize-ann (cadr ann-result) (caddr ann-result))]
                [type (ann-type annotated-e)]
                [eval-result (eval-annotated annotated-e empty-env fuel)])
               (cond
                [(eq? (car eval-result) 'ok)
                 `(ok ,(typed type (typed-value (cadr eval-result))))]
                [else eval-result]))]
        [else ann-result])))

;;; ============================================================
;;; Type Environment Builder
;;; ============================================================

;;; Build paired type/value environments for use with typecheck-eval-env

(define (make-typed-env bindings)
  (let loop ([bindings bindings] [tenv '()] [venv '()])
       (if (null? bindings)
           (values tenv venv)
           (let* ([binding (car bindings)]
                  [name (car binding)]
                  [type (cadr binding)]
                  [value (caddr binding)])
                 (loop (cdr bindings)
                       (cons (cons name type) tenv)
                       (cons (cons name value) venv))))))

;;; ============================================================
;;; Typed Prelude
;;; ============================================================

;;; A prelude with explicit type signatures.

(define typed-prelude-defs
  '(;; Basic combinators with types
    (id      (∀ (a) (-> a a))
        (fn (x) x))
    
    (const   (∀ (a b) (-> a (-> b a)))
           (fn (x) (fn (y) x)))
    
    (compose (∀ (a b c) (-> (-> b c) (-> (-> a b) (-> a c))))
             (fn (f) (fn (g) (fn (x) (f (g x))))))
    
    ;; Boolean operations
    (bool-not (-> Bool Bool)
              (fn (b) (if b #f #t)))
    
    ;; Arithmetic (monomorphic for now)
    (inc     (-> Int Int)
         (fn (x) (prim 'add x 1)))
    
    (dec     (-> Int Int)
         (fn (x) (prim 'sub x 1)))
    
    (double  (-> Int Int)
            (fn (x) (prim 'mul x 2)))
    
    (square  (-> Int Int)
            (fn (x) (prim 'mul x x)))))

;;; Build typed prelude environments
(define (build-typed-prelude fuel)
  (let loop ([defs typed-prelude-defs]
             [tenv empty-tenv]
             [venv empty-env]
             [remaining fuel])
       (if (null? defs)
           (values tenv venv)
           (let* ([def (car defs)]
                  [name (car def)]
                  [type (cadr def)]
                  [expr (caddr def)]
                  [result (eval-expr expr venv remaining)])
                 (if (eq? (car result) 'ok)
                     (loop (cdr defs)
                           (tenv-extend tenv name type)
                           (env-extend venv name (cadr result))
                           (caddr result))
                     (values tenv venv))))))

;;; typed-run-prelude : Expr × Fuel → Result
;;; Evaluate with the typed prelude.
(define (typed-run-prelude expr fuel)
  (call-with-values
   (lambda () (build-typed-prelude 100))
   (lambda (tenv venv)
           (typecheck-eval-env expr tenv venv fuel))))

;;; ============================================================
;;; Display Typed Values
;;; ============================================================

;;; show-typed : TypedValue → String
(define (show-typed tv)
  (if (typed? tv)
      (string-append (format "~s" (typed-value tv))
                     " : "
                     (type->string (typed-type tv)))
      (format "~s" tv)))

;;; print-typed : TypedValue → void
(define (print-typed tv)
  (display (show-typed tv))
  (newline))

;;; ============================================================
;;; REPL Support
;;; ============================================================

;;; typed-repl-eval : Expr → void
;;; Evaluate and display with types (for REPL use).
(define (typed-repl-eval expr)
  (let ([result (typed-run expr 1000)])
       (cond
        [(and (pair? result) (eq? (car result) 'ok))
         (print-typed (cadr result))]
        [(and (pair? result) (eq? (car result) 'error))
         (display "Type error: ")
         (display (format-type-error result))
         (newline)]
        [else
         (display "Error: ")
         (display result)
         (newline)])))
