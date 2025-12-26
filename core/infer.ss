;;; core/infer.ss — Bidirectional Type Inference
;;;
;;; Types flow in two directions:
;;;   - Inference (↑): Expression → Type (synthesize a type)
;;;   - Checking (↓): Expression × Type → Bool (verify against expected)
;;;
;;; The key insight: some forms synthesize, some check.
;;;   - Variables, applications, annotations: synthesize
;;;   - Lambdas, let-bodies: check against expected type
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Based on "Complete and Easy Bidirectional Typechecking for
;;; Higher-Rank Polymorphism" (Dunfield & Krishnaswami, 2013)

;;; Dependencies
;;; (load "types.ss")
;;; (load "kinds.ss")

;;; ============================================================
;;; Type Environment
;;; ============================================================

;;; A type environment maps term variables to types.
;;; (List (Pair Symbol Type))

(define empty-tenv '())

(define (tenv-lookup env var)
  (let ([entry (assq var env)])
    (if entry (cdr entry) #f)))

(define (tenv-extend env var type)
  (cons (cons var type) env))

(define (tenv-extend* env bindings)
  (append bindings env))

;;; ============================================================
;;; Fresh Type Variables
;;; ============================================================

;;; For inference, we need fresh type variables.
;;; We use a counter embedded in the inference state.

(define *fresh-counter* 0)

(define (reset-fresh!)
  (set! *fresh-counter* 0))

(define (fresh-tvar)
  (set! *fresh-counter* (+ *fresh-counter* 1))
  (string->symbol (string-append "τ" (number->string *fresh-counter*))))

(define (fresh-tvar-named prefix)
  (set! *fresh-counter* (+ *fresh-counter* 1))
  (string->symbol (string-append prefix (number->string *fresh-counter*))))

;;; ============================================================
;;; Substitution
;;; ============================================================

;;; A substitution maps type variables to types.
;;; We apply substitutions to types to instantiate variables.

(define empty-subst '())

(define (subst-lookup s var)
  (let ([entry (assq var s)])
    (if entry (cdr entry) #f)))

(define (subst-extend s var type)
  (cons (cons var type) s))

;;; apply-subst : Subst × Type → Type
;;; Apply a substitution to a type.
(define (apply-subst s type)
  (cond
    [(type-var? type)
     (let ([replacement (subst-lookup s type)])
       (if replacement
           (apply-subst s replacement)  ; Chase chains
           type))]
    [(or (base-type? type) (hole? type)) type]
    [(not (pair? type)) type]
    ;; Don't substitute bound variables
    [(eq? (car type) '∀)
     (let ([bound (cadr type)]
           [body (caddr type)])
       ;; Remove bound vars from substitution
       (let ([s* (filter (lambda (p) (not (memq (car p) bound))) s)])
         `(∀ ,bound ,(apply-subst s* body))))]
    [(eq? (car type) 'μ)
     (let ([var (cadr type)]
           [body (caddr type)])
       (let ([s* (filter (lambda (p) (not (eq? (car p) var))) s)])
         `(μ ,var ,(apply-subst s* body))))]
    [else
     (cons (car type)
           (map (lambda (t) (apply-subst s t)) (cdr type)))]))

;;; compose-subst : Subst × Subst → Subst
;;; Compose two substitutions: (compose s1 s2) applies s2 then s1.
(define (compose-subst s1 s2)
  (append
    (map (lambda (p) (cons (car p) (apply-subst s1 (cdr p)))) s2)
    s1))

;;; ============================================================
;;; Unification
;;; ============================================================

;;; Unification finds a substitution that makes two types equal.
;;; Returns (ok subst) or (error message).

(define (unify t1 t2)
  (unify-with empty-subst t1 t2))

(define (unify-with s t1 t2)
  (let ([t1 (apply-subst s t1)]
        [t2 (apply-subst s t2)])
    (cond
      ;; Same type
      [(type=? t1 t2) `(ok ,s)]

      ;; Type variable on left
      [(type-var? t1)
       (if (occurs? t1 t2)
           `(error occurs-check ,t1 ,t2)
           `(ok ,(subst-extend s t1 t2)))]

      ;; Type variable on right
      [(type-var? t2)
       (if (occurs? t2 t1)
           `(error occurs-check ,t2 ,t1)
           `(ok ,(subst-extend s t2 t1)))]

      ;; Holes unify with anything
      [(hole? t1) `(ok ,s)]
      [(hole? t2) `(ok ,s)]

      ;; Both are compound types with same constructor
      [(and (pair? t1) (pair? t2) (eq? (car t1) (car t2)))
       (unify-lists s (cdr t1) (cdr t2))]

      ;; Type application
      [(and (type-app? t1) (type-app? t2))
       (let ([h1 (type-app-head t1)]
             [h2 (type-app-head t2)]
             [a1 (type-app-args t1)]
             [a2 (type-app-args t2)])
         (let ([result (unify-with s h1 h2)])
           (if (eq? (car result) 'ok)
               (unify-lists (cadr result) a1 a2)
               result)))]

      [else `(error type-mismatch ,t1 ,t2)])))

;;; unify-lists : Subst × (List Type) × (List Type) → Result
(define (unify-lists s ts1 ts2)
  (cond
    [(and (null? ts1) (null? ts2)) `(ok ,s)]
    [(or (null? ts1) (null? ts2))
     `(error arity-mismatch ,ts1 ,ts2)]
    [else
     (let ([result (unify-with s (car ts1) (car ts2))])
       (if (eq? (car result) 'ok)
           (unify-lists (cadr result) (cdr ts1) (cdr ts2))
           result))]))

;;; occurs? : Symbol × Type → Boolean
;;; Does the type variable occur in the type? (For occurs check)
(define (occurs? var type)
  (cond
    [(type-var? type) (eq? var type)]
    [(or (base-type? type) (hole? type)) #f]
    [(not (pair? type)) #f]
    [(eq? (car type) '∀)
     (if (memq var (cadr type))
         #f  ; Bound, doesn't count
         (occurs? var (caddr type)))]
    [(eq? (car type) 'μ)
     (if (eq? var (cadr type))
         #f
         (occurs? var (caddr type)))]
    [else (ormap (lambda (t) (occurs? var t)) (cdr type))]))

(define (ormap pred lst)
  (and (pair? lst)
       (or (pred (car lst))
           (ormap pred (cdr lst)))))

;;; ============================================================
;;; Instantiation
;;; ============================================================

;;; Instantiate a polymorphic type with fresh type variables.
;;; (∀ (a b) (-> a b a)) → (-> τ1 τ2 τ1) with fresh τ1, τ2

(define (instantiate type)
  (if (and (pair? type) (eq? (car type) '∀))
      (let* ([vars (cadr type)]
             [body (caddr type)]
             [fresh-vars (map (lambda (_) (fresh-tvar)) vars)]
             [s (map cons vars fresh-vars)])
        (apply-subst s body))
      type))

;;; ============================================================
;;; Generalization
;;; ============================================================

;;; Generalize a type by quantifying over free type variables
;;; not in the environment.

(define (generalize env type)
  (let* ([env-vars (apply append (map (lambda (p) (free-tvars (cdr p))) env))]
         [type-vars (free-tvars type)]
         [gen-vars (unique (filter (lambda (v) (not (memq v env-vars))) type-vars))])
    (if (null? gen-vars)
        type
        `(∀ ,gen-vars ,type))))

;;; unique : (List α) → (List α)
;;; Remove duplicates, preserving order.
(define (unique lst)
  (let loop ([lst lst] [seen '()] [acc '()])
    (cond
      [(null? lst) (reverse acc)]
      [(memq (car lst) seen) (loop (cdr lst) seen acc)]
      [else (loop (cdr lst) (cons (car lst) seen) (cons (car lst) acc))])))

;;; ============================================================
;;; Bidirectional Type Inference
;;; ============================================================

;;; infer : Expr × TEnv → (Result Type Subst)
;;; Synthesize a type for an expression.
(define (infer expr env)
  (cond
    ;; Literals
    [(number? expr)
     (if (integer? expr)
         `(ok Int ,empty-subst)
         `(ok Nat ,empty-subst))]
    [(boolean? expr) `(ok Bool ,empty-subst)]
    [(string? expr) `(ok String ,empty-subst)]
    [(and (pair? expr) (eq? (car expr) 'quote))
     (infer-quoted (cadr expr))]

    ;; Variable
    [(symbol? expr)
     (let ([type (tenv-lookup env expr)])
       (if type
           `(ok ,(instantiate type) ,empty-subst)
           `(error unbound-variable ,expr)))]

    [(not (pair? expr))
     `(error unknown-expression ,expr)]

    ;; Type annotation: (: expr type)
    [(eq? (car expr) ':)
     (let* ([e (cadr expr)]
            [annot-type (caddr expr)]
            [result (check e annot-type env)])
       (if (eq? (car result) 'ok)
           `(ok ,annot-type ,(cadr result))
           result))]

    ;; Lambda: (fn (x) body) — need expected type to check
    ;; Without annotation, we infer with fresh type variables
    [(eq? (car expr) 'fn)
     (let* ([params (cadr expr)]
            [body (caddr expr)]
            [param-types (map (lambda (_) (fresh-tvar)) params)]
            [new-env (tenv-extend* env (map cons params param-types))]
            [result (infer body new-env)])
       (if (eq? (car result) 'ok)
           (let ([body-type (cadr result)]
                 [s (caddr result)])
             `(ok ,(apply-subst s (cons '-> (append param-types (list body-type))))
                  ,s))
           result))]

    ;; Application: (f arg ...)
    [(not (special-form? (car expr)))
     (infer-app (car expr) (cdr expr) env)]

    ;; Let: (let ((x e1) ...) body)
    [(eq? (car expr) 'let)
     (infer-let (cadr expr) (caddr expr) env empty-subst)]

    ;; Fix: (fix name fn-expr) or (fix (name) fn-expr)
    [(eq? (car expr) 'fix)
     (let* ([name-part (cadr expr)]
            [var (if (pair? name-part) (car name-part) name-part)]
            [body (caddr expr)]
            [fix-type (fresh-tvar)]
            [new-env (tenv-extend env var fix-type)]
            [result (infer body new-env)])
       (if (eq? (car result) 'ok)
           (let* ([body-type (cadr result)]
                  [s1 (caddr result)]
                  [unify-result (unify-with s1 fix-type body-type)])
             (if (eq? (car unify-result) 'ok)
                 `(ok ,(apply-subst (cadr unify-result) body-type)
                      ,(cadr unify-result))
                 unify-result))
           result))]

    ;; If: (if test then else)
    [(eq? (car expr) 'if)
     (infer-if (cadr expr) (caddr expr) (cadddr expr) env)]

    ;; Primitive call: (prim 'op args...)
    [(eq? (car expr) 'prim)
     (let ([op (cadr expr)])
       ;; Handle quoted operator: (prim 'neg x) → op is (quote neg)
       (let ([op-sym (if (and (pair? op) (eq? (car op) 'quote))
                         (cadr op)
                         op)])
         (infer-prim op-sym (cddr expr) env)))]

    [else `(error unsupported-expression ,expr)]))

;;; ============================================================
;;; Let Inference (Multiple Bindings)
;;; ============================================================

(define (infer-let bindings body env subst)
  (if (null? bindings)
      ;; All bindings processed, infer body
      (let ([result (infer body env)])
        (if (eq? (car result) 'ok)
            `(ok ,(cadr result) ,(compose-subst (caddr result) subst))
            result))
      ;; Process next binding
      (let* ([binding (car bindings)]
             [var (car binding)]
             [init (cadr binding)]
             [init-result (infer init env)])
        (if (eq? (car init-result) 'ok)
            (let* ([init-type (cadr init-result)]
                   [s1 (caddr init-result)]
                   [combined-subst (compose-subst s1 subst)]
                   [gen-type (generalize env (apply-subst combined-subst init-type))]
                   [new-env (tenv-extend env var gen-type)])
              (infer-let (cdr bindings) body new-env combined-subst))
            init-result))))

;;; ============================================================
;;; If Inference
;;; ============================================================

(define (infer-if test then-expr else-expr env)
  (let ([test-result (infer test env)])
    (if (not (eq? (car test-result) 'ok))
        test-result
        (let* ([test-type (cadr test-result)]
               [s1 (caddr test-result)]
               ;; Check test is Bool
               [bool-unify (unify-with s1 test-type 'Bool)])
          (if (not (eq? (car bool-unify) 'ok))
              `(error if-test-not-bool ,test-type)
              (let* ([s2 (cadr bool-unify)]
                     [then-result (infer then-expr env)])
                (if (not (eq? (car then-result) 'ok))
                    then-result
                    (let* ([then-type (cadr then-result)]
                           [s3 (compose-subst (caddr then-result) s2)]
                           [else-result (infer else-expr env)])
                      (if (not (eq? (car else-result) 'ok))
                          else-result
                          (let* ([else-type (cadr else-result)]
                                 [s4 (compose-subst (caddr else-result) s3)]
                                 ;; Unify branches
                                 [branch-unify (unify-with s4
                                                 (apply-subst s4 then-type)
                                                 (apply-subst s4 else-type))])
                            (if (eq? (car branch-unify) 'ok)
                                (let ([s5 (cadr branch-unify)])
                                  `(ok ,(apply-subst s5 then-type) ,s5))
                                branch-unify)))))))))))

;;; ============================================================
;;; Application Inference
;;; ============================================================

(define (infer-app fn args env)
  (let ([fn-result (infer fn env)])
    (if (eq? (car fn-result) 'ok)
        (let* ([fn-type (cadr fn-result)]
               [s1 (caddr fn-result)])
          (infer-app-args fn-type args s1 env))
        fn-result)))

(define (infer-app-args fn-type args s env)
  (if (null? args)
      `(ok ,(apply-subst s fn-type) ,s)
      (let ([fn-type (apply-subst s fn-type)])
        (cond
          ;; Function type
          [(function-type? fn-type)
           (let* ([param-types (function-param-types fn-type)]
                  [return-type (function-return-type fn-type)])
             (if (= (length args) (length param-types))
                 (let ([result (check-args args param-types s env)])
                   (if (eq? (car result) 'ok)
                       `(ok ,(apply-subst (cadr result) return-type)
                            ,(cadr result))
                       result))
                 `(error arity-mismatch
                         (expected ,(length param-types))
                         (got ,(length args)))))]
          ;; Type variable — create function type
          [(type-var? fn-type)
           (let* ([arg-types (map (lambda (_) (fresh-tvar)) args)]
                  [ret-type (fresh-tvar)]
                  [expected-fn-type (cons '-> (append arg-types (list ret-type)))]
                  [unify-result (unify-with s fn-type expected-fn-type)])
             (if (eq? (car unify-result) 'ok)
                 (let ([s2 (cadr unify-result)])
                   (let ([result (check-args args arg-types s2 env)])
                     (if (eq? (car result) 'ok)
                         `(ok ,(apply-subst (cadr result) ret-type)
                              ,(cadr result))
                         result)))
                 unify-result))]
          [else `(error not-a-function ,fn-type)]))))

(define (check-args args types s env)
  (if (null? args)
      `(ok ,s)
      (let ([result (check (car args) (apply-subst s (car types)) env)])
        (if (eq? (car result) 'ok)
            (check-args (cdr args) (cdr types) (compose-subst (cadr result) s) env)
            result))))

;;; ============================================================
;;; Type Checking (Downward)
;;; ============================================================

;;; check : Expr × Type × TEnv → (Result Subst)
;;; Check that an expression has the expected type.
(define (check expr expected env)
  (cond
    ;; Lambda against function type
    [(and (pair? expr) (eq? (car expr) 'fn) (function-type? expected))
     (let* ([params (cadr expr)]
            [body (caddr expr)]
            [param-types (function-param-types expected)]
            [return-type (function-return-type expected)])
       (if (= (length params) (length param-types))
           (let* ([new-env (tenv-extend* env (map cons params param-types))]
                  [result (check body return-type new-env)])
             result)
           `(error arity-mismatch
                   (expected ,(length param-types))
                   (got ,(length params)))))]

    ;; Polymorphic type — instantiate and check
    [(and (pair? expected) (eq? (car expected) '∀))
     (check expr (instantiate expected) env)]

    ;; Fall back to inference and unification
    [else
     (let ([result (infer expr env)])
       (if (eq? (car result) 'ok)
           (let* ([inferred (cadr result)]
                  [s1 (caddr result)]
                  [unify-result (unify-with s1 inferred expected)])
             (if (eq? (car unify-result) 'ok)
                 `(ok ,(cadr unify-result))
                 unify-result))
           result))]))

;;; ============================================================
;;; Primitive Type Inference
;;; ============================================================

;;; Type signatures for primitives
(define prim-types
  `((add . (-> Int Int Int))
    (sub . (-> Int Int Int))
    (mul . (-> Int Int Int))
    (div . (-> Int Int Int))
    (mod . (-> Int Int Int))
    (neg . (-> Int Int))
    (abs . (-> Int Int))
    (eq? . (∀ (a) (-> a a Bool)))
    (lt? . (-> Int Int Bool))
    (le? . (-> Int Int Bool))
    (gt? . (-> Int Int Bool))
    (ge? . (-> Int Int Bool))
    (zero? . (-> Int Bool))
    (positive? . (-> Int Bool))
    (negative? . (-> Int Bool))
    (not . (-> Bool Bool))
    (cons . (∀ (a) (-> a (List a) (List a))))
    (car . (∀ (a) (-> (List a) a)))
    (cdr . (∀ (a) (-> (List a) (List a))))
    (null? . (∀ (a) (-> (List a) Bool)))
    (pair? . (∀ (a) (-> a Bool)))
    (length . (∀ (a) (-> (List a) Int)))
    (number? . (∀ (a) (-> a Bool)))
    (symbol? . (∀ (a) (-> a Bool)))
    (string? . (∀ (a) (-> a Bool)))))

(define (lookup-prim-type op)
  (let ([entry (assq op prim-types)])
    (if entry (cdr entry) #f)))

(define (infer-prim op args env)
  (let ([prim-type (lookup-prim-type op)])
    (if prim-type
        (let ([inst-type (instantiate prim-type)])
          (infer-app-args inst-type args empty-subst env))
        `(error unknown-primitive ,op))))

;;; ============================================================
;;; Quoted Literals
;;; ============================================================

(define (infer-quoted datum)
  (cond
    [(symbol? datum) `(ok Symbol ,empty-subst)]
    [(number? datum) `(ok Int ,empty-subst)]
    [(string? datum) `(ok String ,empty-subst)]
    [(null? datum) `(ok (List ?) ,empty-subst)]
    [(pair? datum)
     ;; List of things — try to infer element type
     (let ([elem-result (infer-quoted (car datum))])
       (if (eq? (car elem-result) 'ok)
           `(ok (List ,(cadr elem-result)) ,empty-subst)
           `(ok (List ?) ,empty-subst)))]
    [else `(ok ? ,empty-subst)]))

;;; ============================================================
;;; Special Forms
;;; ============================================================

(define (special-form? s)
  (and (symbol? s)
       (memq s '(fn let fix if case prim quote :))))

;;; ============================================================
;;; Convenience API
;;; ============================================================

;;; typeof : Expr → Type | Error
;;; Infer the type of an expression in the empty environment.
(define (typeof expr)
  (reset-fresh!)
  (let ([result (infer expr empty-tenv)])
    (if (eq? (car result) 'ok)
        (let ([type (cadr result)]
              [s (caddr result)])
          (generalize '() (apply-subst s type)))
        result)))

;;; typecheck : Expr × Type → Bool | Error
;;; Check that an expression has the given type.
(define (typecheck expr type)
  (reset-fresh!)
  (let ([result (check expr type empty-tenv)])
    (if (eq? (car result) 'ok)
        #t
        result)))

;;; ============================================================
;;; Pretty Error Messages
;;; ============================================================

(define (format-type-error err)
  (case (cadr err)
    [(unbound-variable)
     (format "Unbound variable: ~a" (caddr err))]
    [(type-mismatch)
     (format "Type mismatch: expected ~a, got ~a"
             (type->string (caddr err))
             (type->string (cadddr err)))]
    [(arity-mismatch)
     (format "Arity mismatch: ~a" (cddr err))]
    [(occurs-check)
     (format "Infinite type: ~a occurs in ~a"
             (caddr err) (type->string (cadddr err)))]
    [(not-a-function)
     (format "Not a function: ~a" (type->string (caddr err)))]
    [(unknown-primitive)
     (format "Unknown primitive: ~a" (caddr err))]
    [else (format "~s" err)]))
