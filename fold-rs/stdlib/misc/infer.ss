; ============================================================
; Bidirectional Type Inference for The Fold
;
; Types flow in two directions:
;   - Inference (up): Expression -> Type (synthesize a type)
;   - Checking (down): Expression x Type -> Bool (verify against expected)
;
; The key insight: some forms synthesize, some check.
;   - Variables, applications, annotations: synthesize
;   - Lambdas, let-bodies: check against expected type
;
; Based on "Complete and Easy Bidirectional Typechecking for
; Higher-Rank Polymorphism" (Dunfield & Krishnaswami, 2013)
;
; Depends on: types.ss, kinds.ss
; ============================================================

; ============================================================
; Type Environment
; ============================================================

; A type environment maps term variables to types.
; (List (Pair Symbol Type))

(empty-tenv '())

; tenv-lookup : TEnv x Symbol -> Type | #f
(tenv-lookup (fn (env var)
                 (let ((entry (assoc var env)))
                      (if entry (cdr entry) #f))))

; tenv-extend : TEnv x Symbol x Type -> TEnv
(tenv-extend (fn (env var type)
                 (cons (cons var type) env)))

; tenv-extend* : TEnv x (List (Pair Symbol Type)) -> TEnv
(tenv-extend* (fn (env bindings)
                  (append bindings env)))

; ============================================================
; Fresh Type Variables (Functional State)
; ============================================================

; Instead of mutation, we thread a counter through.
; State is represented as: (counter . value)

; make-infer-state : Nat -> InferState
(make-infer-state (fn (counter) counter))

; fresh-tvar : InferState -> (InferState x Symbol)
; Generate a fresh type variable, returning new state and the variable
(fresh-tvar (fn (state)
                (let ((new-counter (+ state 1))
                      (var-name (string->symbol (string-append "t" (number->string state)))))
                     (cons new-counter var-name))))

; fresh-tvar-named : InferState x String -> (InferState x Symbol)
(fresh-tvar-named (fn (state prefix)
                      (let ((new-counter (+ state 1))
                            (var-name (string->symbol (string-append prefix (number->string state)))))
                           (cons new-counter var-name))))

; ============================================================
; Substitution
; ============================================================

; A substitution maps type variables to types.
(empty-subst '())

; subst-lookup : Subst x Symbol -> Type | #f
(subst-lookup (fn (s var)
                  (let ((entry (assoc var s)))
                       (if entry (cdr entry) #f))))

; subst-extend : Subst x Symbol x Type -> Subst
(subst-extend (fn (s var type)
                  (cons (cons var type) s)))

; apply-subst : Subst x Type -> Type
; Apply a substitution to a type, chasing chains
(apply-subst (fix apply-subst
                  (fn (s type)
                      (if (type-var? type)
                          (let ((replacement (subst-lookup s type)))
                               (if replacement
                                   (apply-subst s replacement)
                                   type))
                          (if (base-type? type) type
                              (if (hole? type) type
                                  (if (not (pair? type)) type
                                      ; Don't substitute bound variables
                                      (if (eq? (car type) 'forall)
                                          (let ((bound (cadr type))
                                                (body (caddr type)))
                                               ; Remove bound vars from substitution
                                               (let ((filtered-s (filter (fn (pair)
                                                                             (not (memq (car pair) bound)))
                                                                         s)))
                                                    (list 'forall bound (apply-subst filtered-s body))))
                                          (if (eq? (car type) 'mu)
                                              (let ((bound (cadr type))
                                                    (body (caddr type)))
                                                   (let ((filtered-s (filter (fn (pair)
                                                                                 (not (eq? (car pair) bound)))
                                                                             s)))
                                                        (list 'mu bound (apply-subst filtered-s body))))
                                              ; Recurse into compound types
                                              (map (fn (t) (apply-subst s t)) type))))))))))

; compose-subst : Subst x Subst -> Subst
; Compose two substitutions (s2 after s1)
(compose-subst (fn (s1 s2)
                   (let ((applied-s1 (map (fn (pair)
                                              (cons (car pair) (apply-subst s2 (cdr pair))))
                                          s1)))
                        (append applied-s1 s2))))

; ============================================================
; Unification
; ============================================================

; unify : Type x Type -> Subst | (error ...)
; Find a substitution that makes two types equal
(unify (fix unify
            (fn (t1 t2)
                (if (equal? t1 t2)
                    empty-subst
                    (if (type-var? t1)
                        (if (occurs-in? t1 t2)
                            (list 'error 'occurs-check t1 t2)
                            (subst-extend empty-subst t1 t2))
                        (if (type-var? t2)
                            (if (occurs-in? t2 t1)
                                (list 'error 'occurs-check t2 t1)
                                (subst-extend empty-subst t2 t1))
                            (if (hole? t1) (subst-extend empty-subst t1 t2)
                                (if (hole? t2) (subst-extend empty-subst t2 t1)
                                    (if (not (pair? t1))
                                        (list 'error 'type-mismatch t1 t2)
                                        (if (not (pair? t2))
                                            (list 'error 'type-mismatch t1 t2)
                                            (if (not (= (length t1) (length t2)))
                                                (list 'error 'type-mismatch t1 t2)
                                                (unify-lists t1 t2))))))))))))

; unify-lists : (List Type) x (List Type) -> Subst | (error ...)
; Unify corresponding elements of two lists
(unify-lists (fix unify-lists
                  (fn (ts1 ts2)
                      (if (null? ts1)
                          empty-subst
                          (let ((s1 (unify (car ts1) (car ts2))))
                               (if (error-result? s1)
                                   s1
                                   (let ((rest-s (unify-lists
                                                  (map (fn (t) (apply-subst s1 t)) (cdr ts1))
                                                  (map (fn (t) (apply-subst s1 t)) (cdr ts2)))))
                                        (if (error-result? rest-s)
                                            rest-s
                                            (compose-subst s1 rest-s)))))))))

; error-result? : Any -> Boolean
(error-result? (fn (x)
                   (if (pair? x) (eq? (car x) 'error) #f)))

; occurs-in? : Symbol x Type -> Boolean
; Check if a type variable occurs in a type (for occurs check)
(occurs-in? (fix occurs-in?
                 (fn (var type)
                     (if (eq? var type) #t
                         (if (type-var? type) #f
                             (if (not (pair? type)) #f
                                 (any (fn (t) (occurs-in? var t)) type)))))))

; ============================================================
; Type Inference (Synthesis)
; ============================================================

; InferResult = (state . Type) | (error ...)

; infer : TEnv x Expr x InferState -> InferResult
; Synthesize a type for an expression
(infer (fix infer
            (fn (env expr state)
                ; Number literal
                (if (number? expr)
                    (cons state 'Int)
                    ; Boolean literal
                    (if (boolean? expr)
                        (cons state 'Bool)
                        ; String literal
                        (if (string? expr)
                            (cons state 'String)
                            ; Symbol literal (quoted)
                            (if (symbol? expr)
                                (let ((looked-up (tenv-lookup env expr)))
                                     (if looked-up
                                         (cons state looked-up)
                                         (list 'error 'unbound-variable expr)))
                                ; Not a pair - unknown
                                (if (not (pair? expr))
                                    (list 'error 'cannot-infer-type expr)
                                    ; Quote
                                    (if (eq? (car expr) 'quote)
                                        (if (symbol? (cadr expr))
                                            (cons state 'Symbol)
                                            (cons state (t-list '?)))
                                        ; Lambda: (fn (params) body)
                                        (if (eq? (car expr) 'fn)
                                            (let ((params (cadr expr))
                                                  (body (caddr expr)))
                                                 ; Generate fresh type vars for params
                                                 (let ((param-state (foldl
                                                                     (fn (acc p)
                                                                         (let ((fresh-result (fresh-tvar (car acc))))
                                                                              (cons (car fresh-result)
                                                                                    (cons (cons p (cdr fresh-result))
                                                                                          (cdr acc)))))
                                                                     (cons state '())
                                                                     params)))
                                                      (let ((new-state (car param-state))
                                                            (param-types (reverse (cdr param-state))))
                                                           (let ((extended-env (tenv-extend* env param-types)))
                                                                (let ((body-result (infer extended-env body new-state)))
                                                                     (if (error-result? body-result)
                                                                         body-result
                                                                         (cons (car body-result)
                                                                               (cons '-> (append (map cdr param-types)
                                                                                                 (list (cdr body-result)))))))))))
                                            ; Let: (let ((var val)) body)
                                            (if (eq? (car expr) 'let)
                                                (let ((bindings (cadr expr))
                                                      (body (caddr expr)))
                                                     ; Infer types for bindings
                                                     (let ((binding-result
                                                            (foldl
                                                             (fn (acc binding)
                                                                 (if (error-result? acc)
                                                                     acc
                                                                     (let ((var (car binding))
                                                                           (val (cadr binding)))
                                                                          (let ((val-result (infer (cdr (cdr acc)) val (car acc))))
                                                                               (if (error-result? val-result)
                                                                                   val-result
                                                                                   (cons (car val-result)
                                                                                         (cons (cons (cons var (cdr val-result))
                                                                                                     (car (cdr acc)))
                                                                                               (tenv-extend (cdr (cdr acc)) var (cdr val-result)))))))))
                                                             (cons state (cons '() env))
                                                             bindings)))
                                                          (if (error-result? binding-result)
                                                              binding-result
                                                              (infer (cdr (cdr binding-result)) body (car binding-result)))))
                                                ; Application: (f args...)
                                                (let ((func-result (infer env (car expr) state)))
                                                     (if (error-result? func-result)
                                                         func-result
                                                         (let ((func-type (cdr func-result))
                                                               (func-state (car func-result)))
                                                              (if (function-type? func-type)
                                                                  (let ((param-types (function-param-types func-type))
                                                                        (return-type (function-return-type func-type)))
                                                                       (if (= (length (cdr expr)) (length param-types))
                                                                           ; Infer and unify arg types
                                                                           (let ((args-result
                                                                                  (foldl
                                                                                   (fn (acc arg-param)
                                                                                       (if (error-result? acc)
                                                                                           acc
                                                                                           (let ((arg-result (infer env (car arg-param) (car acc))))
                                                                                                (if (error-result? arg-result)
                                                                                                    arg-result
                                                                                                    (let ((unify-result (unify (cdr arg-result) (cdr arg-param))))
                                                                                                         (if (error-result? unify-result)
                                                                                                             unify-result
                                                                                                             (cons (car arg-result)
                                                                                                                   (compose-subst (cdr acc) unify-result))))))))
                                                                                   (cons func-state empty-subst)
                                                                                   (zip (cdr expr) param-types))))
                                                                                (if (error-result? args-result)
                                                                                    args-result
                                                                                    (cons (car args-result)
                                                                                          (apply-subst (cdr args-result) return-type))))
                                                                           (list 'error 'arity-mismatch
                                                                                 (length (cdr expr))
                                                                                 (length param-types))))
                                                                  (list 'error 'not-a-function func-type))))))))))))))))

; ============================================================
; Type Checking (Analysis)
; ============================================================

; check : TEnv x Expr x Type x InferState -> InferState | (error ...)
; Check that an expression has a given type
(check (fix check
            (fn (env expr expected state)
                ; Infer the type and unify with expected
                (let ((inferred (infer env expr state)))
                     (if (error-result? inferred)
                         inferred
                         (let ((unify-result (unify (cdr inferred) expected)))
                              (if (error-result? unify-result)
                                  (list 'error 'type-mismatch (cdr inferred) expected)
                                  (car inferred))))))))

; ============================================================
; Top-Level Interface
; ============================================================

; typeof : Expr -> Type | (error ...)
; Infer the type of an expression in an empty environment
(typeof (fn (expr)
            (let ((result (infer empty-tenv expr (make-infer-state 0))))
                 (if (error-result? result)
                     result
                     (cdr result)))))

; typecheck : Expr x Type -> Boolean
; Check if an expression has a given type
(typecheck (fn (expr expected)
               (let ((result (check empty-tenv expr expected (make-infer-state 0))))
                    (not (error-result? result)))))

; ============================================================
; Pretty Printing Errors
; ============================================================

; format-type-error : (error ...) -> String
(format-type-error (fn (err)
                       (if (not (error-result? err))
                           "not an error"
                           (let ((kind (cadr err)))
                                (if (eq? kind 'unbound-variable)
                                    (string-append "Unbound variable: " (symbol->string (caddr err)))
                                    (if (eq? kind 'type-mismatch)
                                        (string-append "Type mismatch: expected "
                                                       (type->string (caddr err))
                                                       " but got "
                                                       (type->string (cadddr err)))
                                        (if (eq? kind 'not-a-function)
                                            (string-append "Not a function: " (type->string (caddr err)))
                                            (if (eq? kind 'arity-mismatch)
                                                (string-append "Arity mismatch: got "
                                                               (number->string (caddr err))
                                                               " args, expected "
                                                               (number->string (cadddr err)))
                                                (if (eq? kind 'occurs-check)
                                                    "Infinite type (occurs check failed)"
                                                    (string-append "Type error: " (symbol->string kind)))))))))))

; ============================================================
; Module Exports
; (see exports.ss for exported symbols)
; ============================================================
