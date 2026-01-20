(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/kinds.ss")
(load "core/types/infer.ss")

(doc 'module 'annotate)
(doc 'description "Type-Annotated AST. Every expression annotated with its type. Enables type-guided evaluation, pretty-printing with types, type-preserving transformations, and IDE features.")
(doc 'layer 'core)
(doc 'note "Annotated expressions have the form: (ann Type Expr) where Expr may contain nested ann forms.")

(doc 'section 'annotated-ast)

(define (ann type expr)
  (doc 'type (-> Type Expr AnnotatedExpr))
  (doc 'description "Construct an annotated expression: (ann Type Expr).")
  (doc 'export #t)
  `(ann ,type ,expr))

(define (ann? e)
  (doc 'type (-> Any Boolean))
  (doc 'description "Test if value is an annotated expression.")
  (doc 'export #t)
  (and (pair? e) (eq? (car e) 'ann)))

(define (ann-type e)
  (doc 'type (-> AnnotatedExpr Type))
  (doc 'description "Extract the type from an annotated expression.")
  (doc 'export #t)
  (if (ann? e) (cadr e) '?))

(define (ann-expr e)
  (doc 'type (-> AnnotatedExpr Expr))
  (doc 'description "Extract the inner expression from an annotated expression.")
  (doc 'export #t)
  (if (ann? e) (caddr e) e))

(doc 'section 'strip-annotations)

(define (strip-ann e)
  (doc 'type (-> AnnotatedExpr Expr))
  (doc 'description "Remove all type annotations, recovering the original expression.")
  (doc 'export #t)
  (cond
   [(ann? e) (strip-ann (ann-expr e))]
   [(not (pair? e)) e]
   [else (map strip-ann e)]))

(doc 'section 'annotation-inference)

(define (annotate expr env)
  (doc 'type (-> Expr TEnv (Result (Tuple AnnotatedExpr Subst) Error)))
  (doc 'description "Run type inference and produce an annotated expression.")
  (doc 'export #t)
  (annotate-with expr env empty-subst))

(define (annotate-with expr env subst)
  (doc 'type (-> Expr TEnv Subst (Result (Tuple AnnotatedExpr Subst) Error)))
  (doc 'description "Annotate expression with types using given environment and substitution.")
  (cond
   ;; Literals
   [(number? expr)
    (let ([t (if (integer? expr) 'Int 'Nat)])
         `(ok ,(ann t expr) ,subst))]
   
   [(boolean? expr)
    `(ok ,(ann 'Bool expr) ,subst)]
   
   [(string? expr)
    `(ok ,(ann 'String expr) ,subst)]
   
   ;; Quote
   [(and (pair? expr) (eq? (car expr) 'quote))
    (let ([result (infer-quoted (cadr expr))])
         (if (eq? (car result) 'ok)
             `(ok ,(ann (cadr result) expr) ,subst)
             result))]
   
   ;; Variable
   [(symbol? expr)
    (let ([type (tenv-lookup env expr)])
         (if type
             (let ([inst-type (instantiate type)])
                  `(ok ,(ann (apply-subst subst inst-type) expr) ,subst))
             `(error unbound-variable ,expr)))]
   
   [(not (pair? expr))
    `(error unknown-expression ,expr)]
   
   ;; Type annotation: (: expr type)
   [(eq? (car expr) ':)
    (let* ([e (cadr expr)]
           [annot-type (caddr expr)]
           [result (annotate-check e annot-type env subst)])
          (if (eq? (car result) 'ok)
              (let ([ann-e (cadr result)]
                    [s (caddr result)])
                   `(ok ,(ann annot-type (list ': ann-e annot-type)) ,s))
              result))]
   
   ;; Lambda: (fn (params...) body)
   [(eq? (car expr) 'fn)
    (annotate-fn (cadr expr) (caddr expr) env subst)]
   
   ;; Let: (let ((x e) ...) body)
   [(eq? (car expr) 'let)
    (annotate-let (cadr expr) (caddr expr) env subst)]
   
   ;; Fix: (fix name (fn ...))
   [(eq? (car expr) 'fix)
    (annotate-fix (cadr expr) (caddr expr) env subst)]
   
   ;; If: (if test then else)
   [(eq? (car expr) 'if)
    (annotate-if (cadr expr) (caddr expr) (cadddr expr) env subst)]
   
   ;; Case: (case scrutinee ((Tag vars...) body) ...)
   [(eq? (car expr) 'case)
    (annotate-case (cadr expr) (cddr expr) env subst)]
   
   ;; Prim: (prim 'op args...)
   [(eq? (car expr) 'prim)
    (annotate-prim (cadr expr) (cddr expr) env subst)]
   
   ;; Application: (f args...)
   [(not (special-form? (car expr)))
    (annotate-app (car expr) (cdr expr) env subst)]
   
   [else `(error unsupported-expression ,expr)]))

(doc 'section 'lambda-annotation)

(define (annotate-fn params body env subst)
  (doc 'type (-> (List Symbol) Expr TEnv Subst (Result (Tuple AnnotatedExpr Subst) Error)))
  (doc 'description "Annotate a lambda expression with inferred types.")
  (let* ([param-types (map (lambda (_) (fresh-tvar)) params)]
         [new-env (tenv-extend* env (map cons params param-types))]
         [result (annotate-with body new-env subst)])
        (if (eq? (car result) 'ok)
            (let* ([ann-body (cadr result)]
                   [s (caddr result)]
                   [body-type (ann-type ann-body)]
                   [fn-type (apply-subst s
                                         (cons '-> (append param-types (list body-type))))]
                   ;; Annotate parameters with their types
                   [ann-params (map (lambda (p t)
                                            (ann (apply-subst s t) p))
                                    params param-types)])
                  `(ok ,(ann fn-type `(fn ,ann-params ,ann-body)) ,s))
            result)))

(doc 'section 'let-annotation)

(define (annotate-let bindings body env subst)
  (doc 'type (-> (List (Tuple Symbol Expr)) Expr TEnv Subst (Result (Tuple AnnotatedExpr Subst) Error)))
  (doc 'description "Annotate a let expression with inferred types.")
  (annotate-let-bindings bindings body env '() subst))

(define (annotate-let-bindings bindings body env ann-bindings subst)
  (doc 'type (-> (List (Tuple Symbol Expr)) Expr TEnv (List (Tuple Symbol AnnotatedExpr)) Subst (Result (Tuple AnnotatedExpr Subst) Error)))
  (doc 'description "Process let bindings one at a time, annotating each.")
  (if (null? bindings)
      ;; All bindings processed, annotate body
      (let* ([new-env (fold-left
                       (lambda (e b)
                               (tenv-extend e (car b) (ann-type (cadr b))))
                       env
                       (reverse ann-bindings))]
             [result (annotate-with body new-env subst)])
            (if (eq? (car result) 'ok)
                (let* ([ann-body (cadr result)]
                       [s (caddr result)]
                       [body-type (ann-type ann-body)]
                       ;; Update binding types with final substitution
                       [final-bindings (map (lambda (b)
                                                    (list (car b)
                                                          (ann (apply-subst s (ann-type (cadr b)))
                                                               (ann-expr (cadr b)))))
                                            (reverse ann-bindings))])
                      `(ok ,(ann body-type `(let ,final-bindings ,ann-body)) ,s))
                result))
      ;; Process next binding
      (let* ([binding (car bindings)]
             [var (car binding)]
             [init (cadr binding)]
             [result (annotate-with init env subst)])
            (if (eq? (car result) 'ok)
                (let* ([ann-init (cadr result)]
                       [s (caddr result)]
                       [init-type (ann-type ann-init)]
                       [gen-type (generalize env (apply-subst s init-type))]
                       [new-env (tenv-extend env var gen-type)])
                      (annotate-let-bindings
                       (cdr bindings) body new-env
                       (cons (list var (ann gen-type (ann-expr ann-init))) ann-bindings)
                       s))
                result))))

(doc 'section 'fix-annotation)

(define (annotate-fix name fn-expr env subst)
  (doc 'type (-> Symbol Expr TEnv Subst (Result (Tuple AnnotatedExpr Subst) Error)))
  (doc 'description "Annotate a recursive fix expression with inferred types.")
  (let* ([fix-type (fresh-tvar)]
         [new-env (tenv-extend env name fix-type)]
         [result (annotate-with fn-expr new-env subst)])
        (if (eq? (car result) 'ok)
            (let* ([ann-fn (cadr result)]
                   [s1 (caddr result)]
                   [fn-type (ann-type ann-fn)]
                   [unify-result (unify-with s1 fix-type fn-type)])
                  (if (eq? (car unify-result) 'ok)
                      (let* ([s2 (cadr unify-result)]
                             [final-type (apply-subst s2 fn-type)])
                            `(ok ,(ann final-type `(fix ,name ,ann-fn)) ,s2))
                      unify-result))
            result)))

(doc 'section 'if-annotation)

(define (annotate-if test then-expr else-expr env subst)
  (doc 'type (-> Expr Expr Expr TEnv Subst (Result (Tuple AnnotatedExpr Subst) Error)))
  (doc 'description "Annotate an if expression, checking test is Bool and unifying branches.")
  (let ([test-result (annotate-with test env subst)])
       (if (not (eq? (car test-result) 'ok))
           test-result
           (let* ([ann-test (cadr test-result)]
                  [s1 (caddr test-result)]
                  ;; Check test is Bool
                  [bool-result (unify-with s1 (ann-type ann-test) 'Bool)])
                 (if (not (eq? (car bool-result) 'ok))
                     `(error if-test-not-bool ,(ann-type ann-test))
                     (let* ([s2 (cadr bool-result)]
                            [then-result (annotate-with then-expr env s2)])
                           (if (not (eq? (car then-result) 'ok))
                               then-result
                               (let* ([ann-then (cadr then-result)]
                                      [s3 (caddr then-result)]
                                      [else-result (annotate-with else-expr env s3)])
                                     (if (not (eq? (car else-result) 'ok))
                                         else-result
                                         (let* ([ann-else (cadr else-result)]
                                                [s4 (caddr else-result)]
                                                ;; Unify branches
                                                [branch-result (unify-with s4
                                                                           (ann-type ann-then)
                                                                           (ann-type ann-else))])
                                               (if (eq? (car branch-result) 'ok)
                                                   (let* ([s5 (cadr branch-result)]
                                                          [result-type (apply-subst s5 (ann-type ann-then))])
                                                         `(ok ,(ann result-type
                                                                    `(if ,ann-test ,ann-then ,ann-else))
                                                           ,s5))
                                                   branch-result)))))))))))

(doc 'section 'case-annotation)

(define (annotate-case scrutinee clauses env subst)
  (doc 'type (-> Expr (List Clause) TEnv Subst (Result (Tuple AnnotatedExpr Subst) Error)))
  (doc 'description "Annotate a case expression. Pattern variables are bound to Hash type.")
  (let ([scrut-result (annotate-with scrutinee env subst)])
       (if (not (eq? (car scrut-result) 'ok))
           scrut-result
           (let* ([ann-scrut (cadr scrut-result)]
                  [s1 (caddr scrut-result)])
                 (annotate-case-clauses ann-scrut clauses s1 env '() #f)))))

(define (annotate-case-clauses ann-scrut clauses subst env ann-clauses result-type)
  (doc 'type (-> AnnotatedExpr (List Clause) Subst TEnv (List Clause) (Maybe Type) (Result (Tuple AnnotatedExpr Subst) Error)))
  (doc 'description "Process case clauses, unifying their result types.")
  (if (null? clauses)
      (if result-type
          `(ok ,(ann (apply-subst subst result-type)
                     `(case ,ann-scrut ,@(reverse ann-clauses)))
            ,subst)
          `(error no-clauses-in-case))
      (let* ([clause (car clauses)]
             [pattern (car clause)]
             [body (cadr clause)]
             [tag (car pattern)]
             [vars (cdr pattern)]
             ;; Each bound var gets Hash type (block refs)
             [var-types (map (lambda (_) 'Hash) vars)]
             [clause-env (tenv-extend* env (map cons vars var-types))]
             [body-result (annotate-with body clause-env subst)])
            (if (not (eq? (car body-result) 'ok))
                body-result
                (let* ([ann-body (cadr body-result)]
                       [s2 (caddr body-result)]
                       [body-type (ann-type ann-body)]
                       ;; Annotate pattern variables
                       [ann-vars (map (lambda (v) (ann 'Hash v)) vars)]
                       [ann-pattern (cons tag ann-vars)]
                       [ann-clause (list ann-pattern ann-body)])
                      (if result-type
                          ;; Unify with previous clause result type
                          (let ([unify-result (unify-with s2 body-type result-type)])
                               (if (eq? (car unify-result) 'ok)
                                   (annotate-case-clauses
                                    ann-scrut
                                    (cdr clauses)
                                    (cadr unify-result)
                                    env
                                    (cons ann-clause ann-clauses)
                                    (apply-subst (cadr unify-result) body-type))
                                   unify-result))
                          ;; First clause establishes result type
                          (annotate-case-clauses
                           ann-scrut (cdr clauses) s2 env
                           (cons ann-clause ann-clauses) body-type)))))))

(doc 'section 'prim-annotation)

(define (annotate-prim op args env subst)
  (doc 'type (-> (Either Symbol (List Any)) (List Expr) TEnv Subst (Result (Tuple AnnotatedExpr Subst) Error)))
  (doc 'description "Annotate a primitive operation with inferred types.")
  (let ([op-sym (if (and (pair? op) (eq? (car op) 'quote))
                    (cadr op)
                    op)])
       (let ([prim-type (lookup-prim-type op-sym)])
            (if (not prim-type)
                `(error unknown-primitive ,op-sym)
                (let ([inst-type (instantiate prim-type)])
                     ;; Pass op-sym (the bare symbol) to avoid double-quoting
                     (annotate-prim-args op-sym inst-type args env '() subst))))))

(define (annotate-prim-args op fn-type remaining env ann-args subst)
  (doc 'type (-> Symbol Type (List Expr) TEnv (List AnnotatedExpr) Subst (Result (Tuple AnnotatedExpr Subst) Error)))
  (doc 'description "Process primitive arguments one at a time, type-checking each.")
  (if (null? remaining)
      ;; All args processed
      (let ([result-type (if (function-type? fn-type)
                             (function-return-type fn-type)
                             fn-type)])
           `(ok ,(ann (apply-subst subst result-type)
                      `(prim ',op ,@(reverse ann-args)))
             ,subst))
      ;; Process next arg
      (if (not (function-type? fn-type))
          `(error prim-arity-mismatch ,op)
          (let* ([param-types (function-param-types fn-type)]
                 [return-type (function-return-type fn-type)]
                 [expected-type (if (null? param-types) '? (car param-types))]
                 [result (annotate-with (car remaining) env subst)])
                (if (not (eq? (car result) 'ok))
                    result
                    (let* ([ann-arg (cadr result)]
                           [s1 (caddr result)]
                           [unify-result (unify-with s1
                                                     (ann-type ann-arg)
                                                     (apply-subst s1 expected-type))])
                          (if (not (eq? (car unify-result) 'ok))
                              unify-result
                              (let* ([s2 (cadr unify-result)]
                                     ;; Build remaining function type
                                     [new-fn-type (if (null? (cdr param-types))
                                                      return-type
                                                      (cons '->
                                                            (append (cdr param-types)
                                                                    (list return-type))))])
                                    (annotate-prim-args
                                     op new-fn-type (cdr remaining) env
                                     (cons ann-arg ann-args)
                                     s2)))))))))

(doc 'section 'application-annotation)

(define (annotate-app fn args env subst)
  (doc 'type (-> Expr (List Expr) TEnv Subst (Result (Tuple AnnotatedExpr Subst) Error)))
  (doc 'description "Annotate a function application with inferred types.")
  (let ([fn-result (annotate-with fn env subst)])
       (if (not (eq? (car fn-result) 'ok))
           fn-result
           (let* ([ann-fn (cadr fn-result)]
                  [s1 (caddr fn-result)]
                  [fn-type (apply-subst s1 (ann-type ann-fn))])
                 (annotate-app-args ann-fn fn-type args env '() s1)))))

(define (annotate-app-args ann-fn fn-type remaining env ann-args subst)
  (doc 'type (-> AnnotatedExpr Type (List Expr) TEnv (List AnnotatedExpr) Subst (Result (Tuple AnnotatedExpr Subst) Error)))
  (doc 'description "Process application arguments one at a time, type-checking each.")
  (if (null? remaining)
      ;; All args processed
      (let ([result-type (if (function-type? fn-type)
                             (function-return-type fn-type)
                             fn-type)])
           `(ok ,(ann (apply-subst subst result-type)
                      `(,ann-fn ,@(reverse ann-args)))
             ,subst))
      ;; Process next arg
      (cond
       ;; Function type — check arg against param
       [(function-type? fn-type)
        (let* ([param-types (function-param-types fn-type)]
               [return-type (function-return-type fn-type)]
               [expected-type (if (null? param-types) '? (car param-types))]
               [result (annotate-with (car remaining) env subst)])
              (if (not (eq? (car result) 'ok))
                  result
                  (let* ([ann-arg (cadr result)]
                         [s1 (caddr result)]
                         [unify-result (unify-with s1
                                                   (ann-type ann-arg)
                                                   (apply-subst s1 expected-type))])
                        (if (not (eq? (car unify-result) 'ok))
                            unify-result
                            (let* ([s2 (cadr unify-result)]
                                   [new-fn-type (if (null? (cdr param-types))
                                                    return-type
                                                    (cons '->
                                                          (append (cdr param-types)
                                                                  (list return-type))))])
                                  (annotate-app-args
                                   ann-fn new-fn-type (cdr remaining) env
                                   (cons ann-arg ann-args)
                                   s2))))))]
       
       ;; Type variable — create function type
       [(type-var? fn-type)
        (let* ([arg-types (map (lambda (_) (fresh-tvar)) remaining)]
               [ret-type (fresh-tvar)]
               [expected-fn-type (cons '-> (append arg-types (list ret-type)))]
               [unify-result (unify-with subst fn-type expected-fn-type)])
              (if (eq? (car unify-result) 'ok)
                  (let ([s2 (cadr unify-result)])
                       (annotate-app-args
                        ann-fn (apply-subst s2 expected-fn-type) remaining env
                        ann-args s2))
                  unify-result))]
       
       [else `(error not-a-function ,fn-type)])))

(doc 'section 'check-mode-annotation)

(define (annotate-check expr expected env subst)
  (doc 'type (-> Expr Type TEnv Subst (Result (Tuple AnnotatedExpr Subst) Error)))
  (doc 'description "Annotate expression in checking mode against expected type.")
  (cond
   ;; Lambda against function type
   [(and (pair? expr) (eq? (car expr) 'fn) (function-type? expected))
    (let* ([params (cadr expr)]
           [body (caddr expr)]
           [param-types (function-param-types expected)]
           [return-type (function-return-type expected)])
          (if (not (= (length params) (length param-types)))
              `(error arity-mismatch (expected ,(length param-types))
                (got ,(length params)))
              (let* ([new-env (tenv-extend* env (map cons params param-types))]
                     [result (annotate-check body return-type new-env subst)])
                    (if (eq? (car result) 'ok)
                        (let* ([ann-body (cadr result)]
                               [s (caddr result)]
                               [ann-params (map (lambda (p t) (ann t p))
                                                params param-types)])
                              `(ok ,(ann expected `(fn ,ann-params ,ann-body)) ,s))
                        result))))]
   
   ;; Default: infer and unify
   [else
    (let ([result (annotate-with expr env subst)])
         (if (eq? (car result) 'ok)
             (let* ([ann-e (cadr result)]
                    [s1 (caddr result)]
                    [unify-result (unify-with s1 (ann-type ann-e) expected)])
                   (if (eq? (car unify-result) 'ok)
                       `(ok ,ann-e ,(cadr unify-result))
                       unify-result))
             result))]))

(doc 'section 'convenience-api)

(define (annotate-expr expr)
  (doc 'type (-> Expr (Either AnnotatedExpr Error)))
  (doc 'description "Annotate an expression with types. Main entry point.")
  (doc 'export #t)
  (reset-fresh!)
  (let ([result (annotate expr empty-tenv)])
       (if (eq? (car result) 'ok)
           (let* ([ann-e (cadr result)]
                  [s (caddr result)])
                 (finalize-ann ann-e s))
           result)))

(define (finalize-ann ann-e subst)
  (doc 'type (-> AnnotatedExpr Subst AnnotatedExpr))
  (doc 'description "Apply final substitution to all types in an annotated expression.")
  (cond
   [(ann? ann-e)
    (ann (apply-subst subst (ann-type ann-e))
         (finalize-ann-inner (ann-expr ann-e) subst))]
   [else ann-e]))

(define (finalize-ann-inner e subst)
  (doc 'type (-> Any Subst Any))
  (doc 'description "Recursively apply substitution to nested annotated expressions.")
  (cond
   [(ann? e) (finalize-ann e subst)]
   [(not (pair? e)) e]
   [else (map (lambda (x) (finalize-ann-inner x subst)) e)]))

(doc 'section 'pretty-print)

(define (pp-ann ann-e)
  (doc 'type (-> AnnotatedExpr String))
  (doc 'description "Pretty-print annotated code showing types.")
  (doc 'export #t)
  (pp-ann-indent ann-e 0))

(define (pp-ann-indent ann-e indent)
  (doc 'type (-> AnnotatedExpr Nat String))
  (doc 'description "Pretty-print with given indentation level.")
  (let ([ind (make-string indent #\space)])
       (if (ann? ann-e)
           (let ([type (ann-type ann-e)]
                 [expr (ann-expr ann-e)])
                (cond
                 ;; Simple values — inline
                 [(or (number? expr) (boolean? expr) (string? expr) (symbol? expr))
                  (string-append (format "~a" expr) " : " (type->string type))]
                 
                 ;; Quote — inline
                 [(and (pair? expr) (eq? (car expr) 'quote))
                  (string-append (format "~s" expr) " : " (type->string type))]
                 
                 ;; Lambda — multi-line
                 [(and (pair? expr) (eq? (car expr) 'fn))
                  (let* ([params (cadr expr)]
                         [body (caddr expr)]
                         [param-strs (map (lambda (p)
                                                  (if (ann? p)
                                                      (format "(~a : ~a)"
                                                              (ann-expr p)
                                                              (type->string (ann-type p)))
                                                      (format "~a" p)))
                                          params)])
                        (string-append
                         "(fn (" (join-strings " " param-strs) ")
"
                         ind "  " (pp-ann-indent body (+ indent 2)) ")
"
                         ind ": " (type->string type)))]
                 
                 ;; Let — multi-line
                 [(and (pair? expr) (eq? (car expr) 'let))
                  (let* ([bindings (cadr expr)]
                         [body (caddr expr)]
                         [binding-strs (map (lambda (b)
                                                    (format "  (~a ~a)"
                                                            (car b)
                                                            (pp-ann-indent (cadr b) (+ indent 4))))
                                            bindings)])
                        (string-append
                         "(let (
" ind
                         (join-strings (string-append "
" ind) binding-strs)
                         ")
" ind "  "
                         (pp-ann-indent body (+ indent 2)) ")
"
                         ind ": " (type->string type)))]
                 
                 ;; If — multi-line
                 [(and (pair? expr) (eq? (car expr) 'if))
                  (string-append
                   "(if " (pp-ann-indent (cadr expr) (+ indent 4)) "
"
                   ind "    " (pp-ann-indent (caddr expr) (+ indent 4)) "
"
                   ind "    " (pp-ann-indent (cadddr expr) (+ indent 4)) ")
"
                   ind ": " (type->string type))]
                 
                 ;; Case — multi-line
                 [(and (pair? expr) (eq? (car expr) 'case))
                  (let* ([scrutinee (cadr expr)]
                         [clauses (cddr expr)]
                         [clause-strs (map (lambda (c)
                                                   (let* ([pattern (car c)]
                                                          [body (cadr c)]
                                                          [tag (car pattern)]
                                                          [vars (cdr pattern)]
                                                          [var-strs (map (lambda (v)
                                                                                 (if (ann? v)
                                                                                     (format "~a" (ann-expr v))
                                                                                     (format "~a" v)))
                                                                         vars)])
                                                         (string-append
                                                          "  ((" (symbol->string tag)
                                                          (if (null? var-strs) ""
                                                              (string-append " " (join-strings " " var-strs)))
                                                          ")
" ind "    "
                                                          (pp-ann-indent body (+ indent 4)) ")")))
                                           clauses)])
                        (string-append
                         "(case " (pp-ann-indent scrutinee (+ indent 6)) "
" ind
                         (join-strings (string-append "
" ind) clause-strs)
                         ")
" ind ": " (type->string type)))]
                 
                 ;; Prim — inline or short
                 [(and (pair? expr) (eq? (car expr) 'prim))
                  (let ([args (cddr expr)])
                       (string-append
                        "(prim '" (format "~a" (cadr expr)) " "
                        (join-strings " " (map (lambda (a) (pp-ann-indent a 0)) args))
                        ") : " (type->string type)))]
                 
                 ;; Application — varies
                 [(pair? expr)
                  (string-append
                   "(" (join-strings " "
                                     (map (lambda (e) (pp-ann-indent e (+ indent 1))) expr))
                   ") : " (type->string type))]
                 
                 [else (format "~s : ~a" expr (type->string type))]))
           ;; Not annotated
           (format "~s" ann-e))))

(doc 'section 'display)

(define (show-annotated expr)
  (doc 'type (-> Expr Unit))
  (doc 'description "Display an expression with inferred type annotations.")
  (doc 'export #t)
  (let ([result (annotate-expr expr)])
       (if (and (pair? result) (eq? (car result) 'error))
           (begin
            (display "Type error: ")
            (display (format-type-error result))
            (newline))
           (begin
            (display (pp-ann result))
            (newline)))))
