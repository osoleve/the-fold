;;; fabric/stitches/dep-infer.ss — Dependent Type Inference
;;;
;;; Extends bidirectional type inference to handle dependent types.
;;;
;;; Key extensions:
;;;   - Pi type synthesis and checking
;;;   - Sigma type synthesis and checking
;;;   - Universe checking
;;;   - Conversion checking via NbE
;;;   - Dependent application with substitution
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - types.ss
;;;   - dep-types.ss
;;;   - nbe.ss
;;;   - infer.ss (for base inference)

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/dep-types.ss")
(load "core/lang/nbe.ss")
(load "core/types/infer.ss")

;;; ============================================================
;;; Extended Type Context
;;; ============================================================

;;; A dependent type context tracks both type and term variables.
;;; Term variables can appear in types.

;;; ctx-entry: (name . (type . value-opt))
;;; value-opt is either a value or #f if abstract

(define empty-dep-ctx '())

(define (dep-ctx-lookup ctx name)
  (let ([entry (assq name ctx)])
       (if entry
           (cadr entry)  ; The type
           #f)))

(define (dep-ctx-lookup-value ctx name)
  (let ([entry (assq name ctx)])
       (if entry
           (if (pair? (cddr entry))
               (caddr entry)
               #f)
           #f)))

(define (dep-ctx-extend ctx name type)
  (cons (list name type) ctx))

(define (dep-ctx-extend-def ctx name type value)
  (cons (list name type value) ctx))

;;; ============================================================
;;; Dependent Type Synthesis
;;; ============================================================

;;; dep-synth : Expr × Context → (ok Type) | (error ...)
;;; Synthesize a type for an expression in dependent context.
(define (dep-synth expr ctx)
  (cond
   ;; Universe
   [(eq? expr 'Type)
    `(ok (Type 1))]  ; Type₀ : Type₁
   
   [(and (pair? expr) (eq? (car expr) 'Type))
    `(ok (Type ,(+ 1 (cadr expr))))]
   
   ;; Base types synthesize to Type
   [(base-type? expr)
    `(ok Type)]
   
   ;; Literals
   [(integer? expr) `(ok Int)]
   [(boolean? expr) `(ok Bool)]
   [(string? expr) `(ok String)]
   
   ;; Variable
   [(symbol? expr)
    (let ([type (dep-ctx-lookup ctx expr)])
         (if type
             `(ok ,type)
             `(error unbound-variable ,expr)))]
   
   ;; Quote
   [(and (pair? expr) (eq? (car expr) 'quote))
    (dep-synth-quoted (cadr expr))]
   
   [(not (pair? expr))
    `(error unknown-expression ,expr)]
   
   ;; Type annotation: (the A e) or (: e A)
   [(or (eq? (car expr) 'the) (eq? (car expr) (quote :)))
    (let* ([annot-type (if (eq? (car expr) 'the) (cadr expr) (caddr expr))]
           [e (if (eq? (car expr) 'the) (caddr expr) (cadr expr))]
           [type-check (dep-check-type annot-type ctx)]
           [check-result (if (eq? (car type-check) 'ok)
                             (dep-check e annot-type ctx)
                             type-check)])
          (if (eq? (car check-result) 'ok)
              `(ok ,annot-type)
              check-result))]
   
   ;; Pi type formation: (Π ((x : A)) B)
   [(eq? (car expr) 'Π)
    (dep-synth-pi expr ctx)]
   
   ;; Sigma type formation: (Σ ((x : A)) B)
   [(eq? (car expr) 'Σ)
    (dep-synth-sigma expr ctx)]
   
   ;; Lambda with annotation: (fn ((x : A)) body)
   [(and (eq? (car expr) 'fn)
         (pair? (cadr expr))
         (pair? (caadr expr))
         (= (length (caadr expr)) 3))
    (dep-synth-lambda expr ctx)]
   
   ;; Lambda without annotation: infer from expected type
   [(eq? (car expr) 'fn)
    `(error lambda-needs-annotation ,expr)]
   
   ;; Pair: (pair e1 e2)
   [(eq? (car expr) 'pair)
    (dep-synth-pair expr ctx)]
   
   ;; First projection: (fst p)
   [(eq? (car expr) 'fst)
    (dep-synth-fst expr ctx)]
   
   ;; Second projection: (snd p)
   [(eq? (car expr) 'snd)
    (dep-synth-snd expr ctx)]
   
   ;; Arrow type: (-> A B)
   [(eq? (car expr) '->)
    (dep-synth-arrow expr ctx)]
   
   ;; Product type: (× A B)
   [(eq? (car expr) '×)
    (dep-synth-product expr ctx)]
   
   ;; Vec type: (Vec n A)
   [(eq? (car expr) 'Vec)
    (dep-synth-vec expr ctx)]
   
   ;; Matrix type: (Matrix m n A)
   [(eq? (car expr) 'Matrix)
    (dep-synth-matrix expr ctx)]
   
   ;; Application
   [else
    (dep-synth-app expr ctx)]))

;;; ============================================================
;;; Pi Type Synthesis
;;; ============================================================

;;; (Π ((x : A)) B) : Type_{max(l₁, l₂)}
;;; where A : Type_{l₁} and B : Type_{l₂} under x:A
(define (dep-synth-pi expr ctx)
  (let* ([binding (car (cadr expr))]
         [var (car binding)]
         [domain-expr (caddr binding)]
         [codomain-expr (caddr expr)])
        ;; Check domain is a type
        (let ([domain-synth (dep-synth domain-expr ctx)])
             (if (not (eq? (car domain-synth) 'ok))
                 domain-synth
                 (let ([domain-type (cadr domain-synth)])
                      (if (not (universe-type? domain-type))
                          `(error expected-type-got ,domain-type)
                          ;; Check codomain is a type in extended context
                          (let* ([new-ctx (dep-ctx-extend ctx var domain-expr)]
                                 [codomain-synth (dep-synth codomain-expr new-ctx)])
                                (if (not (eq? (car codomain-synth) 'ok))
                                    codomain-synth
                                    (let ([codomain-type (cadr codomain-synth)])
                                         (if (not (universe-type? codomain-type))
                                             `(error expected-type-got ,codomain-type)
                                             ;; Result universe is max of both
                                             `(ok ,(universe-max domain-type codomain-type))))))))))))

;;; ============================================================
;;; Sigma Type Synthesis
;;; ============================================================

(define (dep-synth-sigma expr ctx)
  (let* ([binding (car (cadr expr))]
         [var (car binding)]
         [fst-type-expr (caddr binding)]
         [snd-type-expr (caddr expr)])
        (let ([fst-synth (dep-synth fst-type-expr ctx)])
             (if (not (eq? (car fst-synth) 'ok))
                 fst-synth
                 (let ([fst-type (cadr fst-synth)])
                      (if (not (universe-type? fst-type))
                          `(error expected-type-got ,fst-type)
                          (let* ([new-ctx (dep-ctx-extend ctx var fst-type-expr)]
                                 [snd-synth (dep-synth snd-type-expr new-ctx)])
                                (if (not (eq? (car snd-synth) 'ok))
                                    snd-synth
                                    (let ([snd-type (cadr snd-synth)])
                                         (if (not (universe-type? snd-type))
                                             `(error expected-type-got ,snd-type)
                                             `(ok ,(universe-max fst-type snd-type))))))))))))

;;; ============================================================
;;; Lambda Synthesis
;;; ============================================================

;;; (fn ((x : A)) body) : (Π ((x : A)) B) where body : B
(define (dep-synth-lambda expr ctx)
  (let* ([params (cadr expr)]
         [body (caddr expr)]
         [binding (car params)]
         [var (car binding)]
         [domain-expr (caddr binding)])
        ;; Check domain is a type
        (let ([domain-check (dep-check-type domain-expr ctx)])
             (if (not (eq? (car domain-check) 'ok))
                 domain-check
                 ;; Extend context and synthesize body
                 (let* ([new-ctx (dep-ctx-extend ctx var domain-expr)]
                        [body-synth (dep-synth body new-ctx)])
                       (if (not (eq? (car body-synth) 'ok))
                           body-synth
                           (let ([body-type (cadr body-synth)])
                                `(ok ,(t-pi var domain-expr body-type)))))))))

;;; ============================================================
;;; Pair Synthesis
;;; ============================================================

;;; (pair e1 e2) : (Σ ((x : A)) B) where e1 : A and e2 : B[e1/x]
;;; Note: without annotation, we can't determine the dependency
(define (dep-synth-pair expr ctx)
  (let* ([fst-expr (cadr expr)]
         [snd-expr (caddr expr)]
         [fst-synth (dep-synth fst-expr ctx)])
        (if (not (eq? (car fst-synth) 'ok))
            fst-synth
            (let* ([fst-type (cadr fst-synth)]
                   [snd-synth (dep-synth snd-expr ctx)])
                  (if (not (eq? (car snd-synth) 'ok))
                      snd-synth
                      (let ([snd-type (cadr snd-synth)])
                           ;; Create non-dependent Sigma
                           `(ok ,(t-sigma '_ fst-type snd-type))))))))

;;; ============================================================
;;; Projection Synthesis
;;; ============================================================

;;; (fst p) : A where p : (Σ ((x : A)) B)
(define (dep-synth-fst expr ctx)
  (let* ([p-expr (cadr expr)]
         [p-synth (dep-synth p-expr ctx)])
        (if (not (eq? (car p-synth) 'ok))
            p-synth
            (let ([p-type (cadr p-synth)])
                 (if (not (sigma-type? p-type))
                     `(error expected-sigma-got ,p-type)
                     `(ok ,(sigma-fst-type p-type)))))))

;;; (snd p) : B[fst p/x] where p : (Σ ((x : A)) B)
(define (dep-synth-snd expr ctx)
  (let* ([p-expr (cadr expr)]
         [p-synth (dep-synth p-expr ctx)])
        (if (not (eq? (car p-synth) 'ok))
            p-synth
            (let ([p-type (cadr p-synth)])
                 (if (not (sigma-type? p-type))
                     `(error expected-sigma-got ,p-type)
                     (let* ([var (sigma-var p-type)]
                            [snd-type (sigma-snd-type p-type)]
                            ;; Substitute (fst p) for x
                            [fst-expr `(fst ,p-expr)]
                            [result-type (dep-subst-type snd-type var fst-expr)])
                           `(ok ,result-type)))))))

;;; ============================================================
;;; Arrow and Product Synthesis
;;; ============================================================

(define (dep-synth-arrow expr ctx)
  (let ([types (cdr expr)])
       (let loop ([ts types])
            (if (null? ts)
                `(ok Type)
                (let ([t-synth (dep-synth (car ts) ctx)])
                     (if (not (eq? (car t-synth) 'ok))
                         t-synth
                         (if (not (universe-type? (cadr t-synth)))
                             `(error expected-type-got ,(cadr t-synth))
                             (loop (cdr ts)))))))))

(define (dep-synth-product expr ctx)
  (let ([types (cdr expr)])
       (let loop ([ts types])
            (if (null? ts)
                `(ok Type)
                (let ([t-synth (dep-synth (car ts) ctx)])
                     (if (not (eq? (car t-synth) 'ok))
                         t-synth
                         (if (not (universe-type? (cadr t-synth)))
                             `(error expected-type-got ,(cadr t-synth))
                             (loop (cdr ts)))))))))

;;; ============================================================
;;; Vec and Matrix Type Synthesis
;;; ============================================================

(define (dep-synth-vec expr ctx)
  (if (not (= (length expr) 3))
      `(error malformed-vec-type ,expr)
      (let* ([n-expr (cadr expr)]
             [A-expr (caddr expr)]
             [n-check (dep-check n-expr 'Nat ctx)])
            (if (not (eq? (car n-check) 'ok))
                n-check
                (let ([A-check (dep-check-type A-expr ctx)])
                     (if (not (eq? (car A-check) 'ok))
                         A-check
                         `(ok Type)))))))

(define (dep-synth-matrix expr ctx)
  (if (not (= (length expr) 4))
      `(error malformed-matrix-type ,expr)
      (let* ([m-expr (cadr expr)]
             [n-expr (caddr expr)]
             [A-expr (cadddr expr)]
             [m-check (dep-check m-expr 'Nat ctx)]
             [n-check (if (eq? (car m-check) 'ok)
                          (dep-check n-expr 'Nat ctx)
                          m-check)])
            (if (not (eq? (car n-check) 'ok))
                n-check
                (let ([A-check (dep-check-type A-expr ctx)])
                     (if (not (eq? (car A-check) 'ok))
                         A-check
                         `(ok Type)))))))

;;; ============================================================
;;; Application Synthesis
;;; ============================================================

;;; (f x) : B[x/y] where f : (Π ((y : A)) B) and x : A
(define (dep-synth-app expr ctx)
  (let* ([func-expr (car expr)]
         [args (cdr expr)]
         [func-synth (dep-synth func-expr ctx)])
        (if (not (eq? (car func-synth) 'ok))
            func-synth
            (let ([func-type (cadr func-synth)])
                 (dep-synth-app-args func-type args ctx)))))

(define (dep-synth-app-args func-type args ctx)
  (if (null? args)
      `(ok ,func-type)
      (cond
       ;; Pi type: check arg and substitute
       [(pi-type? func-type)
        (let* ([var (pi-var func-type)]
               [domain (pi-domain func-type)]
               [codomain (pi-codomain func-type)]
               [arg-expr (car args)]
               [arg-check (dep-check arg-expr domain ctx)])
              (if (not (eq? (car arg-check) 'ok))
                  arg-check
                  ;; Substitute arg for var in codomain
                  (let ([result-type (dep-subst-type codomain var arg-expr)])
                       (dep-synth-app-args result-type (cdr args) ctx))))]
       
       ;; Arrow type (non-dependent Pi)
       [(function-type? func-type)
        (let* ([param-types (function-param-types func-type)]
               [return-type (function-return-type func-type)])
              (if (< (length args) (length param-types))
                  ;; Partial application
                  (let ([remaining-params (list-tail param-types (length args))])
                       (dep-check-args args (take param-types (length args)) ctx
                                       (lambda () `(ok ,(cons '-> (append remaining-params (list return-type)))))))
                  ;; Full application
                  (dep-check-args args param-types ctx
                                  (lambda () `(ok ,return-type)))))]
       
       [else `(error not-a-function ,func-type)])))

;;; take : (List a) × Int → (List a)
(define (take lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

;;; ============================================================
;;; Type Checking
;;; ============================================================

;;; dep-check : Expr × Type × Context → (ok) | (error ...)
;;; Check that an expression has the expected type.
(define (dep-check expr expected ctx)
  (cond
   ;; Lambda against Pi type
   [(and (pair? expr) (eq? (car expr) 'fn) (pi-type? expected))
    (let* ([params (cadr expr)]
           [body (caddr expr)]
           [var (pi-var expected)]
           [domain (pi-domain expected)]
           [codomain (pi-codomain expected)]
           ;; Get param name from lambda (or use Pi's var)
           [param-name (if (and (pair? (car params)) (pair? (caar params)))
                           (caaar params)  ; annotated param
                           (if (symbol? (car params))
                               (car params)
                               var))]
           [new-ctx (dep-ctx-extend ctx param-name domain)])
          ;; Check body against codomain (with substitution if names differ)
          (let ([check-type (if (eq? param-name var)
                                codomain
                                (dep-subst-type codomain var param-name))])
               (dep-check body check-type new-ctx)))]
   
   ;; Lambda against arrow type
   [(and (pair? expr) (eq? (car expr) 'fn) (function-type? expected))
    (let* ([params (cadr expr)]
           [body (caddr expr)]
           [param-types (function-param-types expected)]
           [return-type (function-return-type expected)])
          (if (not (= (length params) (length param-types)))
              `(error arity-mismatch (expected ,(length param-types)) (got ,(length params)))
              (let* ([param-names (map (lambda (p)
                                               (if (pair? p) (car p) p))
                                       params)]
                     [new-ctx (fold-left (lambda (ctx pair)
                                                 (dep-ctx-extend ctx (car pair) (cdr pair)))
                                         ctx
                                         (map cons param-names param-types))])
                    (dep-check body return-type new-ctx))))]
   
   ;; Pair against Sigma type
   [(and (pair? expr) (eq? (car expr) 'pair) (sigma-type? expected))
    (let* ([fst-expr (cadr expr)]
           [snd-expr (caddr expr)]
           [var (sigma-var expected)]
           [fst-type (sigma-fst-type expected)]
           [snd-type-raw (sigma-snd-type expected)]
           [fst-check (dep-check fst-expr fst-type ctx)])
          (if (not (eq? (car fst-check) 'ok))
              fst-check
              ;; Check second component with substituted type
              (let ([snd-type (dep-subst-type snd-type-raw var fst-expr)])
                   (dep-check snd-expr snd-type ctx))))]
   
   ;; Type annotation in check mode
   [(and (pair? expr) (or (eq? (car expr) 'the) (eq? (car expr) (quote :))))
    (let* ([annot-type (if (eq? (car expr) 'the) (cadr expr) (caddr expr))]
           [e (if (eq? (car expr) 'the) (caddr expr) (cadr expr))])
          ;; Check types are convertible
          (if (not (dep-types-equal? annot-type expected ctx))
              `(error type-mismatch (expected ,expected) (got ,annot-type))
              (dep-check e annot-type ctx)))]
   
   ;; Fall back to synthesis and conversion
   [else
    (let ([synth-result (dep-synth expr ctx)])
         (if (not (eq? (car synth-result) 'ok))
             synth-result
             (let ([inferred (cadr synth-result)])
                  (if (dep-types-equal? inferred expected ctx)
                      '(ok)
                      `(error type-mismatch (expected ,expected) (got ,inferred))))))]))

;;; dep-check-type : Type × Context → (ok) | (error ...)
;;; Check that something is a valid type.
(define (dep-check-type type-expr ctx)
  (let ([synth (dep-synth type-expr ctx)])
       (if (not (eq? (car synth) 'ok))
           synth
           (let ([type-of-type (cadr synth)])
                (if (universe-type? type-of-type)
                    '(ok)
                    `(error expected-type-got ,type-of-type))))))

;;; dep-check-args : (List Expr) × (List Type) × Context × Thunk → Result
(define (dep-check-args args expected-types ctx cont)
  (if (null? args)
      (cont)
      (let ([check-result (dep-check (car args) (car expected-types) ctx)])
           (if (not (eq? (car check-result) 'ok))
               check-result
               (dep-check-args (cdr args) (cdr expected-types) ctx cont)))))

;;; ============================================================
;;; Type Equality with NbE
;;; ============================================================

;;; dep-types-equal? : Type × Type × Context → Boolean
;;; Check if two types are definitionally equal using NbE.
(define (dep-types-equal? t1 t2 ctx)
  ;; Build NbE environment from context
  (let* ([nbe-env (ctx->nbe-env ctx)]
         [v1 (nbe-eval t1 nbe-env)]
         [v2 (nbe-eval t2 nbe-env)])
        (convert? v1 v2 0)))

;;; ctx->nbe-env : Context → NbEEnv
;;; Convert type context to NbE environment.
(define (ctx->nbe-env ctx)
  (fold-left
   (lambda (env entry)
           (let ([name (car entry)]
                 [type (cadr entry)])
                ;; Bind name to neutral value (unknown)
                (env-extend env name (V-neutral (N-var name)))))
   nbe-empty-env
   ctx))

;;; ============================================================
;;; Quoted Literals
;;; ============================================================

(define (dep-synth-quoted datum)
  (cond
   [(symbol? datum) `(ok Symbol)]
   [(number? datum) `(ok Int)]
   [(string? datum) `(ok String)]
   [(null? datum) `(ok (List ?))]
   [(pair? datum)
    (let ([elem-result (dep-synth-quoted (car datum))])
         (if (eq? (car elem-result) 'ok)
             `(ok (List ,(cadr elem-result)))
             `(ok (List ?))))]
   [else `(ok ?)]))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

;;; dep-typeof : Expr → Type | Error
;;; Infer the type of an expression in empty context.
(define (dep-typeof expr)
  (let ([result (dep-synth expr empty-dep-ctx)])
       (if (eq? (car result) 'ok)
           (cadr result)
           result)))

;;; dep-typecheck : Expr × Type → Boolean | Error
;;; Check that an expression has the given type.
(define (dep-typecheck expr type)
  (let ([result (dep-check expr type empty-dep-ctx)])
       (eq? (car result) 'ok)))
