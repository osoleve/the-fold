;;; core/types/dep-infer.ss — Dependent Type Inference
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
(load "core/types/gadt.ss")
(load "core/types/existential.ss")

;;; ============================================================
;;; Extended Type Context
;;; ============================================================

;;; A dependent type context tracks both type and term variables.
;;; Term variables can appear in types.

;;; ctx-entry: (name . (type . value-opt))
;;; value-opt is either a value or #f if abstract

(define empty-dep-ctx '())

;;; dep-ctx-lookup : Context × Symbol → (Option Type)
(define (dep-ctx-lookup ctx name)
  (let ([entry (assq name ctx)])
       (if entry
           (cadr entry)  ; The type
           #f)))

;;; dep-ctx-lookup-value : Context × Symbol → (Option α)
(define (dep-ctx-lookup-value ctx name)
  (let ([entry (assq name ctx)])
       (if entry
           (if (pair? (cddr entry))
               (caddr entry)
               #f)
           #f)))

;;; dep-ctx-extend : Context × Symbol × Type → Context
(define (dep-ctx-extend ctx name type)
  (cons (list name type) ctx))

;;; dep-ctx-extend-def : Context × Symbol × Type × α → Context
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
   
   ;; Equality type: (= A x y)
   [(eq? (car expr) '=)
    (dep-synth-equality expr ctx)]
   
   ;; refl: (refl A x) - proof of (= A x x)
   [(eq? (car expr) 'refl)
    (dep-synth-refl expr ctx)]
   
   ;; J eliminator: (J A P d x y p)
   [(eq? (car expr) 'J)
    (dep-synth-J expr ctx)]
   
   ;; sym: (sym p) - proof of symmetry
   [(eq? (car expr) 'sym)
    (dep-synth-sym expr ctx)]
   
   ;; trans: (trans p q) - proof of transitivity
   [(eq? (car expr) 'trans)
    (dep-synth-trans expr ctx)]
   
   ;; cong: (cong f p) - congruence
   [(eq? (car expr) 'cong)
    (dep-synth-cong expr ctx)]
   
   ;; subst: (subst P p x) - substitution
   [(eq? (car expr) 'subst)
    (dep-synth-subst expr ctx)]
   
   ;; Refinement type: (refine ((x : T)) P)
   [(eq? (car expr) 'refine)
    (dep-synth-refinement expr ctx)]
   
   ;; Refinement introduction: (refine-intro v prf)
   [(eq? (car expr) 'refine-intro)
    (dep-synth-refine-intro expr ctx)]
   
   ;; Refinement elimination: (refine-elim e)
   [(eq? (car expr) 'refine-elim)
    (dep-synth-refine-elim expr ctx)]
   
   ;; Inductive type declaration: (data (Name params...) [ctor : type] ...)
   [(eq? (car expr) 'data)
    (dep-synth-data expr ctx)]
   
   ;; Eliminator application: (elim-TypeName P method... target)
   [(and (symbol? (car expr))
         (string-prefix? "elim-" (symbol->string (car expr))))
    (dep-synth-eliminator expr ctx)]
   
   ;; Module signature: (sig Name decl ...)
   [(eq? (car expr) 'sig)
    (dep-synth-sig expr ctx)]
   
   ;; GADT declaration: (gadt (Name indices...) [ctor : type] ...)
   [(eq? (car expr) 'gadt)
    (dep-synth-gadt expr ctx)]
   
   ;; GADT case: (gadt-case scrutinee ((Tag vars...) body) ...)
   [(gadt-case-expr? expr)
    (dep-synth-gadt-case expr ctx)]
   
   ;; Existential type: (∃ ((a : K)) T)
   [(eq? (car expr) '∃)
    (dep-synth-existential expr ctx)]
   
   ;; Pack: (pack WitnessType Value : ExistentialType)
   [(pack-expr? expr)
    (dep-synth-pack expr ctx)]
   
   ;; Unpack: (unpack ((a val) packed-expr) body)
   [(unpack-expr? expr)
    (dep-synth-unpack expr ctx)]
   
   ;; Application
   [else
    (dep-synth-app expr ctx)]))

;;; ============================================================
;;; Pi Type Synthesis
;;; ============================================================

;;; (Π ((x : A)) B) : Type_{max(l₁, l₂)}
;;; where A : Type_{l₁} and B : Type_{l₂} under x:A
;;; dep-synth-pi : Expr × Context → (Result Type Error)
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

;;; dep-synth-sigma : Expr × Context → (Result Type Error)
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
;;; dep-synth-lambda : Expr × Context → (Result Type Error)
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
;;; dep-synth-pair : Expr × Context → (Result Type Error)
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
;;; dep-synth-fst : Expr × Context → (Result Type Error)
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
;;; dep-synth-snd : Expr × Context → (Result Type Error)
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

;;; dep-synth-arrow : Expr × Context → (Result Type Error)
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

;;; dep-synth-product : Expr × Context → (Result Type Error)
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

;;; dep-synth-vec : Expr × Context → (Result Type Error)
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

;;; dep-synth-matrix : Expr × Context → (Result Type Error)
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
;;; Equality Type Synthesis
;;; ============================================================
;;;
;;; Propositional equality implements Martin-Löf's identity type.
;;;
;;; Type Formation:
;;;   (= A x y) : Type  when A : Type, x : A, y : A
;;;
;;; Introduction:
;;;   (refl A x) : (= A x x)  when x : A
;;;
;;; Elimination (J):
;;;   (J A P d x y p) : (P x y p)
;;;   where:
;;;     A : Type
;;;     P : (Π ((a : A)) (Π ((b : A)) (Π ((_ : (= A a b))) Type)))
;;;     d : (Π ((z : A)) (P z z (refl A z)))
;;;     x : A
;;;     y : A
;;;     p : (= A x y)
;;;
;;; Computation:
;;;   (J A P d x x (refl A x)) ≡ (d x)

;;; dep-synth-equality : Expr × Context → (Result Type Error)
;;; (= A x y) : Type when A : Type, x : A, y : A
(define (dep-synth-equality expr ctx)
  (if (not (= (length expr) 4))
      `(error malformed-equality-type ,expr)
      (let* ([A-expr (cadr expr)]
             [x-expr (caddr expr)]
             [y-expr (cadddr expr)]
             [A-synth (dep-synth A-expr ctx)])
            (if (not (eq? (car A-synth) 'ok))
                A-synth
                (let ([A-type (cadr A-synth)])
                     (if (not (universe-type? A-type))
                         `(error expected-type-got ,A-type)
                         ;; Check x : A and y : A
                         (let ([x-check (dep-check x-expr A-expr ctx)])
                              (if (not (eq? (car x-check) 'ok))
                                  x-check
                                  (let ([y-check (dep-check y-expr A-expr ctx)])
                                       (if (not (eq? (car y-check) 'ok))
                                           y-check
                                           ;; Result is Type at same level as A
                                           `(ok ,A-type)))))))))))

;;; dep-synth-refl : Expr × Context → (Result Type Error)
;;; (refl A x) : (= A x x) when x : A
(define (dep-synth-refl expr ctx)
  (cond
   ;; Bare 'refl without annotation - needs type info from context
   [(= (length expr) 1)
    `(error refl-needs-annotation refl)]
   ;; (refl A x) - annotated form
   [(= (length expr) 3)
    (let* ([A-expr (cadr expr)]
           [x-expr (caddr expr)]
           [A-synth (dep-synth A-expr ctx)])
          (if (not (eq? (car A-synth) 'ok))
              A-synth
              (let ([A-type (cadr A-synth)])
                   (if (not (universe-type? A-type))
                       `(error expected-type-got ,A-type)
                       ;; Check x : A
                       (let ([x-check (dep-check x-expr A-expr ctx)])
                            (if (not (eq? (car x-check) 'ok))
                                x-check
                                `(ok (= ,A-expr ,x-expr ,x-expr))))))))]
   [else `(error malformed-refl ,expr)]))

;;; dep-synth-J : Expr × Context → (Result Type Error)
;;; (J A P d x y p) : (P x y p)
;;; J is the eliminator for equality types (also called "transport" or "subst").
(define (dep-synth-J expr ctx)
  (if (not (= (length expr) 7))
      `(error malformed-J-eliminator ,expr)
      (let* ([A-expr (cadr expr)]
             [P-expr (caddr expr)]
             [d-expr (cadddr expr)]
             [x-expr (car (cddddr expr))]
             [y-expr (cadr (cddddr expr))]
             [p-expr (caddr (cddddr expr))]
             ;; Check A is a type
             [A-check (dep-check-type A-expr ctx)])
            (if (not (eq? (car A-check) 'ok))
                A-check
                ;; Build the expected type for P:
                ;; P : (Π ((a : A)) (Π ((b : A)) (Π ((_ : (= A a b))) Type)))
                (let* ([eq-type-ab `(= ,A-expr a b)]
                       [P-expected (t-pi 'a A-expr
                                         (t-pi 'b A-expr
                                               (t-pi '_ eq-type-ab 'Type)))]
                       [P-check (dep-check P-expr P-expected ctx)])
                      (if (not (eq? (car P-check) 'ok))
                          P-check
                          ;; Check d : (Π ((z : A)) (P z z (refl A z)))
                          (let* ([d-expected (t-pi 'z A-expr
                                                   `(,P-expr z z (refl ,A-expr z)))]
                                 [d-check (dep-check d-expr d-expected ctx)])
                                (if (not (eq? (car d-check) 'ok))
                                    d-check
                                    ;; Check x : A
                                    (let ([x-check (dep-check x-expr A-expr ctx)])
                                         (if (not (eq? (car x-check) 'ok))
                                             x-check
                                             ;; Check y : A
                                             (let ([y-check (dep-check y-expr A-expr ctx)])
                                                  (if (not (eq? (car y-check) 'ok))
                                                      y-check
                                                      ;; Check p : (= A x y)
                                                      (let* ([eq-type `(= ,A-expr ,x-expr ,y-expr)]
                                                             [p-check (dep-check p-expr eq-type ctx)])
                                                            (if (not (eq? (car p-check) 'ok))
                                                                p-check
                                                                ;; Result type: (P x y p)
                                                                `(ok (,P-expr ,x-expr ,y-expr ,p-expr))))))))))))))))

;;; ============================================================
;;; Derived Equality Operations
;;; ============================================================
;;;
;;; These can all be derived from J, but we provide direct synthesis
;;; for better error messages and efficiency.

;;; dep-synth-sym : Expr × Context → (Result Type Error)
;;; (sym p) : (= A y x) when p : (= A x y)
;;; Symmetry: if x = y then y = x
(define (dep-synth-sym expr ctx)
  (if (not (= (length expr) 2))
      `(error malformed-sym ,expr)
      (let* ([p-expr (cadr expr)]
             [p-synth (dep-synth p-expr ctx)])
            (if (not (eq? (car p-synth) 'ok))
                p-synth
                (let ([p-type (cadr p-synth)])
                     (if (not (equality-type? p-type))
                         `(error expected-equality-got ,p-type)
                         (let* ([A (equality-carrier p-type)]
                                [x (equality-lhs p-type)]
                                [y (equality-rhs p-type)])
                               ;; Result: (= A y x)
                               `(ok (= ,A ,y ,x)))))))))

;;; dep-synth-trans : Expr × Context → (Result Type Error)
;;; (trans p q) : (= A x z) when p : (= A x y), q : (= A y z)
;;; Transitivity: if x = y and y = z then x = z
(define (dep-synth-trans expr ctx)
  (if (not (= (length expr) 3))
      `(error malformed-trans ,expr)
      (let* ([p-expr (cadr expr)]
             [q-expr (caddr expr)]
             [p-synth (dep-synth p-expr ctx)])
            (if (not (eq? (car p-synth) 'ok))
                p-synth
                (let ([p-type (cadr p-synth)])
                     (if (not (equality-type? p-type))
                         `(error expected-equality-got ,p-type)
                         (let* ([A (equality-carrier p-type)]
                                [x (equality-lhs p-type)]
                                [y (equality-rhs p-type)]
                                ;; Check q : (= A y z)
                                [q-synth (dep-synth q-expr ctx)])
                               (if (not (eq? (car q-synth) 'ok))
                                   q-synth
                                   (let ([q-type (cadr q-synth)])
                                        (if (not (equality-type? q-type))
                                            `(error expected-equality-got ,q-type)
                                            (let ([q-A (equality-carrier q-type)]
                                                  [q-y (equality-lhs q-type)]
                                                  [z (equality-rhs q-type)])
                                                 ;; Check carrier types match
                                                 (if (not (dep-types-equal? A q-A ctx))
                                                     `(error type-mismatch (expected ,A) (got ,q-A))
                                                     ;; Check y matches
                                                     (if (not (dep-types-equal? y q-y ctx))
                                                         `(error equality-endpoint-mismatch (expected ,y) (got ,q-y))
                                                         ;; Result: (= A x z)
                                                         `(ok (= ,A ,x ,z)))))))))))))))

;;; dep-synth-cong : Expr × Context → (Result Type Error)
;;; (cong f p) : (= B (f x) (f y)) when f : (-> A B), p : (= A x y)
;;; Congruence: if x = y then f(x) = f(y)
(define (dep-synth-cong expr ctx)
  (if (not (= (length expr) 3))
      `(error malformed-cong ,expr)
      (let* ([f-expr (cadr expr)]
             [p-expr (caddr expr)]
             [f-synth (dep-synth f-expr ctx)])
            (if (not (eq? (car f-synth) 'ok))
                f-synth
                (let ([f-type (cadr f-synth)])
                     ;; f can be either arrow type or Pi type
                     (cond
                      [(function-type? f-type)
                       (let* ([param-types (function-param-types f-type)]
                              [return-type (function-return-type f-type)])
                             (if (not (= (length param-types) 1))
                                 `(error cong-needs-unary-function ,f-type)
                                 (let* ([A (car param-types)]
                                        [B return-type]
                                        [p-synth (dep-synth p-expr ctx)])
                                       (if (not (eq? (car p-synth) 'ok))
                                           p-synth
                                           (let ([p-type (cadr p-synth)])
                                                (if (not (equality-type? p-type))
                                                    `(error expected-equality-got ,p-type)
                                                    (let ([p-A (equality-carrier p-type)]
                                                          [x (equality-lhs p-type)]
                                                          [y (equality-rhs p-type)])
                                                         (if (not (dep-types-equal? A p-A ctx))
                                                             `(error type-mismatch (expected ,A) (got ,p-A))
                                                             ;; Result: (= B (f x) (f y))
                                                             `(ok (= ,B (,f-expr ,x) (,f-expr ,y)))))))))))]
                      [(pi-type? f-type)
                       (let* ([A (pi-domain f-type)]
                              [B (pi-codomain f-type)]
                              [var (pi-var f-type)]
                              [p-synth (dep-synth p-expr ctx)])
                             (if (not (eq? (car p-synth) 'ok))
                                 p-synth
                                 (let ([p-type (cadr p-synth)])
                                      (if (not (equality-type? p-type))
                                          `(error expected-equality-got ,p-type)
                                          (let* ([p-A (equality-carrier p-type)]
                                                 [x (equality-lhs p-type)]
                                                 [y (equality-rhs p-type)])
                                                (if (not (dep-types-equal? A p-A ctx))
                                                    `(error type-mismatch (expected ,A) (got ,p-A))
                                                    ;; For dependent functions: use heterogeneous equality
                                                    ;; Result: HEq B[x/var] B[y/var] (f x) (f y)
                                                    ;; This degenerates to (= B (f x) (f y)) when B is non-dependent
                                                    (let* ([B-x (dep-subst-type B var x)]
                                                           [B-y (dep-subst-type B var y)]
                                                           [result-type (make-heq-type B-x B-y
                                                                                       `(,f-expr ,x)
                                                                                       `(,f-expr ,y))])
                                                          `(ok ,result-type))))))))]
                      [else `(error not-a-function ,f-type)]))))))

;;; dep-synth-subst : Expr × Context → (Result Type Error)
;;; (subst P p x) : (P y) when P : (-> A Type), p : (= A x y), x : (P x)
;;; Substitution: transport a proof along an equality
(define (dep-synth-subst expr ctx)
  (if (not (= (length expr) 4))
      `(error malformed-subst ,expr)
      (dep-synth-subst-impl (cadr expr) (caddr expr) (cadddr expr) ctx)))

;;; dep-synth-subst-impl : Expr × Expr × Expr × Context → (Result Type Error)
(define (dep-synth-subst-impl P-expr p-expr prf-expr ctx)
  (let ([P-synth (dep-synth P-expr ctx)])
       (if (not (eq? (car P-synth) 'ok))
           P-synth
           (let ([P-type (cadr P-synth)])
                (dep-synth-subst-with-type P-expr p-expr prf-expr P-type ctx)))))

;;; dep-synth-subst-with-type : Expr × Expr × Expr × Type × Context → (Result Type Error)
(define (dep-synth-subst-with-type P-expr p-expr prf-expr P-type ctx)
  (cond
   [(function-type? P-type)
    (let* ([param-types (function-param-types P-type)]
           [return-type (function-return-type P-type)])
          (if (or (not (= (length param-types) 1))
                  (not (universe-type? return-type)))
              `(error subst-needs-predicate ,P-type)
              (dep-synth-subst-check P-expr p-expr prf-expr (car param-types) ctx)))]
   [(pi-type? P-type)
    (let ([A (pi-domain P-type)]
          [codomain (pi-codomain P-type)])
         (if (not (universe-type? codomain))
             `(error subst-needs-predicate ,P-type)
             (dep-synth-subst-check P-expr p-expr prf-expr A ctx)))]
   [else `(error subst-needs-predicate ,P-type)]))

;;; dep-synth-subst-check : Expr × Expr × Expr × Type × Context → (Result Type Error)
(define (dep-synth-subst-check P-expr p-expr prf-expr A ctx)
  (let ([p-synth (dep-synth p-expr ctx)])
       (if (not (eq? (car p-synth) 'ok))
           p-synth
           (let ([p-type (cadr p-synth)])
                (if (not (equality-type? p-type))
                    `(error expected-equality-got ,p-type)
                    (let ([p-A (equality-carrier p-type)]
                          [x (equality-lhs p-type)]
                          [y (equality-rhs p-type)])
                         (if (not (dep-types-equal? A p-A ctx))
                             `(error type-mismatch (expected ,A) (got ,p-A))
                             ;; Check prf : (P x)
                             (let ([Px-type `(,P-expr ,x)])
                                  (let ([prf-check (dep-check prf-expr Px-type ctx)])
                                       (if (not (eq? (car prf-check) 'ok))
                                           prf-check
                                           ;; Result: (P y)
                                           `(ok (,P-expr ,y))))))))))))

;;; ============================================================
;;; Refinement Type Synthesis
;;; ============================================================
;;;
;;; Refinement types: {x : T | P}
;;; Syntax: (refine ((x : T)) P)
;;;
;;; Type Formation:
;;;   (refine ((x : T)) P) : Type  when T : Type and P : Bool (under x:T)
;;;
;;; Introduction:
;;;   (refine-intro T v prf) : {x:T | P}
;;;   when v : T and prf : P[v/x]
;;;
;;; Elimination:
;;;   (refine-elim e) : T  when e : {x:T | P}
;;;   Extracts the underlying value, forgetting the refinement
;;;
;;; Checking:
;;;   To check e : {x:T | P}, check e : T and verify P[e/x]

;;; dep-synth-refinement : Expr × Context → (Result Type Error)
;;; (refine ((x : T)) P) : Type when T : Type and P : Bool under x:T
(define (dep-synth-refinement expr ctx)
  (if (not (= (length expr) 3))
      `(error malformed-refinement-type ,expr)
      (let* ([binding (car (cadr expr))]
             [var (car binding)]
             [base-type-expr (caddr binding)]
             [predicate (caddr expr)])
            ;; Check base type is a type
            (let ([base-check (dep-check-type base-type-expr ctx)])
                 (if (not (eq? (car base-check) 'ok))
                     base-check
                     ;; Check predicate is well-typed under x:T
                     ;; For now, we just check it's a Bool expression
                     (let* ([new-ctx (dep-ctx-extend ctx var base-type-expr)]
                            [pred-synth (dep-synth predicate new-ctx)])
                           (if (not (eq? (car pred-synth) 'ok))
                               pred-synth
                               (let ([pred-type (cadr pred-synth)])
                                    ;; Predicate must be Bool
                                    (if (not (eq? pred-type 'Bool))
                                        `(error refinement-predicate-not-bool (expected Bool) (got ,pred-type))
                                        `(ok Type))))))))))

;;; dep-synth-refine-intro : Expr × Context → (Result Type Error)
;;; (refine-intro (refine-type) v prf) : refine-type
;;; where refine-type = (refine ((x : T)) P)
;;; v : T, prf : P[v/x]
(define (dep-synth-refine-intro expr ctx)
  (if (not (= (length expr) 4))
      `(error malformed-refine-intro ,expr)
      (let* ([refine-type-expr (cadr expr)]
             [v-expr (caddr expr)]
             [prf-expr (cadddr expr)])
            ;; Check refine-type is a refinement type
            (let ([type-synth (dep-synth refine-type-expr ctx)])
                 (if (not (eq? (car type-synth) 'ok))
                     type-synth
                     (if (not (refinement-type? refine-type-expr))
                         `(error not-a-refinement-type ,refine-type-expr)
                         (let* ([var (refinement-var refine-type-expr)]
                                [base-type (refinement-base-type refine-type-expr)]
                                [predicate (refinement-predicate refine-type-expr)])
                               ;; Check v : T
                               (let ([v-check (dep-check v-expr base-type ctx)])
                                    (if (not (eq? (car v-check) 'ok))
                                        v-check
                                        ;; Check prf : P[v/x] (predicate with v substituted)
                                        (let ([subst-pred (dep-subst-type predicate var v-expr)])
                                             ;; For now, we just check prf is a Bool
                                             ;; In a full system, we'd check it's a proof of the predicate
                                             (let ([prf-synth (dep-synth prf-expr ctx)])
                                                  (if (not (eq? (car prf-synth) 'ok))
                                                      prf-synth
                                                      ;; Result is the refinement type
                                                      `(ok ,refine-type-expr)))))))))))))

;;; dep-synth-refine-elim : Expr × Context → (Result Type Error)
;;; (refine-elim e) : T when e : {x:T | P}
;;; Extracts the underlying value
(define (dep-synth-refine-elim expr ctx)
  (if (not (= (length expr) 2))
      `(error malformed-refine-elim ,expr)
      (let* ([e-expr (cadr expr)]
             [e-synth (dep-synth e-expr ctx)])
            (if (not (eq? (car e-synth) 'ok))
                e-synth
                (let ([e-type (cadr e-synth)])
                     (if (not (refinement-type? e-type))
                         `(error expected-refinement-type-got ,e-type)
                         ;; Result is the base type
                         `(ok ,(refinement-base-type e-type))))))))

;;; ============================================================
;;; Application Synthesis
;;; ============================================================

;;; (f x) : B[x/y] where f : (Π ((y : A)) B) and x : A
;;; dep-synth-app : Expr × Context → (Result Type Error)
(define (dep-synth-app expr ctx)
  (let* ([func-expr (car expr)]
         [args (cdr expr)]
         [func-synth (dep-synth func-expr ctx)])
        (if (not (eq? (car func-synth) 'ok))
            func-synth
            (let ([func-type (cadr func-synth)])
                 (dep-synth-app-args func-type args ctx)))))

;;; dep-synth-app-args : Type × (List Expr) × Context → (Result Type Error)
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

;;; dep-check-args : (List Expr) × (List Type) × Context × Thunk → (Result α Error)
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
;;; If a context entry has a value (3rd element), evaluate it; otherwise use neutral.
(define (ctx->nbe-env ctx)
  (fold-left
   (lambda (env entry)
           (let ([name (car entry)])
                (if (and (>= (length entry) 3) (caddr entry))
                    ;; Entry has a value - evaluate it
                    (env-extend env name (nbe-eval (caddr entry) env))
                    ;; No value - bind to neutral (unknown)
                    (env-extend env name (V-neutral (N-var name))))))
   nbe-empty-env
   ctx))

;;; ============================================================
;;; Quoted Literals
;;; ============================================================

;;; dep-synth-quoted : α → (Result Type Error)
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
;;; Inductive Type Synthesis
;;; ============================================================
;;;
;;; Inductive types introduce:
;;;   1. A type former: the data declaration itself
;;;   2. Constructors: functions to build values
;;;   3. Eliminator: structural recursion principle
;;;
;;; Type Formation:
;;;   (data (D params...) [c₁ : T₁] ... [cₙ : Tₙ]) : Type
;;;   when each Tᵢ returns D with the same parameters
;;;
;;; Constructor Types:
;;;   Each constructor cᵢ has type Tᵢ (as declared)
;;;
;;; Eliminator Type:
;;;   elim-D : (P : D → Type) → ... → (x : D) → (P x)

;;; string-prefix? : String × String → Boolean
;;; Check if str starts with prefix.
(define (string-prefix? prefix str)
  (and (>= (string-length str) (string-length prefix))
       (string=? prefix (substring str 0 (string-length prefix)))))

;;; dep-synth-data : Expr × Context → (Result Type Error)
;;; Synthesize type for a data declaration.
;;; (data (Name params...) [ctor : type] ...) : Type
(define (dep-synth-data expr ctx)
  (if (not (data-type-well-formed? expr))
      `(error malformed-data-declaration ,expr)
      (let* ([name (data-name expr)]
             [params (data-params expr)]
             [ctors (data-constructors expr)]
             [param-bindings (data-param-bindings expr)])
            ;; Check all parameters are well-typed
            (let ([param-check (dep-synth-data-params param-bindings ctx)])
                 (if (not (eq? (car param-check) 'ok))
                     param-check
                     ;; Build context with:
                     ;; 1. Type parameters (e.g., A : Type)
                     ;; 2. Type constructor (e.g., List : Type -> Type)
                     (let* ([ctx-with-params (dep-extend-ctx-with-params ctx param-bindings)]
                            ;; Add the type constructor to context
                            [type-ctor-type (make-type-constructor-type param-bindings)]
                            [extended-ctx (dep-ctx-extend ctx-with-params name type-ctor-type)]
                            [ctor-check (dep-synth-data-constructors ctors name extended-ctx)])
                           (if (not (eq? (car ctor-check) 'ok))
                               ctor-check
                               ;; Data declaration is a type
                               `(ok Type))))))))

;;; make-type-constructor-type : (List (Symbol . Type)) → Type
;;; Build the type of a type constructor from its parameters.
;;; E.g., for (List A), returns (-> Type Type)
;;; For no params, returns Type.
(define (make-type-constructor-type params)
  (if (null? params)
      'Type
      (let ([param-types (map cdr params)])
           (fold-right (lambda (pt acc) `(-> ,pt ,acc))
                       'Type
                       param-types))))

;;; dep-synth-data-params : (List (Symbol . Type)) × Context → (Result () Error)
;;; Check that all parameter types are valid types.
(define (dep-synth-data-params params ctx)
  (if (null? params)
      '(ok)
      (let* ([param (car params)]
             [ptype (cdr param)]
             [type-check (dep-check-type ptype ctx)])
            (if (not (eq? (car type-check) 'ok))
                type-check
                (let ([new-ctx (dep-ctx-extend ctx (car param) ptype)])
                     (dep-synth-data-params (cdr params) new-ctx))))))

;;; dep-extend-ctx-with-params : Context × (List (Symbol . Type)) → Context
;;; Extend context with all parameter bindings.
(define (dep-extend-ctx-with-params ctx params)
  (fold-left (lambda (ctx p)
                     (dep-ctx-extend ctx (car p) (cdr p)))
             ctx
             params))

;;; dep-synth-data-constructors : (List (Symbol . Type)) × Symbol × Context → (Result () Error)
;;; Check that all constructor types are well-formed and return the inductive type.
(define (dep-synth-data-constructors ctors type-name ctx)
  (if (null? ctors)
      '(ok)
      (let* ([ctor (car ctors)]
             [ctor-name (car ctor)]
             [ctor-type (cdr ctor)])
            ;; Check constructor type is well-formed
            (let ([type-check (dep-check-type ctor-type ctx)])
                 (if (not (eq? (car type-check) 'ok))
                     `(error ctor-type-error (constructor ,ctor-name) ,(cadr type-check))
                     ;; Check constructor returns the inductive type
                     (let ([return-type (get-constructor-return-type ctor-type)])
                          (if (not (constructor-returns-type? return-type type-name))
                              `(error constructor-wrong-return-type
                                (constructor ,ctor-name)
                                (expected ,type-name)
                                (got ,return-type))
                              (dep-synth-data-constructors (cdr ctors) type-name ctx))))))))

;;; get-constructor-return-type : Type → Type
;;; Get the return type of a constructor (unwrap nested Pi types).
(define (get-constructor-return-type type)
  (cond
   [(pi-type? type)
    (get-constructor-return-type (pi-codomain type))]
   [(function-type? type)
    (function-return-type type)]
   [else type]))

;;; constructor-returns-type? : Type × Symbol → Boolean
;;; Check if a return type matches the inductive type name.
(define (constructor-returns-type? return-type type-name)
  (cond
   [(eq? return-type type-name) #t]
   [(and (pair? return-type) (eq? (car return-type) type-name)) #t]
   [else #f]))

;;; ============================================================
;;; Eliminator Synthesis
;;; ============================================================
;;;
;;; Eliminators are synthesized when we see (elim-D ...) where D
;;; is a known inductive type.
;;;
;;; For now, we require the data declaration to be in the context
;;; or we check against known built-in types.

;;; dep-synth-eliminator : Expr × Context → (Result Type Error)
;;; Synthesize type for eliminator application.
;;; (elim-TypeName P method... target) : (P target)
(define (dep-synth-eliminator expr ctx)
  (let* ([elim-sym (car expr)]
         [elim-str (symbol->string elim-sym)]
         [type-name (string->symbol (substring elim-str 5 (string-length elim-str)))]
         [args (cdr expr)])
        ;; Look up the data type declaration in context
        ;; For now, handle known types (Nat, Bool, List)
        (cond
         [(eq? type-name 'Nat)
          (dep-synth-elim-nat args ctx)]
         [(eq? type-name 'Bool)
          (dep-synth-elim-bool args ctx)]
         [(eq? type-name 'List)
          (dep-synth-elim-list args ctx)]
         [else
          `(error unknown-inductive-type ,type-name)])))

;;; dep-synth-elim-nat : (List Expr) × Context → (Result Type Error)
;;; elim-Nat P z s n : (P n)
;;; where P : Nat → Type
;;;       z : (P zero)
;;;       s : Π(m : Nat). (P m) → (P (succ m))
;;;       n : Nat
(define (dep-synth-elim-nat args ctx)
  (if (not (= (length args) 4))
      `(error elim-nat-wrong-arity (expected 4) (got ,(length args)))
      (let* ([P-expr (car args)]
             [z-expr (cadr args)]
             [s-expr (caddr args)]
             [n-expr (cadddr args)]
             ;; Check P : Nat → Type
             [P-expected (t-pi 'm 'Nat 'Type)]
             [P-check (dep-check P-expr P-expected ctx)])
            (if (not (eq? (car P-check) 'ok))
                `(error elim-nat-motive-error ,(cdr P-check))
                ;; Check z : (P zero)
                (let ([z-expected `(,P-expr zero)])
                     (let ([z-check (dep-check z-expr z-expected ctx)])
                          (if (not (eq? (car z-check) 'ok))
                              `(error elim-nat-zero-case-error ,(cdr z-check))
                              ;; Check s : Π(m : Nat). (P m) → (P (succ m))
                              (let ([s-expected (t-pi 'm 'Nat
                                                      (t-pi 'ih `(,P-expr m)
                                                            `(,P-expr (succ m))))])
                                   (let ([s-check (dep-check s-expr s-expected ctx)])
                                        (if (not (eq? (car s-check) 'ok))
                                            `(error elim-nat-succ-case-error ,(cdr s-check))
                                            ;; Check n : Nat
                                            (let ([n-check (dep-check n-expr 'Nat ctx)])
                                                 (if (not (eq? (car n-check) 'ok))
                                                     n-check
                                                     ;; Result: (P n)
                                                     `(ok (,P-expr ,n-expr))))))))))))))

;;; dep-synth-elim-bool : (List Expr) × Context → (Result Type Error)
;;; elim-Bool P t f b : (P b)
;;; where P : Bool → Type
;;;       t : (P true)
;;;       f : (P false)
;;;       b : Bool
(define (dep-synth-elim-bool args ctx)
  (if (not (= (length args) 4))
      `(error elim-bool-wrong-arity (expected 4) (got ,(length args)))
      (let* ([P-expr (car args)]
             [t-expr (cadr args)]
             [f-expr (caddr args)]
             [b-expr (cadddr args)]
             ;; Check P : Bool → Type
             [P-expected (t-pi 'b 'Bool 'Type)]
             [P-check (dep-check P-expr P-expected ctx)])
            (if (not (eq? (car P-check) 'ok))
                `(error elim-bool-motive-error ,(cdr P-check))
                ;; Check t : (P true)
                (let ([t-expected `(,P-expr true)])
                     (let ([t-check (dep-check t-expr t-expected ctx)])
                          (if (not (eq? (car t-check) 'ok))
                              `(error elim-bool-true-case-error ,(cdr t-check))
                              ;; Check f : (P false)
                              (let ([f-expected `(,P-expr false)])
                                   (let ([f-check (dep-check f-expr f-expected ctx)])
                                        (if (not (eq? (car f-check) 'ok))
                                            `(error elim-bool-false-case-error ,(cdr f-check))
                                            ;; Check b : Bool
                                            (let ([b-check (dep-check b-expr 'Bool ctx)])
                                                 (if (not (eq? (car b-check) 'ok))
                                                     b-check
                                                     ;; Result: (P b)
                                                     `(ok (,P-expr ,b-expr))))))))))))))

;;; dep-synth-elim-list : (List Expr) × Context → (Result Type Error)
;;; elim-List P n c xs : (P xs)
;;; where P : (List A) → Type
;;;       n : (P nil)
;;;       c : Π(x : A). Π(xs : List A). (P xs) → (P (cons x xs))
;;;       xs : (List A)
;;; Note: This is simplified - full version would track element type A
(define (dep-synth-elim-list args ctx)
  (if (not (= (length args) 4))
      `(error elim-list-wrong-arity (expected 4) (got ,(length args)))
      (let* ([P-expr (car args)]
             [n-expr (cadr args)]
             [c-expr (caddr args)]
             [xs-expr (cadddr args)]
             ;; Synthesize xs to get the list type
             [xs-synth (dep-synth xs-expr ctx)])
            (if (not (eq? (car xs-synth) 'ok))
                xs-synth
                (let ([xs-type (cadr xs-synth)])
                     ;; Result: (P xs)
                     ;; Simplified: we just return the application without full checking
                     `(ok (,P-expr ,xs-expr)))))))

;;; ============================================================
;;; Module Signature Synthesis
;;; ============================================================
;;;
;;; Module signatures are first-class types that describe module interfaces.
;;;
;;; A signature (sig Name decl ...) synthesizes to Type (it's a specification).
;;; We check that all declarations within are well-formed.

;;; dep-synth-sig : Expr × Context → (Result Type Error)
;;; Synthesize type for a module signature.
;;; (sig Name decl ...) : Type
(define (dep-synth-sig expr ctx)
  (if (not (sig-well-formed? expr))
      `(error malformed-signature ,expr)
      (let* ([name (sig-name expr)]
             [decls (sig-decls expr)])
            ;; Check each declaration is well-formed
            (let ([decl-check (dep-synth-sig-decls decls ctx)])
                 (if (not (eq? (car decl-check) 'ok))
                     decl-check
                     ;; Signature is a type
                     `(ok Type))))))

;;; dep-synth-sig-decls : (List Decl) × Context → (Result () Error)
;;; Check that all signature declarations are well-formed.
(define (dep-synth-sig-decls decls ctx)
  (if (null? decls)
      '(ok)
      (let* ([decl (car decls)]
             [check (dep-synth-sig-decl decl ctx)])
            (if (not (eq? (car check) 'ok))
                check
                ;; Extend context with this declaration for subsequent checks
                (let ([new-ctx (dep-extend-ctx-with-sig-decl ctx decl)])
                     (dep-synth-sig-decls (cdr decls) new-ctx))))))

;;; dep-synth-sig-decl : Decl × Context → (Result () Error)
;;; Check that a single signature declaration is well-formed.
(define (dep-synth-sig-decl decl ctx)
  (cond
   ;; Abstract type: [type T : Kind]
   [(sig-type-decl-abstract? decl)
    (let ([kind (sig-type-decl-kind decl)])
         ;; Check kind is well-formed (simplified: accept Type, (-> Type Type), etc.)
         (if (or (eq? kind 'Type)
                 (and (pair? kind) (eq? (car kind) '->)))
             '(ok)
             `(error invalid-kind ,kind)))]
   
   ;; Type alias: [type T = Type]
   [(sig-type-decl-alias? decl)
    (let ([type-def (sig-type-decl-def decl)])
         (dep-check-type type-def ctx))]
   
   ;; Value declaration: [val name : Type]
   [(sig-val-decl? decl)
    (let ([val-type (sig-val-decl-type decl)])
         (if val-type
             (dep-check-type val-type ctx)
             `(error malformed-val-decl ,decl)))]
   
   ;; Data export: [data DataDecl]
   [(sig-data-decl? decl)
    (let ([data (sig-data-decl-data decl)])
         (if (and data (data-type? data))
             (dep-synth-data data ctx)
             `(error malformed-data-decl ,decl)))]
   
   ;; Include: [include SigName]
   [(sig-include-decl? decl)
    ;; For now, just accept includes - full checking would require sig registry
    '(ok)]
   
   [else `(error unknown-sig-decl ,decl)]))

;;; dep-extend-ctx-with-sig-decl : Context × Decl → Context
;;; Extend context with bindings introduced by a signature declaration.
(define (dep-extend-ctx-with-sig-decl ctx decl)
  (cond
   ;; Abstract type: add T : Kind to context
   [(sig-type-decl-abstract? decl)
    (let ([name (sig-type-decl-name decl)]
          [kind (sig-type-decl-kind decl)])
         (dep-ctx-extend ctx name kind))]
   
   ;; Type alias: add T : Type to context (T is a type, aliased to type-def)
   ;; Note: For full alias expansion, we'd need a separate alias registry
   [(sig-type-decl-alias? decl)
    (let ([name (sig-type-decl-name decl)])
         ;; T has type Type (it's a type name)
         (dep-ctx-extend ctx name 'Type))]
   
   ;; Value: add name : Type to context
   [(sig-val-decl? decl)
    (let ([name (sig-val-decl-name decl)]
          [val-type (sig-val-decl-type decl)])
         (if (and name val-type)
             (dep-ctx-extend ctx name val-type)
             ctx))]
   
   ;; Data: add type name and constructors to context
   [(sig-data-decl? decl)
    (let ([data (sig-data-decl-data decl)])
         (if (and data (data-type? data))
             (let* ([type-name (data-name data)]
                    [ctors (data-constructors data)]
                    [ctx-with-type (dep-ctx-extend ctx type-name 'Type)])
                   ;; Add constructors
                   (fold-left (lambda (c ctor)
                                      (dep-ctx-extend c (car ctor) (cdr ctor)))
                              ctx-with-type
                              ctors))
             ctx))]
   
   ;; Include: would require sig registry lookup
   [(sig-include-decl? decl) ctx]
   
   [else ctx]))

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

;;; ============================================================
;;; GADT Support
;;; ============================================================
;;;
;;; GADTs extend inductive types with type indices that can be refined
;;; by pattern matching. The key innovation is that each constructor can
;;; specify a more precise return type, and matching on that constructor
;;; brings the type refinement into scope.

;;; GADT Registry (Global State)
(define *gadt-registry* '())

;;; reset-gadt-registry! : → Unit
(define (reset-gadt-registry!)
  (set! *gadt-registry* '()))

;;; register-gadt! : GADTDecl → Unit
(define (register-gadt! decl)
  (set! *gadt-registry* (gadt-registry-add *gadt-registry* decl)))

;;; lookup-gadt : Symbol → GADTDecl | #f
(define (lookup-gadt name)
  (gadt-registry-lookup *gadt-registry* name))

;;; dep-synth-gadt : GADTDecl × Context → (Result Type Error)
;;; Type-check a GADT declaration and register it.
(define (dep-synth-gadt decl ctx)
  (if (not (gadt-well-formed? decl))
      `(error malformed-gadt-declaration ,decl)
      (let* ([name (gadt-name decl)]
             [index-kinds (gadt-index-kinds decl)]
             [ctors (gadt-constructors decl)])
            ;; Check all constructor types
            (let ([ctor-checks (dep-check-gadt-ctors name index-kinds ctors ctx)])
                 (if (not (eq? (car ctor-checks) 'ok))
                     ctor-checks
                     (begin
                      (register-gadt! decl)
                      `(ok (gadt-type ,name))))))))

;;; dep-check-gadt-ctors : Symbol × (List (Symbol . Kind)) × (List (Symbol . Type)) × Context
;;;                        → (ok) | (error ...)
(define (dep-check-gadt-ctors gadt-name index-kinds ctors ctx)
  (if (null? ctors)
      '(ok)
      (let* ([ctor (car ctors)]
             [ctor-name (car ctor)]
             [ctor-type (cdr ctor)]
             [type-check (dep-check-type ctor-type ctx)])
            (if (not (eq? (car type-check) 'ok))
                `(error ctor-type-invalid ,ctor-name ,type-check)
                (if (not (gadt-ctor-returns-gadt? ctor-type gadt-name))
                    `(error ctor-wrong-return-type ,ctor-name ,ctor-type ,gadt-name)
                    (dep-check-gadt-ctors gadt-name index-kinds (cdr ctors) ctx))))))

;;; dep-synth-gadt-case : Expr × Context → (Result Type Error)
;;; Type-check a gadt-case expression with type refinement.
(define (dep-synth-gadt-case expr ctx)
  (let* ([scrutinee (gadt-case-scrutinee expr)]
         [clauses (gadt-case-clauses expr)]
         [scrut-synth (dep-synth scrutinee ctx)])
        (if (not (eq? (car scrut-synth) 'ok))
            scrut-synth
            (let ([scrut-type (cadr scrut-synth)])
                 (let ([gadt-info (dep-decompose-gadt-type scrut-type)])
                      (if (not gadt-info)
                          `(error scrutinee-not-gadt-type ,scrut-type)
                          (let* ([gadt-name (car gadt-info)]
                                 [scrut-indices (cdr gadt-info)]
                                 [gadt-decl (lookup-gadt gadt-name)])
                                (if (not gadt-decl)
                                    `(error unknown-gadt ,gadt-name)
                                    (dep-synth-gadt-clauses gadt-decl scrut-indices clauses ctx)))))))))

;;; dep-decompose-gadt-type : Type → (Symbol . (List Type)) | #f
(define (dep-decompose-gadt-type t)
  (cond
   [(symbol? t)
    (if (lookup-gadt t)
        (cons t '())
        #f)]
   [(and (pair? t) (symbol? (car t)))
    (if (lookup-gadt (car t))
        (cons (car t) (cdr t))
        #f)]
   [else #f]))

;;; dep-synth-gadt-clauses : GADTDecl × (List Type) × (List Clause) × Context
;;;                          → (Result Type Error)
(define (dep-synth-gadt-clauses gadt-decl scrut-indices clauses ctx)
  (if (null? clauses)
      `(error empty-gadt-case)
      (let loop ([clauses clauses] [types '()])
           (if (null? clauses)
               ;; All clauses checked, unify return types
               (dep-unify-gadt-return-types (reverse types) ctx)
               (let ([result (dep-synth-gadt-clause gadt-decl scrut-indices (car clauses) ctx)])
                    (if (not (eq? (car result) 'ok))
                        result
                        (loop (cdr clauses) (cons (cadr result) types))))))))

;;; dep-synth-gadt-clause : GADTDecl × (List Type) × Clause × Context → (Result Type Error)
(define (dep-synth-gadt-clause gadt-decl scrut-indices clause ctx)
  (let* ([pattern (gadt-clause-pattern clause)]
         [body (gadt-clause-body clause)]
         [ctor-name (gadt-pattern-ctor pattern)]
         [pattern-vars (gadt-pattern-vars pattern)]
         [gadt-name (gadt-name gadt-decl)]
         [index-vars (gadt-index-vars gadt-decl)]
         [ctor-type (gadt-ctor-type gadt-decl ctor-name)])
        (if (not ctor-type)
            `(error unknown-constructor ,ctor-name ,gadt-name)
            (let* ([ctor-return-indices (gadt-ctor-return-indices ctor-type gadt-name)]
                   [refinements (extract-type-equations scrut-indices ctor-return-indices index-vars)])
                  ;; Check for structural mismatch (unreachable branch)
                  (if (eq? refinements 'mismatch)
                      `(error unreachable-gadt-branch
                        (constructor ,ctor-name)
                        (scrutinee-indices ,scrut-indices)
                        (ctor-indices ,ctor-return-indices))
                      (let* ([raw-param-types (gadt-ctor-param-types ctor-type)]
                             [param-types (map (lambda (t) (apply-refinements t refinements)) raw-param-types)]
                             [expected-arity (length param-types)]
                             [actual-arity (length pattern-vars)])
                            (if (not (= expected-arity actual-arity))
                                `(error pattern-arity-mismatch
                                  (constructor ,ctor-name)
                                  (expected ,expected-arity)
                                  (got ,actual-arity))
                                ;; Build refined context
                                (let* ([ctx-with-refs (dep-apply-refinements ctx refinements)]
                                       [ctx-with-vars (dep-bind-pattern-vars ctx-with-refs pattern-vars param-types)])
                                      (dep-synth body ctx-with-vars)))))))))

;;; dep-apply-refinements : Context × (List (Symbol . Type)) → Context
(define (dep-apply-refinements ctx refinements)
  (fold-left (lambda (c ref)
                     (dep-ctx-extend-def c (car ref) 'Type (cdr ref)))
             ctx
             refinements))

;;; dep-bind-pattern-vars : Context × (List Symbol) × (List Type) → Context
(define (dep-bind-pattern-vars ctx vars types)
  (fold-left (lambda (c pair)
                     (dep-ctx-extend c (car pair) (cdr pair)))
             ctx
             (map cons vars types)))

;;; dep-unify-gadt-return-types : (List Type) × Context → (Result Type Error)
(define (dep-unify-gadt-return-types types ctx)
  (if (null? types)
      `(error empty-gadt-case)
      (let ([first-type (car types)])
           (let loop ([remaining (cdr types)])
                (if (null? remaining)
                    `(ok ,first-type)
                    (if (dep-types-equal? first-type (car remaining) ctx)
                        (loop (cdr remaining))
                        `(error gadt-case-branch-type-mismatch
                          (first ,first-type)
                          (other ,(car remaining)))))))))

;;; gadt-define-test-gadts! : → Unit
;;; Helper to set up test GADTs.
(define (gadt-define-test-gadts!)
  (reset-gadt-registry!)
  (register-gadt!
   '(gadt (Expr a)
     [Lit  : (-> Int (Expr Int))]
     [Add  : (-> (Expr Int) (Expr Int) (Expr Int))]
     [Eq   : (-> (Expr Int) (Expr Int) (Expr Bool))]
     [If   : (∀ (b) (-> (Expr Bool) (Expr b) (Expr b) (Expr b)))]))
  (register-gadt!
   '(gadt (Vec (n : Nat) a)
     [VNil  : (Vec 0 a)]
     [VCons : (∀ (m) (-> a (Vec m a) (Vec (succ m) a)))])))

;;; ============================================================
;;; Existential Type Support
;;; ============================================================
;;;
;;; Existential types allow hiding type information behind an interface.
;;; The key operations are:
;;;   - Pack: hide a concrete type inside an existential
;;;   - Unpack: use an existential with the type held abstract (skolemized)

;;; dep-synth-existential : Expr × Context → (Result Type Error)
;;; (∃ ((a : K)) T) : Type when K is valid and T : Type under a:K
(define (dep-synth-existential expr ctx)
  (if (not (existential-well-formed? expr))
      `(error malformed-existential-type ,expr)
      (let* ([var-bindings (cadr expr)]
             [body (caddr expr)])
            ;; Check each binding has a valid kind
            (let loop ([bindings var-bindings] [ctx ctx])
                 (if (null? bindings)
                     ;; All bindings valid, check body is a type
                     (let ([body-check (dep-check-type body ctx)])
                          (if (eq? (car body-check) 'ok)
                              '(ok Type)
                              body-check))
                     (let* ([b (car bindings)]
                            [var (binding-var b)]
                            [kind (binding-type b)])
                           ;; Kind should be Type or a valid kind expression
                           (if (or (eq? kind 'Type) (eq? kind '*))
                               (loop (cdr bindings)
                                     (dep-ctx-extend ctx var kind))
                               ;; Check kind is valid
                               (let ([kind-check (dep-check-type kind ctx)])
                                    (if (not (eq? (car kind-check) 'ok))
                                        `(error invalid-existential-kind ,kind)
                                        (loop (cdr bindings)
                                              (dep-ctx-extend ctx var kind)))))))))))

;;; dep-synth-pack : Expr × Context → (Result Type Error)
;;; (pack WitnessType Value : ExistentialType) : ExistentialType
;;; Check that Value : Body[WitnessType/a]
(define (dep-synth-pack expr ctx)
  (if (not (pack-well-formed? expr))
      `(error malformed-pack ,expr)
      (let* ([witness-type (pack-witness-type expr)]
             [value-expr (pack-value expr)]
             [exist-type (pack-existential-type expr)])
            (if (not (existential-type? exist-type))
                `(error pack-not-existential ,exist-type)
                (let* ([vars (existential-vars exist-type)]
                       [body (existential-body exist-type)])
                      ;; For now, support single variable (common case)
                      (if (not (= (length vars) 1))
                          `(error pack-multi-var-not-yet-supported ,vars)
                          (let* ([var (car vars)]
                                 ;; Substitute witness for hidden var in body
                                 [expected-value-type (subst-type body var witness-type)]
                                 ;; Check witness type is a valid type
                                 [witness-check (dep-check-type witness-type ctx)])
                                (if (not (eq? (car witness-check) 'ok))
                                    witness-check
                                    ;; Check value has expected type
                                    (let ([value-check (dep-check value-expr expected-value-type ctx)])
                                         (if (eq? (car value-check) 'ok)
                                             `(ok ,exist-type)
                                             value-check))))))))))

;;; dep-synth-unpack : Expr × Context → (Result Type Error)
;;; (unpack ((a val) packed-expr) body) : T
;;; where packed-expr : ∃a.S, val:S[skolem/a] in body, and T doesn't mention skolem
(define (dep-synth-unpack expr ctx)
  (if (not (unpack-well-formed? expr))
      `(error malformed-unpack ,expr)
      (let* ([type-var (unpack-type-var expr)]
             [val-var (unpack-val-var expr)]
             [packed-expr (unpack-packed-expr expr)]
             [body (unpack-body expr)]
             ;; Synthesize packed expression
             [packed-synth (dep-synth packed-expr ctx)])
            (if (not (eq? (car packed-synth) 'ok))
                packed-synth
                (let ([packed-type (cadr packed-synth)])
                     (if (not (existential-type? packed-type))
                         `(error unpack-not-existential ,packed-type)
                         (let* ([exist-vars (existential-vars packed-type)]
                                [exist-body (existential-body packed-type)])
                               ;; For now, support single variable
                               (if (not (= (length exist-vars) 1))
                                   `(error unpack-multi-var-not-yet-supported ,exist-vars)
                                   (let* ([exist-var (car exist-vars)]
                                          ;; Generate fresh skolem
                                          [skolem (fresh-skolem exist-var)]
                                          ;; Substitute skolem for existential var in body type
                                          [skolemized-body (subst-type exist-body exist-var skolem)]
                                          ;; Extend context with type var bound to skolem
                                          [ctx-with-skolem (dep-ctx-extend-def ctx type-var 'Type skolem)]
                                          ;; Extend context with value var bound to skolemized type
                                          [ctx-with-val (dep-ctx-extend ctx-with-skolem val-var skolemized-body)]
                                          ;; Synthesize body
                                          [body-synth (dep-synth body ctx-with-val)])
                                         (if (not (eq? (car body-synth) 'ok))
                                             body-synth
                                             ;; Check result type doesn't mention skolem
                                             (let ([result-type (cadr body-synth)])
                                                  (if (type-mentions-skolem? result-type skolem)
                                                      `(error skolem-escape ,skolem ,result-type)
                                                      `(ok ,result-type)))))))))))))
