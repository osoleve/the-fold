;;; core/types/dep-types.ss — Dependent Type Extensions
;;;
;;; Extends the base type system with dependent types:
;;;   - Pi types (Π) — Dependent function types
;;;   - Sigma types (Σ) — Dependent pair types
;;;   - Universe hierarchy (Type₀, Type₁, ...)
;;;
;;; This module provides the syntax and predicates for dependent types.
;;; Normalization and conversion checking is in nbe.ss.
;;; Type checking rules are in dep-infer.ss.
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - types.ss

(load "core/base/prelude.ss")
(load "core/types/types.ss")

;;; ============================================================
;;; Extended Type Grammar
;;; ============================================================
;;;
;;; In addition to types.ss grammar:
;;;
;;;   Type ::= ...
;;;          | (Π ((x : Type)) Type)    ; Pi type (dependent function)
;;;          | (Σ ((x : Type)) Type)    ; Sigma type (dependent pair)
;;;          | Type                      ; Universe (synonym for Type₀)
;;;          | (Type n)                  ; Universe at level n
;;;
;;; The arrow (-> A B) is sugar for (Π ((_ : A)) B) when _ is unused.
;;; The product (× A B) is sugar for (Σ ((_ : A)) B) when _ is unused.

;;; ============================================================
;;; Dependent Type Predicates
;;; ============================================================

;;; pi-type? : Type → Boolean
(define (pi-type? t)
  (and (pair? t) (eq? (car t) 'Π)))

;;; sigma-type? : Type → Boolean
(define (sigma-type? t)
  (and (pair? t) (eq? (car t) 'Σ)))

;;; vec-type? : Type → Boolean
(define (vec-type? t)
  (and (pair? t) (eq? (car t) 'Vec)))

;;; matrix-type? : Type → Boolean
(define (matrix-type? t)
  (and (pair? t) (eq? (car t) 'Matrix)))

;;; universe-type? : Type → Boolean
(define (universe-type? t)
  (or (eq? t 'Type)
      (and (pair? t) (eq? (car t) 'Type))))

;;; equality-type? : Type → Boolean
;;; Check if this is a propositional equality type: (= A x y)
(define (equality-type? t)
  (and (pair? t) (eq? (car t) '=)))

;;; dep-type? : Type → Boolean
;;; Is this a dependent type construct (Pi, Sigma, Universe, Vec, Matrix, or Equality)?
(define (dep-type? t)
  (or (pi-type? t) (sigma-type? t) (universe-type? t) (vec-type? t) (matrix-type? t) (equality-type? t)))

;;; ============================================================
;;; Pi Type Operations
;;; ============================================================

;;; pi-type-well-formed? : SExpr → Boolean
;;; Check if a Pi type expression is well-formed.
;;; Form: (Π ((x : A)) B) or (Π ((x : A) (y : B)) C) for multiple params
(define (pi-type-well-formed? t)
  (and (pair? t)
       (eq? (car t) 'Π)
       (= (length t) 3)
       (let ([bindings (cadr t)]
             [body (caddr t)])
            (and (list? bindings)
                 (not (null? bindings))
                 (andmap typed-binding? bindings)))))

;;; pi-var : Type → Symbol
;;; Get the first bound variable of a Pi type.
(define (pi-var t)
  (if (pi-type? t)
      (caar (cadr t))
      #f))

;;; pi-domain : Type → Type
;;; Get the domain type of a Pi type.
(define (pi-domain t)
  (if (pi-type? t)
      (caddar (cadr t))  ; Third element of first binding
      'Void))

;;; pi-codomain : Type → Type
;;; Get the codomain type of a Pi type (may reference the bound var).
(define (pi-codomain t)
  (if (pi-type? t)
      (caddr t)
      'Void))

;;; pi-bindings : Type → (List (Symbol . Type))
;;; Extract all bindings from a Pi type as alist.
(define (pi-bindings t)
  (if (pi-type? t)
      (map (lambda (b) (cons (car b) (caddr b)))
           (cadr t))
      '()))

;;; ============================================================
;;; Sigma Type Operations
;;; ============================================================

;;; sigma-type-well-formed? : SExpr → Boolean
(define (sigma-type-well-formed? t)
  (and (pair? t)
       (eq? (car t) 'Σ)
       (= (length t) 3)
       (let ([binding (cadr t)]
             [body (caddr t)])
            (and (list? binding)
                 (= (length binding) 1)
                 (typed-binding? (car binding))))))

;;; sigma-var : Type → Symbol
(define (sigma-var t)
  (if (sigma-type? t)
      (caar (cadr t))
      #f))

;;; sigma-fst-type : Type → Type
(define (sigma-fst-type t)
  (if (sigma-type? t)
      (caddar (cadr t))
      'Void))

;;; sigma-snd-type : Type → Type
(define (sigma-snd-type t)
  (if (sigma-type? t)
      (caddr t)
      'Void))

;;; ============================================================
;;; Equality Type Operations
;;; ============================================================
;;;
;;; Propositional equality: (= A x y)
;;;   - A is the type at which equality is asserted
;;;   - x and y are terms of type A being compared
;;;
;;; Introduction:
;;;   refl : (Π ((A : Type)) (Π ((x : A)) (= A x x)))
;;;
;;; Elimination (J, "eliminator" or "transport"):
;;;   J : (Π ((A : Type))
;;;        (Π ((P : (Π ((x : A)) (Π ((y : A)) (Π ((_ : (= A x y))) Type)))))
;;;         (Π ((d : (Π ((x : A)) (P x x refl))))
;;;          (Π ((x : A))
;;;           (Π ((y : A))
;;;            (Π ((p : (= A x y)))
;;;             (P x y p)))))))
;;;
;;; Computation rule:
;;;   J A P d x x refl ≡ d x
;;;
;;; This implements propositional equality as found in Martin-Löf Type Theory.

;;; equality-type-well-formed? : SExpr → Boolean
;;; Check if an equality type expression is well-formed: (= A x y)
(define (equality-type-well-formed? t)
  (and (pair? t)
       (eq? (car t) '=)
       (= (length t) 4)))

;;; equality-carrier : Type → Type
;;; Get the carrier type A from (= A x y)
(define (equality-carrier t)
  (if (equality-type? t)
      (cadr t)
      'Void))

;;; equality-lhs : Type → Expr
;;; Get the left-hand side x from (= A x y)
(define (equality-lhs t)
  (if (equality-type? t)
      (caddr t)
      #f))

;;; equality-rhs : Type → Expr
;;; Get the right-hand side y from (= A x y)
(define (equality-rhs t)
  (if (equality-type? t)
      (cadddr t)
      #f))

;;; ============================================================
;;; Universe Operations
;;; ============================================================

;;; universe-level : Type → Nat
(define (universe-level t)
  (cond
   [(eq? t 'Type) 0]
   [(and (pair? t) (eq? (car t) 'Type)) (cadr t)]
   [else 0]))

;;; universe-max : Type × Type → Type
;;; The maximum universe level of two types.
(define (universe-max u1 u2)
  (let ([l1 (universe-level u1)]
        [l2 (universe-level u2)])
       (if (= (max l1 l2) 0)
           'Type
           `(Type ,(max l1 l2)))))

;;; universe-succ : Type → Type
;;; The successor universe.
(define (universe-succ u)
  `(Type ,(+ 1 (universe-level u))))

;;; ============================================================
;;; Typed Binding Helpers
;;; ============================================================

;;; typed-binding? : SExpr → Boolean
;;; Check if this is a valid typed binding: (x : T)
(define (typed-binding? b)
  (and (list? b)
       (= (length b) 3)
       (symbol? (car b))
       (eq? (cadr b) ':)))

;;; binding-var : Binding → Symbol
(define (binding-var b)
  (car b))

;;; binding-type : Binding → Type
(define (binding-type b)
  (caddr b))

;;; ============================================================
;;; Dependent Type Constructors
;;; ============================================================

;;; t-pi : Symbol × Type × Type → Type
;;; Construct a Pi type (dependent function type).
;;; (t-pi 'n 'Nat '(Vec Int n)) → (Π ((n : Nat)) (Vec Int n))
(define (t-pi var domain codomain)
  `(Π ((,var : ,domain)) ,codomain))

;;; t-pi* : ((Symbol . Type) ...) × Type → Type
;;; Construct a multi-parameter Pi type from an alist of bindings.
(define (t-pi* bindings body)
  (if (null? bindings)
      body
      (let ([binding (car bindings)])
           (t-pi (car binding) (cdr binding)
                 (t-pi* (cdr bindings) body)))))

;;; t-sigma : Symbol × Type × Type → Type
;;; Construct a Sigma type (dependent pair type).
(define (t-sigma var fst-type snd-type)
  `(Σ ((,var : ,fst-type)) ,snd-type))

;;; t-type : [Nat] → Type
;;; Construct a universe type.
(define (t-type . args)
  (if (null? args)
      'Type
      (if (= (car args) 0)
          'Type
          `(Type ,(car args)))))

;;; t-eq : Type × Expr × Expr → Type
;;; Construct an equality type (propositional equality).
;;; (t-eq 'Nat 'x 'y) → (= Nat x y)
(define (t-eq carrier lhs rhs)
  `(= ,carrier ,lhs ,rhs))

;;; ============================================================
;;; Equality Proof Term Predicates
;;; ============================================================
;;;
;;; refl is the canonical proof of (= A x x)
;;; J is the eliminator for equality proofs

;;; refl-term? : Expr → Boolean
;;; Check if this is a refl proof term: (refl A x) or just 'refl
(define (refl-term? e)
  (or (eq? e 'refl)
      (and (pair? e) (eq? (car e) 'refl))))

;;; refl-type : Expr → Type | #f
;;; Get the type annotation from (refl A x), returns #f for bare 'refl
(define (refl-carrier e)
  (cond
   [(eq? e 'refl) #f]
   [(and (pair? e) (eq? (car e) 'refl) (>= (length e) 2))
    (cadr e)]
   [else #f]))

;;; refl-witness : Expr → Expr | #f
;;; Get the witnessed term from (refl A x), returns #f for bare 'refl
(define (refl-witness e)
  (cond
   [(eq? e 'refl) #f]
   [(and (pair? e) (eq? (car e) 'refl) (= (length e) 3))
    (caddr e)]
   [else #f]))

;;; j-term? : Expr → Boolean
;;; Check if this is a J eliminator application: (J A P d x y p)
(define (j-term? e)
  (and (pair? e) (eq? (car e) 'J)))

;;; ============================================================
;;; Free Variables with Dependent Types
;;; ============================================================

;;; dep-free-tvars : Type → (List Symbol)
;;; Collect free type variables, handling dependent types.
(define (dep-free-tvars t)
  (dep-free-tvars-with '() t))

(define (dep-free-tvars-with bound t)
  (cond
   [(type-var? t)
    (if (memq t bound) '() (list t))]
   [(or (base-type? t) (hole? t) (universe-type? t)) '()]
   [(not (pair? t)) '()]
   
   ;; Pi type: (Π ((x : A)) B) - x is bound in B
   [(eq? (car t) 'Π)
    (let* ([bindings (cadr t)]
           [body (caddr t)]
           [domain-vars (apply append
                               (map (lambda (b)
                                            (dep-free-tvars-with bound (binding-type b)))
                                    bindings))]
           [bound-vars (map binding-var bindings)]
           [new-bound (append bound-vars bound)]
           [body-vars (dep-free-tvars-with new-bound body)])
          (append domain-vars body-vars))]
   
   ;; Sigma type: (Σ ((x : A)) B) - x is bound in B
   [(eq? (car t) 'Σ)
    (let* ([binding (car (cadr t))]
           [body (caddr t)]
           [domain-vars (dep-free-tvars-with bound (binding-type binding))]
           [new-bound (cons (binding-var binding) bound)]
           [body-vars (dep-free-tvars-with new-bound body)])
          (append domain-vars body-vars))]
   
   ;; Fall back to base type handling
   [(eq? (car t) '∀)
    (let ([new-bound (append (cadr t) bound)])
         (dep-free-tvars-with new-bound (caddr t)))]
   [(eq? (car t) 'μ)
    (let ([new-bound (cons (cadr t) bound)])
         (dep-free-tvars-with new-bound (caddr t)))]
   [else
    (apply append (map (lambda (sub) (dep-free-tvars-with bound sub)) (cdr t)))]))

;;; ============================================================
;;; Substitution with Dependent Types (Capture-Avoiding)
;;; ============================================================

;;; fresh-var : Symbol × (List Symbol) → Symbol
;;; Generate a fresh variable name that is not in the avoid list.
(define (fresh-var base avoid)
  (let loop ([n 0])
       (let ([candidate (string->symbol
                         (string-append (symbol->string base)
                                        (number->string n)))])
            (if (memq candidate avoid)
                (loop (+ n 1))
                candidate))))

;;; dep-free-vars : Type → (List Symbol)
;;; Collect all free variables in a type (not just type vars, but term vars too).
(define (dep-free-vars t)
  (dep-free-vars-with '() t))

(define (dep-free-vars-with bound t)
  (cond
   [(symbol? t)
    (if (or (memq t bound) (base-type? t))
        '()
        (list t))]
   [(not (pair? t)) '()]
   [(or (hole? t) (universe-type? t)) '()]
   
   ;; Pi type: (Π ((x : A) ...) B) - bindings are bound in later bindings and body
   [(eq? (car t) 'Π)
    (let* ([bindings (cadr t)]
           [body (caddr t)])
          (dep-free-vars-in-bindings-and-body bound bindings body))]
   
   ;; Sigma type: (Σ ((x : A)) B) - x is bound in B
   [(eq? (car t) 'Σ)
    (let* ([binding (car (cadr t))]
           [body (caddr t)]
           [domain-vars (dep-free-vars-with bound (binding-type binding))]
           [new-bound (cons (binding-var binding) bound)]
           [body-vars (dep-free-vars-with new-bound body)])
          (append domain-vars body-vars))]
   
   ;; Forall: (∀ (vars ...) body)
   [(eq? (car t) '∀)
    (let ([new-bound (append (cadr t) bound)])
         (dep-free-vars-with new-bound (caddr t)))]
   
   ;; Mu: (μ var body)
   [(eq? (car t) 'μ)
    (let ([new-bound (cons (cadr t) bound)])
         (dep-free-vars-with new-bound (caddr t)))]
   
   ;; Lambda: (fn (x) body) or (lambda (x) body)
   [(or (eq? (car t) 'fn) (eq? (car t) 'lambda))
    (let ([params (cadr t)]
          [body (caddr t)])
         (let ([new-bound (if (list? params)
                              (append params bound)
                              (cons params bound))])
              (dep-free-vars-with new-bound body)))]
   
   [else
    (apply append (map (lambda (sub) (dep-free-vars-with bound sub)) (cdr t)))]))

;;; dep-free-vars-in-bindings-and-body : (List Symbol) × (List Binding) × Type → (List Symbol)
;;; Helper for Pi types: each binding's var is bound in subsequent bindings and body.
(define (dep-free-vars-in-bindings-and-body bound bindings body)
  (if (null? bindings)
      (dep-free-vars-with bound body)
      (let* ([b (car bindings)]
             [bvar (binding-var b)]
             [btype (binding-type b)]
             [domain-vars (dep-free-vars-with bound btype)]
             [new-bound (cons bvar bound)]
             [rest-vars (dep-free-vars-in-bindings-and-body new-bound (cdr bindings) body)])
            (append domain-vars rest-vars))))

;;; dep-subst-type : Type × Symbol × Type → Type
;;; Substitute var with replacement in type, with capture-avoiding substitution.
;;; When substituting into a binder, if the bound variable would capture free
;;; variables in the replacement, we alpha-rename the binder first.
(define (dep-subst-type type var replacement)
  (cond
   [(eq? type var) replacement]
   [(or (base-type? type) (hole? type) (universe-type? type)) type]
   [(symbol? type) type]  ; Other symbols (not the var we're substituting)
   [(not (pair? type)) type]
   
   ;; Pi type: substitute in bindings and body with capture-avoidance
   [(eq? (car type) 'Π)
    (let* ([bindings (cadr type)]
           [body (caddr type)]
           [replacement-fv (dep-free-vars replacement)])
          (dep-subst-pi-bindings bindings body var replacement replacement-fv))]
   
   ;; Sigma type: substitute with capture-avoidance
   [(eq? (car type) 'Σ)
    (let* ([binding (car (cadr type))]
           [body (caddr type)]
           [bound-var (binding-var binding)]
           [domain (binding-type binding)]
           [replacement-fv (dep-free-vars replacement)])
          (cond
           ;; var is shadowed - substitute only in domain
           [(eq? var bound-var)
            `(Σ ((,bound-var : ,(dep-subst-type domain var replacement)))
              ,body)]
           ;; bound-var would capture - alpha-rename first
           [(memq bound-var replacement-fv)
            (let* ([all-vars (append replacement-fv (dep-free-vars body))]
                   [fresh (fresh-var bound-var all-vars)]
                   [renamed-body (dep-subst-type body bound-var fresh)]
                   [new-domain (dep-subst-type domain var replacement)]
                   [new-body (dep-subst-type renamed-body var replacement)])
                  `(Σ ((,fresh : ,new-domain)) ,new-body))]
           ;; Safe to substitute
           [else
            `(Σ ((,bound-var : ,(dep-subst-type domain var replacement)))
              ,(dep-subst-type body var replacement))]))]
   
   ;; Forall - handle binding with capture-avoidance
   [(eq? (car type) '∀)
    (let ([bound-vars (cadr type)]
          [body (caddr type)])
         (if (memq var bound-vars)
             type  ; var is shadowed
             (let ([replacement-fv (dep-free-vars replacement)])
                  (if (ormap (lambda (bv) (memq bv replacement-fv)) bound-vars)
                      ;; Would capture - need to rename
                      (let* ([all-vars (append replacement-fv (dep-free-vars body))]
                             [renamed-vars-body
                              (fold-left
                               (lambda (acc bv)
                                       (let ([vars (car acc)]
                                             [b (cdr acc)])
                                            (if (memq bv replacement-fv)
                                                (let ([fresh (fresh-var bv all-vars)])
                                                     (cons (cons fresh vars)
                                                           (dep-subst-type b bv fresh)))
                                                (cons (cons bv vars) b))))
                               (cons '() body)
                               bound-vars)]
                             [new-vars (reverse (car renamed-vars-body))]
                             [renamed-body (cdr renamed-vars-body)])
                            `(∀ ,new-vars ,(dep-subst-type renamed-body var replacement)))
                      ;; Safe to substitute
                      `(∀ ,bound-vars ,(dep-subst-type body var replacement))))))]
   
   ;; Mu - handle binding with capture-avoidance
   [(eq? (car type) 'μ)
    (let ([bound-var (cadr type)]
          [body (caddr type)])
         (cond
          [(eq? var bound-var) type]  ; var is shadowed
          [(memq bound-var (dep-free-vars replacement))
           ;; Would capture - rename
           (let* ([all-vars (append (dep-free-vars replacement) (dep-free-vars body))]
                  [fresh (fresh-var bound-var all-vars)]
                  [renamed-body (dep-subst-type body bound-var fresh)])
                 `(μ ,fresh ,(dep-subst-type renamed-body var replacement)))]
          [else
           `(μ ,bound-var ,(dep-subst-type body var replacement))]))]
   
   ;; Default recursive case
   [else
    (cons (car type)
          (map (lambda (sub) (dep-subst-type sub var replacement)) (cdr type)))]))

;;; dep-subst-pi-bindings : (List Binding) × Type × Symbol × Type × (List Symbol) → Type
;;; Substitute through Pi type bindings with proper capture-avoidance.
;;; Each binding's var becomes bound for subsequent bindings and the body.
(define (dep-subst-pi-bindings bindings body var replacement replacement-fv)
  (if (null? bindings)
      ;; No more bindings - just substitute in body
      `(Π () ,(dep-subst-type body var replacement))
      (let* ([b (car bindings)]
             [bvar (binding-var b)]
             [btype (binding-type b)]
             [rest (cdr bindings)])
            (cond
             ;; var is shadowed by this binding
             [(eq? var bvar)
              ;; Substitute only in this binding's type, leave rest alone
              `(Π ,(cons (list bvar ': (dep-subst-type btype var replacement))
                         rest)
                ,body)]
             ;; bvar would capture free vars in replacement - alpha-rename
             [(memq bvar replacement-fv)
              (let* ([all-vars (append replacement-fv
                                       (dep-free-vars body)
                                       (apply append (map (lambda (b) (dep-free-vars (binding-type b))) rest)))]
                     [fresh (fresh-var bvar all-vars)]
                     ;; Rename bvar to fresh in rest of bindings and body
                     [renamed-rest (map (lambda (rb)
                                                (list (binding-var rb) ':
                                                      (dep-subst-type (binding-type rb) bvar fresh)))
                                        rest)]
                     [renamed-body (dep-subst-type body bvar fresh)]
                     ;; Now substitute in this binding's type
                     [new-btype (dep-subst-type btype var replacement)]
                     ;; Recursively process rest with fresh var in replacement-fv
                     [new-replacement-fv (cons fresh (remove bvar replacement-fv))]
                     [result (dep-subst-pi-bindings renamed-rest renamed-body var replacement new-replacement-fv)])
                    ;; Prepend this binding to result
                    `(Π ,(cons (list fresh ': new-btype) (cadr result))
                      ,(caddr result)))]
             ;; Safe to substitute
             [else
              (let* ([new-btype (dep-subst-type btype var replacement)]
                     [result (dep-subst-pi-bindings rest body var replacement replacement-fv)])
                    `(Π ,(cons (list bvar ': new-btype) (cadr result))
                      ,(caddr result)))]))))

;;; remove : α × (List α) → (List α)
;;; Remove first occurrence of element from list.
(define (remove x lst)
  (cond
   [(null? lst) '()]
   [(equal? x (car lst)) (cdr lst)]
   [else (cons (car lst) (remove x (cdr lst)))]))

;;; ============================================================
;;; Type Display with Dependent Types
;;; ============================================================

;;; dep-type->string : Type → String
;;; Pretty-print a type, including dependent types.
(define (dep-type->string t)
  (cond
   [(symbol? t) (symbol->string t)]
   [(eq? t '?) "?"]
   [(and (pair? t) (eq? (car t) '?))
    (string-append "?" (symbol->string (cadr t)))]
   
   ;; Pi type: Π(x : A). B
   [(pi-type? t)
    (let ([bindings (cadr t)]
          [body (caddr t)])
         (string-append "Π"
                        (join-strings ""
                                      (map (lambda (b)
                                                   (string-append "(" (symbol->string (binding-var b))
                                                                  " : " (dep-type->string (binding-type b)) ")"))
                                           bindings))
                        ". "
                        (dep-type->string body)))]
   
   ;; Sigma type: Σ(x : A). B
   [(sigma-type? t)
    (let ([binding (car (cadr t))]
          [body (caddr t)])
         (string-append "Σ(" (symbol->string (binding-var binding))
                        " : " (dep-type->string (binding-type binding))
                        "). " (dep-type->string body)))]
   
   ;; Universe
   [(universe-type? t)
    (if (eq? t 'Type)
        "Type"
        (string-append "Type" (number->string (universe-level t))))]
   
   ;; Vec n A
   [(vec-type? t)
    (string-append "Vec(" (format "~a" (cadr t)) ", " (dep-type->string (caddr t)) ")")]
   
   ;; Matrix m n A
   [(matrix-type? t)
    (string-append "Matrix(" (format "~a" (cadr t)) "x" (format "~a" (caddr t)) ", " (dep-type->string (cadddr t)) ")")]
   
   ;; Equality type (= A x y)
   [(equality-type? t)
    (string-append (format "~a" (equality-lhs t))
                   " ≡ "
                   (format "~a" (equality-rhs t))
                   " : "
                   (dep-type->string (equality-carrier t)))]
   
   ;; Arrow type
   [(and (pair? t) (eq? (car t) '->))
    (string-append "("
                   (join-strings " → " (map dep-type->string (cdr t)))
                   ")")]
   
   ;; Product type
   [(and (pair? t) (eq? (car t) '×))
    (string-append "("
                   (join-strings " × " (map dep-type->string (cdr t)))
                   ")")]
   
   ;; Other compound types
   [(pair? t)
    (string-append "("
                   (join-strings " " (map dep-type->string t))
                   ")")]
   
   [else (format "~s" t)]))

;;; ============================================================
;;; Type Wellformedness with Dependent Types
;;; ============================================================

;;; dep-type? predicate (extended version of type?)
;;; Note: This is a simple syntax check, not a kinding check.
(define (well-formed-dep-type? t)
  (cond
   [(base-type? t) #t]
   [(eq? t '?) #t]
   [(eq? t 'Type) #t]
   [(and (pair? t) (eq? (car t) '?)) #t]
   [(type-var? t) #t]
   [(not (pair? t)) #f]
   
   [(eq? (car t) 'Π) (pi-type-well-formed? t)]
   [(eq? (car t) 'Σ) (sigma-type-well-formed? t)]
   [(eq? (car t) 'Vec) (and (= (length t) 3) (well-formed-dep-type? (caddr t)))]
   [(eq? (car t) 'Matrix) (and (= (length t) 4) (well-formed-dep-type? (cadddr t)))]
   [(eq? (car t) 'Type) (and (= (length t) 2) (integer? (cadr t)) (>= (cadr t) 0))]
   [(eq? (car t) '=) (equality-type-well-formed? t)]
   
   ;; Delegate to base type? for other forms
   [else (type? t)]))
