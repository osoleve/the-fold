;;; fabric/stitches/dep-types.ss — Dependent Type Extensions
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

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/types.ss")

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

;;; dep-type? : Type → Boolean
;;; Is this a dependent type construct (Pi, Sigma, Universe, Vec, or Matrix)?
(define (dep-type? t)
  (or (pi-type? t) (sigma-type? t) (universe-type? t) (vec-type? t) (matrix-type? t)))

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
;;; Substitution with Dependent Types
;;; ============================================================

;;; dep-subst-type : Type × Symbol × Type → Type
;;; Substitute var with replacement in type, handling dependent types.
(define (dep-subst-type type var replacement)
  (cond
   [(eq? type var) replacement]
   [(or (base-type? type) (hole? type) (universe-type? type)) type]
   [(type-var? type) type]
   [(not (pair? type)) type]
   
   ;; Pi type: substitute in domain and codomain (unless shadowed)
   [(eq? (car type) 'Π)
    (let* ([bindings (cadr type)]
           [body (caddr type)]
           [bound-vars (map binding-var bindings)])
          (if (memq var bound-vars)
              ;; var is shadowed - substitute only in domains before shadow
              (let ([new-bindings (dep-subst-bindings-until bindings var replacement)])
                   `(Π ,new-bindings ,body))
              ;; var not shadowed - substitute everywhere
              (let ([new-bindings (map (lambda (b)
                                               (list (binding-var b) ':
                                                     (dep-subst-type (binding-type b) var replacement)))
                                       bindings)]
                    [new-body (dep-subst-type body var replacement)])
                   `(Π ,new-bindings ,new-body))))]
   
   ;; Sigma type: substitute in both components (unless shadowed)
   [(eq? (car type) 'Σ)
    (let* ([binding (car (cadr type))]
           [body (caddr type)]
           [bound-var (binding-var binding)])
          (if (eq? var bound-var)
              ;; var is shadowed - substitute only in domain
              `(Σ ((,bound-var : ,(dep-subst-type (binding-type binding) var replacement)))
                ,body)
              ;; var not shadowed - substitute in both
              `(Σ ((,bound-var : ,(dep-subst-type (binding-type binding) var replacement)))
                ,(dep-subst-type body var replacement))))]
   
   ;; Forall and mu - handle binding
   [(eq? (car type) '∀)
    (if (memq var (cadr type))
        type
        `(∀ ,(cadr type) ,(dep-subst-type (caddr type) var replacement)))]
   [(eq? (car type) 'μ)
    (if (eq? var (cadr type))
        type
        `(μ ,(cadr type) ,(dep-subst-type (caddr type) var replacement)))]
   
   ;; Default recursive case
   [else
    (cons (car type)
          (map (lambda (sub) (dep-subst-type sub var replacement)) (cdr type)))]))

;;; dep-subst-bindings-until : (List Binding) × Symbol × Type → (List Binding)
(define (dep-subst-bindings-until bindings var replacement)
  (if (null? bindings)
      '()
      (let* ([b (car bindings)]
             [bvar (binding-var b)]
             [btype (binding-type b)])
            (if (eq? bvar var)
                ;; This binding shadows var - substitute in its type but stop
                (cons (list bvar ': (dep-subst-type btype var replacement))
                      (cdr bindings))
                ;; Not shadowed yet - substitute and continue
                (cons (list bvar ': (dep-subst-type btype var replacement))
                      (dep-subst-bindings-until (cdr bindings) var replacement))))))

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
   
   ;; Delegate to base type? for other forms
   [else (type? t)]))
