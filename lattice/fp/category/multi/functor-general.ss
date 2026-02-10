;;; @module functor-general
;;; @requires multi-category

(require 'multi-category)

(doc 'module 'functor-general)
(doc 'description "Inter-Category Functors

A functor F : C → D between (possibly different) categories maps:
  - Objects: F(A) for A ∈ Obj(C) produces F(A) ∈ Obj(D)
  - Morphisms: F(f) for f : A → B produces F(f) : F(A) → F(B)

Functor laws:
  - Identity preservation: F(id_A) = id_{F(A)}
  - Composition preservation: F(g ∘ f) = F(g) ∘ F(f)

The standard functor infrastructure (templates.ss) assumes endofunctors
on Set. This module provides functors between explicit categories.

KEY USE CASE: Free ⊣ Forgetful for algebraic effects
  Free_Σ : Set → Σ-Alg    (build term algebra from generators)
  U_Σ    : Σ-Alg → Set    (extract carrier from algebra)")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ====
;;; General Functor Definition
;;; ====

(doc 'section 'functor-general-definition)

;;; make-functor-general : Symbol × Category × Category ×
;;;                        (Obj_C → Obj_D) × (Mor_C → Mor_D) → FunctorGeneral
;;; Create a functor between two categories.
;;;
;;; Parameters:
;;;   name: Identifying symbol
;;;   source: Source category C
;;;   target: Target category D
;;;   on-obj: Object mapping F : Obj(C) → Obj(D)
;;;   on-mor: Morphism mapping F : Mor(C) → Mor(D)
(define (make-functor-general name source target on-obj on-mor)
  (doc 'type '(-> Symbol Category Category
                  (-> Obj Obj)
                  (-> Mor Mor)
                  FunctorGeneral))
  (list 'functor-general name source target on-obj on-mor))

(define (functor-general? x)
  (doc 'type '(-> Any Boolean))
  (and (pair? x)
       (eq? (car x) 'functor-general)
       (= (length x) 6)))

(define (functor-general-name fg)
  (doc 'type '(-> FunctorGeneral Symbol))
  (if (functor-general? fg) (cadr fg) 'unknown))

(define (functor-general-source fg)
  (doc 'type '(-> FunctorGeneral Category))
  (if (functor-general? fg) (caddr fg) #f))

(define (functor-general-target fg)
  (doc 'type '(-> FunctorGeneral Category))
  (if (functor-general? fg) (cadddr fg) #f))

(define (functor-general-on-obj fg)
  (doc 'type '(-> FunctorGeneral (-> Obj Obj)))
  (if (functor-general? fg) (car (cddddr fg)) #f))

(define (functor-general-on-mor fg)
  (doc 'type '(-> FunctorGeneral (-> Mor Mor)))
  (if (functor-general? fg) (cadr (cddddr fg)) #f))

;;; ====
;;; Functor Operations
;;; ====

;;; functor-apply-obj : FunctorGeneral × Obj → Obj
;;; Apply functor to an object: F(A).
(define (functor-apply-obj fg obj)
  (doc 'type '(-> FunctorGeneral Obj Obj))
  ((functor-general-on-obj fg) obj))

;;; functor-apply-mor : FunctorGeneral × Mor → Mor
;;; Apply functor to a morphism: F(f).
(define (functor-apply-mor fg mor)
  (doc 'type '(-> FunctorGeneral Mor Mor))
  ((functor-general-on-mor fg) mor))

;;; ====
;;; Identity Functor
;;; ====

;;; id-functor-for : Category → FunctorGeneral
;;; The identity functor Id_C : C → C.
(define (id-functor-for cat)
  (doc 'type '(-> Category FunctorGeneral))
  (doc 'description "The identity functor on category C.
Id_C(A) = A, Id_C(f) = f")
  (make-functor-general
   (string->symbol (format "Id-~a" (category-name cat)))
   cat
   cat
   identity  ; on-obj: A ↦ A
   identity)) ; on-mor: f ↦ f

;;; ====
;;; Functor Composition
;;; ====

;;; functor-compose-general : FunctorGeneral × FunctorGeneral → FunctorGeneral
;;; Compose two functors: G ∘ F where F : A → B and G : B → C.
;;; Result: G∘F : A → C
;;; Precondition: target of F = source of G (not checked).
(define (functor-compose-general G F)
  (doc 'type '(-> FunctorGeneral FunctorGeneral FunctorGeneral))
  (doc 'description "Compose functors G ∘ F.
(G ∘ F)(A) = G(F(A)), (G ∘ F)(f) = G(F(f))")
  (make-functor-general
   (string->symbol (format "~a∘~a"
                           (functor-general-name G)
                           (functor-general-name F)))
   (functor-general-source F)  ; source of F
   (functor-general-target G)  ; target of G
   ;; on-obj: (G ∘ F)(A) = G(F(A))
   (lambda (obj)
     ((functor-general-on-obj G)
      ((functor-general-on-obj F) obj)))
   ;; on-mor: (G ∘ F)(f) = G(F(f))
   (lambda (mor)
     ((functor-general-on-mor G)
      ((functor-general-on-mor F) mor)))))

;;; ====
;;; Free Functor: Set → Σ-Alg
;;; ====

(doc 'section 'free-functor)
(doc 'description "Free Functor for Algebraic Signatures

Free_Σ : Set → Σ-Alg

On objects: X ↦ Free(X), the term algebra over generators from X
On morphisms: f : X → Y ↦ Free(f) : Free(X) → Free(Y)
             which maps (gen x) to (gen (f x)) in terms

The free algebra Free(X) has:
  - Carrier: Terms built from generators in X and signature operations
  - Operations: Syntax (constructor application)
  - Equational theory: Laws from the signature (via rewriting)")

;;; make-free-functor-gen : Signature → FunctorGeneral
;;; Create the Free functor for a signature.
;;;
;;; NOTE: This returns a FunctorGeneral (inter-category functor).
;;; For the action on morphisms, we need free-fmap from free-algebra.ss.
;;; Since we're in multi/ we define the structure; actual implementation
;;; needs the signature operations.
(define (make-free-functor-gen sig)
  (doc 'type '(-> Signature FunctorGeneral))
  (doc 'description "The Free functor Free_Σ : Set → Σ-Alg.
Free(X) = term algebra over generators from X.
Free(f) = lift f to terms by mapping generators.")
  (let ([sig-name (if (and (pair? sig) (eq? (car sig) 'signature))
                      (cadr sig)
                      'unknown)])
    (make-functor-general
     (string->symbol (format "Free-~a" sig-name))
     cat-Set
     (cat-Alg sig)
     ;; on-obj: X ↦ Free(X) (term algebra)
     ;; Returns an algebra object. The carrier is 'terms over X'.
     ;; In practice, this is the term algebra construction.
     (lambda (carrier-type)
       ;; Build algebra with syntactic operations
       ;; Carrier elements are terms (gen x), (op term ...), etc.
       (list 'algebra sig (list 'free carrier-type)
             ;; ops: syntactic constructors
             (build-free-algebra-ops sig)))
     ;; on-mor: f : X → Y ↦ Free(f) : Free(X) → Free(Y)
     ;; This is an algebra homomorphism
     (lambda (f)
       ;; Return algebra homomorphism that maps (gen x) to (gen (f x))
       (lambda (term)
         (free-fmap-term sig f term))))))

;;; build-free-algebra-ops : Signature → OpList
;;; Build the syntactic operation implementations for a free algebra.
(define (build-free-algebra-ops sig)
  (doc 'type '(-> Signature OpList))
  (if (not (and (pair? sig) (eq? (car sig) 'signature)))
      '()
      (let ([ops (caddr sig)])  ; signature operations
        (map (lambda (op-entry)
               (let ([op-name (car op-entry)]
                     [arity (cdr op-entry)])
                 (cons op-name
                       (cond
                         [(= arity 0) (lambda () op-name)]
                         [(= arity 1) (lambda (x) (list op-name x))]
                         [(= arity 2) (lambda (x y) (list op-name x y))]
                         [else (lambda args (cons op-name args))]))))
             ops))))

;;; free-fmap-term : Signature × (a → b) × Term → Term
;;; Apply a function to all generators in a term.
(define (free-fmap-term sig f term)
  (doc 'type '(-> Signature (-> a b) Term Term))
  (cond
    ;; Generator: apply f
    [(and (pair? term) (eq? (car term) 'gen) (= (length term) 2))
     (list 'gen (f (cadr term)))]
    ;; 0-arity constant (symbol from signature)
    [(symbol? term) term]
    ;; n-arity operation: recurse into arguments
    [(pair? term)
     (cons (car term)
           (map (lambda (arg) (free-fmap-term sig f arg))
                (cdr term)))]
    ;; Literal: unchanged
    [else term]))

;;; ====
;;; Forgetful Functor: Σ-Alg → Set
;;; ====

(doc 'section 'forgetful-functor)
(doc 'description "Forgetful Functor for Algebraic Signatures

U_Σ : Σ-Alg → Set (Forgetful functor)

On objects: A ↦ |A| (carrier set of algebra)
On morphisms: h : A → B ↦ underlying function h : |A| → |B|

The forgetful functor 'forgets' the algebraic structure,
retaining only the underlying set and function.")

;;; make-forgetful-functor-gen : Signature → FunctorGeneral
;;; Create the Forgetful functor for a signature.
(define (make-forgetful-functor-gen sig)
  (doc 'type '(-> Signature FunctorGeneral))
  (doc 'description "The Forgetful functor U_Σ : Σ-Alg → Set.
U(A) = carrier of A.
U(h) = underlying function of homomorphism h.")
  (let ([sig-name (if (and (pair? sig) (eq? (car sig) 'signature))
                      (cadr sig)
                      'unknown)])
    (make-functor-general
     (string->symbol (format "U-~a" sig-name))
     (cat-Alg sig)
     cat-Set
     ;; on-obj: A ↦ |A| (carrier type)
     ;; Extract carrier from algebra
     (lambda (alg)
       (if (and (pair? alg) (eq? (car alg) 'algebra))
           (caddr alg)  ; carrier field
           'unknown-carrier))
     ;; on-mor: h ↦ underlying function
     ;; For algebra-hom, extract the function
     (lambda (hom)
       (if (and (pair? hom) (eq? (car hom) 'algebra-hom))
           (cadddr hom)  ; function field
           identity)))))

;;; ====
;;; Functor Law Verification
;;; ====

(doc 'section 'verification)

;;; verify-functor-identity : FunctorGeneral × Obj × Any → Boolean
;;; Verify F(id_A) = id_{F(A)} at a test value.
(define (verify-functor-identity fg obj test-value)
  (doc 'type '(-> FunctorGeneral Obj Any Boolean))
  (doc 'description "Verify functor preserves identity morphisms.")
  (let* ([source-cat (functor-general-source fg)]
         [target-cat (functor-general-target fg)]
         [id-source (cat-id source-cat obj)]
         [F-id (functor-apply-mor fg id-source)]
         [F-obj (functor-apply-obj fg obj)]
         [id-target (cat-id target-cat F-obj)])
    (equal? (F-id test-value)
            (id-target test-value))))

;;; verify-functor-composition : FunctorGeneral × Mor × Mor × Any → Boolean
;;; Verify F(g ∘ f) = F(g) ∘ F(f) at a test value.
(define (verify-functor-composition fg g f test-value)
  (doc 'type '(-> FunctorGeneral Mor Mor Any Boolean))
  (doc 'description "Verify functor preserves composition.")
  (let* ([source-cat (functor-general-source fg)]
         [target-cat (functor-general-target fg)]
         [compose-source (category-compose source-cat)]
         [compose-target (category-compose target-cat)]
         [F-gf (functor-apply-mor fg (compose-source g f))]
         [Fg (functor-apply-mor fg g)]
         [Ff (functor-apply-mor fg f)])
    (equal? (F-gf test-value)
            ((compose-target Fg Ff) test-value))))

;;; ====
;;; Bridge to Standard Functors
;;; ====

(doc 'section 'bridge)
(doc 'description "Bridge Functions

Convert between standard endofunctors (from templates.ss) and
general functors. This enables using existing infrastructure
with the multi-category framework.")

;;; functor->functor-general : Functor → FunctorGeneral
;;; Lift a standard endofunctor to a general functor Set → Set.
(define (functor->functor-general func)
  (doc 'type '(-> Functor FunctorGeneral))
  (doc 'description "Convert standard endofunctor to general functor.
The source and target categories are both Set.")
  (let ([name (if (and (pair? func) (> (length func) 2))
                  (caddr func)
                  'F)]
        [fmap (if (pair? func) (cadr func) (lambda (f x) x))])
    (make-functor-general
     name
     cat-Set
     cat-Set
     ;; on-obj: For endofunctors, objects are implicit types
     identity
     ;; on-mor: fmap, but curried differently
     ;; Standard fmap is (f, x) → F(x). We need f → (x → F(f)(x)).
     (lambda (f)
       (lambda (x) (fmap f x))))))

;;; functor-general->functor : FunctorGeneral → Functor
;;; Extract a standard endofunctor from a general functor.
;;; Only works if source = target = Set.
(define (functor-general->functor fg)
  (doc 'type '(-> FunctorGeneral Functor))
  (doc 'description "Convert general functor to standard functor.
Requires source and target to both be Set.")
  (let ([name (functor-general-name fg)]
        [on-mor (functor-general-on-mor fg)])
    ;; Convert on-mor from f → (x → y) to (f, x) → y
    (list 'functor
          (lambda (f x) ((on-mor f) x))
          name)))

;;; ====
;;; Display
;;; ====

;;; functor-general->string : FunctorGeneral → String
(define (functor-general->string fg)
  (doc 'type '(-> FunctorGeneral String))
  (if (functor-general? fg)
      (format "~a : ~a → ~a"
              (functor-general-name fg)
              (category-name (functor-general-source fg))
              (category-name (functor-general-target fg)))
      "Not a functor"))

;;; ====
;;; Exports
;;; ====

(doc 'section 'exports)
(doc 'description "Exports

Functor Definition:
  make-functor-general, functor-general?
  functor-general-name, functor-general-source, functor-general-target
  functor-general-on-obj, functor-general-on-mor

Functor Operations:
  functor-apply-obj, functor-apply-mor
  id-functor-for, functor-compose-general

Free and Forgetful Functors:
  make-free-functor-gen, make-forgetful-functor-gen
  build-free-algebra-ops, free-fmap-term

Verification:
  verify-functor-identity, verify-functor-composition

Bridge:
  functor->functor-general, functor-general->functor

Display:
  functor-general->string")
