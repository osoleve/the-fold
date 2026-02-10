;;; lattice/fp/category/multi/category.ss — Multi-Category
;;; @module multi-category
;;; @requires prelude

(require 'prelude)

(doc 'module 'multi-category)
(doc 'description "Categories as First-Class Objects

This module provides categories as explicit values, enabling:
  - Inter-category functors (F : C → D where C ≠ D)
  - Properly indexed natural transformations
  - Eilenberg-Moore categories for algebraic signatures

The standard category infrastructure assumes all functors are endofunctors
on an implicit 'Set' category. For the Free ⊣ Forgetful adjunction:
  Free_Σ : Set → Σ-Alg
  U_Σ    : Σ-Alg → Set

We need explicit categories to properly type the counit ε_A : Free(U(A)) → A,
which must be indexed by algebras A ∈ Obj(Σ-Alg), not just their carriers.

KEY DEFINITIONS:
  Category C consists of:
    - Obj(C): Collection of objects
    - Mor(C): Collection of morphisms
    - For each f ∈ Mor(C): dom(f), cod(f) ∈ Obj(C)
    - For each A ∈ Obj(C): id_A ∈ Mor(C)
    - Composition: f ∘ g when dom(f) = cod(g)

  Axioms:
    - Identity: id ∘ f = f = f ∘ id
    - Associativity: (f ∘ g) ∘ h = f ∘ (g ∘ h)")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ====
;;; Category Definition
;;; ====

(doc 'section 'category-definition)

;;; make-category : Symbol × (Any → Bool) × (Any → Bool) × ...
;;;                 → Category
;;; Create a category with explicit structure.
;;;
;;; In our Scheme encoding:
;;;   - obj? : Any → Boolean    -- predicate for objects
;;;   - mor? : Any → Boolean    -- predicate for morphisms
;;;   - dom, cod : Mor → Obj    -- source/target of morphisms
;;;   - id-at : Obj → Mor       -- identity morphism at object
;;;   - compose : Mor × Mor → Mor  -- composition (g after f)
;;;
;;; NOTE: We don't represent all morphisms explicitly. In Set, morphisms
;;; are Scheme functions. In Σ-Alg, morphisms are algebra homomorphisms.
(define (make-category name obj? mor? dom cod id-at compose)
  (doc 'type '(-> Symbol
                  (-> Any Boolean)
                  (-> Any Boolean)
                  (-> Mor Obj)
                  (-> Mor Obj)
                  (-> Obj Mor)
                  (-> Mor Mor Mor)
                  Category))
  (list 'category name obj? mor? dom cod id-at compose))

(define (category? x)
  (doc 'type '(-> Any Boolean))
  (and (pair? x)
       (eq? (car x) 'category)
       (= (length x) 8)))

(define (category-name cat)
  (doc 'type '(-> Category Symbol))
  (if (category? cat) (cadr cat) 'unknown))

(define (category-obj? cat)
  (doc 'type '(-> Category (-> Any Boolean)))
  (if (category? cat) (caddr cat) (lambda (_) #f)))

(define (category-mor? cat)
  (doc 'type '(-> Category (-> Any Boolean)))
  (if (category? cat) (cadddr cat) (lambda (_) #f)))

(define (category-dom cat)
  (doc 'type '(-> Category (-> Mor Obj)))
  (if (category? cat) (car (cddddr cat)) #f))

(define (category-cod cat)
  (doc 'type '(-> Category (-> Mor Obj)))
  (if (category? cat) (cadr (cddddr cat)) #f))

(define (category-id-at cat)
  (doc 'type '(-> Category (-> Obj Mor)))
  (if (category? cat) (caddr (cddddr cat)) #f))

(define (category-compose cat)
  (doc 'type '(-> Category (-> Mor Mor Mor)))
  (if (category? cat) (cadddr (cddddr cat)) #f))

;;; ====
;;; Category Operations
;;; ====

;;; cat-id : Category × Obj → Mor
;;; Get the identity morphism at an object.
(define (cat-id cat obj)
  (doc 'type '(-> Category Obj Mor))
  ((category-id-at cat) obj))

;;; cat-compose : Category × Mor × Mor → Mor
;;; Compose two morphisms: (g ∘ f) where cod(f) = dom(g).
(define (cat-compose cat g f)
  (doc 'type '(-> Category Mor Mor Mor))
  ((category-compose cat) g f))

;;; ====
;;; The Category of Sets (Set)
;;; ====

(doc 'section 'cat-set)
(doc 'description "The Category Set

Objects: Types (represented as symbols or type tags)
Morphisms: Functions between types
Identity: The identity function
Composition: Function composition

In our untyped Scheme setting, we can't fully enforce typing.
Objects are represented symbolically, and morphisms are functions.
This is a 'large' category - Scheme functions are our morphisms.")

;;; cat-Set : Category
;;; The category of sets and functions.
(define cat-Set
  (make-category
   'Set
   ;; obj?: Types are represented as symbols
   ;; In practice, we treat any symbol as a valid object
   symbol?
   ;; mor?: Morphisms are procedures
   procedure?
   ;; dom, cod: For functions, we can't infer the domain/codomain
   ;; We use placeholder - in Set these are typically implicit
   (lambda (f) 'Set-object)  ; domain (placeholder)
   (lambda (f) 'Set-object)  ; codomain (placeholder)
   ;; id-at: identity function at any object
   (lambda (obj) identity)
   ;; compose: function composition (g after f)
   (lambda (g f) (lambda (x) (g (f x))))))

;;; cat-Hask : Category
;;; Alias for Set (Haskell category convention).
(define cat-Hask cat-Set)

;;; ====
;;; Eilenberg-Moore Category Construction
;;; ====

(doc 'section 'eilenberg-moore)
(doc 'description "Eilenberg-Moore Category for Algebraic Signatures

Given an algebraic signature Σ (from free-algebra.ss), the
Eilenberg-Moore category Σ-Alg has:

  Objects: Σ-Algebras (carrier set + operation implementations)
  Morphisms: Algebra homomorphisms (structure-preserving maps)
  Identity: The identity homomorphism
  Composition: Function composition (preserves structure)

This is the target category for the Free functor: Set → Σ-Alg.
The counit ε_A : Free(U(A)) → A is indexed by algebras A ∈ Σ-Alg.")

;;; cat-Alg : Signature → Category
;;; Build the Eilenberg-Moore category for a signature.
;;; Objects are algebras over the signature.
;;; Morphisms are algebra homomorphisms.
;;;
;;; NOTE: This depends on algebra?, algebra-hom?, etc. from free-algebra.ss.
;;; Since this is multi-category infrastructure, we define predicates inline.
(define (cat-Alg sig)
  (doc 'type '(-> Signature Category))
  (doc 'description "Construct the category of Σ-algebras for signature Σ.
Objects: Σ-algebras
Morphisms: Algebra homomorphisms")
  (let ([sig-name (if (and (pair? sig) (eq? (car sig) 'signature))
                      (cadr sig)
                      'unknown)])
    (make-category
     (string->symbol (format "~a-Alg" sig-name))
     ;; obj?: Algebras for this signature
     (lambda (x)
       (and (pair? x)
            (eq? (car x) 'algebra)
            (= (length x) 4)
            ;; Check algebra's signature matches
            (let ([alg-sig (cadr x)])
              (and (pair? alg-sig)
                   (eq? (car alg-sig) 'signature)
                   (eq? (cadr alg-sig) sig-name)))))
     ;; mor?: Algebra homomorphisms
     (lambda (x)
       (and (pair? x)
            (eq? (car x) 'algebra-hom)
            (= (length x) 4)))
     ;; dom: Source algebra of homomorphism
     (lambda (hom)
       (if (and (pair? hom) (eq? (car hom) 'algebra-hom))
           (cadr hom)
           #f))
     ;; cod: Target algebra of homomorphism
     (lambda (hom)
       (if (and (pair? hom) (eq? (car hom) 'algebra-hom))
           (caddr hom)
           #f))
     ;; id-at: Identity homomorphism at algebra A
     (lambda (alg)
       (list 'algebra-hom alg alg identity))
     ;; compose: Compose homomorphisms
     (lambda (g f)
       (list 'algebra-hom
             (cadr f)    ; source of f
             (caddr g)   ; target of g
             (lambda (x) ((cadddr g) ((cadddr f) x))))))))

;;; ====
;;; Category Laws Verification
;;; ====

(doc 'section 'verification)

;;; verify-category-identity : Category × Obj × Mor → Boolean
;;; Verify identity law: id ∘ f = f = f ∘ id
(define (verify-category-identity cat obj f test-value)
  (doc 'type '(-> Category Obj Mor Any Boolean))
  (doc 'description "Verify identity morphism law for a specific morphism.
f should be a morphism with domain/codomain = obj.")
  (let ([id-mor (cat-id cat obj)]
        [compose (category-compose cat)])
    (and (equal? ((compose id-mor f) test-value)
                 (f test-value))
         (equal? ((compose f id-mor) test-value)
                 (f test-value)))))

;;; verify-category-associativity : Category × Mor × Mor × Mor × Any → Boolean
;;; Verify associativity: (h ∘ g) ∘ f = h ∘ (g ∘ f)
(define (verify-category-associativity cat h g f test-value)
  (doc 'type '(-> Category Mor Mor Mor Any Boolean))
  (doc 'description "Verify composition associativity for three morphisms.")
  (let ([compose (category-compose cat)])
    (equal? ((compose (compose h g) f) test-value)
            ((compose h (compose g f)) test-value))))

;;; ====
;;; Display
;;; ====

;;; category->string : Category → String
(define (category->string cat)
  (doc 'type '(-> Category String))
  (if (category? cat)
      (format "Category<~a>" (category-name cat))
      "Not a category"))

;;; ====
;;; Exports
;;; ====

(doc 'section 'exports)
(doc 'description "Exports

Category Definition:
  make-category, category?, category-name
  category-obj?, category-mor?
  category-dom, category-cod
  category-id-at, category-compose

Category Operations:
  cat-id, cat-compose

Standard Categories:
  cat-Set, cat-Hask
  cat-Alg (constructor for Eilenberg-Moore category)

Verification:
  verify-category-identity
  verify-category-associativity

Display:
  category->string")
