(load "lattice/fp/category/monad-derivation.ss")

(doc 'module 'kleisli)
(doc 'description "Kleisli Categories

For a monad M = (T, η, μ) on category C, the Kleisli category K(M) is:
  - Objects: same as C (types in our setting)
  - Morphisms A → B in K(M): morphisms A → T(B) in C (Kleisli arrows)
  - Identity: η_A : A → T(A) (the monad's return)
  - Composition: for f : A → T(B) and g : B → T(C),
      g ∘_K f = μ_C ∘ T(g) ∘ f = bind f g (in monadic terms)

The 'fish operator' (>=>) is Kleisli composition: (f >=> g) x = f x >>= g

This provides the 'computational interpretation' of monads:
  - Kleisli arrows are effectful computations
  - Kleisli composition sequences effectful computations
  - The monad laws become category laws in K(M)

Dually, every adjunction F ⊣ G gives rise to both:
  - Kleisli category: arrows are A → G(F(A))
  - Eilenberg-Moore category: algebras for the monad (not in this module)")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'kleisli-category-construction)
(doc 'description "Kleisli Category Construction

A Kleisli category K(M) bundles:
  - The underlying monad ops (MonadOps)
  - Identity morphisms (return)
  - Composition (the fish operator)

Morphisms in K(M) are represented as ordinary Scheme procedures
of type A → M(B). The category structure provides composition.")

;;; make-kleisli-category : MonadOps → KleisliCategory
;;; Construct the Kleisli category for a monad.
(define (make-kleisli-category monad-ops)
  (doc 'type '(-> MonadOps KleisliCategory))
  (doc 'description "Create the Kleisli category K(M) for monad M")
  (let ([return-fn (monad-ops-return monad-ops)]
        [bind-fn (monad-ops-bind monad-ops)])
    (list 'kleisli-category
          (monad-ops-name monad-ops)
          monad-ops
          return-fn
          ;; Kleisli composition: (f >=> g) x = f x >>= g
          (lambda (f g)
            (lambda (x)
              (bind-fn (f x) g))))))

(define (kleisli-category? x)
  (doc 'type '(-> Any Boolean))
  (and (pair? x)
       (eq? (car x) 'kleisli-category)
       (= (length x) 5)))

(define (kleisli-category-name kc)
  (doc 'type '(-> KleisliCategory Symbol))
  (if (kleisli-category? kc) (cadr kc) 'unknown))

(define (kleisli-category-monad kc)
  (doc 'type '(-> KleisliCategory MonadOps))
  (if (kleisli-category? kc) (caddr kc) #f))

(define (kleisli-identity kc)
  (doc 'type '(-> KleisliCategory (-> a (M a))))
  (doc 'description "Get the identity morphism (return) for the Kleisli category")
  (if (kleisli-category? kc) (cadddr kc) #f))

(define (kleisli-compose kc)
  (doc 'type '(-> KleisliCategory (-> (-> a (M b)) (-> b (M c)) (-> a (M c)))))
  (doc 'description "Get the composition operator (>=>) for the Kleisli category")
  (if (kleisli-category? kc) (car (cddddr kc)) #f))

(doc 'section 'kleisli-arrow-operations)
(doc 'description "Kleisli Arrow Operations

Convenience functions for working with Kleisli arrows.
A Kleisli arrow is just a function A → M(B), but these
helpers make intent clearer and provide category-theoretic
operations.")

;;; kleisli-id : KleisliCategory → (A → M A)
;;; The identity Kleisli arrow at any type.
(define (kleisli-id kc)
  (doc 'type '(-> KleisliCategory (-> a (M a))))
  (kleisli-identity kc))

;;; kleisli->>> : KleisliCategory × (A → M B) × (B → M C) → (A → M C)
;;; Compose two Kleisli arrows (left-to-right, like >>>)
;;; (f >>> g) x = f x >>= g
(define (kleisli->>> kc f g)
  (doc 'type '(-> KleisliCategory (-> a (M b)) (-> b (M c)) (-> a (M c))))
  (doc 'description "Left-to-right Kleisli composition: f >>> g = f >=> g")
  ((kleisli-compose kc) f g))

;;; kleisli-<<< : KleisliCategory × (B → M C) × (A → M B) → (A → M C)
;;; Compose two Kleisli arrows (right-to-left, like <<<)
;;; (g <<< f) x = f x >>= g
(define (kleisli-<<< kc g f)
  (doc 'type '(-> KleisliCategory (-> b (M c)) (-> a (M b)) (-> a (M c))))
  (doc 'description "Right-to-left Kleisli composition: g <<< f = f >=> g")
  ((kleisli-compose kc) f g))

;;; fish : KleisliCategory × (A → M B) × (B → M C) → (A → M C)
;;; Alias for kleisli->>> using the traditional 'fish' name
(define (fish kc f g)
  (doc 'type '(-> KleisliCategory (-> a (M b)) (-> b (M c)) (-> a (M c))))
  (doc 'description "The 'fish' operator (>=>): Kleisli composition")
  (kleisli->>> kc f g))

;;; lift : KleisliCategory × (A → B) → (A → M B)
;;; Lift a pure function into a Kleisli arrow.
;;; lift f = return . f
(define (kleisli-lift kc f)
  (doc 'type '(-> KleisliCategory (-> a b) (-> a (M b))))
  (doc 'description "Lift a pure function to a Kleisli arrow: lift f = return . f")
  (let ([return-fn (kleisli-identity kc)])
    (lambda (x)
      (return-fn (f x)))))

;;; kleisli-map : KleisliCategory × (A → B) × M A → M B
;;; Apply a pure function inside the monad (fmap via Kleisli)
(define (kleisli-map kc f ma)
  (doc 'type '(-> KleisliCategory (-> a b) (M a) (M b)))
  (let* ([monad (kleisli-category-monad kc)]
         [fmap-fn (monad-ops-fmap monad)])
    (fmap-fn f ma)))

(doc 'section 'kleisli-law-verification)
(doc 'description "Kleisli Category Law Verification

The Kleisli category K(M) must satisfy category laws:
  1. Left identity:  return >=> f = f
  2. Right identity: f >=> return = f
  3. Associativity:  (f >=> g) >=> h = f >=> (g >=> h)

These correspond exactly to the monad laws! This is the deep
connection: monad laws ARE category laws for the Kleisli category.

Note: For function-based monads (State, Reader), use the -with-eq
variants that accept a custom equality predicate.")

;;; verify-kleisli-left-identity : KleisliCategory × (A → M B) × A → Boolean
;;; Check: return >=> f = f (at a specific input)
;;; Uses equal? - works for data-based monads (List, Maybe)
(define (verify-kleisli-left-identity kc f a)
  (verify-kleisli-left-identity-with-eq kc f a equal?))

;;; verify-kleisli-left-identity-with-eq : KleisliCategory × (A → M B) × A × Eq → Boolean
(define (verify-kleisli-left-identity-with-eq kc f a eq?)
  (let* ([return-fn (kleisli-identity kc)]
         [compose (kleisli-compose kc)]
         [lhs ((compose return-fn f) a)]
         [rhs (f a)])
    (eq? lhs rhs)))

;;; verify-kleisli-right-identity : KleisliCategory × (A → M B) × A → Boolean
;;; Check: f >=> return = f (at a specific input)
(define (verify-kleisli-right-identity kc f a)
  (verify-kleisli-right-identity-with-eq kc f a equal?))

(define (verify-kleisli-right-identity-with-eq kc f a eq?)
  (let* ([return-fn (kleisli-identity kc)]
         [compose (kleisli-compose kc)]
         [lhs ((compose f return-fn) a)]
         [rhs (f a)])
    (eq? lhs rhs)))

;;; verify-kleisli-associativity : KleisliCategory × Arrows × A → Boolean
;;; Check: (f >=> g) >=> h = f >=> (g >=> h) (at a specific input)
(define (verify-kleisli-associativity kc f g h a)
  (verify-kleisli-associativity-with-eq kc f g h a equal?))

(define (verify-kleisli-associativity-with-eq kc f g h a eq?)
  (let* ([compose (kleisli-compose kc)]
         [lhs ((compose (compose f g) h) a)]
         [rhs ((compose f (compose g h)) a)])
    (eq? lhs rhs)))

;;; verify-kleisli-laws : KleisliCategory × Arrows × TestValue → Boolean
;;; Verify all three category laws.
(define (verify-kleisli-laws kc f g h a)
  (verify-kleisli-laws-with-eq kc f g h a equal?))

(define (verify-kleisli-laws-with-eq kc f g h a eq?)
  (and (verify-kleisli-left-identity-with-eq kc f a eq?)
       (verify-kleisli-right-identity-with-eq kc f a eq?)
       (verify-kleisli-associativity-with-eq kc f g h a eq?)))

(doc 'section 'kleisli-combinators)
(doc 'description "Kleisli Combinators

Common combinators for working with Kleisli arrows.
These correspond to standard monad combinators but
expressed in terms of Kleisli composition.")

;;; kleisli-sequence : KleisliCategory × [A → M A] → (A → M A)
;;; Sequence a list of Kleisli arrows into one.
;;; sequence [] = return
;;; sequence [f] = f
;;; sequence (f:fs) = f >=> sequence fs
(define (kleisli-sequence kc arrows)
  (doc 'type '(-> KleisliCategory (List (-> a (M a))) (-> a (M a))))
  (if (null? arrows)
      (kleisli-identity kc)
      (let loop ([fs arrows])
        (if (null? (cdr fs))
            (car fs)
            (kleisli->>> kc (car fs) (loop (cdr fs)))))))

;;; kleisli-pipe : KleisliCategory × A × [A → M A] → M A
;;; Feed a value through a sequence of Kleisli arrows.
(define (kleisli-pipe kc x arrows)
  (doc 'type '(-> KleisliCategory a (List (-> a (M a))) (M a)))
  ((kleisli-sequence kc arrows) x))

;;; kleisli-replicate : KleisliCategory × Nat × (A → M A) → (A → M A)
;;; Compose a Kleisli arrow with itself n times.
;;; replicate 0 f = return
;;; replicate n f = f >=> replicate (n-1) f
(define (kleisli-replicate kc n f)
  (doc 'type '(-> KleisliCategory Nat (-> a (M a)) (-> a (M a))))
  (if (<= n 0)
      (kleisli-identity kc)
      (let loop ([k n] [acc (kleisli-identity kc)])
        (if (<= k 0)
            acc
            (loop (- k 1) (kleisli->>> kc f acc))))))

(doc 'section 'prebuilt-kleisli-categories)

;;; kc-list : KleisliCategory
;;; Kleisli category for the List monad.
;;; Arrows: A → [B] (non-deterministic computations)
(define kc-list
  (make-kleisli-category monad-list-derived))

;;; kc-state : KleisliCategory
;;; Kleisli category for the State monad.
;;; Arrows: A → (S → (B, S)) (stateful computations)
(define kc-state
  (make-kleisli-category monad-state-derived))

(doc 'section 'display)

;;; kleisli-category->string : KleisliCategory → String
(define (kleisli-category->string kc)
  (if (kleisli-category? kc)
      (format "Kleisli<~a>" (kleisli-category-name kc))
      "Not a Kleisli category"))

(doc 'section 'exports)
(doc 'description "Exports

KleisliCategory:
  make-kleisli-category, kleisli-category?
  kleisli-category-name, kleisli-category-monad

Operations:
  kleisli-identity, kleisli-compose
  kleisli-id, kleisli->>>, kleisli-<<<, fish
  kleisli-lift, kleisli-map

Law Verification:
  verify-kleisli-left-identity, verify-kleisli-right-identity
  verify-kleisli-associativity, verify-kleisli-laws
  (and -with-eq variants for function-based monads)

Combinators:
  kleisli-sequence, kleisli-pipe, kleisli-replicate

Pre-built:
  kc-list, kc-state

Display:
  kleisli-category->string")
