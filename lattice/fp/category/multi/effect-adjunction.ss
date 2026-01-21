;;; lattice/fp/category/multi/effect-adjunction.ss — Effect Adjunctions
;;; @module effect-adjunction
;;; @requires adjunction-inter free-algebra

(load "lattice/fp/category/multi/adjunction-inter.ss")
(load "lattice/fp/category/free-algebra.ss")

(doc 'module 'effect-adjunction)
(doc 'description "Effect Adjunctions with Correct Counit Semantics

This module provides the principled version of Free ⊣ Forgetful
adjunctions for algebraic effects using the multi-category framework.

THE PROBLEM:
Standard NatTransform has parametric components - a single function
∀A. F(A) → G(A) that can't access the object index. The counit
ε_A : Free(U(A)) → A needs to evaluate terms using algebra A's
operations, but can't access A in the parametric encoding.

THE SOLUTION:
Indexed natural transformations where component-at(A) receives
the algebra A explicitly. The counit ε_A can then call eval-term
with the algebra's operations.

RESULT:
  (apply-counit adj algebra term)
  → evaluates term using algebra's operations
  → equivalent to (handle (algebra-to-handler algebra) term)

This makes the categorical semantics precise: handling IS counit
application with the handler viewed as an algebra.")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'see-also '(effect-category.ss free-algebra.ss adjunction-inter.ss))

;;; ====
;;; Free ⊣ Forgetful for Signatures
;;; ====

(doc 'section 'free-forgetful-gen)
(doc 'description "Free ⊣ Forgetful Adjunction (Multi-Category Version)

For algebraic signature Σ:
  Free_Σ : Set → Σ-Alg
  U_Σ    : Σ-Alg → Set

Unit η_X : X → U(Free(X))
  η_X(x) = (gen x) — embed element as generator

Counit ε_A : Free(U(A)) → A
  ε_A(term) = eval-term A term — evaluate using algebra A

This is the correct formulation where the counit component at
algebra A has access to A for evaluation.")

;;; make-free-forgetful-adjunction-gen : Signature → AdjunctionInter
;;; Construct the Free ⊣ Forgetful adjunction with correct counit.
(define (make-free-forgetful-adjunction-gen sig)
  (doc 'type '(-> Signature AdjunctionInter))
  (doc 'description "Create Free ⊣ Forgetful with indexed counit.
The counit ε_A evaluates terms using algebra A's operations.")
  (let* ([Free (make-free-functor-gen sig)]
         [U (make-forgetful-functor-gen sig)]
         ;; Identity functors for unit/counit domains
         [Id-Set (id-functor-for cat-Set)]
         [Id-Alg (id-functor-for (cat-Alg sig))]
         ;; Composed functors
         [U-Free (functor-compose-general U Free)]  ; U∘Free : Set → Set
         [Free-U (functor-compose-general Free U)]  ; Free∘U : Σ-Alg → Σ-Alg
         ;; Unit η : Id_Set ⟹ U∘Free
         ;; η_X(x) = (gen x)
         [unit (make-nat-indexed
                'η
                Id-Set
                U-Free
                ;; component-at : Type → (Value → Term)
                ;; The carrier type X doesn't affect embedding
                (lambda (carrier-type)
                  (lambda (x) (make-gen x))))]
         ;; Counit ε : Free∘U ⟹ Id_{Σ-Alg}
         ;; ε_A(term) = eval-term A term
         ;; THIS IS THE KEY: component-at receives the ALGEBRA A
         [counit (make-nat-indexed
                  'ε
                  Free-U
                  Id-Alg
                  ;; component-at : Algebra → (Term → Value)
                  ;; Receives the algebra A and evaluates terms in it
                  (lambda (alg)
                    (lambda (term)
                      (eval-term alg term))))])
    (make-adjunction-inter
     (string->symbol (format "Free-~a⊣U" (signature-name sig)))
     Free
     U
     unit
     counit)))

;;; ====
;;; Pre-built Adjunctions for Standard Signatures
;;; ====

(doc 'section 'prebuilt-adjunctions)

;;; adj-magma-gen : AdjunctionInter
;;; Free ⊣ Forgetful for Magma with correct counit.
(define adj-magma-gen
  (make-free-forgetful-adjunction-gen sig-magma))

;;; adj-semigroup-gen : AdjunctionInter
;;; Free ⊣ Forgetful for Semigroup with correct counit.
(define adj-semigroup-gen
  (make-free-forgetful-adjunction-gen sig-semigroup))

;;; adj-monoid-gen : AdjunctionInter
;;; Free ⊣ Forgetful for Monoid with correct counit.
(define adj-monoid-gen
  (make-free-forgetful-adjunction-gen sig-monoid))

;;; adj-group-gen : AdjunctionInter
;;; Free ⊣ Forgetful for Group with correct counit.
(define adj-group-gen
  (make-free-forgetful-adjunction-gen sig-group))

;;; ====
;;; Handler via Counit
;;; ====

(doc 'section 'handle-via-counit)
(doc 'description "Handling via Counit Application

The counit ε_A : Free(U(A)) → A interprets free terms in algebra A.
This is EXACTLY what effect handling does:
  - A handler provides an algebra (operations + return case)
  - handle runs the effectful computation through the algebra

Using the indexed counit makes this connection explicit and verifiable.")

;;; handle-via-counit : AdjunctionInter × Algebra × Term → Value
;;; Handle an effect by applying the counit at a specific algebra.
(define (handle-via-counit adj alg term)
  (doc 'type '(-> AdjunctionInter Algebra Term Value))
  (doc 'description "Handle effectful term using algebra.
This is counit application: ε_A(term) = eval-term A term.
Equivalent to (handle (algebra-to-handler alg) (term->eff term)).")
  (apply-counit adj alg term))

;;; run-with-algebra : Signature × Algebra × Term → Value
;;; Convenience function: build adjunction and apply counit.
(define (run-with-algebra sig alg term)
  (doc 'type '(-> Signature Algebra Term Value))
  (doc 'description "Run a term through an algebra using Free ⊣ Forgetful.")
  (let ([adj (make-free-forgetful-adjunction-gen sig)])
    (handle-via-counit adj alg term)))

;;; ====
;;; Fold as Counit
;;; ====

(doc 'section 'fold-counit)
(doc 'description "Catamorphism / Fold as Counit Application

The universal property of free algebras states:
For any algebra A and any function f : X → |A|,
there exists a UNIQUE algebra homomorphism h : Free(X) → A
such that h(gen x) = f(x).

This homomorphism is exactly ε_A ∘ Free(f).

In other words: a fold over a free structure is determined by
how you handle generators, and the counit gives evaluation.")

;;; fold-free : Signature × Algebra × (a → |A|) × Term[a] → |A|
;;; Fold a free term into an algebra using a generator handler.
(define (fold-free sig alg f term)
  (doc 'type '(-> Signature Algebra (-> a |A|) Term |A|))
  (doc 'description "Fold a term by:
1. Map generators via f
2. Evaluate the resulting term in algebra A
This is the universal property of free algebras.")
  ;; First map f over generators: Free(f)(term)
  (let ([mapped-term (free-fmap-term sig f term)])
    ;; Then evaluate in algebra: ε_A(mapped-term)
    (eval-term alg mapped-term)))

;;; ====
;;; Triangle Identity Verification for Effects
;;; ====

(doc 'section 'effect-triangles)
(doc 'description "Triangle Identities for Effect Adjunctions

For Free ⊣ U:

LEFT TRIANGLE: ε_{Free(X)} ∘ Free(η_X) = id_{Free(X)}
  Starting with term : Free(X)
  η_X(gen x) = gen (gen x)  (double-wrap)
  Free(η_X)(term) wraps each generator again
  ε_{Free(X)} evaluates, unwrapping one layer
  Result: original term

RIGHT TRIANGLE: U(ε_A) ∘ η_{U(A)} = id_{U(A)}
  Starting with x : |A| (element of carrier)
  η_{|A|}(x) = gen x
  ε_A(gen x) = x (generator evaluates to itself)
  U(ε_A) = ε_A on elements
  Result: x")

;;; verify-effect-triangle-left : Signature × Term → Boolean
;;; Verify left triangle for Free ⊣ Forgetful.
(define (verify-effect-triangle-left sig term)
  (doc 'type '(-> Signature Term Boolean))
  (doc 'description "Verify ε_{Free(X)} ∘ Free(η_X) = id at term.")
  (let* ([adj (make-free-forgetful-adjunction-gen sig)]
         ;; For left triangle, we need Free(X) as the object
         ;; In our encoding, Free(X) is an algebra object
         ;; Use the free algebra for the term's generators
         [free-alg (list 'algebra sig '(free test)
                         (build-free-algebra-ops sig))])
    (verify-triangle-left-inter adj 'test-carrier term)))

;;; verify-effect-triangle-right : Signature × Algebra × Value [× (a → a → Bool)] → Boolean
;;; Verify right triangle for Free ⊣ Forgetful.
(define (verify-effect-triangle-right sig alg val . opts)
  (doc 'type '(-> Signature Algebra Value [(-> a a Boolean)] Boolean))
  (doc 'description "Verify U(ε_A) ∘ η_{U(A)} = id at value in |A|.
Optional equality predicate for algebras with opaque carriers (e.g., functions).")
  (let ([adj (make-free-forgetful-adjunction-gen sig)]
        [eq? (if (null? opts) equal? (car opts))])
    ;; Starting with val : |A|
    ;; η_{|A|}(val) = (gen val)
    ;; ε_A((gen val)) = val
    (let* ([embedded (make-gen val)]
           [evaluated (apply-counit adj alg embedded)])
      (eq? evaluated val))))

;;; ====
;;; Composed Effect Handlers
;;; ====

(doc 'section 'composed-handlers)
(doc 'description "Effect Handler Composition

When composing handlers h₁, h₂ for effects E₁, E₂:
  h₁(h₂(eff)) corresponds to adjunction composition

If adj₁ handles E₁ and adj₂ handles E₂:
  (adjunction-compose-inter adj₁ adj₂) handles E₁ then E₂

This explains why handler order matters - adjunction composition
doesn't commute in general.")

;;; compose-handlers-via-adjunction : AdjunctionInter × AdjunctionInter → AdjunctionInter
;;; Compose effect adjunctions for nested handlers.
(define (compose-handlers-via-adjunction outer-adj inner-adj)
  (doc 'type '(-> AdjunctionInter AdjunctionInter AdjunctionInter))
  (doc 'description "Compose adjunctions for nested effect handling.
outer-adj handles the outer effect (applied last).
inner-adj handles the inner effect (applied first).")
  (adjunction-compose-inter outer-adj inner-adj))

;;; ====
;;; Display
;;; ====

;;; effect-adjunction->string : AdjunctionInter → String
(define (effect-adjunction->string adj)
  (doc 'type '(-> AdjunctionInter String))
  (if (adjunction-inter? adj)
      (format "EffAdj<~a>: Free ⊣ U (indexed counit)"
              (adjunction-inter-name adj))
      "Not an effect adjunction"))

;;; ====
;;; Exports
;;; ====

(doc 'section 'exports)
(doc 'description "Exports

Adjunction Construction:
  make-free-forgetful-adjunction-gen

Pre-built Adjunctions:
  adj-magma-gen, adj-semigroup-gen
  adj-monoid-gen, adj-group-gen

Handling:
  handle-via-counit, run-with-algebra

Fold:
  fold-free

Verification:
  verify-effect-triangle-left, verify-effect-triangle-right

Composition:
  compose-handlers-via-adjunction

Display:
  effect-adjunction->string")
