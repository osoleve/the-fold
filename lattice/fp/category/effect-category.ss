(load "lattice/fp/category/free-algebra.ss")
(load "lattice/fp/control/effects.ss")

(doc 'module 'effect-category)
(doc 'description "Categorical Foundations for Algebraic Effects

This module makes explicit that algebraic effects use the Free Effect ⊣ Forgetful
adjunction. This categorical perspective enables:

  1. HANDLER COMPOSITION AS ADJUNCTION COMPOSITION
     Nesting handlers h₁(h₂(eff)) corresponds to composing the adjunctions
     for each effect. This gives principled laws for handler ordering.

  2. EFFECT ROWS AS FUNCTOR CATEGORY MORPHISMS
     Effect row extension (row-extend E r) corresponds to a morphism in
     the functor category [Set, Set]. Row polymorphism becomes natural
     transformation polymorphism.

  3. DELIMITED CONTINUATIONS AS KAN EXTENSIONS
     The captured continuation in an effect handler is a left Kan extension.
     Resuming the continuation is the universal property of the Kan extension.

KEY INSIGHT:
The Eff monad (Pure a | Op effect continuation) is precisely the free monad
over the effect signature functor. A handler provides an algebra, and
`handle` evaluates the free monad term in that algebra.

  Eff e a ≅ Free (EffectF e) a
  Handler ≅ (EffectF e)-Algebra
  handle h ≅ fold with algebra h (the counit ε)")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'see-also '(effects.ss free-algebra.ss monad-derivation.ss adjunction.ss))

;;; ====
;;; Effect Signatures as Algebraic Signatures
;;; ====

(doc 'section 'effect-as-signature)
(doc 'description "Effect Signatures as Algebraic Signatures

An effect signature (from effects.ss) defines operations with arities.
We can view this as an algebraic signature where:
  - Each operation is a term constructor
  - The arity determines the operation's 'shape'
  - No equational laws (effects are free, not quotiented)

This is the 'polynomial functor' view: an effect signature Σ with
operations {op₁ : A₁ → R₁, ..., opₙ : Aₙ → Rₙ} defines a functor
ΣF(X) = Σᵢ Aᵢ × (Rᵢ → X)

The free monad Free(ΣF) is exactly Eff.")

;;; effect-sig-to-signature : EffectSig → Signature
;;; Convert an effect signature to an algebraic signature.
;;; Each operation becomes a binary operation (payload × continuation).
(define (effect-sig-to-signature eff-sig)
  (doc 'type '(-> EffectSig Signature))
  (doc 'description "Convert effect signature to algebraic signature.
Operations are viewed as binary: (payload, continuation) → term.")
  (let* ([name (effect-sig-name eff-sig)]
         [ops (effect-sig-operations eff-sig)]
         ;; Convert each operation to arity 2 (payload + continuation)
         ;; The 'pure operation has arity 1 (just the value)
         [sig-ops (cons '(pure . 1)
                        (map (lambda (op)
                               (cons (operation-name op) 2))
                             ops))])
    ;; No laws: effects are free (no equational theory)
    (make-signature name sig-ops '())))

;;; State effect as algebraic signature
(doc sig-effect-state 'type 'Signature)
(doc sig-effect-state 'description "State effect as algebraic signature:
  pure : 1-ary (value → term)
  state-get : 2-ary (unit payload, state → continuation)
  state-put : 2-ary (new-state payload, unit → continuation)")
(define sig-effect-state
  (effect-sig-to-signature sig-State))

;;; Reader effect as algebraic signature
(doc sig-effect-reader 'type 'Signature)
(doc sig-effect-reader 'description "Reader effect as algebraic signature")
(define sig-effect-reader
  (effect-sig-to-signature sig-Reader))

;;; Writer effect as algebraic signature
(doc sig-effect-writer 'type 'Signature)
(doc sig-effect-writer 'description "Writer effect as algebraic signature")
(define sig-effect-writer
  (effect-sig-to-signature sig-Writer))

;;; Exception effect as algebraic signature
(doc sig-effect-exception 'type 'Signature)
(doc sig-effect-exception 'description "Exception effect as algebraic signature")
(define sig-effect-exception
  (effect-sig-to-signature sig-Exception))

;;; NonDet effect as algebraic signature
(doc sig-effect-nondet 'type 'Signature)
(doc sig-effect-nondet 'description "NonDet effect as algebraic signature")
(define sig-effect-nondet
  (effect-sig-to-signature sig-NonDet))

;;; ====
;;; Handlers as Algebras
;;; ====

(doc 'section 'handlers-as-algebras)
(doc 'description "Handlers as Algebras

A handler (from effects.ss) provides:
  - return-case: how to handle pure values
  - effect-cases: how to interpret each operation

This is exactly an algebra over the effect signature:
  - return-case = interpretation of 'pure operation
  - effect-cases = interpretations of effect operations

The key categorical insight: a handler IS an EffectF-algebra.
The `handle` function IS the unique algebra homomorphism from
the free algebra (Eff) to the handler's algebra.")

;;; handler-to-algebra : Handler × Signature → Algebra
;;; View a handler as an algebra over an effect signature.
;;; The carrier type is the handler's result type.
(define (handler-to-algebra h sig)
  (doc 'type '(-> Handler Signature Algebra))
  (doc 'description "Convert a handler to an algebra.
The handler's return-case and effect-cases become operation implementations.")
  (let* ([return-fn (handler-return h)]
         [effect-cases (handler-effects h)]
         ;; Build ops alist: pure + effect operations
         [ops (cons (cons 'pure return-fn)
                    ;; Effect cases already have (tag . handler-fn) form
                    effect-cases)])
    (make-algebra sig 'handler-result ops)))

;;; algebra-to-handler : Algebra → Handler
;;; Convert an algebra back to a handler for effect evaluation.
;;; This is the inverse of handler-to-algebra (up to representation).
(define (algebra-to-handler alg)
  (doc 'type '(-> Algebra Handler))
  (doc 'description "Convert an algebra to a handler.
The 'pure operation becomes return-case, other operations become effect-cases.
This enables using algebraic structures to interpret effects.")
  (let* ([ops (algebra-ops alg)]
         [pure-entry (assq 'pure ops)]
         [pure-fn (if pure-entry (cdr pure-entry) identity)]
         [effect-cases (filter (lambda (entry) (not (eq? (car entry) 'pure))) ops)])
    (deep-handler pure-fn effect-cases)))

;;; ====
;;; Free Effect Adjunction
;;; ====

(doc 'section 'free-effect-adjunction)
(doc 'description "Free Effect ⊣ Forgetful Adjunction

For each effect signature Σ, we have an adjunction:

    Free_Σ ⊣ Forgetful_Σ : Σ-Alg → Set

Where:
  - Free_Σ(X) = Eff Σ X (effectful computations over X)
  - Forgetful_Σ(A) = carrier of algebra A
  - Unit η : X → Eff Σ X given by η(x) = Pure x
  - Counit ε : Eff Σ (Forgetful A) → A given by evaluation in A

The derived monad Free_Σ ∘ Forgetful_Σ on Set is the Eff monad.

IMPORTANT THEOREM:
Handlers compose because adjunctions compose. Given:
  - Handler h₁ for effect E₁ (adjunction F₁ ⊣ G₁)
  - Handler h₂ for effect E₂ (adjunction F₂ ⊣ G₂)

The composition h₁ ∘ h₂ corresponds to the composed adjunction
F₁∘F₂ ⊣ G₂∘G₁ (note the reversal in the right adjoints).")

;;; make-effect-adjunction : Signature → Adjunction
;;; Build the Free Effect ⊣ Forgetful adjunction for an effect signature.
(define (make-effect-adjunction sig)
  (doc 'type '(-> Signature Adjunction))
  (doc 'description "Construct Free ⊣ Forgetful for an effect signature.
This is the categorical foundation underlying algebraic effects.")

  ;; Free functor: X → Eff Σ X
  ;; On morphisms: lifts f : X → Y to fmap f : Eff Σ X → Eff Σ Y
  (let* ([free-functor
          (make-named-functor
           (string->symbol (format "Free-~a" (signature-name sig)))
           ;; fmap for Eff: map over pure values
           (lambda (f eff)
             (eff-map f eff)))]

         ;; Forgetful functor: Alg → Set
         ;; On objects: extracts the carrier
         ;; On morphisms: just the underlying function
         [forgetful-functor
          (make-named-functor
           (string->symbol (format "Forget-~a" (signature-name sig)))
           (lambda (f x) (f x)))]

         ;; Unit η : Id → G∘F
         ;; η_X : X → Eff Σ X
         ;; η_X(x) = eff-return x = make-eff-pure x
         [unit
          (make-nat-transform
           (string->symbol (format "η-~a" (signature-name sig)))
           functor-id
           forgetful-functor
           eff-return)]

         ;; Counit ε : F∘G → Id
         ;; For effects, this is the evaluation/interpretation map
         ;; ε_A : Eff Σ (carrier A) → A
         ;; Given by evaluating the effectful computation in algebra A
         ;;
         ;; IMPORTANT LIMITATION:
         ;; The counit is indexed by the target algebra A. In our encoding,
         ;; NatTransform components are parametric (they receive values, not
         ;; the object index). The counit here is therefore PARTIAL:
         ;; - For pure values: correctly extracts the value
         ;; - For impure effects: returns unchanged (cannot evaluate without algebra)
         ;;
         ;; To actually evaluate effects, use `evaluate-counit` which takes
         ;; the algebra explicitly, or use `handle` with a converted handler.
         [counit
          (make-nat-transform
           (string->symbol (format "ε-~a" (signature-name sig)))
           free-functor
           functor-id
           ;; Partial counit: works for pure, identity on impure
           ;; For full evaluation, use evaluate-counit with an algebra
           (lambda (eff)
             (if (eff-pure? eff)
                 (eff-pure-value eff)
                 eff)))])

    (make-adjunction
     (string->symbol (format "free-eff-~a" (signature-name sig)))
     free-functor
     forgetful-functor
     unit
     counit)))

;;; Pre-built adjunctions for common effects
(doc adj-effect-state 'type 'Adjunction)
(doc adj-effect-state 'description "Free ⊣ Forgetful for State effect")
(define adj-effect-state
  (make-effect-adjunction sig-effect-state))

(doc adj-effect-reader 'type 'Adjunction)
(doc adj-effect-reader 'description "Free ⊣ Forgetful for Reader effect")
(define adj-effect-reader
  (make-effect-adjunction sig-effect-reader))

(doc adj-effect-writer 'type 'Adjunction)
(doc adj-effect-writer 'description "Free ⊣ Forgetful for Writer effect")
(define adj-effect-writer
  (make-effect-adjunction sig-effect-writer))

(doc adj-effect-exception 'type 'Adjunction)
(doc adj-effect-exception 'description "Free ⊣ Forgetful for Exception effect")
(define adj-effect-exception
  (make-effect-adjunction sig-effect-exception))

(doc adj-effect-nondet 'type 'Adjunction)
(doc adj-effect-nondet 'description "Free ⊣ Forgetful for NonDet effect")
(define adj-effect-nondet
  (make-effect-adjunction sig-effect-nondet))

;;; ====
;;; Counit Evaluation
;;; ====

(doc 'section 'counit-evaluation)
(doc 'description "Explicit Counit Evaluation

The counit ε_A : Eff Σ (carrier A) → A requires the target algebra A to
perform evaluation. Since NatTransform cannot carry the algebra, we provide
explicit evaluation functions that take the algebra as an argument.

This is the 'fold' or 'catamorphism' of the free monad: recursively
interpret the effect tree using the algebra's operations.")

;;; evaluate-counit : Adjunction × Algebra × Eff → Value
;;; Evaluate an effect using an algebra (explicit counit instantiation).
;;; This is the proper way to interpret effects categorically.
(define (evaluate-counit adj alg eff)
  (doc 'type '(-> Adjunction Algebra (Eff e a) Value))
  (doc 'description "Evaluate an effect in an algebra.
This is the counit ε_A applied to a specific algebra A.
Converts the algebra to a handler and runs the effect through it.")
  (handle (algebra-to-handler alg) eff))

;;; evaluate-with-algebra : Algebra × Eff → Value
;;; Shorthand when you have an algebra but not the adjunction.
(define (evaluate-with-algebra alg eff)
  (doc 'type '(-> Algebra (Eff e a) Value))
  (doc 'description "Evaluate an effect directly with an algebra.
Convenience function that converts algebra to handler and runs.")
  (handle (algebra-to-handler alg) eff))

;;; ====
;;; Handler Composition as Adjunction Composition
;;; ====

(doc 'section 'handler-adjunction-composition)
(doc 'description "Handler Composition as Adjunction Composition

When we compose handlers, we are implicitly composing their adjunctions.
This gives the mathematical foundation for effect handler ordering.

Given handlers h₁ : E₁-Alg and h₂ : E₂-Alg with adjunctions:
  F₁ ⊣ G₁ (for E₁)
  F₂ ⊣ G₂ (for E₂)

The composition handle h₁ (handle h₂ eff) corresponds to:
  Eff (E₁+E₂) X --h₂--> Eff E₁ (B) --h₁--> C

Categorically, this is the composition of adjunctions (reversed):
  F₂∘F₁ ⊣ G₁∘G₂

KEY INSIGHT: Order matters! h₁(h₂(eff)) ≠ h₂(h₁(eff)) in general
because adjunction composition doesn't commute. This explains why
effect handler order affects semantics.")

;;; compose-effect-adjunctions : Adjunction × Adjunction → Adjunction
;;; Compose two effect adjunctions.
;;; Note: F₂∘F₁ ⊣ G₁∘G₂ (right adjoints reverse)
(define (compose-effect-adjunctions adj1 adj2)
  (doc 'type '(-> Adjunction Adjunction Adjunction))
  (doc 'description "Compose effect adjunctions for nested handlers.
If adj1 handles outer effect and adj2 handles inner effect,
result handles the composition.")
  (adjunction-compose adj1 adj2))

;;; handlers-commute? : Handler × Handler × Eff → Boolean
;;; Test if two handlers commute on a given computation.
;;; Handlers commute iff h₁(h₂(eff)) ≈ h₂(h₁(eff)).
(define (handlers-commute? h1 h2 eff eq?)
  (doc 'type '(-> Handler Handler (Eff e a) (a × a → Boolean) Boolean))
  (doc 'description "Test if two handlers commute (produce equivalent results).
Uses provided equality predicate for result comparison.")
  (eq? (handle h1 (handle h2 eff))
       (handle h2 (handle h1 eff))))

;;; ====
;;; Effect Rows as Functor Category Morphisms
;;; ====

(doc 'section 'effect-rows-morphisms)
(doc 'description "Effect Rows as Functor Category Morphisms

An effect row (row (E₁ E₂ ...)) represents a combination of effects.
In categorical terms, this is an object in the functor category [Set, Set].

Row extension row-extend : E × Row → Row is a morphism in this category.
It takes an effect functor E and a row (representing combined effects)
and produces a new row.

Effect row polymorphism (∀r. row-extend E r → ...) becomes:
  Natural transformation polymorphism over the functor category.

This perspective explains:
  - Why rows form a (semi)lattice: functor category has products/coproducts
  - Why row extension is coherent: functorial composition
  - Why handlers can be polymorphic over rows: naturality")

;;; make-row-functor : EffectRow → Functor
;;; View an effect row as a functor (coproduct of effect functors).
(define (make-row-functor row)
  (doc 'type '(-> EffectRow Functor))
  (doc 'description "Create a functor from an effect row.
The row (E₁ E₂ ...) becomes the coproduct functor E₁ + E₂ + ...")
  (make-named-functor
   (string->symbol (format "Row~a" (effect-row-effects row)))
   ;; fmap for coproduct: apply fmap of whichever effect is active
   (lambda (f eff)
     (eff-map f eff))))

;;; row-extension-morphism : Symbol × EffectRow → NatTransform
;;; Row extension as a natural transformation.
;;; Adds an effect to a row, viewed as a functor category morphism.
(define (row-extension-morphism effect-name row)
  (doc 'type '(-> Symbol EffectRow NatTransform))
  (doc 'description "View row extension as a natural transformation.
This is the inclusion Row → (E + Row) in the functor category.")
  (let ([extended-row (row-add row effect-name)])
    (make-nat-transform
     (string->symbol (format "extend-~a" effect-name))
     (make-row-functor row)
     (make-row-functor extended-row)
     ;; Inclusion: identity on underlying computation
     (lambda (eff) eff))))

;;; ====
;;; Delimited Continuations as Kan Extensions
;;; ====

(doc 'section 'continuations-kan)
(doc 'description "Delimited Continuations as Kan Extensions

When a handler captures a continuation k : Response → Eff e a,
this is a left Kan extension along the inclusion of values into effects.

The universal property: for any morphism from the base to the target
category, there's a unique factorization through the Kan extension.

In effect terms: the continuation k can be:
  - Resumed once (linear use): standard effect handling
  - Resumed multiple times: non-determinism, backtracking
  - Never resumed: exception handling, abortion

The Kan extension perspective explains why continuations have these
capabilities: they ARE the universal solution to the extension problem.

Technical detail:
  Lan_η(F) exists and equals F for the unit η : Id → Eff
  This is because η is fully faithful on pure computations.")

;;; continuation-as-kan : (Response → Eff e a) → KanExtension
;;; Represent a captured continuation as a Kan extension structure.
;;; This is primarily for documentation/verification purposes.
(define (continuation-as-kan k)
  (doc 'type '(-> (-> Response (Eff e a)) KanExtension))
  (doc 'description "View a captured continuation as a left Kan extension.
The continuation k : Response → Eff e a is Lan_η(pure) at type Response → a.")
  (list 'kan-extension
        'left
        k
        ;; The universal property: factor through the continuation
        (lambda (f)
          (lambda (resp)
            (eff-bind (k resp) f)))))

;;; kan-extension? : Any → Boolean
(define (kan-extension? x)
  (and (pair? x)
       (eq? (car x) 'kan-extension)))

;;; kan-resume : KanExtension × Response → Eff e a
;;; Resume a continuation (use the Kan extension).
(define (kan-resume kan resp)
  (doc 'type '(-> KanExtension Response (Eff e a)))
  (doc 'description "Resume a continuation by applying the Kan extension.")
  (if (kan-extension? kan)
      ((caddr kan) resp)
      (error 'kan-resume "Expected Kan extension")))

;;; kan-factor : KanExtension × (a → Eff e b) → (Response → Eff e b)
;;; Factor through a continuation (universal property of Kan extension).
(define (kan-factor kan f)
  (doc 'type '(-> KanExtension (-> a (Eff e b)) (-> Response (Eff e b))))
  (doc 'description "Factor a morphism through the Kan extension.
This is the universal property: any f : a → Eff e b factors uniquely
through the continuation.")
  (if (kan-extension? kan)
      ((cadddr kan) f)
      (error 'kan-factor "Expected Kan extension")))

;;; ====
;;; Verification Utilities
;;; ====

(doc 'section 'verification)
(doc 'description "Verification Utilities

Functions to verify the categorical laws hold for our effect encodings.")

;;; verify-effect-unit : Signature × Any → Boolean
;;; Verify that eff-return satisfies the unit law: handle h (return x) = return-case h x
(define (verify-effect-unit handler x)
  (doc 'type '(-> Handler Any Boolean))
  (doc 'description "Verify unit law: handle h (return x) = (handler-return h) x")
  (equal? (handle handler (eff-return x))
          ((handler-return handler) x)))

;;; verify-effect-naturality : Handler × (a → b) × (Eff e a) → Boolean
;;; Verify naturality: handle h (fmap f m) = fmap f (handle h m)
;;; Only holds when f doesn't interact with the effect.
(define (verify-effect-naturality handler f eff eq?)
  (doc 'type '(-> Handler (-> a b) (Eff e a) (b × b → Boolean) Boolean))
  (doc 'description "Verify naturality of handler w.r.t. pure functions.")
  (eq? (handle handler (eff-map f eff))
       (eff-map f (handle handler eff))))

;;; ====
;;; Display and Introspection
;;; ====

(doc 'section 'display)

;;; effect-adjunction->string : Adjunction → String
(define (effect-adjunction->string adj)
  (doc 'type '(-> Adjunction String))
  (if (adjunction? adj)
      (format "EffectAdj<~a: Free ⊣ Forget>" (adjunction-name adj))
      "Not an adjunction"))

;;; ====
;;; Summary: The Categorical Picture
;;; ====

(doc 'section 'summary)
(doc 'description "Summary: The Categorical Picture of Algebraic Effects

╔═══════════════════════════════════════════════════════════════════════╗
║                    CATEGORICAL FOUNDATIONS                             ║
╠═══════════════════════════════════════════════════════════════════════╣
║                                                                        ║
║  EFFECT SIGNATURE Σ                                                    ║
║       │                                                                ║
║       ▼                                                                ║
║  POLYNOMIAL FUNCTOR ΣF(X) = Σᵢ Aᵢ × (Rᵢ → X)                          ║
║       │                                                                ║
║       ▼                                                                ║
║  FREE MONAD Eff Σ = Free(ΣF)                                          ║
║       │                                                                ║
║       ▼                                                                ║
║  ADJUNCTION: Free_Σ ⊣ Forgetful_Σ                                     ║
║       │                                                                ║
║       ├──── Unit η : X → Eff Σ X         (eff-return)                  ║
║       └──── Counit ε : Eff Σ A → A       (handle with algebra A)       ║
║                                                                        ║
╠═══════════════════════════════════════════════════════════════════════╣
║                                                                        ║
║  HANDLER h : ΣF-Algebra                                               ║
║       │                                                                ║
║       ├──── return-case : a → b          (pure operation)             ║
║       └──── effect-cases : op → (payload → (resp → b) → b)            ║
║                                                                        ║
║  handle h : Eff Σ a → b                                               ║
║       = unique algebra homomorphism from free algebra to h            ║
║       = fold with algebra h                                           ║
║       = instantiation of counit ε at algebra h                        ║
║                                                                        ║
╠═══════════════════════════════════════════════════════════════════════╣
║                                                                        ║
║  HANDLER COMPOSITION                                                   ║
║       h₁(h₂(eff)) corresponds to adjunction composition               ║
║       F₂∘F₁ ⊣ G₁∘G₂ (right adjoints reverse!)                         ║
║                                                                        ║
║  EFFECT ROWS                                                          ║
║       row = object in functor category [Set, Set]                     ║
║       row-extend = coproduct injection in [Set, Set]                  ║
║                                                                        ║
║  CONTINUATIONS                                                         ║
║       k : Response → Eff e a is left Kan extension Lan_η              ║
║       Universal property enables resume/abort/fork                    ║
║                                                                        ║
╚═══════════════════════════════════════════════════════════════════════╝")

;;; ====
;;; Exports
;;; ====

(doc 'section 'exports)
(doc 'description "Exports

Effect Signatures:
  effect-sig-to-signature
  sig-effect-state, sig-effect-reader, sig-effect-writer
  sig-effect-exception, sig-effect-nondet

Handlers and Algebras:
  handler-to-algebra, algebra-to-handler

Counit Evaluation:
  evaluate-counit, evaluate-with-algebra

Effect Adjunctions:
  make-effect-adjunction
  adj-effect-state, adj-effect-reader, adj-effect-writer
  adj-effect-exception, adj-effect-nondet

Adjunction Composition:
  compose-effect-adjunctions
  handlers-commute?

Effect Rows as Functors:
  make-row-functor
  row-extension-morphism

Continuations as Kan Extensions:
  continuation-as-kan, kan-extension?
  kan-resume, kan-factor

Verification:
  verify-effect-unit
  verify-effect-naturality

Display:
  effect-adjunction->string")
