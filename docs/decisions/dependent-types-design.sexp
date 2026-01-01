;;; Scripture: Dependent Types Design for The Fold
;;; The Foundation of Typed Block Computation
;;;
;;; Author: Shepherd (Opus 4.5)
;;; Date: 2025-12-29
;;; Status: DESIGN DOCUMENT (Awaiting Outsider Approval)

((title . "Dependent Types Architecture for The Fold")
 (author . shepherd)
 (tier . shepherd)
 (domain . type-system)
 (status . design-draft)
 (timestamp . "2025-12-29T06:30:00Z")
 (related-issues . ("the-fold-mm8" "the-fold-7lk" "the-fold-2c7" "the-fold-prc" "the-fold-twx"))

 (body . "

=============================================================================
DEPENDENT TYPES ARCHITECTURE FOR THE FOLD
=============================================================================

This document specifies the design of dependent types for The Fold's type
system, extending the existing bidirectional type inference with full
dependent types while preserving integration with Blocks and capabilities.

=== Executive Summary ===

We extend The Fold's type system from simple polymorphism to full dependent
types, enabling:

  - Types that depend on values (Pi types, Sigma types)
  - Type-level computation (functions that return types)
  - Precise specifications (vectors with length, bounded integers)
  - GADTs (type indices for state machines, proofs)

The design prioritizes:

  1. Integration with existing Block semantics
  2. Compatibility with bidirectional inference
  3. Practical usability over theoretical purity
  4. Incremental adoptability (gradual typing via holes)

=============================================================================
PART I: CORE DECISIONS
=============================================================================

--- Decision 1: Universe Stratification ---

DECISION: Tarski-style universe hierarchy with cumulativity.

We adopt a Tarski-style universe where:
  - Type₀ : Type₁ : Type₂ : ...
  - Type_n can be lifted to Type_{n+1} (cumulativity)
  - Concrete programs live in Type₀ (the universe of small types)

Syntax extension:
  Type        ; Synonym for Type₀ (most common)
  (Type n)    ; Explicit universe level n

Rationale:
  - Avoids Girard's paradox (Type : Type inconsistency)
  - Tarski style allows explicit universe polymorphism when needed
  - Russell-style (Type : Type) is simpler but unsound
  - Cumulativity reduces annotation burden in practice

Alternative rejected:
  - Russell-style (Type : Type): Simpler but unsound
  - Stratified without cumulativity: Too verbose

Implementation note:
  Universe levels are represented as natural numbers.
  Constraint solving during inference tracks level requirements.

--- Decision 2: Normalization Approach ---

DECISION: Normalization by Evaluation (NbE) with environment-based closures.

We adopt NbE because:
  - Natural integration with existing eval.ss evaluator
  - Efficient: evaluates once, reads back once
  - Handles open terms (variables) gracefully via neutral values
  - Well-understood implementation path

The normalization pipeline:
  1. Evaluate expression in environment → Value
  2. Read back value to normal form → Expr
  3. Compare normal forms syntactically

Values (semantic domain):
  - V-lam (closure)       ; Unevaluated function body + env
  - V-Pi (A, closure)     ; Dependent function type
  - V-Sigma (A, closure)  ; Dependent pair type
  - V-Type n              ; Universe at level n
  - V-neutral var         ; Free variable (stuck)
  - V-neutral app         ; Application to neutral
  - V-block tag payload   ; Block value

Rationale:
  - Builds on existing eval.ss infrastructure
  - Handles β-reduction implicitly during evaluation
  - Neutral values handle open terms without substitution

Alternative rejected:
  - Hereditary substitution: More complex, similar power
  - Reduction-based normalization: Repeatedly traverses, less efficient

--- Decision 3: Equality Theory ---

DECISION: Intensional equality with definitional equality checked by NbE.

We use intensional type theory where:
  - Definitional equality: α-equivalent after normalization
  - Propositional equality: Type family (Id A x y) requiring proofs
  - No function extensionality or UIP by default

Why intensional:
  - Decidable type checking (essential for bidirectional inference)
  - Simpler implementation (no quotient types)
  - Can add extensional features via axioms if needed

Equality checking algorithm:
  1. Normalize both types via NbE
  2. Compare normal forms structurally (α-equivalent)
  3. Handle η-expansion for functions and pairs

Alternative rejected:
  - Extensional TT: Undecidable type checking
  - Observational TT: More complex, overkill for our needs
  - Cubical TT: Powerful but very complex implementation

--- Decision 4: Conversion Checking Algorithm ---

DECISION: NbE-based conversion with η-expansion for functions and pairs.

Conversion checking (Γ ⊢ A ≡ B : Type):
  1. Evaluate A and B in current environment
  2. Apply η-expansion based on type structure
  3. Compare resulting values structurally

η-expansion rules:
  - Functions: f ≡ λx. f x (when comparing at function type)
  - Pairs: p ≡ (fst p, snd p) (when comparing at Sigma type)

This ensures:
  - (λx. f x) ≡ f for any function f
  - (fst p, snd p) ≡ p for any pair p

Implementation sketch:
  (define (convert? env type val1 val2)
    (let* ([t (normalize env type)]
           [v1 (eta-expand t val1 env)]
           [v2 (eta-expand t val2 env)])
      (equal-values? v1 v2)))

--- Decision 5: Integration Strategy ---

DECISION: Extend existing type system incrementally, preserving all current features.

Integration approach:
  1. Keep all existing types.ss constructs working
  2. Add Pi and Sigma as new type formers
  3. Extend infer.ss for dependent type synthesis
  4. Add new value forms to eval.ss
  5. Type holes remain for gradual adoption

Phases:
  Phase 1: Pi types (dependent functions)
  Phase 2: Sigma types (dependent pairs)
  Phase 3: Universe hierarchy
  Phase 4: Type-level computation
  Phase 5: GADTs and refined indices

Each phase is independently useful and testable.

=============================================================================
PART II: SYNTAX EXTENSIONS
=============================================================================

--- New Type Forms ---

Pi Types (Dependent Function Types):
  Current:  (-> Int Int Int)              ; Non-dependent
  New:      (Π ((n : Nat)) (Vec Int n))   ; Dependent on n

  Full syntax:
    (Π ((x₁ : T₁) (x₂ : T₂) ...) Tbody)

  Where Tbody may reference x₁, x₂, etc.

Sigma Types (Dependent Pairs):
  Current:  (× Int String)                 ; Non-dependent
  New:      (Σ ((n : Nat)) (Vec Int n))    ; Dependent on n

  Full syntax:
    (Σ ((x : A)) B[x])

  Second component's type depends on first component's value.

Universe Levels:
  Type        ; Sugar for (Type 0)
  (Type 0)    ; Universe of small types
  (Type 1)    ; Universe containing Type₀
  (Type n)    ; General form

--- New Expression Forms ---

Dependent Lambda:
  (λ ((x : T)) body)          ; Annotated parameter
  (fn ((x : T)) body)         ; Alternative syntax (Fold style)

Dependent Pair Construction:
  (pair x y)                  ; Sugar when types inferable
  (dpair A B x y)             ; Explicit types

Pair Elimination:
  (fst p)                     ; First projection
  (snd p)                     ; Second projection
  (let-pair ((x y) p) body)   ; Simultaneous destructuring

Type Annotations:
  (the T e)                   ; Assert e has type T (forcing check mode)
  (: e T)                     ; Existing annotation syntax

--- Compatibility with Current Syntax ---

Non-dependent forms remain sugar:

  (-> A B)     ≡  (Π ((_ : A)) B)    ; _ not used in B
  (× A B)      ≡  (Σ ((_ : A)) B)    ; _ not used in B
  (fn (x) e)   ≡  (λ ((x : ?)) e)    ; Hole for inferred type

=============================================================================
PART III: TYPE RULES (FORMAL SPECIFICATION)
=============================================================================

--- Judgment Forms ---

Bidirectional type checking:
  Γ ⊢ e ⇒ A      ; Synthesis: infer type A for expression e
  Γ ⊢ e ⇐ A      ; Checking: verify expression e has type A
  Γ ⊢ A ≡ B      ; Conversion: types A and B are definitionally equal

--- Formation Rules ---

                    Γ ⊢ A ⇐ Typeₙ    Γ, x:A ⊢ B ⇐ Typeₘ
  ────────────────  ──────────────────────────────────────
  Γ ⊢ Typeₙ ⇐ Typeₙ₊₁     Γ ⊢ (Π ((x : A)) B) ⇐ Type_{max(n,m)}


                    Γ ⊢ A ⇐ Typeₙ    Γ, x:A ⊢ B ⇐ Typeₘ
                    ──────────────────────────────────────
                      Γ ⊢ (Σ ((x : A)) B) ⇐ Type_{max(n,m)}

--- Introduction Rules ---

              Γ, x:A ⊢ e ⇐ B
  ───────────────────────────────────────
    Γ ⊢ (λ ((x : A)) e) ⇐ (Π ((x : A)) B)


            Γ ⊢ a ⇐ A    Γ ⊢ b ⇐ B[a/x]
  ─────────────────────────────────────────────
    Γ ⊢ (pair a b) ⇐ (Σ ((x : A)) B)

--- Elimination Rules ---

  Γ ⊢ f ⇒ (Π ((x : A)) B)    Γ ⊢ a ⇐ A
  ─────────────────────────────────────────
           Γ ⊢ (f a) ⇒ B[a/x]


  Γ ⊢ p ⇒ (Σ ((x : A)) B)
  ───────────────────────────
      Γ ⊢ (fst p) ⇒ A


  Γ ⊢ p ⇒ (Σ ((x : A)) B)    Γ ⊢ (fst p) ⇒ a : A
  ──────────────────────────────────────────────────
            Γ ⊢ (snd p) ⇒ B[a/x]

--- Conversion Rule ---

  Γ ⊢ e ⇐ A    Γ ⊢ A ≡ B
  ─────────────────────────
        Γ ⊢ e ⇐ B

--- Synthesis/Check Mode Switch ---

  Γ ⊢ e ⇒ A
  ────────────    (mode switch when inference succeeds)
  Γ ⊢ e ⇐ A

  Γ ⊢ A ⇐ Type    Γ ⊢ e ⇐ A
  ──────────────────────────    (annotation synthesizes)
       Γ ⊢ (the A e) ⇒ A

=============================================================================
PART IV: INTEGRATION WITH BLOCKS
=============================================================================

--- Block Types with Indices ---

Current block type:
  (Block Tag PayloadType)

Extended dependent block type:
  (Block Tag ((idx : I)) PayloadType[idx])

Example - State Machine Blocks:
  (Block 'state ((s : State)) (StateData s))

Where StateData depends on which state we're in:
  (StateData 'idle)     = Unit
  (StateData 'running)  = ProcessId
  (StateData 'finished) = ExitCode

--- Refinement Types for Blocks ---

Block refs can carry type information:
  (Ref (Block 'vec ((n : Nat)) (VecData n)))

This enables:
  - Type-safe block references
  - Length-indexed structures stored in CAS
  - Protocol verification for block chains

--- Content-Addressed Type Blocks ---

Types themselves can be stored as blocks:
  (type->block (Π ((n : Nat)) (Vec Int n)))

This creates:
  (Block 'type (encode-type ...) [])

Benefits:
  - Types are content-addressed
  - Type equality is hash equality (for closed types)
  - Types can be referenced across sessions

=============================================================================
PART V: MIGRATION PLAN
=============================================================================

--- Phase 1: Foundation (This Issue) ---

  1. Define Value type for NbE (semantic domain)
  2. Implement normalization (eval → readback)
  3. Add conversion checking
  4. All existing tests pass

Estimated: Core infrastructure, no user-visible changes.

--- Phase 2: Pi Types (the-fold-7lk) ---

  1. Add (Π ...) type syntax
  2. Extend parser/lower for Pi
  3. Add λ with annotated parameters
  4. Implement type checking rules
  5. Non-dependent (→ A B) desugars to Pi

Estimated: First dependent functions working.

--- Phase 3: Sigma Types (the-fold-2c7) ---

  1. Add (Σ ...) type syntax
  2. Add pair/fst/snd constructs
  3. Non-dependent (× A B) desugars to Sigma
  4. Pattern matching integration

Estimated: Dependent pairs working.

--- Phase 4: Universes (the-fold-prc) ---

  1. Add Type₀, Type₁, ... representation
  2. Implement universe polymorphism
  3. Add level inference
  4. Cumulativity subtyping

Estimated: Full universe hierarchy.

--- Phase 5: Type-Level Computation (the-fold-twx) ---

  1. Type families
  2. Type-level functions
  3. Compile-time evaluation

Estimated: Types computing on values.

--- Phase 6: GADTs (the-fold-x11o) ---

  1. Indexed data types
  2. Type refinement in patterns
  3. Constraint propagation

Estimated: Full GADT support.

--- Backward Compatibility Guarantees ---

  - All existing valid programs remain valid
  - (→ A B) and (× A B) retain their meaning
  - Type holes (?) continue to work
  - Existing primitives retain their types
  - No breaking changes to types.ss API

=============================================================================
PART VI: EXAMPLE PROGRAMS
=============================================================================

--- Example 1: Length-Indexed Vectors ---

;; Vector type indexed by length
(define-type (Vec A n)
  (case n
    ((Zero) (data 'Nil))
    ((Succ m) (data 'Cons A (Vec A m)))))

;; Safe head - only works on non-empty vectors
(: vec-head (Π ((A : Type) (n : Nat)) (→ (Vec A (Succ n)) A)))
(define (vec-head A n v)
  (case v
    ((Cons x xs) x)))

;; Append with length addition
(: vec-append
   (Π ((A : Type) (m : Nat) (n : Nat))
      (→ (Vec A m) (Vec A n) (Vec A (add m n)))))

--- Example 2: Typed Finite State Machine ---

;; State type
(define-type State
  (+ (Idle) (Processing) (Done)))

;; Data that depends on state
(define-type (StateData s)
  (case s
    ((Idle) Unit)
    ((Processing) String)
    ((Done) Int)))

;; State machine block type
(define-type StateMachine
  (Σ ((s : State)) (StateData s)))

;; Transition preserves typing
(: transition
   (→ StateMachine
      (+ (Stay StateMachine)
         (Move StateMachine))))

--- Example 3: Typed Printf ---

;; Format string determines argument types
(define-type (FormatArgs fmt)
  (case (head fmt)
    ((nil) Unit)
    ((%d rest) (× Int (FormatArgs rest)))
    ((%s rest) (× String (FormatArgs rest)))
    ((c rest) (FormatArgs rest))))

(: printf
   (Π ((fmt : Format)) (→ (FormatArgs fmt) String)))

--- Example 4: Proof-Carrying Blocks ---

;; A sorted list with proof
(define-type (SortedList A)
  (Σ ((xs : (List A)))
     (Sorted xs)))

;; Sorting returns proof
(: sort
   (Π ((A : Type) (ord : (Ord A)))
      (→ (List A) (SortedList A))))

;; Merge preserves sortedness (by construction)
(: merge
   (Π ((A : Type))
      (→ (SortedList A) (SortedList A) (SortedList A))))

=============================================================================
PART VII: ERROR MESSAGE DESIGN
=============================================================================

Dependent type errors include source locations (per the-fold-7s1y):

Type mismatch in dependent position:
  Error at vec.fold:15:8
  Expected: Vec Int (Succ n)
  Got:      Vec Int n

  In application: (vec-head xs)
  Context: n was bound to Zero at line 14

Universe level error:
  Error at types.fold:42:1
  Type₀ is not a member of Type₀

  Hint: Use (Type 1) for types that contain (Type 0)

Conversion failure:
  Error at proof.fold:88:12
  Cannot convert: (add m (Succ n))
  To:             (Succ (add m n))

  These are definitionally distinct. You may need to prove
  commutativity of addition explicitly.

=============================================================================
PART VIII: REFERENCES AND PRIOR ART
=============================================================================

Primary Influences:
  - Mini-TT (Coquand et al.)
  - Pie language (The Little Typer)
  - Idris 1 & 2 (Brady)
  - Dunfield & Krishnaswami bidirectional typing

Key Papers:
  - 'Checking Dependent Types with NbE' (Christiansen, tutorial)
  - 'Complete and Easy Bidirectional Typechecking' (Dunfield & Krishnaswami)
  - 'Quantitative Type Theory in Practice' (Brady, Idris 2)
  - 'Generic bidirectional typing for dependent type theories' (2024)

Implementation References:
  - elaboration-zoo (Kovács)
  - pi-forall (Weirich)
  - Yaffle (Idris 2 core)

=============================================================================
APPENDIX A: VALUE DOMAIN FOR NBE
=============================================================================

;; Semantic values for normalization
(define-record-type value
  (fields
   tag      ; Symbol: 'lam, 'pi, 'sigma, 'type, 'neutral, 'block, ...
   payload  ; Depends on tag
   env))    ; Environment for closures

;; Value constructors
(define (V-lam closure) ...)
(define (V-Pi A B-closure) ...)
(define (V-Sigma A B-closure) ...)
(define (V-Type n) ...)
(define (V-neutral term) ...)
(define (V-pair v1 v2) ...)
(define (V-block tag payload refs) ...)

;; Neutral values (stuck computation)
(define-record-type neutral
  (fields
   head     ; Symbol (variable name)
   spine))  ; List of eliminations applied

;; Spine elements
(define (Elim-app arg) ...)  ; Function application
(define (Elim-fst) ...)      ; First projection
(define (Elim-snd) ...)      ; Second projection
(define (Elim-case arms) ...)

=============================================================================
APPENDIX B: READBACK (VALUE → NORMAL FORM)
=============================================================================

;; readback : Value × Level → Expr
;; Convert semantic value to normal form expression
(define (readback val lvl)
  (case (value-tag val)
    ((lam)
     (let* ([x (fresh-var lvl)]
            [body-val (apply-closure (value-payload val) (V-neutral x))]
            [body (readback body-val (+ lvl 1))])
       `(λ ((,x : ?)) ,body)))

    ((pi)
     (let* ([A (car (value-payload val))]
            [B-clo (cdr (value-payload val))]
            [x (fresh-var lvl)]
            [A-nf (readback A lvl)]
            [B-val (apply-closure B-clo (V-neutral x))]
            [B-nf (readback B-val (+ lvl 1))])
       `(Π ((,x : ,A-nf)) ,B-nf)))

    ((sigma)
     (let* ([A (car (value-payload val))]
            [B-clo (cdr (value-payload val))]
            [x (fresh-var lvl)]
            [A-nf (readback A lvl)]
            [B-val (apply-closure B-clo (V-neutral x))]
            [B-nf (readback B-val (+ lvl 1))])
       `(Σ ((,x : ,A-nf)) ,B-nf)))

    ((type)
     `(Type ,(value-payload val)))

    ((neutral)
     (readback-neutral (value-payload val) lvl))

    ((pair)
     `(pair ,(readback (car (value-payload val)) lvl)
            ,(readback (cdr (value-payload val)) lvl)))

    ((block)
     ;; Blocks read back to themselves (first-class values)
     val)))

=============================================================================
APPENDIX C: CONVERSION CHECKING
=============================================================================

;; convert? : Env × Type × Value × Value → Boolean
;; Check if two values are definitionally equal at a type
(define (convert? env type v1 v2)
  (let ([type-nf (normalize env type)])
    (case (value-tag type-nf)

      ;; Function types: η-expand and compare bodies
      ((pi)
       (let* ([A (car (value-payload type-nf))]
              [B-clo (cdr (value-payload type-nf))]
              [x (V-neutral (gensym 'x))]
              [B (apply-closure B-clo x)]
              [v1x (apply-value v1 x)]
              [v2x (apply-value v2 x)])
         (convert? (cons x env) B v1x v2x)))

      ;; Pair types: η-expand and compare components
      ((sigma)
       (let* ([A (car (value-payload type-nf))]
              [B-clo (cdr (value-payload type-nf))]
              [v1-fst (project-fst v1)]
              [v1-snd (project-snd v1)]
              [v2-fst (project-fst v2)]
              [v2-snd (project-snd v2)]
              [B (apply-closure B-clo v1-fst)])
         (and (convert? env A v1-fst v2-fst)
              (convert? env B v1-snd v2-snd))))

      ;; Other types: structural comparison
      (else
       (equal-values? v1 v2)))))

=============================================================================
END OF DESIGN DOCUMENT
=============================================================================

This document awaits Outsider approval before implementation proceeds.

"))
