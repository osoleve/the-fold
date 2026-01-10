;;; Scripture Decision Record: FOLD-2025-001
;;; Dependent Type System Core Architecture

((id . "FOLD-2025-001")
 (date . "2025-12-29T06:30:00Z")
 (settled-by . design-proposal)  ; Awaiting Outsider approval
 (domain . type-system)
 (status . proposed)

 (context . "
The Fold's current type system (types.ss, infer.ss) supports simple
polymorphism with bidirectional inference. To enable type-safe block
protocols, length-indexed vectors, and state machine verification,
we need dependent types. This decision establishes the foundational
architecture for dependent types in The Fold.
")

 (decision . "
1. Universe Stratification: Tarski-style with cumulativity
   - Type₀ : Type₁ : Type₂ : ...
   - Cumulativity allows lifting without explicit coercions

2. Normalization: Normalization by Evaluation (NbE)
   - Leverages existing eval.ss infrastructure
   - Efficient: single evaluation + readback pass
   - Handles open terms via neutral values

3. Equality Theory: Intensional type theory
   - Definitional equality via NbE normalization
   - Decidable type checking (essential for inference)
   - Propositional equality via Id type when needed

4. Conversion Checking: NbE with η-expansion
   - η for functions: f ≡ λx.f x
   - η for pairs: p ≡ (fst p, snd p)

5. Integration: Incremental extension of current system
   - Pi types extend function types
   - Sigma types extend product types
   - Type holes remain for gradual typing
")

 (rationale . "
NbE is the natural choice because:
- eval.ss already implements an evaluator
- Closures and environments are already understood
- The alternative (hereditary substitution) is more complex
- This is the approach used by Mini-TT, Pie, Idris 2

Intensional equality because:
- Decidable type checking is required for bidirectional inference
- Extensional features can be added via axioms if needed
- Keeps implementation tractable

Tarski universes because:
- Russell-style (Type : Type) is inconsistent
- Cumulativity reduces verbosity
- Universe polymorphism possible when needed
")

 (alternatives . (
   (russell-style . "Simpler but unsound; Type : Type leads to paradoxes")
   (extensional-tt . "Undecidable type checking; incompatible with bidirectional")
   (hereditary-substitution . "More complex; NbE is more natural given eval.ss")
   (no-eta-expansion . "Would break expected equivalences")
 ))

 (implications . (
   "eval.ss will be extended with semantic Value types"
   "types.ss will add Pi and Sigma type constructors"
   "infer.ss will gain conversion checking via NbE"
   "New primitives: fst, snd, pair, the"
   "Kind system (kinds.ss) will need universe-kind integration"
   "Type-level computation requires careful fuel management"
 ))

 (related . ())  ; First decision in this series

 (blocking . (
   "the-fold-7lk: Pi types implementation"
   "the-fold-2c7: Sigma types implementation"
   "the-fold-prc: Universe hierarchy"
   "the-fold-twx: Type-level computation"
   "the-fold-x11o: GADTs"
 ))

 (references . (
   "David Christiansen: Checking Dependent Types with NbE (Tutorial)"
   "Dunfield & Krishnaswami: Complete and Easy Bidirectional Typechecking"
   "Mini-TT: A small dependent type theory"
   "Edwin Brady: Idris 2 - Quantitative Type Theory in Practice"
 )))
