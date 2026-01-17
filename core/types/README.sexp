((name "types")
 (purpose "Type system implementation")
 (description "Complete type system for The Fold including base types,
higher-kinded types, dependent types (Pi and Sigma), type classes with
functional dependencies, full Rank-N polymorphism with impredicativity,
GADTs, existential types, and bidirectional type inference.")

 (modules
  ;; Foundation Layer - Core type representations
  ((types.ss "Base and compound types (Int, Bool, ->, x, +, List, forall)")
   (kinds.ss "Higher-kinded types, kind polymorphism, dependent kinds")
   (dep-types.ss "Dependent types: Pi, Sigma, universes, inductive types, module signatures")

   ;; Polymorphism Extensions (definition + inference unified)
   (rank-n.ss "Rank-N polymorphism: rank calculation, subsumption, impredicativity, inference")
   (gadt.ss "GADTs: type-indexed data, constructor refinement, inference")
   (existential.ss "Existential types: pack/unpack, skolemization, inference")

   ;; Inference Engines
   (infer.ss "Bidirectional type inference (Hindley-Milner base)")
   (dep-infer.ss "Dependent type inference: Pi/Sigma checking, universe levels")

   ;; Backwards-compatibility stubs (merged into their main modules)
   (rank-n-infer.ss "STUB: loads rank-n.ss")
   (gadt-infer.ss "STUB: loads gadt.ss")
   (existential-infer.ss "STUB: loads existential.ss")

   ;; Pattern Matching
   (dep-match.ss "Dependent pattern matching with type refinement")
   (pattern-check.ss "Pattern exhaustiveness, redundancy, coverage (Maranget algorithm)")

   ;; Type Class System
   (resolve.ss "Type class instance resolution and evidence dictionaries")
   (type-families.ss "Associated type families (type-level functions in classes)")

   ;; Advanced Features
   (termination.ss "Termination checking: structural, lexicographic, size-change")
   (dep-graph.ss "Graph property types: Acyclic, Connected, Tree, DAG, Path")
   (tactics.ss "Proof tactics: reflexivity, symmetry, transitivity, congruence")
   (contracts.ss "Contract system with flat/function/dependent contracts and blame")
   (annotate.ss "Type-annotated AST representation for tooling")

   ;; Validation & Tooling
   (sig-check.ss "Unified signature parsing and validation (tokenizer, parser, kind checker)")
   (sig-parser.ss "STUB: loads sig-check.ss")
   (type-annotation-check.ss "STUB: loads sig-check.ss, provides script entry point")
   (kind-check.ss "Kind system validation (pre-commit sanity checks)")))

 (dependencies (base lang))

 (architecture
  ((layers
    "The type system is organized in layers:
     1. Foundation (types.ss, kinds.ss) - Core representations
     2. Extensions (dep-types.ss, rank-n.ss, gadt.ss, existential.ss) - Type grammar + inference
     3. Inference (infer.ss, dep-infer.ss) - Core checking algorithms
     4. Analysis (pattern-check.ss, termination.ss, tactics.ss) - Verification
     5. Tooling (annotate.ss, sig-check.ss, kind-check.ss) - Developer tools")
   (inference-flow
    "dep-infer.ss orchestrates all inference engines:
     - Uses rank-n.ss for polymorphism (inference unified with definition)
     - Uses gadt.ss for GADT patterns (inference unified with definition)
     - Uses existential.ss for pack/unpack (inference unified with definition)
     - Coordinates bidirectional checking flow")
   (consolidation
    "Recent consolidation merged definition + inference pairs:
     - rank-n.ss + rank-n-infer.ss -> rank-n.ss (~1400 lines)
     - gadt.ss + gadt-infer.ss -> gadt.ss (~550 lines)
     - existential.ss + existential-infer.ss -> existential.ss (~520 lines)
     - sig-parser.ss + type-annotation-check.ss -> sig-check.ss (~1200 lines)
     Backwards-compatible stubs preserve imports for external code.")))

 (key-concepts
  ((bidirectional "Types flow both up (synthesis) and down (checking)")
   (dependent-types "Types that depend on values (e.g., Vec n a)")
   (universes "Type : Type1 : Type2 : ... hierarchy prevents paradoxes")
   (type-classes "Ad-hoc polymorphism via dictionary passing")
   (rank-n-polymorphism "First-class polymorphic functions, runST-style types")
   (impredicativity "Type variables can unify with polymorphic types")
   (quick-look "Guided instantiation: delay polymorphism decisions until more info")
   (gadts "Constructors with refined return types enable type-level computation")
   (existentials "Hide implementation types behind abstract interfaces")
   (termination "Ensure type-level computation terminates for decidable checking")
   (gradual-holes "Holes (? and (? name)) for partial type specifications")
   (hole-constraints "Named holes enforce consistency; constraints recorded for inference")))

 (type-classes
  ((single-param
    "Standard type classes with one type parameter"
    (Functor "fmap : (a -> b) -> f a -> f b")
    (Applicative "pure, <*> : extends Functor")
    (Monad ">>= : m a -> (a -> m b) -> m b")
    (Contravariant "contramap : (a -> b) -> f b -> f a")
    (Eq "==, /= : a -> a -> Bool")
    (Ord "compare, <, <=, >, >= : extends Eq")
    (Show "show : a -> String")
    (Semigroup "<> : a -> a -> a")
    (Monoid "mempty : extends Semigroup"))
   (multi-param
    "Type classes with multiple parameters (* -> * -> *)"
    (Bifunctor "bimap, first, second")
    (Category "id, compose for morphism types")
    (Profunctor "dimap, lmap, rmap")
    (Arrow "arr, first*, second*, ***, &&& : extends Category"))
   (with-fundeps
    "Multi-param classes with functional dependencies"
    (Convertible "convert : a -> b")
    (Collection "c -> e : empty, insert, member?")
    (MonadReader "m -> r : ask, local")
    (MonadState "m -> s : get, put, modify")
    (MonadWriter "m -> w : tell, listen, pass")))))
