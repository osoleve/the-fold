## 6. The Module System


The Fold organizes verified code into a *Module DAG*—a directed acyclic graph of modules with declared dependencies.

### 6.1 Module DAG Architecture

**Terminology**: We use "skill" internally, but it's just a module with additional metadata. The structure is a DAG, not a mathematical lattice (no meet/join operations).

**Tiered Structure**:

```
Tier 0 (Foundational):     linalg, data, algebra, random, numeric
         │                 No lattice dependencies, only Core
         ▼
Tier 1 (Intermediate):     autodiff, geometry, diffgeo, query, fp, info, topology
         │                 Depend on Tier 0
         │                 diffgeo provides charts, tangent spaces, Lie groups, curvature
         │                 fp/optics provides composable data accessors (lenses, prisms, etc.)
         ▼
Tier 2+ (Advanced):        physics/diff, physics/diff3d, physics/classical, sim, pipeline
                           Multiple dependencies, domain-specific
                           Physics includes lens library for functional state access
```

**DAG Properties**:
- **Acyclic**: No circular dependencies (enforced by tiering)
- **Topological ordering**: Modules can be loaded in dependency order
- **Compositionality**: Verifying a module only requires verified dependencies

### 6.2 Manifest Format

Each module declares metadata in `manifest.sexp`:

```scheme
(skill linalg
  (version "0.1.0")
  (tier 0)
  (path "lattice/linalg")
  (purity total)
  (stability stable)
  (fuel-bound "O(n³) for matrix ops, O(n) for vectors")
  (deps ())

  (description "Pure functional linear algebra: vectors, matrices,
                decompositions, solvers")

  (keywords (linear-algebra matrix vector quaternion decomposition))

  (exports
    (vec vec2 vec3 vec4 vec+ vec- vec* dot cross normalize)
    (matrix matrix-add matrix-mul matrix-transpose matrix-inverse)
    (decomp lu-decompose qr-decompose svd cholesky))

  (modules
    (vec "vec.ss" "Vector operations")
    (matrix "matrix.ss" "Matrix operations")
    (decomp "decomposition.ss" "Matrix decompositions")
    (solvers "solvers.ss" "Linear system solvers")))
```

**Formal Schema**:

| Field | Type | Description |
|----|----|----|
| `version` | SemVer | Semantic version string |
| `tier` | Nat | Dependency tier (0 = no deps) |
| `path` | String | Relative path from project root |
| `purity` | `total \| partial \| effect` | Purity guarantee |
| `stability` | `stable \| experimental` | API stability |
| `fuel-bound` | String | Big-O complexity bound (see §6.3.4) |
| `deps` | List<Symbol> | Direct dependencies |
| `exports` | List<(Module Symbol+)> | Public API |
| `modules` | List<(Name File Desc)> | Internal modules |

### 6.3 Compositional Verification

The tiered structure enables compositional verification—verifying a module requires only its direct dependencies, not the transitive closure.

#### 6.3.1 What "Verified" Means

We define `verified(M)` as the conjunction of three properties:

1. **Type-safe**: All exports type-check against their declared signatures. Internal functions type-check. No ill-typed terms exist in M.

2. **Fuel-bounded**: Every exported function terminates within its declared fuel bound for all well-typed inputs. If `manifest.sexp` declares `(fuel-bound "O(n²)")`, then for input of size n, the function consumes at most c·n² fuel for some constant c.

3. **Purity-respecting**: If the manifest declares `(purity total)`, the module performs no effects. If `(purity partial)`, it may diverge but performs no effects. Only `(purity effect)` modules may perform IO.

Formally:
```
verified(M) ≜ type-safe(M) ∧ fuel-bounded(M) ∧ purity-respecting(M)
```

#### 6.3.2 Compositional Verification Theorem

**Theorem** (Compositional Verification):
```
∀ module M with declared dependencies D₁, ..., Dₙ:
  verified(D₁) ∧ ... ∧ verified(Dₙ) ∧ locally-verified(M, {D₁...Dₙ})
  ⟹ verified(M)
```

Where `locally-verified(M, Deps)` means:
- M type-checks assuming Deps provide their declared signatures
- M's fuel consumption, measured with Deps as black boxes at their declared bounds, satisfies M's declared bound
- M's purity, assuming Deps respect their purity declarations, satisfies M's declared purity

**Proof sketch**:
- *Type safety*: By compositionality of typing judgments. If Γ_deps ⊢ M : τ and each D_i provides Γ_deps(D_i), then the combined context is sound.
- *Fuel bounds*: By composition of O-notation. If M calls f ∈ D_i with bound O(g), and M makes at most h calls, M's contribution is O(h · g). The manifest bound must dominate this.
- *Purity*: By monotonicity. Pure code calling pure code is pure. Effect code may call anything.

**Practical implication**: To verify a new module, you need only:
1. Verify it type-checks against dependency signatures
2. Verify its fuel bound (by inspection or testing)
3. Verify its purity claim

You do NOT need to re-verify dependencies or examine their implementations.

#### 6.3.3 Fuel Bound Composition

If module A has bound O(f_A) and module B has bound O(f_B):

| Composition | Resulting Bound |
|----|----|
| Sequential (A then B) | O(f_A + f_B) |
| Nested (A calls B once) | O(f_A + f_B) |
| Nested (A calls B n times) | O(f_A + n · f_B) |
| Independent (max) | O(max(f_A, f_B)) |

**Example**:
```scheme
;; linalg declares O(n³) for matrix-mul
;; autodiff calls matrix-mul in backward pass
;; If backward pass is O(k) operations, each O(n³):
;; autodiff declares O(k · n³)
```

**Type Safety at Boundaries**:

Module interfaces are typed. Calls across module boundaries are type-checked, ensuring type-safe composition.

#### 6.3.4 Fuel Bounds as Badges

The `fuel-bound` field in manifests isn't just documentation—it's a *badge*: a precomputed guarantee about the code at that content hash.

**What a fuel badge represents**:
- A promise that the code terminates within the stated complexity
- A commitment that holds for all well-typed inputs
- An invariant tied to the specific content hash (change the code, recompute the badge)

**Why this matters**:
- *Predictable composition*: When assembling modules, you know what you're getting. No surprises where a "simple" function turns out to be exponential.
- *Agent-safe execution*: Autonomous agents can safely call any badged function without risking runaway computation.
- *Trust delegation*: You don't need to analyze every function—trust the badge, verified once when the code was committed.

**Tooling for measurement**:

You don't have to figure out fuel bounds yourself. The Fold provides measurement infrastructure:

```scheme
;; Profile a function with representative inputs
(fuel-profile my-function test-inputs)
; → Reports actual fuel consumption across input sizes

;; Verify declared bound matches observed behavior
(verify-fuel-bound 'my-module)
; → Checks all exports against their manifest claims
```

The badge system transforms complexity analysis from "something you have to think about" into "something that was already measured and recorded." When you see `(fuel-bound "O(n²)")` in a manifest, that's not a hope—it's a verified fact about that specific code hash.

### 6.4 Semantic Discovery

The `lattice/meta/` module provides agent-facing discovery tools:

**Knowledge Graph** (`kg.ss`):
- Parses all manifests into a CAS-backed graph
- Entities: skills, modules, exports
- Relations: depends-on, exports, contains

```scheme
(kg-build!)              ; Build KG from manifests
(kg-skills)              ; List all skills
(kg-deps 'autodiff)      ; → (linalg)
(kg-uses 'linalg)        ; → (autodiff geometry physics/diff ...)
```

**BM25 Search Engine** (`bm25.ss`):

Pure functional BM25 implementation for ranked retrieval:

```scheme
(lf "matrix decomposition")    ; Full-text search
; → ((linalg 0.85 skill ...) (physics/diff 0.62 skill ...))

(lfe 'matrix-inverse)          ; Exact symbol lookup
; → (matrix-inverse 1.0 export (linalg matrix))
```

**DAG Navigation** (`dag.ss`):

```scheme
(lattice-path 'physics/diff 'linalg)  ; Find dependency path
; → (physics/diff autodiff linalg)

(lattice-hubs 5)                       ; Most-depended-on modules
; → ((linalg . 14) (data . 10) (fp . 7) (diffgeo . 4) ...)

(lattice-impact 'linalg)               ; Transitive dependents
; → 15
```

**File Export Discovery** (`shell/introspect/exports.ss`):

For modules without manifest entries, or when developing new code that depends on existing infrastructure, direct file scanning provides instant API discovery:

```scheme
(exports-of "lattice/fp/templates.ss")
; → (ap-with applicative-ap applicative-either make-functor ...)

(lef "lattice/fp/templates.ss")        ; Pretty-print grouped by category
; → Constructors (7): make-applicative, make-foldable, make-functor, ...
;   Predicates (8): applicative?, foldable?, functor?, ...
;   Accessors & Operations (60): ap-with, applicative-ap, ...
;   Values & Instances (6): mconcat, mtimes, over, ...

(exports-of-summary "core/blocks/block.ss")
; → core/blocks/block.ss: 15 exports (1 predicates, 13 ops, 1 values)
```

The categorization uses naming conventions: `make-*` → constructors, `*?` → predicates, symbols with `-` → operations, plain symbols → values. This eliminates the friction of tracing through files to discover APIs when building new modules.

### 6.5 The FP Toolkit

`lattice/fp/` is a comprehensive functional programming library:

**Control** (`fp/control/`):
- Monads: State, Reader, Writer, Maybe, Either
- Effects: Algebraic effects (experimental)
- Continuations: Call/cc, delimited continuations
- Free monads: Syntax/semantics separation

**Data** (`fp/data/`):
- **Lazy streams**: Infinite sequences with demand-driven evaluation. Functor, Applicative, and Monad instances enable stream comprehensions. Classic sequences (Fibonacci, primes) defined co-recursively.
- **List zippers**: O(1) cursor navigation and modification. The `(left, focus, right)` representation with reversed left context enables efficient movement. Comonad instance supports contextual computations like moving averages.
- **Tree zippers**: Rose tree (n-ary tree) navigation via Huet's zipper. Crumb-based path tracking enables reconstruction after deep modifications. Preorder traversal iterators.

**Parsing** (`fp/parsing/`):
- Parser combinators with packrat memoization
- Regex compilation
- JSON, S-expression, SQL parsers

**Rewriting** (`fp/rewrite/`):
- Term rewriting systems
- Strategic rewriting (innermost, outermost)
- Fusion rules for optimization

**Open Protocols** (`fp/protocol.ss`):
- Clojure-style protocol system for extensible dispatch
- Types register implementations at load time; dispatch on first argument's type tag
- Enables Open/Closed Principle: extend behavior without modifying existing code
- Used by physics lenses for polymorphic body access

**Game Theory** (`fp/game/`):
- **Cooperative games**: Coalition formation, Shapley value, core solutions
- **Matching theory**: Stable marriage, hospital-residents, top trading cycles
- **Voting theory**: Power indices (Shapley-Shubik, Banzhaf), weighted voting
- **Fair division**: Cake cutting protocols (cut-and-choose, Dubins-Spanier), adjusted winner procedure, envy-free allocation

**Type Classes** (dictionary-passing style):
- **Functor**: `fmap` for structure-preserving transformations
- **Applicative**: `pure` and `<*>` for effectful computations
- **Monad**: `return` and `>>=` for sequencing effects
- **Comonad**: `extract` and `extend` for contextual computations—the dual of Monad. Where Monad builds up context, Comonad tears it down. Zippers are the canonical Comonad: `extract` gets the focus, `extend f` applies `f` at every position with full context available.

All implemented via dictionary-passing, maintaining Core purity.

**Category Theory** (`fp/category/`):

The category module provides first-class categorical structures that unify and explain the type class infrastructure:

- **Natural Transformations** (`natural-transform.ss`): Morphisms between functors with vertical composition, horizontal (Godement) composition, and whiskering. Naturality verification functions ensure the naturality square commutes.

- **Adjunctions** (`adjunction.ss`): Pairs of functors F ⊣ G with unit and counit satisfying triangle identities. Includes transpose operations (curry/uncurry via the hom-set bijection), adjunction composition, and the free monoid adjunction `adj-free-list`.

- **Monad Derivation** (`monad-derivation.ss`): Every adjunction F ⊣ G yields a monad G∘F via `monad-from-adjunction`. Derives return from the unit η and join from G(ε). The List monad is derived automatically from `adj-free-list`. Includes monad law verification.

- **Comonads** (`comonad.ss`): Full comonad type class with Store, Env, and Traced comonads. `comonad-from-adjunction` derives comonads from adjunctions (F∘G). Comonad composition requires a **distributive law** δ : W₂(W₁(a)) → W₁(W₂(a)) satisfying coherence conditions—`compose-comonads-with-dist` implements this correctly with position-aware extraction via the `copeek` abstraction.

- **Kan Extensions** (`kan-extension.ss`): Right Kan Extension (Ran) and Left Kan Extension (Lan) as universal constructions. The Codensity monad `Ran_M M` provides O(1) bind—the categorical explanation for the `free-queue` and `eff-queue` optimizations in `free.ss` and `effects.ss`.

- **State/Store Adjunction** (`state-store-adjunction.ss`): The canonical product-exponential adjunction (−)×S ⊣ (−)^S. Derives the State monad and Store comonad from first principles, and implements currying as adjunction transposition.

The key insight: **all standard monads and comonads arise from adjunctions**, and **the O(1) bind optimization in effect systems is the Codensity monad**. This provides both theoretical grounding and practical performance understanding.

**Optics** (`fp/optics/`):

A complete hierarchy of composable optics for principled data access and transformation:

```
              Fold
             /    \
        Getter    Traversal
             \    /    \
              Affine   Setter
             /    \     |
          Prism   Lens  |
             \    /    /
               Iso ---- Grate
```

Grate is the categorical dual of Lens: where Lens extracts/replaces a single focus, Grate enables zipping multiple structures together via cotraverse.

| Optic | Targets | Read | Write | Primary Use |
|-------|---------|------|-------|-------------|
| Iso | exactly 1 | yes | yes | Reversible transformations |
| Lens | exactly 1 | yes | yes | Product type fields |
| Prism | 0 or 1 | yes | yes | Sum type variants |
| Affine | 0 or 1 | yes | yes | Optional fields (Lens ∩ Prism) |
| Grate | exactly 1 | no | yes | Zipping structures (dual of Lens) |
| Traversal | 0+ | yes | yes | Multiple targets |
| Fold | 0+ | yes | no | Read-only multi-target |
| Getter | exactly 1 | yes | no | Read-only single-target |
| Setter | 0+ | no | yes | Write-only multi-target |

**Key features**:
- **Unified composition**: `optic-compose` automatically selects the most specific result type (lens+prism→affine, traversal+fold→fold)
- **Operator syntax**: `^.` (view), `^?` (preview), `^..` (to-list), `.~` (set), `%~` (modify) enable ergonomic chaining
- **Law verification**: All optic types include property-based law checkers

**Example**:
```scheme
;; View through lens
(^. '(1 . 2) lens-fst)  ; → 1

;; Preview through prism (returns Maybe)
(^? (just 42) prism-just)  ; → (just 42)
(^? nothing prism-just)     ; → nothing

;; Modify all matching elements
(& '(1 2 3 4) (%~ (traversal-filtered even?) (lambda (x) (* x 10))))
; → (1 20 3 40)

;; Composition: lens+prism automatically yields affine
(define my-affine (optic-compose lens-fst prism-just))
(affine-preview my-affine (cons (just 42) "hello"))  ; → (just 42)
```

The optics tower provides principled abstractions for refactoring higher lattice modules—any module that navigates nested data structures can benefit from composable optics rather than ad-hoc accessor functions.

### 6.6 Module Loading

The `core/lang/module.ss` module provides dependency-aware loading:

**Basic Usage**:
```scheme
(require 'charts)              ; Load module and dependencies
(require 'vec 'matrix)         ; Load multiple modules
```

**Namespaced Modules** (for disambiguation):

When module names collide across directories, use the namespaced form:

```scheme
(require 'diffgeo/charts)      ; → lattice/diffgeo/charts.ss
(require 'algebra/polynomial)  ; → lattice/algebra/polynomial.ss
(require 'numeric/polynomial)  ; → lattice/numeric/polynomial.ss
(require 'fp/control/state)    ; → lattice/fp/control/state.ss
```

The namespaced form searches base directories (`lattice/`, `core/`, `shell/`) for the path.

**Collision Detection**:

When using simple names that have multiple matches, the loader warns:

```
⚠ Warning: 'polynomial' matches 2 files (using first):
      - lattice/algebra/polynomial.ss
      - lattice/numeric/polynomial.ss
    Consider using namespaced form: (require 'algebra/polynomial)
```

**Discovery Functions**:
```scheme
(modules)                      ; List all registered modules
(module-info 'charts)          ; Show path, deps, status
(module-collisions)            ; Audit name collisions
(module-stats)                 ; Show load times
```

**Header Annotations**:

Modules declare dependencies via header comments:
```scheme
;;; @module tangent
;;; @requires prelude matrix vec charts
```

The loader parses these to build the dependency graph automatically.

---
