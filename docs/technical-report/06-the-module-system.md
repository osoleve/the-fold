## 6. The Module System


The Fold organizes verified code into a *Module DAG*—a directed acyclic graph of modules with declared dependencies.

### 6.1 Module DAG Architecture

**Terminology**: We use "skill" internally, but it's just a module with additional metadata. The structure is a DAG, not a mathematical lattice (no meet/join operations).

**Tiered Structure**:

```
Tier 0 (Foundational):     linalg, data, algebra, random, numeric
         │                 No lattice dependencies, only Core
         ▼
Tier 1 (Intermediate):     autodiff, geometry, query, fp, info
         │                 Depend on Tier 0
         ▼
Tier 2+ (Advanced):        physics/diff, sim, pipeline
                           Multiple dependencies, domain-specific
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
| `fuel-bound` | String | Big-O complexity bound |
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
; → ((linalg . 12) (data . 8) (fp . 6) ...)

(lattice-impact 'linalg)               ; Transitive dependents
; → 15
```

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

**Type Classes** (dictionary-passing style):
- **Functor**: `fmap` for structure-preserving transformations
- **Applicative**: `pure` and `<*>` for effectful computations
- **Monad**: `return` and `>>=` for sequencing effects
- **Comonad**: `extract` and `extend` for contextual computations—the dual of Monad. Where Monad builds up context, Comonad tears it down. Zippers are the canonical Comonad: `extract` gets the focus, `extend f` applies `f` at every position with full context available.

All implemented via dictionary-passing, maintaining Core purity.

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
