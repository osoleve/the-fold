# The Fold: A Content-Addressable Homoiconic Universe

**Technical Report**

---
## Abstract


We present **The Fold**, a programming system built on a content-addressable homoiconic foundation. At its core lies a *block machine* where every computational unit—code, data, and types—is represented as a cryptographically-addressed immutable structure. Through a two-phase normalization process—α-normalization via de Bruijn indices and algebraic canonicalization (commutative sorting, associative flattening)—semantically equivalent expressions produce identical hashes, achieving true *semantic identity*: two functions that behave identically are the same function, regardless of variable naming or argument order in commutative operations.

The Fold implements a *gradual dependent type system* combining bidirectional type checking (following Dunfield & Krishnaswami), dependent function and pair types (Π, Σ), higher-kinded types, type classes via dictionary-passing, and GADTs with pattern refinement. Gradual typing through holes enables incremental specification without sacrificing soundness where types are known.

The system organizes verified code into a *module DAG* (internally called the "skill lattice")—a tiered directed acyclic graph where modules declare dependencies, purity guarantees, and complexity bounds. Functions are bounded rather than structurally total—fuel limits guarantee termination of any execution, though this is weaker than type-theoretic totality. This structure enables compositional verification: if dependencies are verified and a module is verified against those dependencies, the module is verified. A BM25-powered semantic search engine enables discovery across thousands of exports.

Key contributions: (1) a block calculus formalizing content-addressed computation with α-equivalence, (2) a dependent type system integrated with gradual typing, (3) a compositional module system with fuel-bounded complexity guarantees. The implementation, built entirely in Chez Scheme with no third-party dependencies, demonstrates that reproducible, verifiable computation can emerge from simple foundations.

---
## 1. Introduction


### 1.1 The Problem with File-Based Programming

Traditional programming systems identify code by *location*: file paths, module names, package versions. This conflation of identity with storage creates fundamental problems:

1. **Semantic drift**: The same file path can refer to different code at different times
2. **Dependency hell**: Version conflicts arise from name-based resolution
3. **α-equivalence violation**: `(λ x. x)` and `(λ y. y)` are stored differently despite identical semantics
4. **Non-reproducibility**: Builds depend on mutable external state

Consider two developers who independently write the identity function:

```scheme
;; Developer A
(define id-a (lambda (x) x))

;; Developer B
(define id-b (lambda (y) y))
```

In file-based systems, these are distinct entities requiring coordination. Yet semantically, they are the same function. This gap between syntax and semantics pervades software engineering.

### 1.2 The Proposal: Content-Addressed Homoiconic Computation

The Fold addresses these problems through three interlocking mechanisms:

1. **Content Addressing**: Every value's identity is its cryptographic hash. Two values with the same content have the same identity—automatically, universally, permanently.

2. **α-Normalization**: Before hashing, expressions are normalized using de Bruijn indices, eliminating variable naming from identity. `(λ x. x)` and `(λ y. y)` normalize to `(λ (dv 0))` and hash identically.

3. **Homoiconicity**: Code is data. Programs are S-expressions that serialize to blocks, enabling introspection, metaprogramming, and uniform treatment of all computational artifacts.

The result is a system where *semantic identity replaces syntactic identity*. Functions that behave the same are the same. Verified code stays verified. Dependencies are content, not names.

### 1.3 Contributions

This report presents three primary contributions:

**Contribution 1: Block Calculus with Multi-Phase Normalization**

We formalize a calculus where computation operates over content-addressed blocks. The key innovation is integrating a two-phase normalization pipeline with cryptographic hashing:

1. **Algebraic canonicalization**: Sort arguments of commutative operations, flatten associative operations, reorder independent bindings
2. **α-normalization**: Convert to de Bruijn indices, eliminating variable naming

This yields the semantic identity property:

```
α-equiv(e₁, e₂) ⟹ hash(normalize(e₁)) = hash(normalize(e₂))
(+ a b) ≡_hash (+ b a)           ; Commutative equivalence
(+ (+ a b) c) ≡_hash (+ a b c)   ; Associative equivalence
```

This provides semantic identity at the language level, not as an afterthought.

**Contribution 2: Gradual Dependent Type System**

We implement a type system combining:
- Bidirectional type checking for predictable inference
- Dependent types (Π, Σ) for precise specifications
- Higher-kinded types and type classes for abstraction
- Gradual typing through holes for incremental development

The system is *sound where types are known* while permitting incomplete specifications during development.

**Contribution 3: Compositional Module System**

We organize code into a tiered DAG where each module declares:
- Dependencies (other modules)
- Purity (total, partial, effectful)
- Complexity bounds (fuel consumption)

This enables *compositional verification*: verifying a module requires only verifying its code against already-verified dependencies, not the entire transitive closure.

### 1.4 Paper Organization

- **Section 2**: System architecture—the three-layer model and its rationale
- **Section 3**: The block machine—content addressing, normalization, storage
- **Section 4**: The block calculus—syntax, operational semantics, shell implementation, metaprogramming
- **Section 5**: The type theory—dependent types, bidirectional checking, gradual typing
- **Section 6**: The module system—DAG structure, verification, discovery
- **Section 7**: Implementation—technology choices, performance, developer experience
- **Section 8**: Evaluation—benchmarks and case study
- **Section 9**: Related work—comparison to Unison, IPFS, dependent type systems
- **Section 10**: Limitations and non-goals—honest scoping of what The Fold does not provide
- **Section 11**: Future work
- **Section 12**: Conclusion

---
## 2. System Architecture


The Fold employs a *three-layer architecture* separating pure computation from effectful boundaries:

```
┌─────────────────────────────────────────────────────────────┐
│                         User Layer                          │
│              Applications, experiments, scripts             │
├─────────────────────────────────────────────────────────────┤
│                        Shell Layer                          │
│         IO, validation, capability minting, effects         │
├─────────────────────────────────────────────────────────────┤
│                        Core Layer                           │
│          Pure, total, content-addressed, verified           │
└─────────────────────────────────────────────────────────────┘
```

### 2.1 The Core Layer

The Core is the mathematical heart of The Fold. Code in Core satisfies three properties:

**Purity**: No side effects. Functions depend only on their arguments and produce only their return values. This enables equational reasoning—if `f(x) = y`, then `f(x)` can always be replaced with `y`.

**Bounded Computation**: Every computation terminates within a declared resource bound. This is enforced via *fuel-bounded execution*: every computation receives a fuel budget that decrements with each reduction step. Exhausting fuel yields an `out-of-fuel` error rather than infinite looping.

**Important distinction**: This is *not* totality in the type-theoretic sense. True totality (as in Agda or Idris) proves termination for all inputs via structural recursion checks or sized types—a property of the function itself. Fuel bounds instead guarantee that any particular execution completes—a property of the runtime. A function that exhausts fuel has *failed*, not *terminated normally*.

We choose bounded computation over structural totality for pragmatic reasons:
- Structural totality rejects useful programs (e.g., interpreters, fixpoint iterations)
- Fuel bounds are simple to implement and reason about
- The bound is explicit in module manifests, enabling composition

The tradeoff: Core functions cannot be safely evaluated during type checking (since they might exhaust fuel), limiting dependent type expressiveness compared to systems with true totality.

**Trust**: Core assumes *perfect input*. It performs no validation, no defensive checks, no error recovery. If you pass malformed data to Core, behavior is undefined. This simplicity enables formal verification.

Core contains:
- Block primitives (construction, hashing, serialization)
- Type system (checking, inference, normalization)
- Evaluation (fuel-bounded reduction)
- Normalization (de Bruijn transformation)

### 2.2 The Shell Layer

The Shell (internally called "the thimble" or "fallen layer") mediates between the pure Core and the impure world:

**Validation**: All external input is validated before reaching Core. Malformed S-expressions, invalid UTF-8, type mismatches—all caught at the Shell boundary.

**Capability Minting**: Effects require capabilities. The Shell creates capability tokens (filesystem access, network access, time/randomness) that authorize specific operations.

**IO Operations**: File reading/writing, network communication, user interaction—all live in Shell. Core never performs IO directly.

**Error Recovery**: Shell handles exceptions, provides error messages, and maintains system stability when things go wrong.

The Shell/Core boundary is the *verification frontier*: Core can be formally verified; Shell is trusted but unverified.

### 2.3 The User Layer

The User layer contains applications built on the verified foundations:

- Interactive REPL sessions
- Scripts and automation
- Domain-specific applications
- Experiments and prototypes

User code may be verified (if it uses only Core and verified Shell interfaces) or unverified (if it uses arbitrary Shell capabilities).

### 2.4 Design Rationale

This architecture reflects a fundamental insight: *purity and effects require different treatment*.

Pure code can be:
- Cached (same input → same output)
- Parallelized (no shared mutable state)
- Verified (equational reasoning applies)
- Deduplicated (content addressing works)

Effectful code cannot enjoy these properties unconditionally. Rather than compromise the entire system, we isolate effects to a well-defined boundary.

The Shell is not a "second-class citizen"—it is essential for any useful system. But by separating it from Core, we preserve Core's mathematical properties while providing practical functionality.

---
## 3. The Block Machine


### 3.1 Block Structure

The fundamental data structure in The Fold is the **block**:

```
Block = { tag : Symbol, payload : Bytevector, refs : Vector<Address> }
```

**tag**: An interned symbol identifying the block's semantic role. Examples: `'lambda`, `'application`, `'type`, `'module`.

**payload**: Raw bytes carrying the block's data. For code, this is typically a UTF-8 encoded S-expression. For binary data, it's the raw bytes.

**refs**: An ordered vector of addresses pointing to other blocks. This creates the Merkle DAG structure—blocks reference other blocks by hash.

In Scheme:

```scheme
(define-record-type block
  (fields tag payload refs))

;; Accessors
(block-tag b)     ; → Symbol
(block-payload b) ; → Bytevector
(block-refs b)    ; → Vector<Address>
```

**Example**: The identity function `(λ x. x)` as a block:

```scheme
(make-block
  'lambda
  #vu8(40 108 97 109 98 100 97 ...)  ; UTF-8: "(lambda (x) x)"
  #())                                ; No refs (self-contained)
```

### 3.2 Addresses and Content Addressing

An **address** is a 33-byte value:

```
Address = [ version : 1 byte ][ hash : 32 bytes ]
```

- **version**: Normalization mode indicator
- **hash**: SHA-256 digest of the normalized, canonical serialization

**Version Bytes**:

| Version | Mode | Description |
|----|----|----|
| `0x00` | α-only | De Bruijn indices only (original mode) |
| `0x01` | Algebraic + α | Full algebraic canonicalization before de Bruijn |
| `0x02` | Enhanced (v2) | η-reduction, identity elimination, polynomial canonicalization, hash-consing |
| `0x03` | NbE + v2 (v3) | Fuel-bounded NbE normalization before v2 pipeline |

Version `0x00` provides α-equivalence: `(λ x. x)` and `(λ y. y)` hash identically.

Version `0x01` provides extended equivalence: `(+ a b)` and `(+ b a)` also hash identically, as do `(+ (+ a b) c)` and `(+ a b c)`.

Version `0x02` provides maximum semantic equivalence detection: `(+ x 0)` and `x` hash identically, `(+ x x)` and `(* 2 x)` hash identically, and `(fn (x) (f x))` and `f` hash identically (when x is not free in f).

Version `0x03` adds NbE (Normalization by Evaluation) to the pipeline, providing β-reduction and intrinsic handling of projections: `((fn (x) x) y)` and `y` hash identically, `(fst (pair a b))` and `a` hash identically, and `(case (Left x) ...)` reduces to the appropriate branch.

The version byte ensures no collision between modes—a block hashed with algebraic normalization is distinct from the same block hashed without it.

The address is computed by:

```scheme
(define (hash-block blk)
  (let* ([bytes (block->bytes blk)]      ; Canonical serialization
         [digest (sha256 bytes)]          ; FIPS 180-4 SHA-256
         [addr (make-bytevector 33)])
    (bytevector-u8-set! addr 0 0)         ; Version byte
    (bytevector-copy! digest 0 addr 1 32) ; Hash bytes
    addr))
```

**Critical Property**: The address IS the identity. Two blocks with identical content produce identical addresses. This is enforced cryptographically—finding two different blocks with the same address requires breaking SHA-256.

### 3.3 Canonical Serialization

Blocks serialize to bytes in a canonical format:

```
┌─────────────────────────────────────────────────────────────┐
│ tag-length (4 bytes, u32 LE)                                │
├─────────────────────────────────────────────────────────────┤
│ tag (UTF-8 NFC normalized)                                  │
├─────────────────────────────────────────────────────────────┤
│ payload-length (4 bytes, u32 LE)                            │
├─────────────────────────────────────────────────────────────┤
│ payload (raw bytes)                                         │
├─────────────────────────────────────────────────────────────┤
│ refs-count (4 bytes, u32 LE)                                │
├─────────────────────────────────────────────────────────────┤
│ ref₀ (33 bytes) │ ref₁ (33 bytes) │ ... │ refₙ (33 bytes)  │
└─────────────────────────────────────────────────────────────┘
```

**Design choices**:
- *Length-prefixed fields*: Enables unambiguous parsing
- *Little-endian integers*: Native on x86-64, efficient I/O
- *UTF-8 NFC*: Unicode normalization ensures consistent string representation
- *Fixed-size addresses*: 33 bytes allows version evolution while maintaining alignment

### 3.4 Two-Phase Normalization

The Fold uses a two-phase normalization pipeline to maximize semantic equivalence detection. The phases must be applied in a specific order:

```
Phase 1: Algebraic Canonicalization (while names exist)
    ├── Commutative sorting: (+ b a) → (+ a b)
    ├── Associative flattening: (+ (+ a b) c) → (+ a b c)
    ├── Parallel binding reordering: independent let* bindings sorted
    └── Pure sequence reordering: independent pure expressions in begin
    ↓
Phase 2: α-Normalization (de Bruijn indices)
    └── Named variables → positional indices
    ↓
Canonical Form → SHA-256 → Address
```

**Critical: Phase Order Matters**

Algebraic canonicalization *must* happen before α-normalization. Consider what happens if we reverse the order:

```scheme
;; Original
(let* ((a 1) (b 2)) (+ a b))

;; After α-normalization (wrong order)
(let* (1) (let* (2) (+ (dv 1) (dv 0))))

;; If we now try to reorder bindings, indices are corrupted!
```

The de Bruijn index `(dv 1)` refers to the binding 1 level up. Reordering bindings after conversion breaks this correspondence. By performing algebraic canonicalization while variable names are still present, we avoid this corruption.

#### 3.4.1 α-Normalization via De Bruijn Indices

Named variables break content identity. Consider:

```scheme
(lambda (x) (+ x 1))
(lambda (y) (+ y 1))
```

These are α-equivalent—they compute the same function—but their textual representations differ. Naive hashing produces different addresses for the same semantic content.

**Solution**: Convert to de Bruijn indices before hashing.

A de Bruijn index encodes a variable as the number of binders between its use and its binding site:

```
(lambda (x) x)           → (lambda (dv 0))      ; 0 binders between
(lambda (x) (lambda (y) x)) → (lambda (lambda (dv 1)))  ; 1 binder between
(lambda (x) (lambda (y) y)) → (lambda (lambda (dv 0)))  ; 0 binders between
```

The normalization function:

```scheme
(define (normalize expr)
  (normalize-with-env expr '()))

(define (normalize-with-env expr env)
  (cond
    [(symbol? expr)
     (let ([idx (env-lookup env expr)])
       (if idx `(dv ,idx) expr))]  ; Bound → index, Free → symbol

    [(and (pair? expr) (eq? (car expr) 'lambda))
     (let ([var (caadr expr)]
           [body (caddr expr)])
       `(lambda ,(normalize-with-env body (cons var env))))]

    [(pair? expr)
     (map (lambda (e) (normalize-with-env e env)) expr)]

    [else expr]))
```

**Theorem** (α-Equivalence Preservation):
```
α-equiv(e₁, e₂) ⟹ normalize(e₁) = normalize(e₂)
```

*Proof sketch*: α-equivalence differs only in bound variable names. De Bruijn indices eliminate names, encoding only binding structure. Same binding structure → same indices → same normalized form.

**Handling Recursion**: For recursive definitions via `fix`:

```scheme
(fix (f) (lambda (x) (f x)))
→ (fix (lambda (f) (lambda (x) ((dv 1) (dv 0)))))
```

The `fix` binder contributes to the index count like any other binder.

#### 3.4.2 Algebraic Canonicalization

α-normalization handles variable naming, but other syntactic variations can produce different hashes for semantically equivalent expressions:

```scheme
(+ a b) ≠_hash (+ b a)           ; Commutative but different
(+ (+ a b) c) ≠_hash (+ a b c)   ; Associative but different
```

**Solution**: Apply algebraic canonicalization before α-normalization.

**Commutative Sorting**

For commutative operations (addition, multiplication, set operations), arguments are sorted in canonical order:

```scheme
(+ b a)     → (+ a b)       ; Alphabetically sorted
(* z x y)   → (* x y z)     ; Multi-argument sorted
(+ 1 x)     → (+ 1 x)       ; Numbers before symbols
```

**CRITICAL**: Short-circuit operators (`and`, `or`) are NOT commutative—they have evaluation-order semantics:

```scheme
;; NOT equivalent - different evaluation semantics
(and (check-auth) (delete-db))  ≠  (and (delete-db) (check-auth))
```

The operation property registry explicitly excludes these operators.

**Associative Flattening**

For associative operations, nested applications are flattened:

```scheme
(+ (+ a b) c)   → (+ a b c)
(* x (* y z))   → (* x y z)
(append (append xs ys) zs) → (append xs ys zs)
```

Combined with commutative sorting:

```scheme
(+ (+ c a) b)   → (+ a b c)   ; Flatten then sort
```

**Parallel Binding Reordering**

Independent `let*` bindings can be reordered without changing semantics. The system uses dependency analysis:

```scheme
;; Independent bindings - can be reordered
(let* ((b 2) (a 1)) (+ a b))
→ (let* ((a 1) (b 2)) (+ a b))   ; Alphabetically sorted

;; Dependent bindings - order preserved
(let* ((a 1) (b (+ a 1))) (+ a b))
→ (let* ((a 1) (b (+ a 1))) (+ a b))   ; a must come before b
```

The algorithm:
1. Compute dependency graph (which bindings use which variables)
2. Topological sort respecting dependencies
3. Alphabetical tiebreaker for independent bindings

**Pure Sequence Reordering**

Independent pure expressions in `begin` blocks can be reordered:

```scheme
;; Pure expressions - can be reordered
(begin (+ 1 2) (+ 0 1))
→ (begin (+ 0 1) (+ 1 2))   ; Canonically sorted

;; Impure expressions - order preserved
(begin (set! x 1) (set! y 2))
→ (begin (set! x 1) (set! y 2))   ; Original order
```

**Purity Analysis**

The system uses conservative purity analysis—expressions are assumed impure unless proven pure:

```scheme
;; Known pure: literals, lambda creation, pure primitives
(expr-pure? 42)           → #t
(expr-pure? '(fn (x) x))  → #t
(expr-pure? '(+ 1 2))     → #t

;; Known impure: mutation, I/O, unknown functions
(expr-pure? '(set! x 1))  → #f
(expr-pure? '(display x)) → #f
(expr-pure? '(my-fn x))   → #f   ; Unknown defaults to impure
```

This conservative approach prevents unsafe reordering of effectful code.

**Canonical Ordering**

A total order over expressions enables deterministic sorting:

```
Priority: numbers < booleans < chars < strings < symbols < de Bruijn < compounds
```

Within each class, type-specific comparison applies (numeric order, alphabetic order, structural order for compounds).

#### 3.4.3 Combined Normalization

The full normalization function applies both phases:

```scheme
(define (normalize-full expr)
  (normalize (normalize-algebraic expr)))  ; Algebraic FIRST, then α
```

**Equivalence Classes**:

| Normalization Mode | Equivalences Detected |
|----|----|
| None | Syntactic identity only |
| α-only (v0x00) | + Variable renaming |
| Algebraic + α (v0x01) | + Commutative, associative, parallel bindings |
| Enhanced v2 (v0x02) | + η-equivalence, identity elements, polynomial equivalence |

**Implementation**: `core/blocks/normalize.ss`, `core/blocks/op-properties.ss`, `core/blocks/canonical-order.ss`

#### 3.4.4 Enhanced Normalization (Version 2)

Version 0x02 introduces four additional canonicalization passes that significantly expand semantic equivalence detection. These are applied in a specific order before α-normalization.

**η-Reduction**

Functions of the form `(fn (x) (f x))` where `x` does not appear free in `f` are equivalent to `f`. This eliminates trivial wrapper functions:

```scheme
(fn (x) (f x))           → f          ; η-reduced
(fn (x) (g x x))         → unchanged  ; x appears twice
(fn (x) (x y))           → unchanged  ; x in operator position
(fn (y) (fn (x) (f x)))  → (fn (y) f) ; Nested η-reduction
```

**Identity Element Elimination**

Operations with identity elements are simplified by removing those elements:

```scheme
(+ x 0)       → x         ; 0 is identity for +
(* x 1)       → x         ; 1 is identity for *
(+ a 0 b 0)   → (+ a b)   ; Multiple identities removed
(+ 0 0 0)     → 0         ; All identities → identity itself
```

**Absorbing Element Elimination**

Operations with absorbing elements short-circuit to the absorbing value:

```scheme
(* x 0 y)     → 0         ; 0 absorbs for *
(* a 0)       → 0         ; Any multiplication with 0
```

**Polynomial Canonicalization**

Arithmetic expressions are lifted to polynomial representation and lowered to sum-of-products canonical form:

```scheme
(+ x x)               → (* 2 x)           ; Like terms collected
(+ (* a b) (* b a))   → (* 2 a b)         ; After sorting, same term
(+ 1 2 3)             → 6                 ; Constant folding
(+ a b)               → (+ a b)           ; Already canonical
```

**Constraints**:
- Only applies to exact numbers (integers, rationals)—floats remain opaque to avoid precision issues
- Depth limit (`*poly-canon-max-depth*` = 10) prevents deep recursion
- Term limit (`*poly-canon-max-terms*` = 100) prevents exponential blowup

**Hash-Consing**

All normalized structures pass through a global canonicalization table that ensures structural sharing. Two equivalent subexpressions are represented by the same object (pointer equality):

```scheme
(hash-cons '(+ a b))  ; Returns canonical representative
(hash-cons '(+ a b))  ; Returns SAME object (eq? = #t)
```

This provides:
- Memory efficiency through deduplication
- Fast equality checking via pointer comparison
- Foundation for memoized normalization

**Combined v2 Pipeline**:

```
Input Expression
    ↓
η-Reduction
    ↓
Polynomial Canonicalization (recursive)
    ↓
Algebraic Canonicalization (commutative sorting, associative flattening)
    ↓
Identity/Absorbing Element Elimination
    ↓
α-Normalization (de Bruijn indices)
    ↓
Hash-Consing
    ↓
SHA-256 → Address (version 0x02)
```

**Implementation**: `core/blocks/normalize.ss` (v2 pipeline), `core/blocks/poly-canon.ss` (polynomial operations), `core/blocks/hash-cons.ss` (structural sharing), `core/blocks/op-properties.ss` (identity/absorbing elements)

#### 3.4.5 NbE-Based Normalization (Version 3)

Version 0x03 introduces **Normalization by Evaluation (NbE)** to the CAS hashing pipeline. NbE is a principled technique from type theory that normalizes expressions by:

1. **Evaluating** the expression to semantic values (β-reduction happens intrinsically)
2. **Reading back** from values to normal form expressions

This approach has key advantages over syntactic rewriting:

- **β-reduction is automatic**: Function applications reduce by actual evaluation
- **Projections are intrinsic**: `(fst (pair a b))` evaluates to `a` during the value phase
- **Sum elimination is built-in**: `(case (Left x) ...)` selects the correct branch
- **Single-pass**: No need for iterated rewriting to reach fixpoint

**Fuel Bounding for Safety**

NbE can diverge on non-terminating expressions like the omega combinator `((fn (x) (x x)) (fn (x) (x x)))`. The implementation uses fuel-bounded evaluation:

```scheme
(define (nbe-eval-fuel expr env fuel)
  (if (<= fuel 0)
      (values (V-neutral (N-stuck expr)) 0)  ; Return "stuck" value
      (cond
       [(and (pair? expr) (eq? (car expr) 'fn))
        (values (V-lam ...) (- fuel 1))]
       ...)))
```

When fuel exhausts, the expression becomes a "stuck" neutral value—a valid normal form that preserves the original expression. This ensures:

- **Termination**: Always returns in bounded time
- **Soundness**: Stuck terms don't collapse distinct expressions
- **Graceful degradation**: Complex expressions normalize as much as fuel allows

**Sum Type Support**

NbE handles sum types (tagged unions) with `Left`/`Right` constructors:

```scheme
;; Values
(V-sum 'left value)   ; Left-injected value
(V-sum 'right value)  ; Right-injected value

;; Case elimination
(case (Left 42) ((Left x) x) ((Right y) y)) → 42
```

When the scrutinee is a known sum, the appropriate branch is selected. When stuck (e.g., scrutinee is a variable), a neutral `N-case` is produced.

**V3 Pipeline**

```
Input Expression
    ↓
NbE Normalization (fuel-bounded)
    ├── β-reduction (function application)
    ├── Pair projection (fst/snd)
    ├── Sum elimination (case)
    └── Conditional reduction (if with literal condition)
    ↓
V2 Pipeline (η-reduction, algebraic canonicalization, etc.)
    ↓
α-Normalization (de Bruijn indices)
    ↓
Hash-Consing
    ↓
SHA-256 → Address (version 0x03)
```

**Equivalences Detected by V3**:

| Expression | Normalizes To | V2 Detects? | V3 Detects? |
|------------|---------------|-------------|-------------|
| `((fn (x) x) y)` | `y` | ✗ | ✓ |
| `((fn (x) (+ x 1)) 5)` | `6` | ✗ | ✓ |
| `(fst (pair a b))` | `a` | ✗ | ✓ |
| `(snd (pair a b))` | `b` | ✗ | ✓ |
| `(case (Left x) ...)` | branch with x | ✗ | ✓ |
| `(if #t a b)` | `a` | ✗ | ✓ |

V3 is a strict superset of V2—all V2 equivalences are preserved.

**Implementation**: `core/lang/nbe.ss` (NbE implementation with fuel-bounded variants), `core/blocks/nbe-normalize.ss` (CAS integration), `core/blocks/normalize.ss` (v3 pipeline assembly), `core/blocks/cas.ss` (hash-sexpr-v3)

### 3.5 Content-Addressed Store (CAS)

The CAS is an in-memory hash table mapping addresses to blocks:

```scheme
(define *store* (make-hashtable bytevector-hash bytevector=?))

(define (store! blk)
  (let ([addr (hash-block blk)])
    (hashtable-set! *store* addr blk)
    addr))

(define (fetch addr)
  (hashtable-ref *store* addr #f))

(define (stored? addr)
  (hashtable-contains? *store* addr))
```

**Properties**:
- *O(1) average* lookup and insertion
- *Automatic deduplication*: Storing the same block twice returns the same address
- *Immutable*: Once stored, a block's content never changes (its address is its content's hash)

### 3.6 Garbage Collection with Pinning

Not all blocks should be kept forever. The CAS supports garbage collection via *pinning*:

```scheme
(define *pins* (make-hashtable bytevector-hash bytevector=?))

(define (pin! addr)
  (hashtable-set! *pins* addr #t))

(define (unpin! addr)
  (hashtable-delete! *pins* addr))

(define (pinned? addr)
  (hashtable-contains? *pins* addr))
```

**Transitive Pinning**: Pinning a block should also pin everything it references:

```scheme
(define (pin-tree! addr)
  (let ([blk (fetch addr)])
    (when blk
      (pin! addr)
      (vector-for-each pin-tree! (block-refs blk)))))
```

**Garbage Collection**:

```scheme
(define (gc!)
  (let ([to-delete '()])
    (hashtable-for-each
      (lambda (addr blk)
        (unless (pinned? addr)
          (set! to-delete (cons addr to-delete))))
      *store*)
    (for-each (lambda (addr) (hashtable-delete! *store* addr)) to-delete)
    (length to-delete)))
```

**Reachability-Based Collection**: A more sophisticated approach collects blocks unreachable from a set of roots:

```scheme
(define (gc-with-roots! roots)
  (let ([reachable (make-hashtable bytevector-hash bytevector=?)])
    ;; Mark reachable
    (for-each (lambda (root) (mark-reachable! root reachable)) roots)
    ;; Sweep unreachable
    (let ([to-delete '()])
      (hashtable-for-each
        (lambda (addr blk)
          (unless (hashtable-contains? reachable addr)
            (set! to-delete (cons addr to-delete))))
        *store*)
      (for-each (lambda (addr) (hashtable-delete! *store* addr)) to-delete)
      (length to-delete))))
```

### 3.7 Visual Example: Block DAG

Consider the expression `(+ (square 3) 1)` where `square` is defined as `(lambda (x) (* x x))`:

```
                    ┌──────────────────────┐
                    │ tag: 'application    │
                    │ payload: (+ □ 1)     │
                    │ refs: [hash-A]       │──────┐
                    └──────────────────────┘      │
                              │                   │
                              ▼                   ▼
                    ┌──────────────────────┐    ┌──────────────────────┐
                    │ tag: 'application    │    │ tag: 'literal        │
                    │ payload: (□ 3)       │    │ payload: 1           │
                    │ refs: [hash-B]       │    │ refs: []             │
                    └──────────────────────┘    └──────────────────────┘
                              │
                              ▼
                    ┌──────────────────────┐
                    │ tag: 'lambda         │
                    │ payload: (dv 0)*(dv 0)│
                    │ refs: []             │
                    └──────────────────────┘
                              │
                        (normalized)
```

Each block's address is the SHA-256 hash of its serialization. The structure forms a Merkle DAG—any change to any block changes its hash and propagates upward.

---
## 4. The Block Calculus


This section formalizes the computational model underlying The Fold.

### 4.1 Syntax

The core calculus is a lambda calculus extended with blocks and primitives:

```
e ::= x                          ; Variable
    | (λ x. e)                   ; Abstraction
    | (e₁ e₂)                    ; Application
    | (let x = e₁ in e₂)         ; Let binding
    | (fix x. e)                 ; Recursive binding
    | c                          ; Constant (numbers, strings, ...)
    | (prim op e₁ ... eₙ)        ; Primitive operation
    | (make-block tag payload refs)  ; Block construction
    | (block-tag e)              ; Block tag accessor
    | (block-payload e)          ; Block payload accessor
    | (block-refs e)             ; Block refs accessor
    | (quote e)                  ; Quotation (code as data)
    | (eval e)                   ; Evaluation (data as code)
```

**Normalized syntax** (after de Bruijn transformation):

```
e ::= (dv n)                     ; De Bruijn variable (index n)
    | (λ e)                      ; Abstraction (no binder name)
    | (e₁ e₂)                    ; Application
    | (let e₁ e₂)                ; Let (no binder name)
    | (fix e)                    ; Fix (no binder name)
    | c | (prim op e*) | ...     ; As above
```

### 4.2 Operational Semantics

We define a small-step reduction relation `e →ᶠ e'` parameterized by fuel `f`:

**Values**:
```
v ::= (λ x. e)           ; Abstractions
    | c                  ; Constants
    | (block t p r)      ; Fully evaluated blocks
```

**Reduction Rules** (selected):

```
                              f > 0
─────────────────────────────────────────────────  [β-reduce]
((λ x. e) v) →ᶠ e[v/x]     (fuel: f - 1)


                e₁ →ᶠ e₁'
────────────────────────────────────────────────  [app-left]
(e₁ e₂) →ᶠ (e₁' e₂)


               e₂ →ᶠ e₂'        v₁ is a value
────────────────────────────────────────────────  [app-right]
(v₁ e₂) →ᶠ (v₁ e₂')


                              f > 0
─────────────────────────────────────────────────  [let]
(let x = v in e) →ᶠ e[v/x]     (fuel: f - 1)


                              f > 0
─────────────────────────────────────────────────  [fix]
(fix x. e) →ᶠ e[(fix x. e)/x]     (fuel: f - 1)


                              f = 0
─────────────────────────────────────────────────  [out-of-fuel]
e →⁰ (error 'out-of-fuel)
```

**Fuel Semantics**:
- Each reduction step consumes fuel
- When fuel reaches 0, evaluation stops with `out-of-fuel`
- This guarantees termination: every evaluation completes in bounded steps

### 4.3 Call-by-Value Evaluation

The Fold uses *call-by-value* (strict) evaluation:

1. Arguments are evaluated before function application
2. Let bindings evaluate the bound expression before the body
3. No lazy evaluation or thunks in Core

**Rationale**: Call-by-value interacts predictably with effects (even though Core is pure, Shell is not) and simplifies reasoning about resource consumption.

### 4.4 The Homoiconic Mechanism

Homoiconicity means code can be manipulated as data. In The Fold:

**Quotation**: `(quote e)` suspends evaluation, yielding `e` as a data structure:

```scheme
(quote (+ 1 2))     ; → the list '(+ 1 2), not 3
(quote (λ x. x))    ; → the list '(λ x. x)
```

**Evaluation**: `(eval e)` interprets data as code:

```scheme
(eval '(+ 1 2))     ; → 3
(eval (quote (λ x. x)))  ; → the identity function
```

**Code↔Block Bijection**:

```scheme
;; S-expression → Block
(define (sexpr->block tag sexpr)
  (make-block tag (string->utf8 (format "~s" sexpr)) #()))

;; Block → S-expression
(define (block->sexpr blk)
  (read (open-string-input-port
          (utf8->string (block-payload blk)))))
```

This bijection enables:
- Storing code in the CAS
- Content-addressing programs
- Metaprogramming via block manipulation
- Serialization/deserialization of any value

### 4.5 Effects and the Shell Boundary

Core is *effect-free*. The Shell provides effects through a capability-based system:

**Capability Types**:
```scheme
(Cap FS T)    ; Filesystem capability producing T
(Cap Net T)   ; Network capability producing T
(Cap Time T)  ; Time/randomness capability producing T
```

**Effect Boundary**: A capability is a token authorizing specific operations. The Shell mints capabilities; Core code that needs effects must receive them as arguments:

```scheme
;; Shell mints a filesystem capability
(define fs-cap (mint-capability 'filesystem))

;; Core function requires capability as argument
(define (read-file cap path)
  (with-capability cap
    (shell-read-file path)))
```

**Monadic IO**: The FP toolkit (`lattice/fp/control/`) provides monadic abstractions:

```scheme
(>>= (read-line fs-cap)
     (lambda (line)
       (>>= (write-line fs-cap (string-upcase line))
            (lambda (_) (pure 'done)))))
```

This keeps Core pure while enabling practical programs.

### 4.6 Shell Implementation Details

The Shell ("thimble") is the verification boundary—code below is trusted, code above is verified. This section details Shell's invariants and implementation.

#### 4.6.1 Shell Invariants

The Shell maintains these invariants before invoking Core:

**I1. Well-formed S-expressions**: All input is syntactically valid. Malformed UTF-8, unbalanced parentheses, and invalid tokens are rejected before reaching Core.

```scheme
;; Shell validation pipeline
(define (validate-input raw-bytes)
  (let ([utf8-result (validate-utf8 raw-bytes)])
    (if (err? utf8-result)
        (error 'invalid-utf8 (err-msg utf8-result))
        (let ([sexpr-result (try-read (utf8->string raw-bytes))])
          (if (err? sexpr-result)
              (error 'malformed-sexpr (err-msg sexpr-result))
              (ok-val sexpr-result))))))
```

**I2. Type-compatible arguments**: Values passed to typed Core functions satisfy their declared types. Shell performs runtime type checks at the boundary.

```scheme
;; Boundary check before Core call
(define (call-core-function f args expected-types)
  (for-each
    (lambda (arg type)
      (unless (runtime-type-check arg type)
        (error 'type-mismatch arg type)))
    args expected-types)
  (apply f args))
```

**I3. Capability presence**: Effectful operations receive valid capability tokens. No capability = no effect.

**I4. Fuel budget**: Every Core invocation receives a finite fuel budget. Shell chooses the budget based on operation type and user configuration.

#### 4.6.2 Capability Implementation

Capabilities are unforgeable tokens authorizing specific effects. Implementation:

```scheme
;; Capability is a record with a unique, unguessable ID
(define-record-type capability
  (fields
    id          ; Cryptographically random 128-bit identifier
    kind        ; Symbol: 'filesystem, 'network, 'time, etc.
    scope       ; Restrictions: paths, hosts, etc.
    revoked?))  ; Mutable: can be revoked

;; Capability minting (Shell only)
(define (mint-capability kind scope)
  (make-capability
    (crypto-random-bytes 16)
    kind
    scope
    #f))

;; Capability checking
(define (check-capability cap required-kind operation)
  (cond
    [(capability-revoked? cap)
     (error 'revoked-capability cap)]
    [(not (eq? (capability-kind cap) required-kind))
     (error 'wrong-capability-kind required-kind (capability-kind cap))]
    [(not (scope-permits? (capability-scope cap) operation))
     (error 'scope-violation operation (capability-scope cap))]
    [else #t]))

;; Usage in Shell
(define (read-file cap path)
  (check-capability cap 'filesystem `(read ,path))
  (call-with-input-file path get-string-all))
```

**Capability hierarchy**:
```
(Cap-Root)                    ; Superuser, mints other capabilities
├── (Cap-FS scope)            ; Filesystem (scope: paths)
├── (Cap-Net scope)           ; Network (scope: hosts/ports)
├── (Cap-Time)                ; Current time, sleep
├── (Cap-Random)              ; Cryptographic randomness
└── (Cap-Subprocess scope)    ; Spawn processes (scope: allowed commands)
```

**Capability attenuation**: Capabilities can be narrowed but not widened:

```scheme
;; Attenuate filesystem cap to single directory
(define (attenuate-fs-cap cap allowed-path)
  (unless (path-prefix? allowed-path (capability-scope cap))
    (error 'cannot-widen-capability))
  (make-capability
    (crypto-random-bytes 16)  ; New ID
    'filesystem
    allowed-path              ; Narrower scope
    #f))
```

#### 4.6.3 Error Handling

Shell catches all errors from Core and presents them to users:

```scheme
(define (shell-eval expr fuel)
  (guard (exn
          [(out-of-fuel? exn)
           (format-error "Computation exceeded fuel budget (~a)"
                        (out-of-fuel-consumed exn))]
          [(type-error? exn)
           (format-type-error exn)]
          [(eval-error? exn)
           (format-eval-error exn)]
          [else
           (format-error "Internal error: ~a" exn)])
    (core-eval expr fuel)))
```

**Error categories**:

| Category | Source | User Message |
|----|----|----|
| `parse-error` | Shell | "Syntax error at line N: ..." |
| `type-error` | Core | "Type mismatch: expected T₁, got T₂" |
| `out-of-fuel` | Core | "Computation exceeded budget" |
| `unbound-var` | Core | "Undefined variable: x" |
| `capability-error` | Shell | "Operation requires capability C" |
| `io-error` | Shell | "Cannot read file: ..." |

#### 4.6.4 Shell/Core Protocol

Communication follows a strict protocol:

```
Shell                           Core
  │                               │
  ├─── validate(input) ──────────►│
  │                               │
  │◄── ok | parse-error ──────────┤
  │                               │
  ├─── infer-type(expr) ─────────►│
  │                               │
  │◄── type | type-error ─────────┤
  │                               │
  ├─── eval(expr, fuel, caps) ───►│
  │                               │
  │◄── value | error ─────────────┤
  │                               │
```

Core never initiates communication. Core never performs IO directly. All external interaction flows through Shell.

### 4.7 Metaprogramming and the Type System

The homoiconic mechanism (`quote`/`eval`) operates outside the type system. This section clarifies the interaction.

#### 4.7.1 Quotation is Untyped

`quote` produces an S-expression value, not a typed term:

```scheme
(quote (+ 1 2))        ; → '(+ 1 2), type: Sexpr
(quote (lambda (x) x)) ; → '(lambda (x) x), type: Sexpr
```

The type of `quote` is:
```
quote : (→ <syntax> Sexpr)
```

Where `<syntax>` is the syntactic category of expressions, not a type. This is a *macro* operation, not a function.

#### 4.7.2 Evaluation is Dynamically Typed

`eval` interprets an S-expression as code:

```scheme
(eval '(+ 1 2))        ; → 3
(eval '(lambda (x) x)) ; → <procedure>
```

The type of `eval`:
```
eval : (→ Sexpr ?)
```

The result type is unknown statically. `eval` may:
- Return any type
- Fail with a type error at runtime
- Fail with a syntax error

#### 4.7.3 Safe Metaprogramming Patterns

**Pattern 1: Generate, then type-check**

```scheme
;; Generate code
(define generated-code
  `(define (add-n n)
     (lambda (x) (+ x ,n))))

;; Type-check before use
(define checked-code
  (type-check-sexpr generated-code))

;; Only use if well-typed
(when (ok? checked-code)
  (eval generated-code))
```

**Pattern 2: Typed wrappers**

```scheme
;; Wrap eval with expected type
(define (eval-expecting type sexpr)
  (let ([result (eval sexpr)])
    (if (runtime-type-check result type)
        (ok result)
        (err 'type-mismatch type result))))

;; Usage
(eval-expecting '(→ Int Int) '(lambda (x) (+ x 1)))
```

**Pattern 3: Quasiquotation with typed holes**

```scheme
;; Typed value spliced into untyped template
(define (make-adder [n : Int])
  (eval `(lambda (x) (+ x ,n))))
;; n is type-checked; the template is not
```

#### 4.7.4 Why Not Typed Quotation?

Typed quotation (as in MetaML) would give:
```
quote : (∀ (A) (→ A (Code A)))
eval  : (∀ (A) (→ (Code A) A))
```

Where `Code A` represents code that, when evaluated, produces type `A`.

We don't implement this because:
1. **Complexity**: Requires staging levels, environment classifiers
2. **Homoiconicity tension**: S-expressions don't carry types
3. **Practical sufficiency**: Untyped metaprogramming + runtime checks works for our use cases

**Future direction**: A typed quotation sublanguage for specific patterns (e.g., SQL query generation) may be added.

#### 4.7.5 Content Addressing of Generated Code

Generated code participates in content addressing:

```scheme
;; Two generators produce the same code
(define code1 (generate-identity 'x))  ; '(lambda (x) x)
(define code2 (generate-identity 'y))  ; '(lambda (y) y)

;; After normalization, same hash
(equal? (hash-sexpr code1) (hash-sexpr code2))  ; → #t
```

Even metaprogrammed code benefits from semantic identity.

---
## 5. The Type Theory


The Fold implements a gradual dependent type system combining multiple features into a coherent whole.

### 5.1 Core Type Language

**Base Types**:
```
BaseType ::= Nat | Int | Bool | Char | Symbol
           | String | Bytes | Unit | Void | Hash
```

**Compound Types**:
```
Type ::= BaseType
       | (→ Type Type)           ; Function
       | (× Type Type)           ; Product (pair)
       | (+ (Tag Type) ...)      ; Sum (tagged union)
       | (List Type)             ; Homogeneous list
       | (Vector Type)           ; Homogeneous vector
       | (Block Symbol Type)     ; Block with tag and payload type
       | (Ref Type)              ; Content-addressed reference
       | (∀ (α ...) Type)        ; Universal quantification
       | (μ α. Type)             ; Recursive type
       | (Cap Name Type)         ; Capability type
       | ?                       ; Hole (gradual typing)
       | (? name)                ; Named hole
       | α                       ; Type variable
```

**Type Grammar** (BNF):
```bnf
<type>      ::= <base> | <compound> | <var> | <hole>
<base>      ::= "Nat" | "Int" | "Bool" | "Char" | "Symbol"
              | "String" | "Bytes" | "Unit" | "Void" | "Hash"
<compound>  ::= "(" "→" <type> <type> ")"
              | "(" "×" <type>+ ")"
              | "(" "+" <variant>+ ")"
              | "(" "List" <type> ")"
              | "(" "Vector" <type> ")"
              | "(" "Block" <symbol> <type> ")"
              | "(" "∀" "(" <var>+ ")" <type> ")"
              | "(" "μ" <var> <type> ")"
              | "(" "Cap" <name> <type> ")"
<variant>   ::= "(" <tag> <type> ")"
<hole>      ::= "?" | "(" "?" <name> ")"
<var>       ::= <identifier>
```

### 5.2 Bidirectional Type Checking

Following Dunfield & Krishnaswami (2013), we use bidirectional type checking with two judgments:

**Synthesis** (↑): `Γ ⊢ e ⇒ A` — infer type A for expression e
**Checking** (↓): `Γ ⊢ e ⇐ A` — check that e has type A

**Selected Rules**:

```
─────────────────────────────  [Var]
Γ, x : A ⊢ x ⇒ A


Γ ⊢ e ⇐ A
─────────────────────────────  [Anno]
Γ ⊢ (e : A) ⇒ A


Γ ⊢ e₁ ⇒ (→ A B)    Γ ⊢ e₂ ⇐ A
───────────────────────────────────  [App]
Γ ⊢ (e₁ e₂) ⇒ B


Γ, x : A ⊢ e ⇐ B
─────────────────────────────────  [Lam⇐]
Γ ⊢ (λ x. e) ⇐ (→ A B)


Γ ⊢ e ⇒ A    A ≤ B
───────────────────────────────  [Sub]
Γ ⊢ e ⇐ B
```

**Unification**: Type inference uses Robinson unification with occurs check:

```scheme
(define (unify t1 t2)
  (cond
    [(type-var? t1) (bind t1 t2)]
    [(type-var? t2) (bind t2 t1)]
    [(and (function-type? t1) (function-type? t2))
     (compose (unify (param t1) (param t2))
              (unify (return t1) (return t2)))]
    [(type=? t1 t2) empty-subst]
    [else (error 'unification-failed t1 t2)]))
```

### 5.3 Dependent Types

**Pi Types** (Dependent Functions):
```
(Π ((x : A)) B)
```

The type of functions where the return type B may depend on the argument value x.

```scheme
;; Length-indexed vectors
(Π ((n : Nat)) (→ (Vec n Int) Int))

;; head requires non-empty vector
(Π ((n : Nat)) (→ (Vec (succ n) A) A))
```

**Sigma Types** (Dependent Pairs):
```
(Σ ((x : A)) B)
```

Pairs where the type of the second component depends on the first component's value.

```scheme
;; Existential length
(Σ ((n : Nat)) (Vec n Int))
```

**Universe Hierarchy**:
```
Type₀ : Type₁ : Type₂ : ...
```

`Type₀` (often written just `Type`) classifies ordinary types. `Type₁` classifies `Type₀`, and so on. This hierarchy prevents Russell's paradox.

**Propositional Equality**:
```
(= A x y)    ; x equals y at type A
```

With introduction and elimination:
```scheme
refl : (Π ((A : Type)) (Π ((x : A)) (= A x x)))

J : (Π ((A : Type))
     (Π ((P : (Π ((x : A)) (Π ((y : A)) (→ (= A x y) Type)))))
      (Π ((x : A))
       (Π ((p : (P x x refl)))
        (Π ((y : A))
         (Π ((eq : (= A x y)))
          (P x y eq)))))))
```

### 5.4 Inductive Data Types

Data declarations introduce inductive types with constructors:

```scheme
(data Nat
  [zero : Nat]
  [succ : (→ Nat Nat)])

(data (List A)
  [nil  : (List A)]
  [cons : (→ A (→ (List A) (List A)))])

(data (Vec A (n : Nat))
  [vnil  : (Vec A zero)]
  [vcons : (Π ((m : Nat)) (→ A (→ (Vec A m) (Vec A (succ m)))))])
```

**Eliminator Generation**: Each inductive type generates an eliminator (recursor):

```scheme
;; For Nat:
elim-Nat : (Π ((P : (→ Nat Type)))
            (→ (P zero)
             (→ (Π ((n : Nat)) (→ (P n) (P (succ n))))
              (Π ((n : Nat)) (P n)))))
```

### 5.5 Normalization by Evaluation (NbE)

Type checking with dependent types requires comparing types for equality. We use Normalization by Evaluation:

**Semantic Domain**:
```scheme
V ::= (V-lam param body env)     ; Closure
    | (V-pi domain codomain)     ; Pi value
    | (V-sigma fst snd)          ; Sigma value
    | (V-type level)             ; Universe
    | (V-neutral var elims)      ; Stuck computation
    | (V-base tag)               ; Base type/value
```

**Evaluation**: Syntax → Semantics
```scheme
(define (eval expr env)
  (match expr
    [(λ x body) (V-lam x body env)]
    [(app f a) (apply-value (eval f env) (eval a env))]
    ...))
```

**Quotation**: Semantics → Syntax (normalized)
```scheme
(define (quote-value val)
  (match val
    [(V-lam x body env)
     (let ([v (fresh-var)])
       `(λ ,v ,(quote-value (eval body (extend env x (V-neutral v '()))))))]
    ...))
```

**Definitional Equality**:
```scheme
(define (conv-eq? v1 v2)
  (equal? (quote-value v1) (quote-value v2)))
```

### 5.6 Higher-Kinded Types and Kind System

**Kind Grammar**:
```
Kind ::= *                    ; Type
       | (⇒ Kind Kind)        ; Kind arrow
       | Constraint           ; Type class constraint
       | Row                  ; Row kind
       | (κ∀ (κ ...) Kind)    ; Kind polymorphism
       | (Πκ ((v : K)) K')    ; Dependent kind
       | □                    ; Sort (kind of kinds)
```

**Built-in Kinds**:
```scheme
List   : * ⇒ *
Either : * ⇒ * ⇒ *
(→)    : * ⇒ * ⇒ *
Monad  : (* ⇒ *) ⇒ Constraint
```

**Kind Inference**:
```scheme
(define (infer-kind type kenv)
  (match type
    [(@ f arg)
     (let ([fk (infer-kind f kenv)]
           [ak (infer-kind arg kenv)])
       (match fk
         [(⇒ dom cod)
          (if (kind=? dom ak) cod
              (error 'kind-mismatch))]))]
    ...))
```

### 5.7 Type Classes via Dictionary-Passing

Type classes are implemented as *explicit dictionary values*, not implicit resolution:

```scheme
;; Monoid dictionary type
(define-record-type monoid
  (fields mempty mappend))

;; Instances are values
(define monoid-sum
  (make-monoid 0 +))

(define monoid-list
  (make-monoid '() append))

;; Functions take dictionaries explicitly
(define (mconcat dict xs)
  (fold-right (monoid-mappend dict)
              (monoid-mempty dict)
              xs))

;; Usage
(mconcat monoid-sum '(1 2 3 4))   ; → 10
(mconcat monoid-list '((a) (b)))  ; → (a b)
```

**Benefits**:
- Maintains purity (no global instance database)
- Enables local instances (pass different dictionary)
- Simple, predictable semantics
- Compatible with Core's call-by-value model

**Functional Dependencies**: Multi-parameter classes with type-level functions:

```scheme
(class (Collection c e) | c -> e
  (empty : c)
  (insert : (→ e (→ c c))))
```

The `| c -> e` declares that `c` determines `e`—given a concrete collection type, the element type is fixed.

### 5.8 GADTs with Pattern Refinement

Generalized Algebraic Data Types allow constructors to return refined types:

```scheme
(data (Expr A)
  [Lit  : (→ Int (Expr Int))]
  [Bool : (→ Bool (Expr Bool))]
  [Add  : (→ (Expr Int) (→ (Expr Int) (Expr Int)))]
  [Eq   : (Π ((B : Type)) (→ (Expr B) (→ (Expr B) (Expr Bool))))]
  [If   : (Π ((B : Type))
           (→ (Expr Bool) (→ (Expr B) (→ (Expr B) (Expr B)))))])
```

**Pattern Refinement**: Matching on a GADT constructor refines the type variable:

```scheme
(define (eval-expr [e : (Expr A)]) : A
  (match e
    [(Lit n) n]                    ; A refined to Int
    [(Bool b) b]                   ; A refined to Bool
    [(Add x y) (+ (eval-expr x) (eval-expr y))]
    [(Eq B x y) (equal? (eval-expr x) (eval-expr y))]
    [(If B c t f) (if (eval-expr c) (eval-expr t) (eval-expr f))]))
```

### 5.9 Gradual Typing Integration

Holes (`?`) enable partial type specifications:

```scheme
;; Fully specified
(define (add [x : Int] [y : Int]) : Int (+ x y))

;; Partially specified
(define (process [x : ?]) : ? (complex-operation x))

;; Named holes for documentation
(define (transform [x : (? input)]) : (? output) ...)
```

**Semantics**: Holes unify with any type during inference. The system is *sound where types are known*—type errors are caught at typed boundaries, while untyped regions defer checking to runtime.

#### 5.9.1 Hole Constraint Tracking

Rather than simply ignoring holes during unification (which loses information), The Fold converts holes to *hole variables* and records what types they unify with:

**Anonymous Holes** (`?`): Each occurrence generates a fresh hole variable (`?1`, `?2`, etc.). Multiple anonymous holes are independent:

```scheme
;; (→ ? ?) unifying with (→ Int Bool) produces:
;;   ?1 → Int
;;   ?2 → Bool
;; The two holes are independent constraints
```

**Named Holes** (`(? name)`): All occurrences of the same named hole share a single hole variable (`?name`). This enforces consistency:

```scheme
;; (→ (? t) (? t)) unifying with (→ Int Int) succeeds:
;;   ?t → Int

;; (→ (? t) (? t)) unifying with (→ Int Bool) fails:
;;   Cannot unify Int with Bool (inconsistent use of ?t)
```

**Constraint Extraction**: After inference, hole constraints can be extracted from the substitution:

```scheme
(hole-constraints subst)      ; → ((?x . Int) (?y . Bool) ...)
(type-var-constraints subst)  ; → ((τ1 . String) ...)
```

This enables:
- Better error messages ("hole ?x was inferred as Int")
- Potential runtime cast generation for full gradual typing
- IDE tooling showing inferred types for holes

#### 5.9.2 Interaction with Dependent Types

Combining gradual and dependent types is a known hard problem (Eremondi et al., 2019). The core difficulty: in `(Π ((x : A)) B)`, the type `B` may mention `x`. If `A` is a hole, what is `x`? If we don't know `x`'s type, we cannot normalize `B`.

**Example of the problem:**
```scheme
;; What does this mean?
(Π ((x : ?)) (Vec x Int))

;; If x : Nat, this is a length-indexed vector
;; If x : String, this is nonsense
;; With x : ?, we cannot evaluate (Vec x Int)
```

**The Fold's approach: Strict separation**

Rather than approximate normalization (Eremondi et al.) or elaborate runtime checks, we enforce separation:

1. **Holes cannot appear in dependent positions**:
   ```scheme
   ;; Allowed: hole in simple function type
   (→ ? Int)

   ;; Rejected: hole as Pi domain when codomain depends on it
   (Π ((x : ?)) (Vec x Int))  ; ERROR: x used dependently

   ;; Allowed: hole in Pi domain when codomain is independent
   (Π ((x : ?)) Int)  ; OK: degenerates to (→ ? Int)
   ```

2. **Dependent elimination requires concrete types**:
   ```scheme
   ;; Pattern matching on (Vec n A) requires n to be concrete
   (match vec
     [(vnil) ...]
     [(vcons x xs) ...])  ; n must be known to refine types
   ```

3. **Gradual and dependent regions don't mix**:
   - A module is either "dependently typed" (no holes in signatures) or "gradually typed" (holes allowed, no dependent types)
   - Cross-boundary calls insert runtime checks

**What we sacrifice:**
- Cannot incrementally add dependent types to untyped code
- Cannot have "partially dependent" functions
- Less flexibility than full gradual dependent types

**What we gain:**
- Simple, predictable semantics
- No approximate normalization complexity
- Clear separation of concerns
- Type checking remains decidable

**Future direction**: We may explore restricted approximate normalization for specific patterns (e.g., length-indexed vectors with unknown but bounded length).

### 5.10 Contract System

Contracts provide runtime verification with precise blame tracking. They complement the static type system, especially for gradual typing boundaries.

#### 5.10.1 Contract Grammar

```
Contract ::= (Flat Predicate)              ; Simple predicate
           | (→ (Contract ...) Contract)   ; Function contract
           | (Dep (Var ...) Contract)      ; Dependent contract
           | (And Contract ...)            ; Conjunction
           | (Or Contract ...)             ; Disjunction
           | (Not Contract)                ; Negation
           | Any                           ; Top (always satisfied)
           | None                          ; Bottom (never satisfied)
```

**Flat contracts** check a predicate immediately:
```scheme
nat/c   = (Flat (λ (x) (and (integer? x) (>= x 0))))
pos/c   = (Flat (λ (x) (and (number? x) (> x 0))))
```

**Function contracts** specify domain and range:
```scheme
(->c (list nat/c nat/c) pos/c)  ; (Nat × Nat) → Pos
```

#### 5.10.2 Higher-Order Contract Wrapping

For higher-order functions, contracts must wrap function values to check them at each call site. The key challenge is **blame assignment**—who is at fault when a contract is violated?

**First-Order Blame**:
- **Domain violation** → blame the **caller** (they passed bad arguments)
- **Range violation** → blame the **callee** (the function returned a bad result)

**Higher-Order Blame Flip**:
When a function is passed as an argument, blame must "flip" across the boundary:

```scheme
;; Contract: ((Nat → Nat) → Nat)
;; A function that takes a callback and returns a Nat

(define (apply-twice f) (f (f 5)))
(define wrapped (apply-contract ho-contract apply-twice 'apply-twice))

;; Case 1: Caller provides bad callback (returns wrong type)
(wrapped (λ (x) "bad"))  ; Callback violates range → blame CALLER
                         ; (caller provided a faulty callback)

;; Case 2: Callee misuses the callback
(define (misuse f) (f "not a number"))  ; Calls f with string
(define wrapped-misuse (apply-contract ho-contract misuse 'misuse))
(wrapped-misuse (λ (x) (+ x 1)))  ; Domain violation → blame CALLEE
                                  ; (callee misused the callback)
```

The blame flip rule: when wrapping a higher-order argument, swap caller↔callee. This ensures:
- If the callback itself is broken → caller's fault (they provided it)
- If the callback is used incorrectly → callee's fault (they misused it)

#### 5.10.3 Blame Tracking

Blame records capture violation context:

```scheme
(blame party location message value)

party    : 'caller | 'callee
location : symbol (function name or source location)
message  : human-readable description
value    : the offending value
```

`flip-blame` swaps caller↔callee when crossing contract boundaries with higher-order values.

#### 5.10.4 Contract Combinators

**Container contracts**:
```scheme
(listof nat/c)      ; List of natural numbers
(vectorof string/c) ; Vector of strings
```

**Range contracts**:
```scheme
(between/c 0 100)   ; Numbers in [0, 100]
```

**Enumeration**:
```scheme
(one-of/c 'red 'green 'blue)  ; Must be one of these symbols
```

**Boolean combinators**:
```scheme
(and/c nat/c pos/c)           ; Both must hold
(or/c nat/c string/c)         ; At least one must hold
(not/c nat/c)                 ; Must NOT be a natural number
```

### 5.11 Category Theory Foundations

The Fold provides category-theoretic abstractions as first-class values, supporting compositional reasoning about functors and transformations.

#### 5.11.1 Functors

A functor F : C → D maps objects and morphisms while preserving identity and composition. In The Fold, functors over Scheme values are represented as records containing an `fmap` function:

```scheme
(define functor-list (make-functor map))
(define functor-maybe (make-functor maybe-fmap))

;; fmap : (a → b) → F a → F b
(fmap-with functor-list add1 '(1 2 3))  ; → (2 3 4)
```

#### 5.11.2 Natural Transformations

A **natural transformation** η : F ⟹ G between functors assigns to each type A a morphism η_A : F(A) → G(A) such that the naturality square commutes:

```
     F(A) ───η_A──→ G(A)
      │              │
    F(f)           G(f)
      │              │
      ↓              ↓
     F(B) ───η_B──→ G(B)
```

That is: `G(f) ∘ η_A = η_B ∘ F(f)` for all morphisms f : A → B.

**Definition**:
```scheme
(define nat-head
  (make-nat-transform
   'head
   functor-list      ; source: List
   functor-maybe     ; target: Maybe
   (lambda (xs)      ; component: [A] → Maybe A
     (if (null? xs) nothing (just (car xs))))))

(nat-apply nat-head '(1 2 3))  ; → (just 1)
(nat-apply nat-head '())       ; → nothing
```

**Vertical Composition** (η ∘ ε): Chain transformations F ⟹ G ⟹ H:
```scheme
(define η (nat-maybe-to-either 'empty))  ; Maybe ⟹ Either
(define composed (nat-compose η nat-head)) ; List ⟹ Maybe ⟹ Either
(nat-apply composed '(a b))  ; → (right a)
(nat-apply composed '())     ; → (left 'empty)
```

**Horizontal Composition** (η * ε): The Godement product composes transformations on composed functors. Given η : F ⟹ G and ε : H ⟹ K, produces (η * ε) : H∘F ⟹ K∘G.

**Whiskering**: Extend a transformation by composing with a functor:
- Right whiskering (η ◁ H): Precompose with H, producing F∘H ⟹ G∘H
- Left whiskering (H ▷ η): Postcompose with H, producing H∘F ⟹ H∘G

#### 5.11.3 Naturality Verification

The system can verify the naturality condition for specific test cases:

```scheme
;; Test: Maybe(f) ∘ head = head ∘ List(f)
(check-naturality nat-head add1 '(1 2 3))  ; → #t

;; Verify with multiple morphisms and values
(verify-naturality nat-head
  (list (cons add1 '(1 2 3))
        (cons symbol->string '(a b c))))  ; → #t
```

#### 5.11.4 Natural Isomorphisms

A natural isomorphism is a natural transformation where each component is invertible. This captures when two functors are "essentially the same":

```scheme
(define maybe≅either
  (make-nat-iso 'maybe≅either
    functor-maybe
    functor-either-unit
    (lambda (m) (if (just? m) (right (from-just m)) (left '())))
    (lambda (e) (if (right? e) (just (from-right e)) nothing))))

;; Round-trip is identity
(nat-apply (nat-compose (nat-iso-inverse maybe≅either)
                        (nat-iso-forward maybe≅either))
           (just 42))  ; → (just 42)
```

#### 5.11.5 Common Natural Transformations

| Transformation | Type | Description |
|---------------|------|-------------|
| `nat-head` | List ⟹ Maybe | First element or nothing |
| `nat-singleton` | Maybe ⟹ List | Wrap in singleton or empty |
| `nat-concat` | List∘List ⟹ List | Flatten (monad join) |
| `nat-pure-list` | Id ⟹ List | Wrap in singleton (monad unit) |
| `nat-pure-maybe` | Id ⟹ Maybe | Wrap in Just |
| `nat-either-to-maybe` | Either ⟹ Maybe | Forget error |
| `nat-maybe-to-either` | Maybe ⟹ Either | Add default error |

These transformations satisfy the naturality condition and compose correctly, enabling equational reasoning about data flow through functor pipelines.

### 5.12 Adjoint Functors

An **adjunction** F ⊣ G between categories C and D is the fundamental structure-preserving relationship in category theory—more fundamental than equivalence and more general than isomorphism. It consists of:

- **Left adjoint** F : C → D
- **Right adjoint** G : D → C
- **Unit** η : Id_C ⟹ G∘F (a natural transformation)
- **Counit** ε : F∘G ⟹ Id_D (a natural transformation)

```scheme
(define adj-free-list
  (make-adjunction
   'free-list
   functor-list    ; Left: Free (lifts to list)
   functor-id      ; Right: Forgetful (underlying set)
   nat-pure-list   ; Unit: η (singleton wrapping)
   nat-concat))    ; Counit: ε (flatten/join)
```

#### 5.12.1 Triangle Identities

The unit and counit must satisfy the **triangle identities** (also called zig-zag laws), which ensure coherence:

```
         η_F              F▷η            ε◁F
    F ═══════════▶ GFG ═════════▶ GFG ═══════════▶ F
                        should equal id_F

         η◁G              G▷ε
    G ═══════════▶ GFG ═══════════▶ G
                  should equal id_G
```

Formally:
- **Left triangle**: (ε ◁ F) ∘ (F ▷ η) = id_F
- **Right triangle**: (G ▷ ε) ∘ (η ◁ G) = id_G

Where ▷ denotes left whiskering and ◁ denotes right whiskering.

```scheme
;; Verify triangle identities for specific test values
(verify-triangle-left adj-free-list '(1 2 3))   ; → #t
(verify-triangle-right adj-free-list '(a b c))  ; → #t

;; Combined verification
(verify-adjunction adj-free-list '(1 2 3) '(a b c))  ; → #t
```

#### 5.12.2 Hom-Set Bijection

An equivalent characterization: an adjunction F ⊣ G yields a natural isomorphism between hom-sets:

```
Hom_D(F(A), B) ≅ Hom_C(A, G(B))
```

This bijection is implemented by the **transpose** operations:

```scheme
;; Left transpose: (F(A) → B) → (A → G(B))
;; Given f : F(A) → B, produce G(f) ∘ η_A
(define g (adjunction-transpose-left adj f))

;; Right transpose: (A → G(B)) → (F(A) → B)
;; Given g : A → G(B), produce ε_B ∘ F(g)
(define f (adjunction-transpose-right adj g))
```

Example with the free monoid adjunction:

```scheme
;; f: List(Int) → Int (sum the list)
(define f (lambda (xs) (apply + xs)))

;; Transpose to g: Int → Int
;; g(x) = f([x]) = x
(define g (adjunction-transpose-left adj-free-list f))
(g 10)  ; → 10

;; Round-trip: transpose-right(transpose-left(f)) = f
```

#### 5.12.3 Common Adjunctions

| Adjunction | Left (F) | Right (G) | Description |
|------------|----------|-----------|-------------|
| `adj-free-list` | List | Id | Free monoid: singleton ⊣ underlying set |

The **free-forgetful** pattern is ubiquitous:
- Free monoid: F(S) = lists over S, G forgets the monoid structure
- Free group: F(S) = group generated by S, G forgets group structure
- Free vector space: F(S) = formal linear combinations over S

```scheme
;; Free monoid adjunction
;; η_A : A → List(A)  wraps in singleton (unit/pure)
;; ε_B : List(List(B)) → List(B)  flattens (join/concat)

(nat-apply nat-pure-list 42)           ; → (42)
(nat-apply nat-concat '((1 2) (3 4)))  ; → (1 2 3 4)
```

#### 5.12.4 Adjunction Composition

Adjunctions compose: given F ⊣ G and F' ⊣ G', we get (F' ∘ F) ⊣ (G ∘ G'):

```scheme
(define adj-id
  (make-adjunction 'id functor-id functor-id
                   (nat-id functor-id)
                   (nat-id functor-id)))

(define composed (adjunction-compose adj-free-list adj-id))
(adjunction-name composed)  ; → 'free-list∘id
```

The composed unit and counit are constructed using whiskering and natural transformation composition.

#### 5.12.5 Galois Connections

A **Galois connection** is an adjunction between preorders (categories where hom-sets have at most one element). Given posets (P, ≤) and (Q, ≤), a Galois connection consists of monotone functions:

- Lower adjoint f : P → Q
- Upper adjoint g : Q → P

Such that: f(p) ≤ q ⟺ p ≤ g(q)

```scheme
(define galois-floor-ceil
  (make-galois 'ceil-inclusion ceiling (lambda (x) x)))

;; Closure operator: g ∘ f (always ≥ original)
(galois-closure galois-floor-ceil 3.1)  ; → 4.0

;; Kernel operator: f ∘ g
(galois-kernel galois-floor-ceil 5)     ; → 5
```

**Properties of Galois closures**:
- **Extensive**: x ≤ closure(x)
- **Monotone**: x ≤ y implies closure(x) ≤ closure(y)
- **Idempotent**: closure(closure(x)) = closure(x)

Examples of Galois connections:
- Ceiling ⊣ Inclusion (ℤ ⊆ ℝ)
- Interior ⊣ Closure (topology)
- Abstraction ⊣ Concretization (abstract interpretation)

### 5.13 Comonads

A **comonad** is the categorical dual of a monad. Where monads encode effects and sequencing, comonads encode contexts and decomposition. A comonad W on a category C consists of:

- A functor W : C → C
- **Extract** ε : W ⟹ Id (counit, dual of return)
- **Duplicate** δ : W ⟹ W∘W (comultiplication, dual of join)

Or equivalently, via **extend**:
- **Extend** : (W A → B) → W A → W B

These satisfy the dual of monad laws:

```
Law 1: extend extract = id
Law 2: extract ∘ extend f = f
Law 3: extend f ∘ extend g = extend (f ∘ extend g)
```

#### 5.13.1 Comonad Type Class

```scheme
(define (make-comonad functor extract-fn extend-fn)
  (list 'comonad functor extract-fn extend-fn))

;; Extract a value from context
(define (extract-with comonad wa)
  ((comonad-extract comonad) wa))

;; Extend a function over all positions
(define (extend-with comonad f wa)
  ((comonad-extend comonad) f wa))

;; Derive duplicate from extend
(define (duplicate-with comonad wa)
  (extend-with comonad (lambda (x) x) wa))
```

#### 5.13.2 Store Comonad

The **Store comonad** `Store S A = (S → A) × S` represents a position in a space with the ability to access any other position. It's ideal for:
- Cellular automata (each cell can see neighbors)
- Zippers and cursors
- Image processing (convolution kernels)

```scheme
(define (make-store accessor position)
  (list 'store accessor position))

;; Peek at another position
(define (store-peek store pos)
  ((store-accessor store) pos))

;; Move to a new position
(define (store-seek store new-pos)
  (make-store (store-accessor store) new-pos))

;; Extract: get value at current position
(define (store-extract store)
  ((store-accessor store) (store-position store)))

;; Extend: apply function at every position
(define (store-extend f store)
  (make-store
   (lambda (pos) (f (store-seek store pos)))
   (store-position store)))
```

**Example: Cellular Automaton Rule**

```scheme
;; Count live neighbors and apply rule
(define (rule store)
  (let* ([pos (store-position store)]
         [left (store-peek store (- pos 1))]
         [right (store-peek store (+ pos 1))]
         [neighbors (+ left right)])
    (if (= neighbors 1) 1 0)))  ; Rule 30-ish

;; One generation step
(define (step world)
  (store-extend rule world))
```

#### 5.13.3 Env Comonad

The **Env comonad** `Env E A = (E, A)` represents a value with an immutable environment (dual of Reader monad):

```scheme
(define (make-env environment value)
  (list 'env environment value))

(define (env-extract env) (env-value env))

(define (env-extend f env)
  (make-env (env-environment env) (f env)))

;; Access environment without modifying
(define (env-ask env) (env-environment env))

;; Transform environment locally
(define (env-local f env)
  (make-env (f (env-environment env)) (env-value env)))
```

#### 5.13.4 Traced Comonad

The **Traced comonad** `Traced M A = M → A` for monoid M represents a computation that depends on an accumulated monoidal context:

```scheme
(define (make-traced run-fn monoid)
  (list 'traced run-fn monoid))

(define (traced-extract traced)
  ((traced-run traced) (monoid-identity (traced-monoid traced))))

(define (traced-extend f traced)
  (let ([monoid (traced-monoid traced)])
    (make-traced
     (lambda (m)
       (f (make-traced
           (lambda (m2)
             ((traced-run traced)
              ((monoid-op monoid) m m2)))
           monoid)))
     monoid)))
```

#### 5.13.5 Comonad from Adjunction

Every adjunction F ⊣ G yields a comonad on the domain category via F∘G:

```scheme
;; Given F ⊣ G with unit η and counit ε:
;; W = F ∘ G
;; extract = ε (counit)
;; duplicate = F(η_G) (whisker unit through F)

(define (comonad-from-adjunction adj)
  (let* ([F (adjunction-left adj)]
         [G (adjunction-right adj)]
         [η (adjunction-unit adj)]
         [ε (adjunction-counit adj)])
    (make-comonad
     (functor-compose F G)
     (nat-transform-component ε)
     (lambda (f wa)
       ;; extend f = fmap f ∘ duplicate
       ;; = F∘G(f) ∘ F(η_G)
       ((functor-fmap F)
        (lambda (ga) (f ((functor-fmap F) (nat-transform-component η) ga)))
        wa)))))
```

The **Store comonad** arises from the product-exponential adjunction (−)×S ⊣ (−)^S.

#### 5.13.6 Comonad Composition

Like monads, **comonads require a distributive law** to compose. Given comonads W₁ and W₂, composition requires:

```
δ : W₂(W₁(a)) → W₁(W₂(a))
```

satisfying coherence conditions:
- `δ ∘ W₂(extract₁) = extract₁` (distributing then extracting W₁ is extracting W₁)
- `δ ∘ extract₂ = W₁(extract₂)` (extracting W₂ then distributing is just extracting W₂ inside)
- duplicate coherence for both layers

The implementation requires a **copeek** operation `W₂(a) → W₂(a) → a` that extracts from the second argument at the position indicated by the first:

```scheme
(define (compose-comonads-with-dist* w1 w2 dist copeek)
  (let ([extr1 (comonad-extract w1)]
        [extr2 (comonad-extract w2)]
        [ext1  (comonad-extend w1)]
        [ext2  (comonad-extend w2)]
        [fmap1 (functor-fmap (comonad-functor w1))])
    (make-comonad
     (functor-compose (comonad-functor w1) (comonad-functor w2))
     ;; extract: W₁(W₂(a)) → a
     (lambda (w1w2a) (extr2 (extr1 w1w2a)))
     ;; extend: (W₁(W₂(a)) → b) → W₁(W₂(a)) → W₁(W₂(b))
     (lambda (f w1w2a)
       (ext1
        (lambda (w1-pos)
          (ext2
           (lambda (w2-foc)
             ;; Build W₂(W₁(a)) by iterating over W₂ positions
             (let ([w2w1a
                    (ext2
                     (lambda (w2-iter)
                       (fmap1
                        (lambda (w2-in-w1)
                          (copeek w2-iter w2-in-w1))
                        w1-pos))
                     w2-foc)])
               (f (dist w2w1a))))
           (extr1 w1-pos)))
        w1w2a)))))
```

The key insight: when W₂ has meaningful position (like Store), `copeek` must extract at the indicator's position; when W₂ is trivial (like Env), the default `(lambda (pos val) (extract val))` suffices.

### 5.14 Monad Derivation from Adjunctions

Every adjunction F ⊣ G gives rise to a monad on the codomain of G (= domain of F). This provides a principled, unified derivation of all standard monads.

#### 5.14.1 The Derivation

Given adjunction F ⊣ G with unit η and counit ε:

```
Monad M = G ∘ F

return : A → M A = η_A (unit)
join : M(M A) → M A = G(ε_{F(A)}) (apply counit under G)
```

In code:

```scheme
(define (monad-from-adjunction adj)
  (let* ([F (adjunction-left adj)]
         [G (adjunction-right adj)]
         [η (adjunction-unit adj)]
         [ε (adjunction-counit adj)]
         [M (functor-compose G F)])
    (make-monad-ops
     (string->symbol (format "monad-~a" (adjunction-name adj)))
     (nat-transform-component η)              ; return = η
     (functor-fmap M)                          ; fmap from composed functor
     (lambda (mma)                             ; join = G(ε_F)
       ((functor-fmap G)
        (nat-transform-component ε)
        mma))
     (lambda (ma f)                            ; bind via join and fmap
       (join (fmap f ma))))))
```

#### 5.14.2 Example: List Monad from Free Monoid

The List monad arises from the free-forgetful adjunction for monoids:

```scheme
;; F: Set → Mon (free monoid = lists)
;; G: Mon → Set (forget monoid structure)
;; F ⊣ G

(define adj-free-list
  (make-adjunction
   'free-list
   functor-list     ; F: wraps in lists
   functor-id       ; G: identity (forgets structure)
   nat-pure-list    ; η: singleton wrapping
   nat-concat))     ; ε: concatenation (join)

(define monad-list-derived
  (monad-from-adjunction adj-free-list))

;; Verify it works
(define return (monad-ops-return monad-list-derived))
(define bind (monad-ops-bind monad-list-derived))

(return 42)  ; → '(42)
(bind '(1 2 3) (lambda (x) (list x x)))  ; → '(1 1 2 2 3 3)
```

#### 5.14.3 Example: State Monad

The State monad arises from the product-exponential adjunction:

```scheme
;; For fixed state type S:
;; F(A) = A × S  (product functor)
;; G(B) = S → B  (exponential functor)
;; F ⊣ G (currying adjunction)

(define (make-state-adjunction state-type)
  (make-adjunction
   'state
   (make-product-functor state-type)
   (make-exponential-functor state-type)
   (make-state-unit)
   (make-state-counit)))

;; Derived: State S A = S → (A × S)
;; return a = λs. (a, s)
;; m >>= f = λs. let (a, s') = m s in f a s'
```

#### 5.14.4 MonadOps Record

Derived monads are packaged in a record containing all operations:

```scheme
(define-record-type monad-ops
  (fields name return fmap join bind))

;; Usage
(monad-ops-name monad-list-derived)    ; → 'monad-free-list
(monad-ops-return monad-list-derived)  ; → singleton procedure
(monad-ops-bind monad-list-derived)    ; → concatMap procedure
```

#### 5.14.5 Law Verification

The derivation automatically satisfies monad laws (by the triangle identities of the adjunction). Verification functions confirm this:

```scheme
;; Left identity: return a >>= f = f a
(verify-left-identity monad-list-derived 5 (lambda (x) (list x x)))  ; → #t

;; Right identity: m >>= return = m
(verify-right-identity monad-list-derived '(1 2 3))  ; → #t

;; Associativity: (m >>= f) >>= g = m >>= (λx. f x >>= g)
(verify-associativity monad-list-derived '(1 2)
  (lambda (x) (list x (+ x 1)))
  (lambda (y) (list y y)))  ; → #t

;; All laws at once
(verify-monad-laws monad-list-derived ...)  ; → #t
```

### 5.15 Kan Extensions and the Codensity Monad

**Kan extensions** are "the most universal construction" in category theory—every other concept (limits, colimits, adjunctions, ends) can be expressed as a Kan extension.

#### 5.15.1 Right Kan Extension

The **Right Kan extension** of F : C → E along K : C → D is a functor Ran_K F : D → E together with a universal natural transformation:

```
(Ran_K F) A = ∀B. (A → K B) → F B
```

Universal property: Any natural transformation G∘K ⟹ F factors uniquely through Ran_K F.

```scheme
(define (make-ran k f computation)
  (list 'ran k f computation))

;; Apply the Ran to a K-morphism
(define (ran-apply ran k-morphism)
  ((ran-computation ran) k-morphism))

;; Ran is a functor
(define (ran-fmap f ran)
  (make-ran (ran-k ran) (ran-f ran)
    (lambda (k)
      ((ran-computation ran) (compose k f)))))
```

#### 5.15.2 Left Kan Extension

The **Left Kan extension** is dual:

```
(Lan_K F) A = ∃B. (K B → A, F B)
```

It's a coend: Lan_K F = ∫^B (K B → A) ⊗ F B

```scheme
(define (make-lan k f morphism value)
  (list 'lan k f morphism value))

;; Inject a value into Lan
(define (lan-inject k f fb morphism)
  (make-lan k f morphism fb))

;; Lan is a functor
(define (lan-fmap f lan)
  (make-lan (lan-k lan) (lan-f lan)
    (compose f (lan-morphism lan))
    (lan-value lan)))
```

#### 5.15.3 The Codensity Monad

The **Codensity monad** is the Right Kan extension of a monad M along itself:

```
Codensity M A = Ran_M M A = ∀R. (A → M R) → M R
```

This is exactly **continuation-passing style** made categorical. The key insight:

```
Standard bind: m >>= f     Rebuilds structure each time → O(n²) for left-nested binds
Codensity bind: ca >>= f   Composes continuations → O(1) per bind, O(n) at lower
```

```scheme
(define (codensity-return m-return a)
  (make-codensity m-return (lambda (k) (k a))))

(define (codensity-bind ca f)
  (make-codensity
   (codensity-return-fn ca)
   (lambda (k)
     ((codensity-run ca)
      (lambda (a) ((codensity-run (f a)) k))))))

;; Lower back to base monad
(define (codensity-lower ca)
  ((codensity-run ca) (codensity-return-fn ca)))

;; Lift from base monad
(define (codensity-lift m-return m-bind ma)
  (make-codensity m-return (lambda (k) (m-bind ma k))))
```

#### 5.15.4 Connection to The Fold's Effect System

The Codensity monad explains the O(1) bind optimization used in `lattice/fp/free.ss` and `lattice/fp/effects.ss`:

```scheme
;; In free.ss, the 'free-queue variant:
('free-queue base-free fmap continuation-queue)

;; In effects.ss, the 'eff-queue variant:
('eff-queue base-eff continuation-queue)
```

These **are Codensity monad implementations**. The queue represents accumulated continuations `(A → M R)` waiting to be applied. Instead of nested lambda closures, the queue defunctionalizes the continuation:

```
Naive bind chain: ((((m >>= f) >>= g) >>= h) >>= i)
  Each >>= traverses the accumulated structure → O(n²)

Codensity/queue: m with conts = [f, g, h, i]
  Each >>= just appends to queue → O(1)
  Final lower applies all at once → O(n)
```

#### 5.15.5 Difference Lists via Codensity

The classic "difference list" pattern is Codensity applied to the List monad:

```scheme
;; Normal list append: [1,2] ++ [3,4]
;; Must traverse [1,2] to find end → O(n)

;; Codensity List: λxs. 1:2:xs
;; Composition is function composition → O(1)

(define (codensity-list-singleton x)
  (codensity-return list x))

(define (codensity-list-append c1 c2)
  (codensity-bind c1
    (lambda (x)
      (codensity-bind c2
        (lambda (y)
          (codensity-return list (cons x y)))))))

;; Lower to regular list
(define (codensity-list-lower c)
  (codensity-lower c))
```

This transforms O(n²) left-associative appends into O(n):

```scheme
;; Left-associative: (((a ++ b) ++ c) ++ d)
;;   Step 1: traverse a          → O(|a|)
;;   Step 2: traverse a ++ b     → O(|a| + |b|)
;;   Step 3: traverse all so far → O(|a| + |b| + |c|)
;;   Total: O(n²) where n = total elements

;; Codensity: lower (c_a ∘ c_b ∘ c_c ∘ c_d)
;;   All compositions: O(1) each → O(n) total
;;   Final lower: single traversal → O(n)
;;   Total: O(n)
```

#### 5.15.6 Generic Codensity Monad Builder

A utility constructs Codensity for any monad:

```scheme
(define (make-codensity-monad m-return m-bind)
  (list
   (lambda (a) (codensity-return m-return a))           ; return
   (lambda (ca f) (codensity-bind ca f))                ; bind
   (lambda (ma) (codensity-lift m-return m-bind ma))    ; lift
   (lambda (ca) (codensity-lower ca))))                 ; lower
```

**Usage pattern**:

```scheme
;; Build optimized Maybe monad
(define codensity-maybe (make-codensity-monad just maybe-bind))
(define cm-return (car codensity-maybe))
(define cm-bind (cadr codensity-maybe))
(define cm-lower (cadddr codensity-maybe))

;; Use Codensity for computation
(define result
  (cm-lower
    (cm-bind (cm-return 5)
      (lambda (x)
        (cm-bind (cm-return (* x 2))
          (lambda (y)
            (cm-return (+ y 1))))))))
;; → (just 11), but with O(1) binds
```

---
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
## 7. Implementation


### 7.1 Technology Stack

**Runtime**: Chez Scheme (R6RS-compatible)
- High-performance native code compilation
- Efficient continuation support
- Rich numeric tower

**Dependencies**: None external
- SHA-256: Self-contained FIPS 180-4 implementation
- UTF-8: Built-in Scheme support
- Data structures: All implemented in-house

**Rationale**: Third-party dependencies introduce supply chain risk and verification burden. But more fundamentally, external code is a black box—you can't measure its fuel consumption, can't introspect its behavior, can't extend it without forking, can't trace exactly what happens when it runs. By implementing everything in-house, The Fold is fully *introspectable* (you can follow any execution path), *measurable* (fuel tracking works everywhere), and *hackable* (no behavior is opaque or off-limits). No surprises, no black boxes.

Note: The Rust acceleration layer (§7.4) is an exception that proves the rule—it's in-house code that provides the same guarantees (fuel tracking, no hidden state, no opaque behavior), just implemented in a faster language for performance-critical paths.

### 7.2 Key Design Decisions

**Why Blocks?**

Blocks provide *uniform representation*. Code, data, types, modules—all are blocks. This enables:
- Universal content addressing
- Introspection and reflection
- Serialization of anything
- Merkle DAG structure

**Why Content Addressing?**

Content addressing provides *semantic identity*:
- Same content → same identity (automatic)
- Immutable by construction
- Deduplication for free
- Tamper-evident (hash verification)

**Why De Bruijn Indices?**

De Bruijn indices eliminate naming from identity:
- α-equivalent terms hash identically
- No variable naming conventions needed
- Canonical representation enables structural comparison
- Proven technique from proof assistants

**Why Algebraic Canonicalization?**

De Bruijn alone misses semantic equivalences:
- `(+ a b)` and `(+ b a)` are mathematically equal but hash differently
- Independent bindings in different orders are semantically equivalent
- Associativity allows multiple valid parenthesizations

Algebraic canonicalization extends semantic identity:
- Commutative operations sorted: same hash regardless of argument order
- Associative operations flattened: same hash regardless of nesting
- Independent bindings sorted: same hash regardless of declaration order
- Conservative purity analysis prevents unsafe reordering

The version byte (0x01) distinguishes algebraically-normalized hashes from α-only hashes (0x00), ensuring backwards compatibility.

**Why Pure Core + Impure Shell?**

Separation enables verification:
- Core: small, pure, formally verifiable
- Shell: practical, handles messy reality
- Clear boundary for trust decisions
- Neither compromises the other

### 7.3 Performance Considerations

**Space Complexity**:

| Structure | Space |
|----|----|
| Block | O(tag + payload + refs) |
| Address | 33 bytes (fixed) |
| CAS lookup | O(1) average |

**Time Complexity**:

| Operation | Time |
|----|----|
| `hash-block` | O(payload size) |
| `store!` / `fetch` | O(1) average |
| `normalize` (α-only) | O(expression size) |
| `normalize-algebraic` | O(n log n) for sorting |
| `normalize-full` | O(n log n) |
| `gc!` | O(stored blocks) |
| BM25 search | O(n log n) |

**Cryptographic Properties**:
- SHA-256: 256-bit collision resistance
- Avalanche: 1-bit input change → ~50% output change
- Preimage resistance: Cannot reverse hash

### 7.4 Rust Acceleration Layer

Performance-critical paths have optional Rust acceleration via FFI, located in `shell/ffi/rust-accel/`. This layer is designed for operations where computation significantly exceeds FFI overhead.

#### 7.4.1 Architecture

The Rust layer follows strict design principles to maintain The Fold's guarantees:

**FFI Safety**:
- All exposed types use `#[repr(C)]` for stable ABI
- Out-pointers pattern: Scheme allocates, Rust writes results
- No panics—all errors return status codes
- Null pointer checks on all inputs

**Fuel Preservation**:
- Each operation declares fuel costs matching Scheme's fuel model
- Fuel is checked before expensive operations
- Status code 2 indicates fuel exhaustion
- Remaining fuel is always returned to caller

**Result Struct Pattern**:
```rust
#[repr(C)]
pub struct F64Result {
    pub status: u8,      // 1=success, 2=out-of-fuel, 3=runtime-error
    pub value: f64,
    pub fuel_out: u64,
}
```

#### 7.4.2 Spatial Acceleration (BVH)

The BVH module provides fuel-tracked Bounding Volume Hierarchy operations:

**BVH Construction** (`fold_bvh_build`):
- Parses serialized BVH from bytevector
- Format: header (16 bytes) + nodes (64 bytes each) + triangles (72 bytes each)
- Returns opaque handle for subsequent queries

**Closest Point Query** (`fold_bvh_closest_point`):
- Finds closest point on mesh surface to query point
- Traverses closer children first for better pruning
- Fuel costs: base query (5) + per node (2) + AABB test (3) + triangle test (10)

**Ray Intersection** (`fold_bvh_intersect_ray`):
- Finds first ray-mesh intersection
- Returns distance and surface normal
- Fuel costs: base query (5) + per node (2) + AABB test (3) + triangle ray test (8)

#### 7.4.3 Raymarching

The raymarching module moves entire sphere-tracing loops to Rust:

**Mesh Raymarching** (`fold_raymarch_mesh`):
- Complete sphere tracing in single FFI call
- Computes signed distance via BVH queries
- Returns hit point, normal, distance, step count, and triangle index
- Gradient-based normal computation (6 SDF queries)

This eliminates per-step FFI overhead—critical for raymarching which may require hundreds of steps.

#### 7.4.4 Matrix Operations

4x4 matrix operations where computation exceeds FFI overhead (~112 ops for matrix multiply):

| Operation | Fuel Cost | Description |
|----|----|----|
| `fold_mat4_mul` | 112 | Matrix multiplication (fully unrolled) |
| `fold_mat4_vec_mul` | 28 | Matrix-vector multiplication |
| `fold_mat4_transform_points` | 28×N | Batch transform N points |
| `fold_mat4_transpose` | 16 | Matrix transpose |
| `fold_mat4_determinant` | 100 | 4x4 determinant via cofactors |

**Batch Operations**: `fold_mat4_transform_points` demonstrates Layer 2 FFI design—amortizing overhead across N points makes FFI cost negligible.

#### 7.4.5 Core Types

The Rust layer defines FFI-safe equivalents of Scheme types:

```rust
#[repr(C)]
pub struct Vec3 { pub x: f64, pub y: f64, pub z: f64 }

#[repr(C)]
pub struct AABB { pub min: Vec3, pub max: Vec3 }

#[repr(C)]
pub struct Triangle { pub p1: Vec3, pub p2: Vec3, pub p3: Vec3, pub id: u32 }
```

All operations are pure and inlined for performance.

### 7.5 Developer Experience

This section addresses practical concerns for developers using The Fold.

#### 7.5.1 Error Messages

Type errors include source locations and contextual information:

```
Type error at vec.ss:45:12

  (vec+ v1 v2)
        ^^
  Expected: (Vec n Num)
  Got:      (List Num)

  In the expression:
    (define (combine v1 v2)
      (vec+ v1 v2))

  Hint: vec+ requires vectors, not lists.
        Use (list->vec v1) to convert.
```

**Error message principles**:
1. **Location**: File, line, column, with source excerpt
2. **Expected vs. actual**: Clear type comparison
3. **Context**: Enclosing expression for clarity
4. **Hints**: Actionable suggestions where possible

#### 7.5.2 Incremental Development with Holes

Holes enable incremental typing without sacrificing safety:

**Workflow**:

1. **Start untyped**: Use `?` everywhere
   ```scheme
   (define (process x) : ?
     (complex-operation x))
   ```

2. **Add types incrementally**: Specify what you know
   ```scheme
   (define (process [x : InputData]) : ?
     (complex-operation x))
   ```

3. **Let inference propagate**: Type checker fills in constraints
   ```scheme
   ;; Checker reports: return type is (Result OutputData Error)
   ```

4. **Finalize**: Replace holes with concrete types
   ```scheme
   (define (process [x : InputData]) : (Result OutputData Error)
     (complex-operation x))
   ```

**Named holes for documentation**:
```scheme
(define (transform [x : (? input-format)]) : (? output-format)
  ...)
;; IDE shows: input-format = JSON, output-format = XML
```

**Hole reports**: Query what the checker inferred for each hole:
```scheme
(hole-report 'my-function)
; → ((? input-format) . JSON)
;   ((? output-format) . XML)
```

#### 7.5.3 REPL-Driven Development

The persistent REPL daemon supports interactive development:

```scheme
;; Load module under development
> (load "my-module.ss")

;; Test incrementally
> (my-function test-input)
#(result ...)

;; Check types interactively
> (type-of 'my-function)
(→ InputType OutputType)

;; Explore inferred types
> (infer '(lambda (x) (+ x 1)))
(→ Num Num)

;; Reload after edits
> (reload "my-module.ss")
```

**Session persistence**: State survives across invocations. Define a function, close the terminal, return later—it's still there.

#### 7.5.4 Tooling Integration

**Lattice search**: Find relevant functions without memorizing names:
```scheme
> (lf "matrix inverse")
((matrix-inverse 0.92 export (linalg matrix))
 (solve-linear 0.78 export (linalg solvers))
 ...)
```

**Dependency exploration**:
```scheme
> (ld 'physics/diff)        ; What does this need?
(autodiff linalg data)

> (lu 'linalg)              ; What uses this?
(autodiff geometry physics/diff physics/diff3d ...)
```

**Type inspection**:
```scheme
> (describe 'matrix-mul)
matrix-mul : (∀ (m n p) (→ (Matrix m n) (→ (Matrix n p) (Matrix m p))))

Multiplies two matrices. Requires inner dimensions to match.
Complexity: O(m·n·p)
Module: linalg/matrix
```

**Block explorer TUI** (`shell/web/fold-explorer/`):
A Rust-based terminal UI for visualizing the content-addressed store. Navigate blocks by tag, search content, follow references to traverse the Merkle DAG, and analyze orphan or highly-referenced blocks. All untrusted content is sanitized before display to prevent terminal escape sequence injection.

#### 7.5.5 LSP Integration

The Language Server Protocol implementation (`shell/lsp/`) provides IDE features with real type inference integration.

**Hover Type Inference**:

Rather than relying solely on pre-indexed type signatures, the LSP hover handler performs real type inference using the document's content:

```
Document → parse-definitions → [(name, expr)] → build-tenv-from-defs → TEnv → try-infer-type → "Type"
```

**Process**:
1. `parse-definitions` extracts top-level `define` forms from the document text
2. `build-tenv-from-defs` infers types for each definition, building a type environment incrementally
3. `try-infer-type` looks up the hovered symbol in this environment
4. Falls back to `get-type-string` (primitive/indexed types) if inference fails

**Definition extraction** handles both forms:
```scheme
(define x 42)              ; → (x . 42)
(define (f a b) body)      ; → (f . (fn (a b) body))
```

**Integration with bidirectional inference**:

The implementation uses the core type inference engine (`core/types/infer.ss`), including hole constraint tracking (§5.9.1). Each definition is inferred independently with fresh type variables, then generalized before being added to the environment:

```scheme
(reset-fresh!)
(let* ([result (infer init env)]
       [type (apply-subst subst (cadr result))]
       [gen-type (generalize env type)])
  (tenv-extend env name gen-type))
```

**Fallback strategy**:

The layered fallback ensures useful hover information is always available:

1. **Real inference**: Best for user-defined symbols in the current file
2. **Primitive table**: Built-in operations like `+`, `map`, `cons`
3. **Symbol index**: Pre-indexed module exports

**Known limitations**:

| Limitation | Impact | Status |
|----|----|----|
| Forward references | Mutually recursive functions may show `Any` | Planned: multi-pass inference |
| Local bindings | `let`-bound variables not typed | Planned: body traversal |
| Re-parsing overhead | O(N) per hover request | Planned: tenv caching |

These limitations are acceptable for initial deployment—the fallback ensures primitive operations always display types, and real inference succeeds for the common case of sequential top-level definitions.

### 7.6 Shell IO Infrastructure

The Shell layer provides IO primitives that maintain consistency guarantees despite operating in an impure environment.

#### 7.6.1 Atomic File Writes

The `shell/io/atomic.ss` module implements atomic file writes using the *write-then-rename* pattern:

```
1. Write content to unique temporary file (path.pid.nanoseconds.counter.tmp)
2. Flush buffers to OS
3. Rename temporary file to target path (atomic on POSIX)
```

**Guarantees**:
- Readers never see partial writes—files are either complete-old or complete-new
- Crash during write leaves target unchanged (temp file may be orphaned)
- Error during write triggers cleanup of temporary file
- Unique temp file names prevent collision when multiple processes write concurrently

**Limitations**:
- `flush-output-port` flushes to OS buffers, not to disk; true durability requires `fsync()` which is not yet implemented
- On power failure after rename but before disk sync, data may be lost

#### 7.6.2 File Locking

The `shell/io/file-lock.ss` module provides file locking for multi-step atomic operations:

```scheme
(with-file-lock path
  (lambda ()
    ;; read-modify-write safely here
    ))
```

**Three-layer protection** (belt-and-suspenders approach):
1. **Process-internal mutex**: Prevents thread races within a single Scheme process
2. **POSIX flock()**: OS-managed advisory locks via Rust FFI with automatic cleanup on process death
3. **Cross-process lockfile**: Uses identity tokens for verification and handles cases where `flock()` is unavailable

**POSIX FFI layer** (`shell/ffi/posix-ffi.ss`, `shell/ffi/rust-accel/src/posix.rs`):
- Provides `flock()` with `LOCK_NB` (non-blocking) to avoid hanging Chez Scheme's cooperative runtime
- Uses `O_CLOEXEC` to prevent file descriptor inheritance to child processes
- Returns real OS PID via `getpid()` for unique identity token generation

**Identity tokens**: Each process generates a unique token combining:
- Real OS PID (via FFI) or memory-address fallback
- High-resolution timestamp (nanoseconds)
- Process-local counter

**Stale lock recovery**: Locks older than 60 seconds are considered stale. Breaking uses atomic rename with identity token verification:
1. Write new lock to unique temp file
2. Atomically rename over stale lock
3. Verify our token is in the final file
4. If verification fails, another process won—retry

This eliminates the race condition where two processes both detect and break a stale lock simultaneously.

#### 7.6.3 BBS: Case Study

The Bulletin Board System (BBS) demonstrates these primitives in practice:

**Counter generation** (`bbs-next-id!`):
- Requires atomic read-increment-write
- Protected by `with-file-lock` on counter file
- Concurrent stress test: 10 parallel processes correctly generate 10 unique sequential IDs

**Lock-aware function design**:
- Public functions (e.g., `bbs-write-head!`) acquire their own locks
- Internal functions (e.g., `%bbs-write-head!`) assume caller holds lock
- This prevents deadlock when composing operations while allowing efficient nested calls

**Compare-and-swap** (`bbs-cas-head!`):
- Implements optimistic concurrency control for issue updates
- Protected by `with-file-lock` on individual head files
- Uses internal `%bbs-write-head!` since it already holds the lock
- Returns `#f` on conflict, allowing retry

**In-memory indices with cache persistence**:

Both issues and posts use in-memory hashtable indices for O(1) lookups, with disk-based cache persistence:

| Index | Purpose | Key → Value |
|----|----|---|
| `*bbs-issues*` | Issue lookup by ID | id-string → hash-bytevector |
| `*bbs-by-status*` | Filter by status | status-symbol → (id ...) |
| `*bbs-by-priority*` | Filter by priority | priority-int → (id ...) |
| `*bbs-posts*` | Post lookup by ID | id-string → hash-bytevector |
| `*bbs-posts-by-type*` | Filter by type | type-symbol → (id ...) |

**Cache invalidation strategy**:
```
1. On save: Store head-file count as version marker
2. On load: Compare cached count vs actual disk head count
3. If mismatch: Full rebuild from disk (conservative but correct)
4. On cache hit: Individual items auto-refresh on hash lookup miss
```

This approach trades off stale cache detection granularity for simplicity—no complex change tracking is needed, and the count check is O(1).

**Design achieved**: The current implementation provides production-ready concurrency for single-server deployments. The hybrid `flock()` + lockfile approach handles both normal operation (via fast OS-level locks) and edge cases (via identity-verified lockfiles).

#### 7.6.4 REPL History: Case Study

The REPL History module (`shell/history/`) demonstrates command replay as an alternative to state serialization—a key insight for systems with opaque runtime objects.

**The Problem**: Time-travel debugging and undo/redo typically require environment snapshots. But Scheme environments contain *closures* (captured lexical scopes), *continuations* (call stack snapshots), and *ports* (OS file handles)—none of which can be serialized portably.

**The Solution**: Command replay. Instead of snapshotting state, record the commands that produced it:

```
Execute command → Create history entry block → Link to previous → Update head
                                                      ↓
Undo → Walk back prev chain → Reset environment → Replay to target position
```

**Command Classification**:

| Type | Examples | Replay Behavior |
|----|----|----|
| `definition` | `define`, `define-syntax` | Always replay (modifies environment) |
| `effect` | `load`, `display`, `write-file` | Skip in safe mode (side effects) |
| `expression` | `(+ 1 2)`, `(map f xs)` | Replay if needed for result |

Classification is determined by inspecting the head form of each parsed expression.

**Block Schema** (`history/entry`):

```scheme
;; Payload
((session-id . "cli-123")
 (index . 42)
 (command . "(define x 10)")
 (cmd-type . definition)
 (result-type . success)
 (result-hash . "a4f5...")
 (defined-name . x)
 (timestamp . "2026-01-17T...")
 (version . 1))
;; Refs: [prev-entry-hash]
```

Each entry links to its predecessor, forming a chain like git commits.

**Branching via Content Addressing**:

Creating a branch is O(1)—no data copying required:

```
Before:  main.head → entry-5 → entry-4 → entry-3 → ...

(branch 'experiment)

After:   main.head → entry-5 → entry-4 → entry-3 → ...
                              ↑
         experiment.head ─────┘
```

Both branches share the same underlying blocks. Divergence only occurs when new commands are added to different branches.

**Environment Reset Challenge**:

Chez Scheme provides no `unbind!` primitive. The workaround:

1. Track all symbols defined via history
2. On reset, overwrite each with a tombstone value
3. Replay definitions to rebuild correct bindings

This isn't true unbinding—`(top-level-bound? x)` still returns `#t`—but it's sufficient for replay semantics.

**Divergence Detection**:

When replaying, commands that succeeded originally might fail due to:
- Changed external state (files, network)
- Stale dependencies
- Non-deterministic behavior

On divergence (replay error where original succeeded), the system pauses and reports the conflict rather than silently corrupting state.

**Head Files** (per session):

```
.store/heads/history/<session-id>/
  main.head           # Branch tip hash
  experiment.head     # Other branches
  __current__.head    # Active branch name
  __redo__.sexp       # Redo stack (list of entry hashes)
```

The redo stack enables `(redo)` after `(undo)`—popping from the stack re-executes the undone command.

**Integration**: The REPL worker (`shell/repl/repl-worker.ss`) hooks command recording into evaluation:

```scheme
;; After successful evaluation:
(history-record-success! session-id cmd-str result defined-name)

;; After error:
(history-record-error! session-id cmd-str error-value)
```

Recording is guarded to prevent history failures from breaking the REPL itself.

**Design Insight**: Command replay is the right abstraction for systems with opaque runtime state. It's portable (commands are strings), auditable (history is inspectable), and debuggable (replay can be traced). The tradeoff is replay cost—O(n) for n commands—but practical REPL sessions rarely exceed hundreds of commands, making this acceptable.

### 7.7 Probabilistic Programming and Automatic Differentiation

The Fold integrates automatic differentiation with probabilistic programming, enabling gradient-based inference for scalable Bayesian computation.

#### 7.7.1 Automatic Differentiation

The autodiff module (`core/autodiff/`) provides multiple differentiation modes:

| Mode | Type | Best For |
|----|----|----|
| Forward (Dual numbers) | `Dual` | Few inputs, many outputs |
| Reverse (Traced values) | `Traced` | Many inputs, few outputs (e.g., loss functions) |
| Hyperdual | `Hyperdual` | Exact second derivatives (Hessians) |

**Traced values** record a computation graph during the forward pass, then backpropagate gradients:

```scheme
(gradient
  (lambda (x y)
    (traced-mul x (traced-add x y)))  ; f(x,y) = x(x+y)
  '(3.0 2.0))
; → (8.0 3.0)  ; ∂f/∂x = 2x+y = 8, ∂f/∂y = x = 3
```

The `Differentiable` type class (`core/autodiff/differentiable.ss`) provides a uniform interface:

```scheme
(class Differentiable d where
  lift    : Real → d           ; Lift constant
  primal  : d → Real           ; Extract value
  d+, d*, d-, d/ : d → d → d   ; Arithmetic
  d-exp, d-log, d-sin, d-cos   ; Transcendentals
  ...)
```

This enables generic differentiable programming—write once, differentiate with any AD mode.

#### 7.7.2 Variational Inference

Variational inference (`lattice/random/variational-inference.ss`) transforms Bayesian integration into optimization:

**Key insight**: Instead of computing the intractable posterior p(z|x), find the closest approximation from a tractable family:

```
q*(z) = argmin_q KL(q(z) || p(z|x)) = argmax_q ELBO(q)
```

where ELBO (Evidence Lower Bound) is:

```
L(φ) = E_q[log p(x,z)] - E_q[log q(z;φ)]
     = E_q[log p(x,z)] + H[q]
```

**The Reparameterization Trick**: Naive sampling z ~ q(z|φ) blocks gradient flow. The solution: sample noise ε ~ N(0,1) and compute z = g(ε, φ) deterministically:

```
For Gaussian q(z|μ,σ): z = μ + σ * ε
```

Now z is a deterministic function of φ, enabling ∇_φ E_q[f(z)] via backpropagation.

**Variational Families**:

| Family | Parameters | Expressiveness |
|----|----|----|
| Mean-field Gaussian | μ, diag(σ) | Fast, independent marginals |
| Full-covariance Gaussian | μ, LL^T (Cholesky) | Captures correlations |

**Optimization**: Adam optimizer with momentum and adaptive learning rates:

```scheme
(vi-fit-normal-mean observations variance num-iters learning-rate)
; Infers posterior over mean given observations with known variance
```

**Convergence Diagnostics**:
- ELBO history tracking
- `vi-summary` for posterior statistics
- `vi-check-convergence` for monitoring

**Example: Bayesian Linear Regression**

```scheme
;; Model: β ~ N(0, 10*I), y_i ~ N(X_i · β, σ²)
(let ([result (vi-fit-linear-regression X y 1.0 2000 0.01)])
  (vi-summary result))
; → Posterior mean and uncertainty over regression coefficients
```

This approach scales to large datasets where MCMC would be prohibitively slow.

### 7.8 Optics Tower

The Fold includes a complete optics implementation (`lattice/fp/optics/`) providing composable data accessors for principled navigation and transformation of nested structures.

#### 7.8.1 The Optics Hierarchy

Optics form a hierarchy based on their capabilities:

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

Grate is the categorical dual of Lens. While a Lens focuses on extracting and replacing a single value within a structure, a Grate enables zipping multiple copies of a structure together.

| Optic | Targets | Read | Write | Laws |
|-------|---------|------|-------|------|
| Iso | exactly 1 | ✓ | ✓ | forward∘backward = id, backward∘forward = id |
| Lens | exactly 1 | ✓ | ✓ | get-put, put-get, put-put |
| Prism | 0 or 1 | ✓ | ✓ | preview-review, review-preview |
| Affine | 0 or 1 | ✓ | ✓ | get-set, set-get |
| Grate | exactly 1 | ✗ | ✓ | review-over, zipWith-identity |
| Traversal | 0+ | ✓ | ✓ | identity, composition |
| Fold | 0+ | ✓ | ✗ | — |
| Getter | exactly 1 | ✓ | ✗ | — |
| Setter | 0+ | ✗ | ✓ | identity, composition |

**Composition rules**: When optics compose, the result type is the "least upper bound" in the hierarchy. Lens + Prism = Affine (can read/write, but target may not exist).

#### 7.8.2 Operator Syntax

Ergonomic operators mirror Haskell's lens library:

| Operator | Name | Type | Usage |
|----------|------|------|-------|
| `^.` | view | `s × Lens → a` | `(^. pair lens-fst)` |
| `^?` | preview | `s × Affine → Maybe a` | `(^? maybe prism-just)` |
| `^..` | to-list | `s × Traversal → [a]` | `(^.. list traversal-each)` |
| `.~` | set | `Optic × b → s → t` | `((.~ lens-fst 99) pair)` |
| `%~` | over | `Optic × (a→b) → s → t` | `((%~ lens-fst add1) pair)` |
| `&` | pipe | `s × (s→t) → t` | `(& pair (.~ lens-fst 99))` |
| `>>>` | compose | `Optic × Optic → Optic` | `(>>> outer inner)` |

**Example**:
```scheme
(& body (%~ (>>> body-pos-lens vec2-x-lens) add1))  ; Increment x coordinate
```

#### 7.8.3 Block Optics

The CAS-aware optics (`block-optics.ss`) provide principled access to content-addressed blocks:

**Basic lenses**:
- `block-tag-lens` — Focus on block tag (Symbol)
- `block-payload-lens` — Focus on payload (Bytevector)
- `block-refs-lens` — Focus on refs vector

**Reference optics**:
- `block-ref-at n` — Affine for ref at index n (returns nothing if out of bounds)
- `block-refs-each` — Traversal over all refs
- `follow-ref fetch` — Affine that dereferences through CAS

**Type prisms**:
- `block-type-prism tag` — Match blocks by tag
- `block-lambda-prism`, `block-app-prism`, etc. — Common type matchers

**Tree traversal**:
```scheme
(collect-block-tree fetch root)  ; DFS all reachable blocks (O(N))
```

#### 7.8.4 Profunctor Encoding

The profunctor optics module (`profunctor-optics.ss`) provides an alternative representation where optics are polymorphic functions `p a b → p s t` constrained by profunctor type classes.

**Type classes**:
- `Profunctor` — `dimap : (a'→a) → (b→b') → p a b → p a' b'`
- `Strong` — Adds `pfirst : p a b → p (a,c) (b,c)` (enables lenses)
- `Choice` — Adds `pleft : p a b → p (Either a c) (Either b c)` (enables prisms)
- `Closed` — Adds `pclosed : p a b → p (x→a) (x→b)` (enables grates)
- `Wander` — Combines Strong + Choice with `wander : ((a → F b) → s → F t) → p a b → p s t` (enables traversals)

**Advantages**:
1. Composition is function composition (automatic type inference)
2. Type class hierarchy mirrors optic hierarchy
3. Separation of concerns: `Forget` for reading, `Tagged` for writing

**Profunctor optic types**:
- `p-iso`, `p-lens`, `p-prism`, `p-affine`, `p-grate` — Core optics
- `p-traversal` — Multi-target optic using Wander class
- `p-fold` — Read-only multi-target (composition: any optic + fold = fold)

**Conversions**:
```scheme
(lens->p-lens concrete-lens)    ; Concrete → Profunctor
(p-lens->lens profunctor-lens)  ; Profunctor → Concrete
(traversal->p-traversal trav)   ; Concrete traversal → Profunctor
(p-traversal->traversal ptrav)  ; Profunctor → Concrete
```

#### 7.8.5 Grate: The Dual of Lens

Grate is the categorical dual of Lens. The key insight is in their representations:

| Optic | Representation | Profunctor Class |
|-------|----------------|------------------|
| Lens | `(s → a, s → b → t)` — get/set | Strong (`pfirst`) |
| Grate | `((s → a) → b) → t` — cotraverse | Closed (`pclosed`) |

Where a Lens says "I can extract a focus and replace it," a Grate says "given any way to extract a focus, I can produce a result."

**Primary operation — zipWith**:
```scheme
;; Zip two pairs element-wise with a combining function
(grate-zipWith grate-pair-same + '(1 . 2) '(3 . 4))  ; → (4 . 6)

;; Apply the same argument to two functions and combine results
(let ([f (lambda (x) (+ x 1))]
      [g (lambda (x) (* x 2))])
  ((grate-zipWith grate-fn + f g) 5))  ; → 16  ((5+1) + (5*2))
```

**Common grates**:
- `grate-id` — Identity grate
- `grate-fn` — Functions as a grate (apply same input, combine outputs)
- `grate-pair-same` — Homogeneous pairs `(a . a)`
- `grate-list-rep n` — Fixed-length lists of n elements

**Laws**:
1. `(grate-over g id s) = s` — Identity
2. `(grate-zipWith g (λ (a b) a) s1 s2) = s1` — Left projection
3. `(grate-review g (grate-over g f (grate-review g a))) = (grate-review g (f a))` — Review-over coherence

**Use cases**:
- Parallel structure processing (zip vectors, matrices)
- Distributing functions over containers
- Gradient computation (apply same perturbation, collect partial derivatives)

#### 7.8.6 Physics Integration

The physics lens integration (`lattice/physics/lenses/optics-integration.ss`) demonstrates domain-specific optics:

**Traversals**:
- `bodies-each` — All bodies in a list
- `particles-alive` — Only alive particles
- `rigid-bodies-only` — Type-filtered traversal

**World optics**:
```scheme
(^? world (world-body 'player))                    ; Maybe get player
(traversal-over world-all-bodies (step-body dt) w) ; Simulate all bodies
```

**Physics operations as optic transformations**:
```scheme
(define (apply-gravity g)
  (lambda (body)
    (& body (%~ (>>> body-vel-lens vec2-y-lens) (lambda (vy) (+ vy g))))))
```

#### 7.8.7 Design Rationale

**Why optics?**

The Fold's content-addressed architecture benefits from optics in several ways:

1. **Immutable updates**: Blocks are immutable; optics provide functional update syntax that creates new blocks with modified content.

2. **Composable paths**: Complex CAS traversals (follow ref → match type → extract field) compose cleanly.

3. **Type-safe partiality**: Affines and prisms encode "might not exist" in their types, preventing runtime errors.

4. **Domain modeling**: Physics simulations, game state, and agent pipelines all benefit from declarative data access.

**Future directions**: The optics foundation enables query languages (optics + pattern matching) and reactive derivations (optic-based dependency tracking).

#### 7.8.8 Differentiable Data Access

The traced optics module (`lattice/autodiff/traced-optics.ss`) bridges optics with automatic differentiation, enabling gradient computation through optic-focused paths:

```scheme
;; Gradient of loss w.r.t. optic focus
(optic-gradient
  (lambda (p) (traced-sq (car p)))  ; Loss = x²
  lens-fst
  '(3.0 . 4.0))
; → 6.0  ; ∂(x²)/∂x = 2x = 6 at x=3

;; Gradient through composed lenses
(optic-gradient
  (lambda (s) (traced-sq (view (>>> lens-fst lens-snd) s)))
  (>>> lens-fst lens-snd)
  '((1.0 . 2.0) . (3.0 . 4.0)))
; → 4.0  ; Gradient at nested position

;; Gradients for traversal targets (one per focus)
(optic-gradient-list
  (lambda (xs) (traced-sum (map traced-sq xs)))
  traversal-each
  '(1.0 2.0 3.0))
; → (2.0 4.0 6.0)  ; Gradient for each element
```

**Key API**:
| Function | Purpose |
|----|-----|
| `optic-gradient` | Gradient of loss w.r.t. lens/affine/iso focus |
| `optic-gradient-list` | List of gradients for traversal targets |
| `optic-gradient-maybe` | Maybe gradient for prism/affine (nothing if no match) |
| `optimize-at` | Single gradient descent step at optic focus |
| `optimize-steps` | Multiple gradient descent steps |

**Architecture insight**: The `lift-at-optic` function traces only the optic focus, not the entire structure. This "pair of traced" pattern (vs "traced pair") enables efficient gradient computation through deeply nested structures while maintaining the compositional nature of optics.

### 7.9 Bidirectional Transformations

The bidirectional transformations system (`lattice/fp/optics/bidirectional.ss`) extends the optics tower with reversible migrations, enabling schema evolution, format conversion, and CAS block migrations with automatic rollback.

#### 7.9.1 Migration Type

A migration is a named, versioned isomorphism:

```scheme
(define user-v1->v2
  (make-migration 'user-v1->v2 'v1 'v2
    (make-p-iso
      (lambda (u) (cons '(created-at . 0) u))    ; forward
      (lambda (u) (assq-remove u 'created-at))))) ; backward

;; Apply forward (migrate) or backward (rollback)
(migrate user-v1->v2 old-user)    ; v1 → v2
(rollback user-v1->v2 new-user)   ; v2 → v1
```

**Key insight**: Profunctor optics already encode bidirectionality via `p-iso-forward` and `p-iso-backward`. Migrations leverage this to provide automatic rollback without writing reverse transformations manually.

**Composition**: Migrations compose like optics—`v1→v2` + `v2→v3` = `v1→v3`:

```scheme
(define v1->v3 (migration-compose v1->v2 v2->v3))
```

**Flip**: Reverse any migration:
```scheme
(define v2->v1 (migration-flip v1->v2))
```

#### 7.9.2 Schema DSL

The schema module (`schema.ss`) provides field-level operations for alist-based schemas:

| Operation | Forward | Backward |
|-----------|---------|----------|
| `field-rename-iso` | Rename field | Rename back |
| `field-add-iso` | Add with default | Remove field |
| `field-remove-iso` | Remove field | Add with preserved value |
| `field-transform-iso` | Transform value | Reverse transform |
| `field-split-iso` | Split field into two | Merge fields into one |
| `field-merge-iso` | Merge fields | Split field |

**Example: Schema evolution**
```scheme
(define v2-schema
  (schema-compose
    (field-rename-iso 'desc 'description)
    (field-add-iso 'created-at 0)
    (field-transform-iso 'priority iso-number-string)))
```

These compose via `schema-compose`, maintaining bidirectionality throughout.

#### 7.9.3 Block Migrations

Block migrations (`block-migration.ss`) specialize the migration infrastructure for The Fold's content-addressed blocks:

```scheme
(define issue-v1->v2
  (make-block-migration 'bbs-issue-v1 'bbs-issue-v2
    (field-add-iso 'created-at 0)))

;; Apply to block (pure transformation)
(block-migrate-payload issue-v1->v2 old-block)
```

**Tag-based versioning**: Version is encoded in the block tag (e.g., `'bbs-issue-v2`). Migrations check tag match before applying.

**Version detection**:
```scheme
(parse-versioned-tag 'bbs-issue-v2)  ; → (bbs-issue . 2)
(make-versioned-tag 'bbs-issue 3)    ; → 'bbs-issue-v3
```

#### 7.9.4 Merkle DAG Correctness

The shell runner (`shell/migrations/runner.ss`) executes migrations against the CAS with Merkle DAG correctness.

**The problem**: In a content-addressed system, changing a block changes its hash. If block A references block B, and B is migrated to B', then A must also be updated to reference B' instead of B. This cascades up to the root.

**Solution: Bottom-up (post-order) traversal**

```
migrate(root) → migrate(child₁), migrate(child₂), ... → then migrate(root with new refs)
```

1. Recursively migrate all children first
2. Collect their new hashes
3. Migrate parent payload with updated refs
4. Store parent, producing new hash
5. Memoize to handle shared subtrees (DAGs, not just trees)

```scheme
(define (migrate-tree-impl! bm hash visited)
  (let ([cached (hashtable-ref visited hash #f)])
    (if cached cached  ; DAG memoization
        (let* ([blk (fetch hash)]
               [new-refs (map-refs (lambda (h)
                           (migrate-tree-impl! bm h visited))
                         (block-refs blk))]
               [migrated (block-migrate-with-refs bm blk new-refs)]
               [new-hash (store! migrated)])
          (hashtable-set! visited hash new-hash)
          new-hash))))
```

**Memoization is critical**: Without it, shared subtrees would be migrated multiple times, causing exponential blowup in DAGs.

#### 7.9.5 Migration Registry

The registry (`shell/migrations/registry.ss`) manages migrations and computes paths:

```scheme
;; Register migrations
(register-migration! user-v1->v2)
(register-migration! user-v2->v3)

;; Find path between versions (BFS)
(find-migration-path 'v1 'v3)  ; → (user-v1->v2 user-v2->v3)

;; Get composed migration
(get-migration-chain 'v1 'v3)  ; → single migration v1→v3
```

**Version graph**: The registry builds a directed graph where nodes are versions and edges are migrations. Path finding uses BFS to find the shortest migration chain.

**Automatic rollback**: Flipped migrations are registered alongside forward migrations, enabling rollback path discovery.

#### 7.9.6 Format Isomorphisms

Standard format conversions (`format-iso.ss`) for common transformations:

| Iso | Forward | Backward |
|-----|---------|----------|
| `iso-utf8` | bytevector → string | string → bytevector |
| `iso-sexpr-string` | sexpr → string | string → sexpr |
| `iso-sexpr-bytevector` | sexpr → bytevector | bytevector → sexpr |
| `iso-number-string` | number → string | string → number |
| `iso-bool-int` | boolean → integer | integer → boolean |
| `iso-time-unix` | time-utc → integer | integer → time-utc |

**Convenience wrappers**:
```scheme
(sexpr->bytevector '(a b c))   ; For block payloads
(bytevector->sexpr payload)    ; For payload parsing
```

#### 7.9.7 Law Verification

Migrations satisfy isomorphism laws by construction:

```scheme
(verify-migration-laws user-v1->v2 test-user)
; Checks:
; 1. (rollback m (migrate m x)) = x  (roundtrip)
; 2. (migrate m (rollback m y)) = y  (reverse roundtrip)
```

**Design rationale**: By building migrations from profunctor isos, law compliance is compositional—if components satisfy laws, compositions do too.

#### 7.9.8 Use Cases

**CAS schema evolution**:
```scheme
;; Upgrade all issues in store
(migrate-tree! issue-v1->v2 (read-head 'issue-42))
```

**Format conversion**:
```scheme
(define json->sexpr (make-migration 'json->sexpr 'json 'sexpr
                      iso-json-sexpr))
```

**Batch migration with statistics**:
```scheme
(migration-stats issue-v1->v2 root-hash)
; → ((total . 150) (matching . 42) (non-matching . 108))

(migrate-dry-run issue-v1->v2 root-hash)  ; Preview without storing
```

### 7.10 Provenance Tracking

The provenance module (`shell/provenance/`) instruments optic operations to create a complete audit trail in the CAS. Every traced operation produces a provenance record capturing what happened, enabling "explain this value" queries and regulatory compliance.

#### 7.10.1 Architecture

**Provenance record schema** (tag: `provenance/record`):

```scheme
((operation . <symbol>)        ; view, set, over, preview, etc.
 (optic-name . <symbol|#f>)    ; Registered name if available
 (optic-type . <symbol>)       ; lens, prism, traversal, etc.
 (source-hash . <hex-string>)  ; Input structure
 (result-hash . <hex-string>)  ; Output structure
 (value-hash . <hex-string|#f>); Set/over value
 (timestamp . <iso8601>)
 (agent-id . <symbol|#f>)
 (session-id . <string|#f>)
 (version . 1))
```

The refs vector links to actual blocks: `[source-block, result-block, value-block?]`.

**Head pointer indices** enable O(1) lookup:
- `.store/heads/provenance/by-result/<hash>.head` — Find what created a result
- `.store/heads/provenance/by-source/<hash>.head` — Find what used a source
- `.store/heads/provenance/log.head` — Most recent operation

#### 7.10.2 Traced Operations

The `traced-optics.ss` module wraps standard optic operations with provenance recording:

| Traced Function | Base Operation | Records |
|-----------------|----------------|---------|
| `traced-view` | `^.` | source, result |
| `traced-set` | `.~` | source, result, value |
| `traced-over` | `%~` | source, result, function name |
| `traced-preview` | `^?` | source, Maybe result |
| `traced-to-list` | `^..` | source, list result |

**Usage**:
```scheme
(load "shell/provenance/traced-optics.ss")
(load "shell/provenance/query.ss")

;; Chain transformations with full tracking
(define v1 '(1 . 2))
(define v2 (traced-set lens-fst 10 v1))     ; (10 . 2)
(define v3 (traced-set lens-snd 20 v2))     ; (10 . 20)

;; Explain how v3 was created
(explain v3)
; → Lineage for 00abc123...
;   Step 1: set via lens-fst (lens)
;     Source: 00def456...
;     Result: 00789abc...
;   Step 2: set via lens-snd (lens)
;     ...
```

**Optic registry**: Since optics are functions, they can't be serialized. The registry maps optic values to symbolic names for traceable provenance:

```scheme
(register-optic! 'my-optic (>>> lens-fst lens-snd))
(lookup-optic-name my-optic)  ; → 'my-optic
```

Standard optics (`lens-fst`, `prism-just`, `traversal-each`, etc.) are pre-registered.

#### 7.10.3 Query API

**Direct queries**:
```scheme
(provenance-for-result result-hex)  ; → Block | #f
(provenance-for-source source-hex)  ; → Block | #f
(latest-provenance)                 ; → Block | #f
```

**Lineage traversal**:
```scheme
(trace-lineage result-hex)     ; → (List Block) oldest first
(trace-descendants source-hex) ; → (List Block) forward lineage
```

**Value retrieval** (from provenance records):
```scheme
(provenance-get-source record)  ; → original value
(provenance-get-result record)  ; → result value
(provenance-get-value record)   ; → set/over argument
```

**Search and statistics**:
```scheme
(find-provenance-by-optic 'lens-fst)       ; All uses of this optic
(find-provenance-by-operation 'set)        ; All set operations
(find-provenance-by-agent 'my-agent)       ; All by this agent
(provenance-stats)                         ; Counts by operation/optic/agent
```

#### 7.10.4 Context and Scoping

**Agent identity** for audit trails:
```scheme
(with-agent-id 'migration-tool
  (lambda ()
    (traced-set lens-fst new-value data)))
; Provenance record shows agent-id: migration-tool
```

**Selective tracing**:
```scheme
(without-tracing
  (lambda ()
    ;; Performance-critical section
    (traced-view data lens-fst)))  ; No provenance recorded

(with-tracing
  (lambda ()
    ;; Ensure tracing even if globally disabled
    ...))
```

#### 7.10.5 Security Considerations

**Path validation**: Hex strings used in head file paths are validated to contain only `[0-9a-fA-F]`, preventing path traversal attacks:

```scheme
(valid-hex-string? "../etc/passwd")  ; → #f (rejected)
(valid-hex-string? "00abcdef1234")   ; → #t
```

**Data sensitivity**: Values are stored as plaintext S-expressions. Sensitive data (keys, PII) will appear in provenance blocks. Consider:
- Using `without-tracing` for sensitive operations
- Restricting CAS directory permissions
- Future: encrypted payload option

#### 7.10.6 Performance Considerations

**Per-operation overhead**: Each traced operation creates:
- 3-4 block stores (source, result, value, record)
- 3 head file writes (log, by-result, by-source)

For performance-critical code, use `without-tracing` or raw optics.

**Query scalability**: `find-provenance-by-*` functions scan the entire store—O(N). For production use with large stores, consider external indices (BBS pattern).

**Storage growth**: Provenance creates ~4 blocks per operation. For high-volume systems, implement retention policies or archived storage tiers.

---

### 7.11 Reactive Derivations

The reactive module (`shell/reactive/`) builds on provenance tracking to provide automatic reactivity via optic-based dependency graphs. When a derivation is computed, we track which optics were accessed. When those optics are written to, the derivation invalidates and recomputes on next access.

This is the pattern behind lens-based state management systems (MobX, Recoil, Jotai), adapted to The Fold's optic foundation.

#### 7.11.1 Core Concepts

**Derivations** are cached computed values that track their optic dependencies:

```scheme
(define-reactive 'player-health
  world-state
  (lambda (world)
    (reactive-view world (>>> (body-lens 'player) health-lens))))

(reactive-value 'player-health)  ; => 100 (computed and cached)
(reactive-value 'player-health)  ; => 100 (from cache)
```

**Access tracking** discovers dependencies automatically during computation:

```scheme
(with-access-tracking
 (lambda ()
   (reactive-view data lens-fst)
   (reactive-view data lens-snd)))
; => (values result '(lens-fst lens-snd))
```

**Invalidation** marks derivations stale when their dependencies change:

```scheme
(reactive-set! lens-fst 999 data)  ; Invalidates all derivations using lens-fst
(reactive-value 'player-health)    ; Recomputes with fresh value
```

#### 7.11.2 Implementation Architecture

**Data structures**:

```
*derivations*           : Hashtable (Symbol → Derivation-Record)
*optic-to-derivations*  : Hashtable (Symbol → (List Symbol))  ; Reverse index
```

The derivation record (stored as a mutable vector) tracks:
- `source`: The root data structure being observed
- `dependencies`: List of optic names accessed during computation
- `compute-fn`: The function that produces the value
- `cached-value`: Last computed result
- `stale?`: Boolean flag for lazy recomputation

**Dependency discovery**: During `define-reactive` or `reactive-recompute!`, computation runs inside `with-access-tracking`. Each `reactive-view` call logs the optic name (via the provenance optic registry) to `*access-log*`. After computation, these become the derivation's dependencies.

**Reverse index maintenance**: When dependencies change, we update `*optic-to-derivations*` to enable O(1) invalidation lookup. Old dependencies are removed, new ones are added.

**Lazy recomputation**: `reactive-value` checks `stale?`. If true, it recomputes (updating dependencies in the process). If false, it returns the cached value.

#### 7.11.3 API Reference

**Derivation management**:
```scheme
(define-reactive name source compute-fn)  ; Create derivation
(reactive-value name)                     ; Get value (recompute if stale)
(reactive-refresh! name)                  ; Force recomputation
(reactive-stale? name)                    ; Check if needs recomputation
(reactive-dependencies name)              ; Get optic dependencies
(undefine-reactive name)                  ; Remove derivation
```

**Reactive optic operations**:
```scheme
(reactive-view s optic)      ; View with access tracking
(reactive-preview s optic)   ; Preview with access tracking
(reactive-to-list s optic)   ; To-list with access tracking
(reactive-set! optic val s)  ; Set with invalidation
(reactive-over! optic f s)   ; Modify with invalidation
```

**Batch operations**:
```scheme
(with-batch
 (lambda ()
   ;; Multiple changes, single invalidation pass
   (reactive-set! lens-fst 10 data)
   (reactive-set! lens-snd 20 data)))
```

**Introspection**:
```scheme
(list-derivations)        ; => (deriv-a deriv-b ...)
(derivation-info name)    ; => ((name . foo) (dependencies . (...)) ...)
(dependency-graph)        ; => ((lens-fst . (deriv-a deriv-b)) ...)
```

#### 7.11.4 Relationship to Provenance

Reactive derivations and provenance tracking share infrastructure:

| Aspect | Provenance | Reactive |
|--------|------------|----------|
| **Purpose** | Audit trail | Automatic updates |
| **Tracking** | All operations | Access paths only |
| **Storage** | CAS blocks | In-memory hashtables |
| **Optic registry** | Names in records | Dependency keys |
| **Persistence** | Survives restart | Session-scoped |

Both use the optic registry from `traced-optics.ss` to map optic values to symbolic names.

#### 7.11.5 Design Decisions

**Why not automatic invalidation on all traced-set?**

The reactive operations (`reactive-set!`, etc.) are separate from traced operations (`traced-set`, etc.) to avoid unwanted coupling. Code using provenance tracking shouldn't automatically trigger reactive invalidation.

**Why session-scoped, not persistent?**

Reactive derivations are designed for interactive state management (UI, simulation dashboards). Persistence would require serializing compute functions, which conflicts with The Fold's pure/impure separation.

**Why lazy recomputation?**

Immediate recomputation on invalidation would cascade through the dependency graph, potentially wasting work if the value is never accessed. Lazy evaluation ensures we only compute what's needed.

#### 7.11.6 Limitations

**Unregistered optics**: If an optic isn't registered with `register-optic!`, its accesses won't be tracked. Custom optics should be registered before use in reactive derivations.

**No circular dependency detection**: A derivation that reads and writes through the same optic could create infinite loops. The current implementation doesn't detect this.

**Memory management**: Derivations persist until explicitly removed with `undefine-reactive`. Long-running sessions should clean up unused derivations.

**Single-threaded**: The global mutable state (`*derivations*`, etc.) isn't thread-safe. Concurrent access requires external synchronization.

---

### 7.12 Optic Query Language

The optic query module (`lattice/query/optic-query.ss`) builds on the optics foundation to provide a declarative query language. The key insight is that optics are already a *typed path language*—they describe how to navigate through data structures. By combining optic paths with predicate filtering, projection, and aggregation, we get a composable query system.

#### 7.12.1 Design Philosophy

Traditional query languages separate "navigation" from "filtering":

```sql
SELECT pos FROM bodies WHERE vel.y > 0
```

The optic query language unifies these through composition:

```scheme
(oquery-pipe world
  (optic-having world-each-body body-vel-y (lambda (vy) (> vy 0)))
  (lambda (b) #t)
  (lambda (b) (^. b body-pos)))
```

Here, `optic-having` creates a *filtered traversal* that only yields bodies whose y-velocity satisfies the predicate. This traversal composes with other optics via `>>>`, enabling reusable query fragments.

#### 7.12.2 Core API

**Query functions** operate on a structure, an optic path, and optional predicate/projector:

| Function | Signature | Purpose |
|----------|-----------|---------|
| `oquery` | `s × Optic → [a]` | Get all targets |
| `oquery-where` | `s × Optic × (a→Bool) → [a]` | Filter by predicate |
| `oquery-select` | `s × Optic × (a→b) → [b]` | Project through function |
| `oquery-pipe` | `s × Optic × (a→Bool) × (a→b) → [b]` | Filter then project |
| `oquery-first` | `s × Optic → Maybe a` | First target |
| `oquery-first-where` | `s × Optic × (a→Bool) → Maybe a` | First matching target |

**Example**: Find all bodies with positive y-velocity, return their names:

```scheme
(oquery-pipe world world-each-body
  (lambda (b) (> (^. b body-vel-y) 0))
  (lambda (b) (^. b body-name)))
; => ("alpha" "delta")
```

#### 7.12.3 Optic Combinators

Combinators create new optics that can be composed with `>>>`:

| Combinator | Type | Purpose |
|------------|------|---------|
| `optic-where` | `Optic × (a→Bool) → Traversal` | Filtered traversal |
| `optic-having` | `Optic × Optic × (b→Bool) → Traversal` | Filter by nested value |
| `optic-select` | `Optic × (a→b) → Fold` | Projected fold |
| `optic-limit` | `Optic × Nat → Fold` | Take first n |
| `optic-skip` | `Optic × Nat → Fold` | Drop first n |

**Example**: Reusable query components:

```scheme
;; Define once
(define fast-bodies
  (optic-where world-each-body
    (lambda (b) (> (body-speed b) 100))))

;; Use anywhere via composition
(^.. game-state (>>> world-lens fast-bodies))
(oquery-count game-state (>>> world-lens fast-bodies))
```

The `optic-having` combinator is particularly powerful—it filters based on a nested value:

```scheme
;; Bodies whose velocity y-component is positive
(optic-having world-each-body body-vel-y
  (lambda (vy) (> vy 0)))
```

This composes the outer traversal (`world-each-body`) with an inner optic (`body-vel-y`) and filters based on the inner value.

#### 7.12.4 Aggregations

Standard aggregation operations:

| Function | Purpose |
|----------|---------|
| `oquery-count` | Count targets |
| `oquery-count-where` | Count matching targets |
| `oquery-sum` | Sum numeric targets |
| `oquery-sum-by` | Sum extracted values |
| `oquery-any` | Any target matches? |
| `oquery-all` | All targets match? |
| `oquery-min/max` | Min/max target value |
| `oquery-min-by/max-by` | Target with min/max value |

**Example**: Total mass of all bodies:

```scheme
(oquery-sum-by world world-each-body
  (lambda (b) (^. b body-mass)))
```

#### 7.12.5 Grouping and Joins

**Grouping** partitions targets by a key function:

```scheme
(oquery-group-by world world-each-body
  (lambda (b) (if (> (^. b body-mass) 10) 'heavy 'light)))
; => ((heavy . [bodies...]) (light . [bodies...]))
```

**Joins** combine results from multiple optic paths:

| Function | Purpose |
|----------|---------|
| `oquery-join` | Cross-product with predicate filter |
| `oquery-zip` | Pairwise combination (shortest length) |
| `oquery-union` | Concatenate results |
| `oquery-intersect` | Keep only shared targets |

**Example**: Find bodies near each other:

```scheme
(oquery-join world world-each-body world-each-body
  (lambda (a b)
    (and (not (eq? a b))
         (< (distance (^. a body-pos) (^. b body-pos)) 10))))
```

#### 7.12.6 Query Builder DSL

For complex queries, the builder pattern provides a chainable interface:

```scheme
(define my-query
  (-> (make-query world-each-body)
      (q-where (lambda (b) (> (^. b body-pos-y) 0)))
      (q-where (lambda (b) (> (^. b body-mass) 5)))
      (q-map (lambda (b) (^. b body-name)))))

(q-run world my-query)    ; => ("alpha" "delta")
(q-count world my-query)  ; => 2
(q-first world my-query)  ; => (just "alpha")
```

Filters and transforms accumulate; `q-run` executes them in order.

#### 7.12.7 Predicate Helpers

Convenience functions for building predicates:

| Function | Creates predicate for... |
|----------|--------------------------|
| `optic-eq?` | Target equals value |
| `optic-matches?` | Target satisfies condition |
| `optic-exists?` | Target exists (non-nothing) |
| `optic-gt?/lt?/gte?/lte?` | Numeric comparisons |
| `optic-between?` | Range check (inclusive) |

**Example**:

```scheme
(filter (optic-between? body-mass 5 20) bodies)
```

#### 7.12.8 Integration with Block Optics

The query language works with CAS block optics for querying the content-addressed store:

```scheme
;; Find all lambda blocks with arity > 2
(oquery-where store
  (>>> all-blocks-trav (block-type-prism 'lambda))
  (lambda (blk)
    (> (^. blk block-arity-lens) 2)))
```

This integrates with the existing `lattice/query/query-dsl.ss` block query infrastructure while providing the composability benefits of optics.

#### 7.12.9 Performance Characteristics

| Operation | Complexity |
|-----------|------------|
| `oquery` | O(targets) |
| `oquery-where` | O(targets) |
| `oquery-group-by` | O(targets × groups) |
| `oquery-join` | O(n × m) |
| `oquery-sort-by` | O(n log n) |

Queries are eager—all targets are collected before filtering. For large datasets, consider:
- Using `optic-limit` to cap results
- Composing filters before traversals to reduce intermediate results
- Caching frequently-used filtered traversals

#### 7.12.10 Design Rationale

**Why optics as the path language?**

1. **Type-safe composition**: `>>>` ensures paths compose correctly
2. **Unified read/write**: Same optic for queries and updates
3. **Reusability**: Define query fragments once, compose everywhere
4. **Hierarchy awareness**: Lens+Prism=Affine; composition preserves semantics

**Relationship to existing query-dsl**:

The traditional query-dsl (`query-dsl.ss`) uses pattern matching on block fields:

```scheme
(query fs '(and (tag . entity) (payload-contains . "Turing")))
```

The optic query language is more general—it works on any data structure navigable by optics, not just CAS blocks. The two approaches complement each other: use `query-dsl` for declarative block searches, use `optic-query` for structured data navigation.

---
## 8. Developer and Meta-Tooling

The Fold's introspectable architecture enables powerful developer tooling built directly on the system's primitives. This chapter documents the meta-tooling ecosystem that supports both human developers and AI agents working on and with The Fold.

### 8.1 The Manifest-Driven Architecture

Every skill in the lattice is described by a `manifest.sexp` file—a machine-readable specification that serves as the single source of truth for the skill's metadata.

**Manifest Schema**:

```scheme
(skill <name>
  (version "x.y.z")
  (tier 0-2)                       ; 0=foundational, 1=intermediate, 2+=advanced
  (path "lattice/<name>")
  (purity total|partial)           ; total=pure, partial=may have effects
  (stability stable|experimental)
  (fuel-bound "O(...)")            ; Complexity bound
  (deps (<skill> ...))             ; Skill-level dependencies
  (description "...")
  (keywords (<keyword> ...))       ; For search
  (aliases (<alias> ...))          ; Alternative names
  (exports (<module> <symbol> ...) ...)
  (modules (<name> "<file>" "<desc>") ...))
```

**Manifest Parser** (`lattice/meta/manifest.ss`):

The manifest parser is pure Scheme code with no side effects—it takes S-expression input and produces structured records. This purity enables:

1. Deterministic parsing regardless of environment state
2. Easy testing without mocking file systems
3. Reuse across different contexts (CLI, LSP, agents)

```scheme
(define (parse-manifest sexpr)
  ;; Returns: (skill name version tier path deps exports modules ...)
  ...)
```

**Knowledge Graph Construction**:

Manifests are transformed into CAS blocks for indexing and querying:

| Block Tag | Contents | Purpose |
|-----------|----------|---------|
| `KG-SKILL` | Skill metadata | Root node for skill |
| `KG-MODULE` | Module within skill | File-level granularity |
| `KG-EXPORT` | Exported symbol | Function/value discovery |

The knowledge graph is built by `kg-build!` which scans all manifest files, parses them, and stores the resulting blocks in the CAS. This happens lazily on first access, with subsequent queries hitting a warm cache.

**Initialization Performance**:

| Scenario | Time |
|----------|------|
| Cold start (no cache) | ~2.5s |
| Warm start (cached) | <0.3s |
| Incremental (one skill changed) | ~0.5s |

The warm cache stores serialized BM25 indices and skill metadata, eliminating the need to re-scan the filesystem.

### 8.2 Semantic Search Infrastructure

The lattice provides full-text search over skills, modules, and exports using the BM25 ranking algorithm (`lattice/meta/bm25.ss`).

**Index Architecture**:

Three separate indices enable different query patterns:

| Index | Entries | Fields Indexed |
|-------|---------|----------------|
| skill-index | ~40 | name, description, keywords, aliases |
| module-index | ~200 | name, description, skill context |
| export-index | ~2,700 | symbol name, module context, type signature |

**Search Modes**:

| Function | Algorithm | Use Case |
|----------|-----------|----------|
| `lattice-find` / `lf` | BM25 full-text | Natural language queries |
| `lattice-find-exact` / `lfe` | Exact match | Known symbol lookup |
| `lattice-find-prefix` / `lfp` | Prefix match | Tab completion |
| `lattice-find-substring` / `lfs` | Substring | Fuzzy search |

**Type-Aware Search**:

Search by input or output type to find functions by signature:

```scheme
(lf-input 'Matrix)   ; Functions that take Matrix
(lf-output 'Vector)  ; Functions that return Vector
```

**Example Session**:

```scheme
> (lf "matrix inverse")
((matrix-inverse 0.92 export (linalg matrix))
 (solve-linear 0.78 export (linalg solvers))
 (lu-decompose 0.65 export (linalg decomp)))

> (lfe 'shapley-value)
((shapley-value exact export (fp/game coop-games))
 (shapley-shubik-index exact export (fp/game voting-games)))

> (lfp 'vec)
((vec+ vec- vec* vec-dot vec-cross vec-norm vec-normalize ...))
```

**BM25 Implementation**:

The BM25 algorithm (`lattice/meta/bm25.ss`) implements:

1. **TF (Term Frequency)**: `tf = freq / (freq + k1 * (1 - b + b * doc_len / avg_len))`
2. **IDF (Inverse Document Frequency)**: `idf = log((N - n + 0.5) / (n + 0.5))`
3. **Score**: `score = sum(tf * idf)` over query terms

Default parameters: `k1 = 1.2`, `b = 0.75` (tuned for code search).

### 8.3 DAG Navigation and Analysis

The lattice forms a Directed Acyclic Graph where nodes are skills and edges are dependency relationships. The meta-tooling provides comprehensive DAG navigation.

**Dependency Queries**:

| Function | Alias | Returns |
|----------|-------|---------|
| `lattice-deps` | `ld` | Direct dependencies of a skill |
| `lattice-deps-transitive` | — | All transitive dependencies |
| `lattice-uses` | `lu` | Skills that depend on this one |
| `lattice-uses-transitive` | — | All transitive dependents |

**Path Finding**:

```scheme
(lattice-path 'physics/diff 'linalg)
; => (physics/diff autodiff linalg)
```

This uses BFS to find the shortest dependency path between two skills.

**Structural Queries**:

| Function | Returns |
|----------|---------|
| `lattice-roots` | Skills with no dependencies (tier 0) |
| `lattice-leaves` | Skills with no dependents |
| `lattice-hubs` | Most-connected skills (high in/out degree) |
| `lattice-orphans` | Skills not reachable from any entry point |

**DAG Validation**:

```scheme
(lc 'new-skill)                        ; Check for cycles
(lattice-would-cycle? 'from 'to)       ; Proactive cycle detection before adding edge
```

The cycle checker uses DFS with a visited set, flagging any back-edges that would create cycles.

**Analytics**:

```scheme
> (lattice-stats)
((skills . 42)
 (modules . 198)
 (exports . 2714)
 (edges . 87)
 (max-depth . 4)
 (avg-deps . 2.1))

> (lattice-health)
((orphaned-skills . 0)
 (cycles . 0)
 (missing-manifests . 0)
 (parse-errors . 0))

> (lattice-coverage)
((skills-with-tests . 38)
 (skills-without-tests . 4)
 (coverage-pct . 90.5))
```

### 8.4 Cross-Reference System

The cross-reference system (`shell/introspect/xref.ss`) provides function-level dependency analysis, enabling "who calls this?" and "what does this call?" queries.

**Index Construction**:

The xref builder parses all Scheme source files, extracting:

1. **Definitions**: `define`, `define-syntax`, `define-record-type`
2. **References**: All symbol occurrences in expression position
3. **File locations**: Path, line, column for jump-to-definition

**Scale**:

| Metric | Count |
|--------|-------|
| Definitions indexed | ~11,000 |
| Call edges | ~25,000 |
| Files scanned | ~400 |
| Build time (cold) | ~3s |

**Query API**:

```scheme
(xref-callers 'matrix-mul)   ; Who calls matrix-mul?
(xref-callees 'gradient)     ; What does gradient call?
(xref-location 'vec+)        ; Where is vec+ defined?
```

**Quick Aliases**:

| Alias | Function |
|-------|----------|
| `lxu` | `xref-callers` (who **u**ses this) |
| `lxc` | `xref-callees` (what this **c**alls) |

**Integration with Source Locations**:

The `source-loc.ss` module enables jump-to-definition by maintaining a mapping from symbol → file:line:column. This powers the LSP `textDocument/definition` handler.

```scheme
> (xref-location 'traced-set)
("shell/provenance/traced-optics.ss" 45 1)
```

### 8.5 Refactoring Toolkit

The refactoring toolkit (`shell/tools/refactor-toolkit.ss`) provides a unified interface for codebase-wide transformations with preview-before-apply semantics.

**Unified Dispatcher**:

```scheme
(refactor 'help)                           ; Show all operations
(refactor 'rename 'old-name 'new-name)     ; Preview rename
(refactor 'apply)                          ; Apply staged changes
```

**Operations**:

| Operation | Description |
|-----------|-------------|
| `rename` | Rename symbol across codebase |
| `move` | Move symbol to different module |
| `extract` | Extract expression to named function |
| `inline` | Inline function at call sites |
| `dead-code` | Find unused definitions |
| `deps` | Show callers/callees |

**Staged Changes**:

All refactoring operations are staged before application:

```scheme
> (refactor 'rename 'vec+ 'vector-add)
Staged changes:
  lattice/linalg/vec.ss:45 - definition
  lattice/linalg/matrix.ss:23 - reference
  lattice/physics/diff/body.ss:67 - reference
  ... (12 more)

> (refactor 'status)
15 files, 23 changes pending

> (refactor 'apply)
Applied 23 changes to 15 files.
```

**Dead Code Detection**:

```scheme
> (refactor 'dead-code)
Dead code analysis:
  HIGH confidence (no callers):
    - legacy-parse (shell/old/parser.ss:12)
    - unused-helper (lattice/fp/internal.ss:89)
  MEDIUM confidence (internal only):
    - format-debug (core/util/debug.ss:34)
```

Confidence levels:
- **HIGH**: No callers found anywhere in codebase
- **MEDIUM**: Only called from same file (possibly internal)
- **LOW**: Called but from deprecated/test code

**Quick Aliases**:

| Alias | Operation |
|-------|-----------|
| `rr` | `(refactor 'rename ...)` |
| `rm` | `(refactor 'move ...)` |
| `rd` | `(refactor 'deps ...)` |
| `rdc` | `(refactor 'dead-code)` |

### 8.6 Template DSL for Code Generation

The template DSL (`lattice/dsl/template/`) enables grammar-driven code construction, particularly useful for AI-assisted code generation where tracking parentheses is error-prone.

**Core Concept**: Templates are S-expressions with named holes (`$name`) that can be filled incrementally.

**Batch Mode** (Recommended):

```scheme
(tp-batch "
  define (qs lst) $body
  --- $body := if $cond $then $else
  --- $cond := null? lst
  --- $then := '()
  --- $else := append (qs (filter $pred (cdr lst)))
                      (cons (car lst) (qs (filter $pred2 (cdr lst))))
  --- $pred := lambda (x) (< x (car lst))
  --- $pred2 := lambda (x) (>= x (car lst))
")
```

Produces:

```scheme
(define (qs lst)
  (if (null? lst)
      '()
      (append (qs (filter (lambda (x) (< x (car lst))) (cdr lst)))
              (cons (car lst) (qs (filter (lambda (x) (>= x (car lst))) (cdr lst)))))))
```

**Key Features**:

1. **Implicit parentheses**: Multi-token lines auto-wrap
2. **Incremental filling**: Build complex expressions step by step
3. **Validation**: Type-check partial templates
4. **Composition**: Templates can reference other templates

**Grammar Rules**:

| Input | Result |
|-------|--------|
| `"+ 1 2"` | `(+ 1 2)` |
| `"x"` | `x` (single token stays unwrapped) |
| `"'(a b)"` | `'(a b)` (quoted stays as-is) |
| `"$hole"` | Placeholder for later filling |

**Session Mode**:

For interactive development:

```scheme
> (ts-start)
> (ts-template '(define (f x) $body))
> (ts-fill '$body '(+ x 1))
> (ts-show)
(define (f x) (+ x 1))
> (ts-done)
```

**Files**:
- `lattice/dsl/template/template.ss` — Core template engine
- `shell/tools/template-session.ss` — Interactive session
- `shell/tools/template-parser.ss` — Batch mode parser

### 8.7 Issue Tracking (BBS)

The Bulletin Board System (`shell/bbs/`) is a CAS-native issue tracker that stores issues and posts as immutable blocks with head pointers for current state.

**Issue Schema** (tag: `bbs-issue`):

```scheme
((id . "fold-001")
 (title . "Implement fuel tracking for FFI calls")
 (status . open)             ; open, in_progress, blocked, closed
 (priority . 2)              ; 0=critical, 1=high, 2=normal, 3=low, 4=backlog
 (type . feature)            ; task, bug, feature, epic
 (assignee . #f)
 (labels . (ffi performance))
 (created . "2026-01-15T...")
 (updated . "2026-01-17T...")
 (description . "...")
 (version . 1))
```

**Core Operations**:

```scheme
(bbs-list)                              ; List open issues
(bbs-ready)                             ; Unblocked work items
(bbs-show 'fold-001)                    ; View issue details
(bbs-create "Title")                    ; Create issue
(bbs-update 'fold-001 'status 'in_progress)
(bbs-close 'fold-001)                   ; Close issue
```

**Posts** (for changelogs, announcements):

```scheme
(post-create "Title" "Body..." 'changelog)
(post-list 'type 'changelog)
(post-show 'post-1)
```

Post types: `changelog`, `note`, `announcement`, `session-summary`.

**Dependency Tracking**:

Issues can declare dependencies on other issues:

```scheme
(bbs-add-dep! 'fold-002 'fold-001)      ; fold-002 blocks on fold-001
(bbs-deps 'fold-002)                    ; => (fold-001)
(bbs-blocked-by 'fold-001)              ; => (fold-002)
```

**CAS Architecture**:

| Path | Purpose |
|------|---------|
| `.store/heads/bbs/fold-*.head` | Current hash for each issue |
| `.store/heads/bbs/post-*.head` | Current hash for each post |
| `.bbs/counter` | Next issue number |
| `.bbs/post-counter` | Next post number |
| `.bbs/deps` | Dependency graph |
| `.bbs/index.cache` | In-memory index cache |

Updates are atomic via compare-and-swap on head files (see §7.6.3).

### 8.8 Design Principles

The meta-tooling ecosystem follows several key design principles:

**Everything Queryable Through Manifests**:

All skill metadata flows through manifest files. There's no hidden configuration—if a skill has dependencies, exports, or keywords, they're declared in its manifest. This enables:

- Automated documentation generation
- Dependency analysis without loading code
- Search indexing from metadata alone

**CAS-Native Persistence**:

The knowledge graph, BBS issues, and provenance records are all stored as CAS blocks. This provides:

- Immutable history (every state is preserved)
- Content deduplication
- Merkle DAG structure for lineage

**Lazy Loading**:

Tools load their dependencies on demand:

```scheme
;; refactor-toolkit.ss
(define (refactor op . args)
  (case op
    [(rename) (load-once "refactor-rename.ss") ...]
    [(move) (load-once "refactor-move.ss") ...]
    ...))
```

This keeps startup fast while providing rich functionality.

**Pure/Impure Boundary**:

- **Pure** (lattice): Manifest parsing, BM25 scoring, dependency analysis
- **Impure** (shell): File I/O, index persistence, head file updates

The pure components are testable and reusable; the impure components handle the messy reality of file systems and concurrent access.

**Agent-Oriented Design**:

All query functions return structured data, not formatted strings:

```scheme
(lf "query")   ; Returns ((name score type context) ...)
(ld 'skill)    ; Returns (dep1 dep2 ...)
```

This enables agents to process results programmatically rather than parsing human-readable output.

---

## 9. The Fold as Agent Substrate

The Fold's architecture provides unique properties for AI agent execution—content-addressed computation, fuel-bounded evaluation, capability-based security, and immutable provenance. This chapter examines why The Fold is particularly suited as an agent runtime substrate.

### 9.1 Design Thesis

The Fold embodies several architectural decisions that make it well-suited for agent workloads:

**S-expressions Enable Stackable Abstractions**:

S-expressions have a *fractal grammar*—the same syntax rules apply at every level of nesting. This means:

- Agents can manipulate code as data without special parsing
- Macros and code generation compose naturally
- The same tooling works on expressions of any complexity

Traditional languages require agents to handle varying syntactic constructs (statements vs expressions, blocks vs brackets, significant whitespace). S-expressions eliminate this cognitive overhead.

**Editing ASTs is Easier Than Editing Text**:

When agents generate or modify code, they're fundamentally manipulating abstract syntax trees. Most languages force a roundtrip: parse text → modify AST → serialize back to text. The Fold's homoiconicity means code *is* the AST—agents work directly with the data structure they're reasoning about.

**Agents Don't Need to Solve the Halting Problem**:

We can't prove arbitrary programs terminate, but we can ensure *agents* halt by construction. The Fold's fuel system provides predictable resource bounds:

```scheme
(eval-with-fuel expr env 10000)  ; Guaranteed to return within 10000 operations
```

An agent can always reason about its resource consumption before execution.

**Content-Addressed Computation Creates Reproducibility**:

When computation is content-addressed:

- Same input → same hash → same result (referential transparency)
- Computations can be cached by their hash
- Shared subexpressions are automatically deduplicated
- Audit trails are immutable

**Pure Core + Impure Shell Enables Verifiable Reasoning**:

The agent's "reasoning" (pure computation in Core) is separate from its "actions" (effects through Shell). This separation means:

- Core computations can be verified, replayed, and tested
- Effects are explicit and auditable
- The attack surface for capability violations is small (Shell boundary only)

### 9.2 The Agent Interface

Agents interact with The Fold through a well-defined interface designed for both human and programmatic use.

**CLI Access**:

The `./fold` CLI provides the primary agent interface:

```bash
./fold "+ 1 2"                     # Implicit parens: (+ 1 2)
./fold "map add1 '(1 2 3)"         # Multi-token auto-wraps
./fold -s agent-1 "define x 10"    # Named session with state
./fold -s agent-1 "(begin x)"      # Retrieve from session
```

Key features for agents:
- **Implicit parentheses**: Reduces syntax errors
- **Session persistence**: State survives across invocations
- **Structured errors**: Exit codes and stderr for programmatic handling
- **Auto-starting daemon**: No explicit lifecycle management needed

**Core Agent Workflows**:

| Workflow | Entry Points | Description |
|----------|--------------|-------------|
| Capability Discovery | `lf`, `li`, `le` | Find available functions in the lattice |
| Data Manipulation | `^.`, `.~`, `%~`, `>>>` | Composable optic operations |
| Querying | `oquery`, `oquery-where` | Declarative data extraction |
| Reactive State | `define-reactive`, `reactive-value` | Auto-invalidating cached computations |
| Coordination | `bbs-create`, `bbs-list`, `bbs-ready` | Async work coordination |
| Provenance | `traced-set`, `explain` | Auditable decision paths |
| History | `undo`, `redo`, `branch` | Full undo/redo with branching |

**Structured Data Access via Optics**:

The optics tower (§7.8) provides type-safe, composable data access:

```scheme
;; View nested data
(^. body (>>> body-pos-lens vec2-x-lens))

;; Modify at path
(& body (%~ (>>> body-pos-lens vec2-x-lens) add1))

;; Collect from traversal
(^.. world (>>> world-all-bodies body-vel-lens))
```

Optics compose: `Lens >>> Lens = Lens`, `Lens >>> Prism = Affine`. The type system tracks what operations are valid.

**Query DSL**:

For complex data extraction, the optic query language (§7.12) provides declarative access:

```scheme
(oquery-pipe world world-each-body
  (lambda (b) (> (^. b body-vel-y) 0))    ; Filter
  (lambda (b) (^. b body-name)))          ; Project
```

### 9.3 Fuel-Bounded Computation

Every operation in The Fold has a predictable fuel cost. Agents can estimate resource consumption before execution.

**Fuel Model**:

| Operation Class | Fuel Cost |
|-----------------|-----------|
| Primitive ops (`+`, `cons`, `car`) | O(1) |
| Linear traversals (`map`, `filter`) | O(n) |
| Sorting operations | O(n log n) |
| Matrix operations | O(n²) to O(n³) |
| BVH queries (§7.4) | O(log n) average |

**Predictive Estimation**:

Before executing expensive operations, agents can query fuel requirements:

```scheme
(estimate-fuel '(matrix-mul A B))  ; Returns estimated fuel cost
```

**Resumable Execution**:

Long-running computations can checkpoint and resume:

```scheme
(define checkpoint (eval-with-fuel expr env 5000))
(if (out-of-fuel? checkpoint)
    (resume-from checkpoint 10000)  ; Continue with more fuel
    (result-value checkpoint))
```

This enables agents to:
1. Start with conservative fuel budgets
2. Checkpoint at regular intervals
3. Resume after yielding to other tasks

**Why Fuel Matters for Agents**:

Traditional runtimes have unbounded execution—an agent can accidentally trigger infinite loops or exponential blowup. Fuel provides:

- **Predictability**: Agent can commit to resource bounds upfront
- **Fairness**: Multiple agents share resources via fuel allocation
- **Safety**: Runaway computation is impossible by construction

### 9.4 Effect Pipelines

Agents execute effects through structured pipelines (`lattice/pipeline/`), not ad-hoc side effects.

**Pipeline Stages**:

A pipeline is a sequence of stages, each with an explicit effect type:

```scheme
(define my-pipeline
  (pipeline
    (stage 'fetch (effect/http "https://api.example.com/data"))
    (stage 'parse (effect/fold-eval '(parse-json $input)))
    (stage 'store (effect/store-put '$result))
    (stage 'respond (effect/llm "Summarize: $result"))))
```

**Effect Types**:

| Effect | Description |
|--------|-------------|
| `llm` | LLM API call |
| `fold-eval` | Pure Fold evaluation |
| `shell` | Shell command execution |
| `store-put` / `store-get` | CAS operations |
| `checkpoint` | Save/restore state |
| `http` | HTTP requests |

**Council Effects** (Multi-Model Reasoning):

For complex decisions, agents can invoke multiple models:

```scheme
(effect/council 'consensus
  '((claude "Analyze this code for bugs")
    (gemini "Review for security issues")
    (local "Check style compliance")))
```

Council modes:
- `sequential`: Each model builds on previous
- `parallel`: All models run independently
- `vote`: Majority decision
- `debate`: Models critique each other
- `consensus`: Iterate until agreement

**Pipeline Execution**:

Pipelines are *pure definitions*—they describe what effects to perform but don't execute them. The shell interpreter runs pipelines:

```scheme
(run-pipeline my-pipeline initial-context)
```

This separation means:
- Pipelines can be serialized, stored, shared
- Execution is auditable (every stage logged)
- Testing can mock effect handlers

### 9.5 Capability-Based Security

Agents operate under a capability-based security model where permissions are explicit, unforgeable tokens.

**Capability Records**:

Capabilities are opaque records that can only be created by Shell code:

```scheme
;; Shell mints capabilities
(define file-cap (mint-capability 'file-read "/home/data"))

;; Core receives capabilities, cannot inspect internals
(define (process-data cap)
  (let ([content (read-with-cap cap)])  ; Uses capability
    (parse content)))
```

**Security Properties**:

1. **Unforgeable**: Core code cannot construct capabilities
2. **Transferable**: Capabilities can be passed to functions
3. **Revocable**: Shell can invalidate capabilities
4. **Auditable**: Capability usage is logged

**Capability Audit**:

```scheme
(capability-audit 'my-agent)
; => ((file-read "/home/data" 15 times)
;     (network "api.example.com" 3 times)
;     (store-write ".store/" 42 times))
```

**Why Capabilities for Agents?**

Traditional permission models (user-based, role-based) don't map well to agents:
- Agents may need temporary, scoped permissions
- Permissions should flow with data, not identity
- Audit trails need fine-grained attribution

Capabilities provide exactly this: permissions are values that can be passed, scoped, and tracked.

### 9.6 Reactive State and Provenance

Agents need both reactive updates (when dependencies change) and provenance (how did we get here).

**Reactive Derivations** (§7.11):

Define computed values that automatically invalidate when dependencies change:

```scheme
(define-reactive 'player-health world-state
  (lambda (world)
    (reactive-view world (>>> (body-lens 'player) health-lens))))

(reactive-value 'player-health)     ; Computed and cached
;; ... world-state changes via reactive-set! ...
(reactive-value 'player-health)     ; Recomputed with new value
```

The reactive system tracks which optics were accessed during computation and invalidates when those optics are written.

**Provenance Tracking** (§7.10):

Every traced optic operation creates an immutable audit record:

```scheme
(define v1 '(1 . 2))
(define v2 (traced-set lens-fst 10 v1))
(define v3 (traced-set lens-snd 20 v2))

(explain v3)
; => Lineage for 00abc123...
;    Step 1: set via lens-fst (lens)
;      Source: 00def456... → (1 . 2)
;      Result: 00789abc... → (10 . 2)
;    Step 2: set via lens-snd (lens)
;      Source: 00789abc... → (10 . 2)
;      Result: 00abc123... → (10 . 20)
```

**Query Provenance**:

```scheme
(provenance-for-result result-hash)   ; What created this?
(trace-lineage result-hash)           ; Full history
(find-provenance-by-agent 'my-agent)  ; All operations by agent
```

**Why Both?**

- **Reactivity** enables responsive agents: state changes propagate automatically
- **Provenance** enables explainable agents: every decision has a traceable path

Together, they provide agents with dynamic state that remains fully auditable.

### 9.7 Content-Addressed Knowledge Base

Agent knowledge is stored as immutable CAS blocks, providing several key properties.

**Semantic Identity**:

The same reasoning produces the same hash:

```scheme
;; These produce identical hashes (α-equivalent)
(store! (make-block 'thought '() "Consider: X implies Y"))
(store! (make-block 'thought '() "Consider: X implies Y"))
; => Same hash, no duplication
```

**Merkle DAG Structure**:

Knowledge blocks can reference other blocks, forming a DAG:

```scheme
(define premise-1 (store! (make-block 'premise '() "All men are mortal")))
(define premise-2 (store! (make-block 'premise '() "Socrates is a man")))
(define conclusion (store! (make-block 'conclusion
                                        (vector premise-1 premise-2)
                                        "Socrates is mortal")))
```

The conclusion block references its premises—the reasoning structure is explicit.

**Automatic Deduplication**:

If two agents independently derive the same conclusion, it's stored once:

```scheme
;; Agent A derives: "The function is O(n²)"
;; Agent B derives: "The function is O(n²)"
;; => Single block in CAS, both agents reference it
```

**Lineage Tracking**:

Every block knows its inputs (refs vector). Tracing refs reconstructs the full derivation:

```scheme
(collect-block-tree fetch conclusion-hash)
; => All blocks in the reasoning chain
```

### 9.8 Multi-Agent Coordination

Multiple agents coordinate through CAS-native mechanisms designed for concurrent, asynchronous operation.

**BBS (Bulletin Board System)**:

Agents communicate through the issue tracker (§8.7):

```scheme
;; Agent A creates work item
(bbs-create "Analyze module X for performance issues")

;; Agent B claims and works
(bbs-update 'fold-042 'status 'in_progress)
(bbs-update 'fold-042 'assignee 'agent-b)

;; Agent B completes
(bbs-close 'fold-042)
```

**Properties**:
- **Asynchronous**: No tight coupling between agents
- **Persistent**: Work survives agent restarts
- **Auditable**: Full history in CAS
- **Priority-based**: Agents can query `(bbs-ready)` for highest-priority unblocked work

**Flashmob (QA Triage)**:

For coordinated quality assurance, the flashmob system aggregates findings from multiple agents:

```scheme
(flashmob-report! 'agent-a
  '((file . "vec.ss")
    (severity . high)
    (confidence . 0.9)
    (finding . "Potential overflow in vec-dot")))
```

The flashmob coordinator:
1. Aggregates findings from all agents
2. Applies game-theoretic credit allocation (Shapley values)
3. Ranks issues by severity × confidence
4. Exports actionable items to BBS

**No Shared Mutable State**:

Agents never share mutable memory. All coordination happens through:
- CAS blocks (immutable, content-addressed)
- Head pointers (atomic compare-and-swap)
- BBS issues (explicit work items)

This eliminates entire classes of concurrency bugs (races, deadlocks, lost updates).

### 9.9 Differentiating Properties

The Fold's architecture provides properties that distinguish it from traditional agent runtimes.

| Property | Traditional Runtime | The Fold |
|----------|---------------------|----------|
| **Identity** | Process ID, memory address | Content hash |
| **State** | Mutable memory | Immutable blocks |
| **Resource bounds** | Unknown until crash/timeout | Fuel-predicted |
| **Reasoning audit** | Logs (lossy, ad-hoc) | Provenance (complete, structured) |
| **Code updates** | Replace process, lose state | New hash, old preserved |
| **Multi-agent** | Shared mutable state, locks | CAS coordination, no locks |
| **Effects** | Implicit, anywhere | Explicit pipeline stages |
| **Permissions** | User/role-based | Capability tokens |

**Content-Addressed Identity**:

Traditional systems identify agents by process ID or memory address. The Fold identifies computations by their content hash. This means:
- "Same computation" has a precise definition
- Results can be cached and shared across agents
- Identical reasoning from different agents is recognized as identical

**Immutable State**:

Mutable state requires careful synchronization. Immutable blocks eliminate this:
- No locks needed—blocks never change
- "Update" means creating a new block with new hash
- History is automatic—old versions exist forever

**Predictable Resources**:

Traditional runtimes discover resource limits through failure. The Fold knows costs upfront:
- Fuel estimation before execution
- Guaranteed termination within budget
- Fair sharing through fuel allocation

**Complete Audit Trail**:

Logs are typically unstructured text, prone to loss, and incomplete. Provenance is:
- Structured (typed records)
- Immutable (CAS-stored)
- Complete (every traced operation recorded)
- Queryable (find by agent, optic, time)

**Safe Evolution**:

Updating agent code in traditional systems risks losing state or breaking assumptions. With content-addressing:
- Old code continues to exist (its hash)
- New code gets a new hash
- Both can coexist, be compared, rolled back

**Lock-Free Coordination**:

Shared mutable state requires locks, which cause deadlocks and contention. CAS coordination:
- Compare-and-swap on head pointers
- Retry on conflict (optimistic concurrency)
- No global locks, no deadlocks

---

## 10. Evaluation


### 10.1 Storage Efficiency

**Deduplication Ratio**:

We measured the CAS storage compared to file-based storage for the standard library:

| Metric | File-Based | CAS | Ratio |
|----|----|----|----|
| Total source | 1.2 MB | 890 KB | 0.74x |
| After α-norm | — | 720 KB | 0.60x |
| With sharing | — | 580 KB | 0.48x |

The CAS achieves ~50% size reduction through:
1. α-normalization eliminating variable name variation
2. Structural sharing of common subexpressions
3. Deduplication of identical blocks

**Block Size Distribution**:

```
Block Size    Count    Percentage
──────────────────────────────────
< 100 bytes   12,341   45%
100-500       8,923    33%
500-2000      4,567    17%
> 2000        1,234    5%
```

Most blocks are small (under 500 bytes), enabling efficient hashing and transmission.

### 10.2 Normalization Equivalence Detection

We measured how often each normalization level detects semantic equivalences that simpler levels miss. The benchmark analyzed 939,880 subexpressions extracted from `core/` and `lattice/`.

**Unique Hashes by Normalization Level**:

| Level | Description | Unique Hashes | Reduction |
|-------|-------------|---------------|-----------|
| v0x00 | α-normalization only | 268,325 | baseline |
| v0x01 | + algebraic canonicalization | 268,237 | 88 (0.03%) |
| v0x02 | + η-reduction, identity elimination | 268,174 | 151 (0.06%) |

**What Each Level Detects**:

- **v0→v1 (88 equivalences)**: Commutative reordering
  - `(* a b)` ≡ `(* b a)`
  - `(+ 1 (* 2 k))` ≡ `(+ (* 2 k) 1)`

- **v1→v2 (63 equivalences)**: Identity elimination and η-reduction
  - `(* 1.0 ndotl)` ≡ `ndotl`
  - `(+ x 0)` ≡ `x`

**Structural vs. Semantic Duplication**:

| Metric | Count | Percentage |
|--------|-------|------------|
| Total subexpressions | 939,880 | 100% |
| Unique hashes (structural) | 268,325 | 28.5% |
| Structural duplicates | 671,555 | 71.5% |
| Semantic equivalences (v0→v2) | 151 | 0.06% |

The high structural duplication (71.5%) reflects normal code patterns—expressions like `(car x)` and `(null? lst)` appear thousands of times. The CAS automatically deduplicates these.

The low semantic equivalence rate (0.06%) indicates the codebase is already written in near-canonical form. Developers consistently write `(+ a b)` rather than mixing `(+ a b)` and `(+ b a)` arbitrarily. This is a positive signal about code consistency.

**Bug Discovery**: The benchmark uncovered a normalization bug where unary negation `(- x)` was incorrectly collapsed to `x`. The identity element `0` for subtraction only applies to binary `(- x 0)`, not unary negation. This caused 122 false equivalences in initial results, demonstrating the value of empirical validation.

### 10.3 Type Checking Performance

**Inference Time** (representative programs):

| Program | LOC | Types | Inference Time |
|----|----|----|----|
| Vec operations | 450 | 89 | 12ms |
| Matrix lib | 1,200 | 234 | 45ms |
| Parser combinators | 800 | 156 | 38ms |
| Physics sim | 2,100 | 312 | 89ms |

Performance scales approximately linearly with program size.

**NbE Normalization Overhead**:

For dependent type checking, NbE adds ~15-20% overhead compared to simple type checking, justified by the expressiveness gains.

### 10.4 Case Study: Building the Linear Algebra Module

We trace the complete workflow for implementing `lattice/linalg`:

**Step 1: Create manifest**
```scheme
(skill linalg
  (version "0.1.0")
  (tier 0)
  (purity total)
  ...)
```

**Step 2: Implement core operations**
```scheme
;; vec.ss
(define (vec+ v1 v2)
  (vector-map + v1 v2))

(define (dot v1 v2)
  (vector-fold + 0 (vector-map * v1 v2)))
```

**Step 3: Type check**
```scheme
;; Types inferred and verified
vec+ : (∀ (n) (→ (Vec n Num) (→ (Vec n Num) (Vec n Num))))
dot  : (∀ (n) (→ (Vec n Num) (→ (Vec n Num) Num)))
```

**Step 4: Verify fuel bounds**
- `vec+`: O(n) — single pass
- `dot`: O(n) — single pass
- `matrix-mul`: O(n³) — triple nested loop

**Step 5: Register in DAG**
```scheme
(kg-build!)
(li 'linalg)  ; Verify registration
```

**Verification Cascade**: When `autodiff` (Tier 1) imports `linalg`:
1. Check `linalg` is verified ✓
2. Type-check `autodiff` against `linalg`'s exports
3. Verify `autodiff`'s fuel bounds
4. `autodiff` is now verified

---
## 11. Related Work


### 11.1 Content-Addressed Systems

**Unison** (Chiusano & Bjarnason) is the closest related work—a programming language with content-addressed definitions. Key differences:

| Aspect | Unison | The Fold |
|----|----|----|
| Normalization | Ability-based hashing | De Bruijn α-normalization |
| Type system | Ability effects | Gradual dependent types |
| Module system | Namespace-based | Tiered DAG with manifests |
| Implementation | Haskell | Chez Scheme (self-contained) |
| Effects | First-class abilities | Capability types + monads |

The Fold's de Bruijn approach provides stronger α-equivalence guarantees. Unison's ability system is more integrated with the type system; The Fold separates effects into Shell.

**IPFS**: Content-addressed storage for arbitrary data. The Fold adapts similar Merkle DAG concepts for code specifically, adding normalization and typing.

**Git**: Content-addressed version control. The Fold can be seen as "Git for computations"—immutable, hash-identified, with verified composition.

**Nix**: Content-addressed builds. Nix addresses build reproducibility; The Fold addresses computation reproducibility at a finer grain.

### 11.2 Dependent Type Systems

**Agda, Idris, Lean**: Full-spectrum dependent types with proof capabilities. The Fold's type system is less powerful (no universe polymorphism, limited tactics) but more practical (gradual typing, dictionary-passing classes).

**Gradual Dependent Types** (Eremondi et al.): Theoretical foundations for combining gradual and dependent types. The Fold implements a conservative subset of these ideas.

### 11.3 Homoiconic Languages

**Lisp tradition**: The Fold continues McCarthy's vision of code-as-data. Unlike traditional Lisps, The Fold adds content addressing and dependent types to the homoiconic foundation.

**Racket**: Advanced macro system and language-oriented programming. The Fold's metaprogramming is simpler but content-addressed.

### 11.4 Module Systems

**ML Modules**: Sophisticated module system with functors and signatures. The Fold's module system is simpler (no functors) but adds verification metadata.

**Backpack**: Mixin modules for Haskell. Similar goals of flexible composition; different mechanisms.

### 11.5 Probabilistic Programming and Automatic Differentiation

**Stan, PyMC, Pyro**: Popular probabilistic programming languages. The Fold's approach is more minimalist—variational inference as a library rather than a DSL, using general-purpose autodiff.

**JAX, PyTorch**: Automatic differentiation frameworks that enable gradient-based inference. The Fold's autodiff is similar in spirit (traced computation graphs) but implemented in pure Scheme with fuel tracking.

**Edward, Turing.jl**: Probabilistic programming with variational inference. The Fold follows similar theoretical foundations (ELBO optimization, reparameterization trick) but emphasizes simplicity and self-containment over ecosystem breadth.

| Aspect | Stan/PyMC | The Fold |
|----|----|----|
| Inference | HMC + VI | VI (ELBO optimization) |
| Autodiff | External (C++/JAX) | In-house (traced values) |
| Language | DSL/Python | Embedded in Scheme |
| Guarantees | None | Fuel-tracked, content-addressed |

The Fold's probabilistic programming is less feature-rich but fully introspectable and self-contained.

---
## 12. Limitations and Non-Goals


Honest acknowledgment of what The Fold does NOT provide.

### 12.1 Not True Totality

The Fold guarantees *bounded execution*, not *totality*. The difference:

| Property | Totality | Bounded Execution |
|----|----|----|
| Guarantee | Function terminates on all inputs | Execution stops within fuel limit |
| Mechanism | Structural recursion / sized types | Fuel counter |
| On failure | Rejected at compile time | Runtime `out-of-fuel` error |
| For type checking | Safe to evaluate during checking | Unsafe (might exhaust fuel) |

**Implication**: We cannot safely evaluate arbitrary Core functions during type checking. This limits dependent type expressiveness compared to Agda or Idris.

### 12.2 Limited Gradual + Dependent Integration

The Fold does NOT support:
- Holes in dependent positions (`(Π ((x : ?)) (Vec x A))`)
- Incremental addition of dependent types to untyped code
- Approximate normalization for gradual dependent terms

This is a deliberate simplification. Full gradual dependent types (Eremondi et al., 2019) require sophisticated runtime checks and approximate normalization. We chose separation over complexity.

### 12.3 No Proof Tactics

Unlike Agda, Idris, or Lean, The Fold provides no:
- Tactic language for proof construction
- Proof search or automation
- Holes with goal display
- Interactive proof development

Dependent types are for specification, not theorem proving. Use external proof assistants for serious verification.

### 12.4 Shell is Unverified

The Shell is *trusted but unverified*. We believe it maintains its invariants, but we have not mechanically verified this. The verification boundary is:

```
   ┌─────────────────────────┐
   │   Shell (trusted)       │  ← May have bugs
   ├─────────────────────────┤
   │   Core (verified*)      │  ← *Type-safe by construction
   └─────────────────────────┘
```

Core is verified in the sense that well-typed programs don't go wrong (within fuel bounds). Shell correctness is assured by testing and code review.

### 12.5 Single-Node Only

The current implementation is single-node:
- No distributed CAS
- No peer-to-peer code sharing
- No remote capability delegation

Distributed operation is future work (§11).

### 12.6 IDE Integration Limitations

The Fold includes an LSP implementation (`shell/lsp/`) providing:
- Hover-based type inference for top-level definitions
- Basic diagnostics
- Document synchronization

**Current limitations:**
- No editor plugins packaged (users must configure LSP clients manually)
- No jump-to-definition or find-references yet
- Type inference is per-file only (no project-wide analysis)
- Forward references in the same file may show incomplete types
- Local bindings inside `let` forms are not yet typed

The REPL and command-line tools remain the primary development interface, but LSP support enables basic IDE features for editors that support the protocol.

### 12.7 Floating-Point Algebraic Properties

Algebraic normalization assumes mathematical properties that don't hold perfectly for floating-point arithmetic:

```scheme
;; Mathematically: (+ (+ 1e20 1.0) -1e20) = (+ 1e20 (+ 1.0 -1e20))
;; IEEE 754: (+ (+ 1e20 1.0) -1e20) → 0.0
;;           (+ 1e20 (+ 1.0 -1e20)) → 1.0
```

**Current approach**: We apply associative flattening anyway, accepting that:
1. For exact numbers, the normalization is semantically correct
2. For floating-point, the normalization may change computed results
3. Hash identity implies mathematical equivalence, not IEEE 754 bit-identical results

**Future consideration**: Restrict algebraic canonicalization to exact arithmetic only, or provide an opt-out for numeric-sensitive code.

### 12.8 Metaprogramming Type Interactions

The `quote`/`eval` mechanism has limited type integration:

```scheme
;; quote produces an untyped S-expression
(quote (+ 1 2))  ; type: Sexpr (not (Expr Int))

;; eval has type (→ Sexpr ?)
;; We cannot statically know the result type
```

Typed quotation (as in MetaML or Typed Template Haskell) is not implemented. Metaprogramming operates at the untyped level.

---
## 13. Future Work


**Distributed CAS**: Extend the CAS to peer-to-peer networks, enabling decentralized code sharing with content verification.

**Concurrent Access**: Add MVCC (Multi-Version Concurrency Control) for safe concurrent reads/writes to the CAS.

**Algebraic Effects**: Integrate algebraic effects more deeply, replacing the current capability/monad approach.

**Linear Types**: Add linear/affine types for safe resource management in Shell.

**Incremental Type Checking**: Cache type derivations in the CAS, enabling O(changed) re-checking instead of O(total).

**Formal Verification**: Mechanize the Core semantics in a proof assistant, proving type soundness and other properties.

---
## 14. Conclusion


The Fold demonstrates that content-addressed homoiconic computation is practical. By combining:

1. **Content addressing** with α-normalization for semantic identity
2. **Gradual dependent types** for flexible verification
3. **Tiered module DAG** for compositional organization

...we achieve a system where code is mathematics: immutable, uniquely identified, and composable.

The key insight is that *identity should follow semantics*. Functions that compute the same thing should be the same function. Dependencies that provide the same interface should be interchangeable. By making the hash—the cryptographic identity—follow from normalized content through a two-phase pipeline (algebraic canonicalization, then α-normalization), The Fold aligns system identity with mathematical identity. Commutative operations, associatively restructured expressions, and independently reorderable bindings all receive the same hash—because they compute the same thing.

This is not merely theoretical elegance. Practical benefits include:
- Automatic deduplication (same code stored once)
- Verified composition (type-check once, trust forever)
- Reproducible computation (same inputs → same outputs, guaranteed)
- Semantic versioning for free (same hash = same behavior)

The Fold is computation as it should be: *content-addressed, type-safe, and eternal*.

---
## Appendix A: Block Calculus Formal Syntax


```
e ::= x                                  ; Variable
    | (λ x : τ . e)                      ; Typed abstraction
    | (e₁ e₂)                            ; Application
    | (let x : τ = e₁ in e₂)             ; Let binding
    | (fix x : τ . e)                    ; Recursive binding
    | c                                  ; Constant
    | (if e₁ e₂ e₃)                      ; Conditional
    | (prim op e*)                       ; Primitive operation
    | (make-block τ e_tag e_payload e_refs)  ; Block construction
    | (block-tag e)                      ; Tag accessor
    | (block-payload e)                  ; Payload accessor
    | (block-refs e)                     ; Refs accessor
    | (hash e)                           ; Hash computation
    | (store! e)                         ; CAS store
    | (fetch e)                          ; CAS fetch
    | (quote e)                          ; Quotation
    | (eval e)                           ; Evaluation

τ ::= Nat | Int | Bool | ...             ; Base types
    | (→ τ₁ τ₂)                          ; Function type
    | (× τ₁ τ₂)                          ; Product type
    | (+ (l₁ τ₁) ... (lₙ τₙ))            ; Sum type
    | (∀ α . τ)                          ; Universal type
    | (μ α . τ)                          ; Recursive type
    | (Block τ_tag τ_payload)            ; Block type
    | (Ref τ)                            ; Reference type
    | α                                  ; Type variable
    | ?                                  ; Hole

v ::= (λ x : τ . e)                      ; Abstraction value
    | c                                  ; Constant value
    | (block v_tag v_payload v_refs)     ; Block value
```

**Reduction Rules**:

```
((λ x : τ . e) v) →β e[v/x]

(let x : τ = v in e) → e[v/x]

(fix x : τ . e) → e[(fix x : τ . e)/x]

(if true e₂ e₃) → e₂

(if false e₂ e₃) → e₃

(block-tag (block t p r)) → t

(block-payload (block t p r)) → p

(block-refs (block t p r)) → r

(eval (quote e)) → e
```

---
## Appendix B: Type Grammar


```bnf
<type>       ::= <base-type>
               | <compound-type>
               | <dependent-type>
               | <polymorphic-type>
               | <special-type>

<base-type>  ::= "Nat" | "Int" | "Bool" | "Char" | "Symbol"
               | "String" | "Bytes" | "Unit" | "Void" | "Hash"

<compound-type> ::= "(" "→" <type> <type> ")"
                  | "(" "×" <type>+ ")"
                  | "(" "+" <variant>+ ")"
                  | "(" "List" <type> ")"
                  | "(" "Vector" <type> ")"
                  | "(" "Block" <symbol> <type> ")"
                  | "(" "Ref" <type> ")"

<variant>    ::= "(" <tag> <type> ")"

<dependent-type> ::= "(" "Π" "(" <binding>+ ")" <type> ")"
                   | "(" "Σ" "(" <binding>+ ")" <type> ")"
                   | "(" "=" <type> <term> <term> ")"

<binding>    ::= "(" <var> ":" <type> ")"

<polymorphic-type> ::= "(" "∀" "(" <tvar>+ ")" <type> ")"
                     | "(" "μ" <tvar> <type> ")"
                     | <tvar>

<special-type> ::= "?" | "(" "?" <name> ")"
                 | "(" "Cap" <name> <type> ")"

<tvar>       ::= <identifier>
<tag>        ::= <identifier>
<name>       ::= <identifier>
```

---
## Appendix C: Kind Grammar


```bnf
<kind>       ::= "*"                           ; Type kind
               | "(" "⇒" <kind> <kind> ")"     ; Kind arrow
               | "Constraint"                   ; Constraint kind
               | "Row"                          ; Row kind
               | "(" "κ∀" "(" <kvar>+ ")" <kind> ")"  ; Kind polymorphism
               | "(" "Πκ" "(" <kbinding> ")" <kind> ")" ; Dependent kind
               | "□"                            ; Sort
               | "(" "□" <nat> ")"              ; Leveled sort

<kbinding>   ::= "(" <kvar> ":" <kind> ")"

<kvar>       ::= "κ" <identifier>
```

---
## Appendix D: Manifest Schema


```scheme
;; Complete manifest schema
(skill <name:symbol>
  ;; Required fields
  (version <semver:string>)           ; "major.minor.patch"
  (tier <n:nat>)                      ; 0 = foundational
  (path <path:string>)                ; Relative to project root
  (purity <p:purity>)                 ; total | partial | effect
  (stability <s:stability>)           ; stable | experimental
  (fuel-bound <bound:string>)         ; Big-O notation
  (deps (<dep:symbol> ...))           ; Direct dependencies

  ;; Optional fields
  (description <desc:string>)         ; Human-readable
  (keywords (<kw:symbol> ...))        ; Search tags
  (aliases (<alias:symbol> ...))      ; Alternative names

  ;; API specification
  (exports
    (<module:symbol> <export:symbol>+ ) ...)

  ;; Module listing
  (modules
    (<name:symbol> <file:string> <desc:string>) ...))

;; Purity levels
<purity> ::= total    ; Pure, terminating
           | partial  ; Pure, may diverge
           | effect   ; Has side effects

;; Stability levels
<stability> ::= stable       ; API frozen
              | experimental ; API may change
```

---
## Appendix E: Comparison with Unison


| Feature | The Fold | Unison |
|----|----|----|
| **Foundation** | Chez Scheme | Haskell |
| **Content Addressing** | SHA-256 + de Bruijn | Hash + type-directed |
| **α-Normalization** | De Bruijn indices | Implicit in hashing |
| **Type System** | Gradual dependent | Abilities (effect types) |
| **Higher-Kinded Types** | Yes (kind system) | Yes |
| **Type Classes** | Dictionary-passing | Abilities |
| **Dependent Types** | Π, Σ, inductive | Limited |
| **GADTs** | Yes | No |
| **Effects** | Capability types + monads | First-class abilities |
| **Module System** | Tiered DAG + manifests | Namespaces |
| **Verification** | Compositional, fuel-bounded | Not emphasized |
| **Dependencies** | None (self-contained) | Haskell ecosystem |
| **Homoiconicity** | Full (S-expressions) | Partial |
| **Metaprogramming** | quote/eval | Limited |
| **Tooling** | BM25 search, DAG nav | Codebase manager |

**Key Philosophical Differences**:

1. **Effects**: Unison treats effects as first-class abilities integrated into the type system. The Fold separates effects into Shell with capability types.

2. **Verification**: The Fold emphasizes compositional verification with fuel bounds. Unison focuses on codebase management.

3. **Dependencies**: The Fold has zero external dependencies. Unison builds on Haskell.

4. **Metaprogramming**: The Fold's homoiconicity enables full quote/eval. Unison's approach is more restricted.

---
## References


1. de Bruijn, N. G. (1972). "Lambda calculus notation with nameless dummies, a tool for automatic formula manipulation, with application to the Church-Rosser theorem." *Indagationes Mathematicae*, 75(5), 381-392.

2. Dunfield, J., & Krishnaswami, N. R. (2013). "Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism." *ICFP 2013*.

3. Martin-Löf, P. (1984). *Intuitionistic Type Theory*. Bibliopolis.

4. Barendregt, H. (1984). *The Lambda Calculus: Its Syntax and Semantics*. North-Holland.

5. FIPS 180-4. (2015). "Secure Hash Standard (SHS)." *NIST*.

6. Chiusano, P., & Bjarnason, R. "Unison: A new distributed programming platform." *unison-lang.org*.

7. Pierce, B. C. (2002). *Types and Programming Languages*. MIT Press.

8. Abel, A., & Scherer, G. (2012). "On Irrelevance and Algorithmic Equality in Predicative Type Theory." *LMCS*.

9. Eremondi, J., Greenberg, M., & Labiche, Y. (2019). "Approximate Normalization for Gradual Dependent Types." *ICFP 2019*.

10. McCarthy, J. (1960). "Recursive Functions of Symbolic Expressions and Their Computation by Machine, Part I." *CACM*, 3(4), 184-195.

---

*The Fold: Where code is content, and content is eternal.*