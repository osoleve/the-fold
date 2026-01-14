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

Version `0x00` provides α-equivalence: `(λ x. x)` and `(λ y. y)` hash identically.

Version `0x01` provides extended equivalence: `(+ a b)` and `(+ b a)` also hash identically, as do `(+ (+ a b) c)` and `(+ a b c)`.

Version `0x02` provides maximum semantic equivalence detection: `(+ x 0)` and `x` hash identically, `(+ x x)` and `(* 2 x)` hash identically, and `(fn (x) (f x))` and `f` hash identically (when x is not free in f).

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
