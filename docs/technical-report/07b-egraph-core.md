## 7b. E-Graph Data Structures

The Fold includes a complete **equality saturation** system for finding optimal equivalent program forms. This chapter covers the core e-graph data structures and operations. The following chapter (7c) covers pattern matching, saturation, and optimization.

### 7b.1 Motivation: Why E-Graphs for CUDA?

Consider optimizing a matrix expression for GPU execution:

```scheme
;; Original: a*b + a*c (two multiplications, one addition)
(+ (* a b) (* a c))

;; Factored: a*(b+c) (one multiplication, one addition)
(* a (+ b c))
```

Both expressions are mathematically equivalent, but the factored form:
- Reduces memory bandwidth (fewer intermediate results)
- Reduces kernel launches (fewer operations to dispatch)
- May enable fused multiply-add (FMA) optimizations

Traditional compilers apply rewrite rules in a fixed order, potentially missing optimal forms. **Equality saturation** explores *all* equivalent forms simultaneously, then extracts the optimal one using a cost model.

### 7b.2 E-Graph Overview

An **e-graph** (equality graph) compactly represents many equivalent expressions:

```
┌─────────────────────────────────────────────────────────┐
│  E-Graph for: (+ (* a b) (* a c)) ≡ (* a (+ b c))      │
├─────────────────────────────────────────────────────────┤
│                                                         │
│   E-Class 0: {a}           ← leaf                       │
│   E-Class 1: {b}           ← leaf                       │
│   E-Class 2: {c}           ← leaf                       │
│   E-Class 3: {(* e0 e1), ...}   ← a*b                  │
│   E-Class 4: {(* e0 e2), ...}   ← a*c                  │
│   E-Class 5: {(+ e1 e2)}        ← b+c                  │
│   E-Class 6: {(+ e3 e4), (* e0 e5)}  ← BOTH forms!    │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

**Key insight**: E-class 6 contains *both* `(+ (* a b) (* a c))` and `(* a (+ b c))` because they've been proven equivalent by rewrite rules.

### 7b.3 Module Organization

The e-graph system consists of eight modules:

| Module | Purpose | Lines | Tests |
|--------|---------|-------|-------|
| `union-find.ss` | Disjoint set with path compression | 180 | 15 |
| `eclass.ss` | E-node and e-class representation | 240 | 26 |
| `egraph.ss` | Core e-graph with hashconsing | 380 | 22 |
| `match.ss` | Pattern matching for rewrite rules | 370 | 35 |
| `saturation.ss` | Equality saturation loop | 300 | 23 |
| `cost.ss` | CUDA/CPU/size cost models | 400 | 28 |
| `extract.ss` | Cost-based extraction | 220 | 26 |
| `scheduler.ss` | Rule scheduling with backoff | 350 | 25 |
| **Total** | | ~2400 | 200 |

This chapter covers the first three modules (union-find, e-classes, e-graph core). Pattern matching and saturation are covered in Chapter 7c.

### 7b.4 Union-Find

The foundation is a **union-find** (disjoint set) data structure with:
- **Path compression**: Flattens trees during `find` for O(α(n)) amortized lookup
- **Union by rank**: Keeps trees shallow during `union`
- **Root enumeration**: Efficiently iterate over all equivalence classes

```scheme
(define uf (make-uf))
(uf-make-set! uf 0)
(uf-make-set! uf 1)
(uf-union! uf 0 1)        ; Merge sets
(uf-same-set? uf 0 1)     ; => #t
```

**Implementation details**:

The union-find structure maintains two vectors:
- `parent`: Maps each ID to its parent (or itself if root)
- `rank`: Approximates tree height for union-by-rank

**Path compression** in `uf-find!`:

```scheme
(define (uf-find! uf id)
  (let ([p (vector-ref parent id)])
    (if (= p id)
        id
        (let ([root (uf-find! uf p)])
          (vector-set! parent id root)  ; Compress path
          root))))
```

**Union by rank** in `uf-union!`:

```scheme
(define (uf-union! uf id1 id2)
  (let ([r1 (uf-find! uf id1)]
        [r2 (uf-find! uf id2)])
    (unless (= r1 r2)
      (let ([rank1 (vector-ref rank r1)]
            [rank2 (vector-ref rank r2)])
        (cond
          [(< rank1 rank2)
           (vector-set! parent r1 r2)]
          [(> rank1 rank2)
           (vector-set! parent r2 r1)]
          [else
           (vector-set! parent r2 r1)
           (vector-set! rank r1 (+ rank1 1))])))))
```

**Root enumeration**:

```scheme
(define (uf-roots uf)
  (filter (lambda (id) (= id (vector-ref parent id)))
          (iota (uf-size uf))))
```

### 7b.5 E-Nodes and E-Classes

An **e-node** represents a term constructor applied to e-class children:

```scheme
(make-enode '+                     ; operator
            (vector class-a class-b))  ; children (e-class IDs)
```

An **e-class** is a set of equivalent e-nodes. The e-class store provides O(1) lookup and supports incremental updates during saturation.

**E-node structure**:

```scheme
(define-record-type enode
  (fields op         ; Symbol or literal
          children   ; Vector of e-class IDs
          data))     ; Optional metadata (cost, source location)
```

**E-class store**:

The e-class store maintains:
- **Nodes**: List of e-nodes in each e-class
- **Parents**: Set of e-classes that reference this class (for rebuild)
- **Data**: Optional analysis data (cost, value, etc.)

```scheme
(define-record-type eclass-store
  (fields nodes      ; Hashtable: class-id → list of enodes
          parents    ; Hashtable: class-id → set of parent enodes
          data))     ; Hashtable: class-id → analysis data
```

**Operations**:

```scheme
(eclass-add-node! store class-id node)
(eclass-get-nodes store class-id)
(eclass-merge! store id1 id2)  ; Merge classes (union lists)
```

### 7b.6 E-Graph Core Operations

The e-graph supports three key operations:

1. **Add**: Insert a term, returning its e-class ID
2. **Merge**: Mark two e-classes as equivalent
3. **Rebuild**: Restore invariants after merging (hashcons repair)

```scheme
(define eg (make-egraph))
(define id-a (egraph-add-term! eg 'a))
(define id-expr (egraph-add-term! eg '(+ (* a b) (* a c))))
(egraph-merge! eg id-a id-another)
(egraph-rebuild! eg)  ; Restore hashcons invariants
```

#### 7b.6.1 Add Operation

`egraph-add-term!` recursively adds a term and its subterms:

```scheme
(define (egraph-add-term! eg term)
  (cond
    [(symbol? term)
     ;; Leaf: create singleton e-class if needed
     (or (hashtable-ref memo term #f)
         (let ([id (new-class!)])
           (eclass-add-node! store id (make-enode term '#()))
           (hashtable-set! memo term id)
           id))]
    [(pair? term)
     ;; Constructor: add children first, then hashcons
     (let* ([op (car term)]
            [children (map (lambda (child) (egraph-add-term! eg child))
                           (cdr term))]
            [node (make-enode op (list->vector children))])
       (hashcons! eg node))]))
```

**Hashconsing**: The e-graph maintains a hashcons table mapping e-nodes (canonicalized by operator and canonical child IDs) to e-class IDs. This ensures structural sharing:

```scheme
(define (hashcons! eg node)
  (let ([canonical (canonicalize-node eg node)])
    (or (hashtable-ref hashcons canonical #f)
        (let ([id (new-class!)])
          (eclass-add-node! store id node)
          (hashtable-set! hashcons canonical id)
          id))))
```

#### 7b.6.2 Merge Operation

`egraph-merge!` marks two e-classes as equivalent using the union-find:

```scheme
(define (egraph-merge! eg id1 id2)
  (let ([c1 (egraph-find eg id1)]
        [c2 (egraph-find eg id2)])
    (unless (= c1 c2)
      (uf-union! (egraph-uf eg) c1 c2)
      (mark-dirty! eg c1)
      (mark-dirty! eg c2))))
```

After merging, the e-graph is temporarily in an inconsistent state: some e-nodes may be duplicates (same operator, same canonical children). **Rebuild** fixes this.

#### 7b.6.3 Rebuild Operation

**Rebuild** is critical: after merging, some e-nodes may become duplicates. Rebuild detects and merges these, propagating equivalences until fixpoint.

```scheme
(define (egraph-rebuild! eg)
  (let loop ()
    (let ([dirty (get-dirty-classes eg)])
      (unless (null? dirty)
        (for-each (lambda (class-id)
                    (rehash-class! eg class-id))
                  dirty)
        (loop)))))  ; Iterate until no dirty classes remain
```

**Rehashing a class**:

For each e-node in a dirty class:
1. Canonicalize its children (via `uf-find!`)
2. Look up the canonicalized e-node in the hashcons table
3. If found in a different class, merge those classes (may create more dirty classes)
4. If not found, update the hashcons table

```scheme
(define (rehash-class! eg class-id)
  (let ([root (egraph-find eg class-id)])
    (for-each (lambda (node)
                (let* ([canon (canonicalize-node eg node)]
                       [existing (hashtable-ref hashcons canon #f)])
                  (cond
                    [(not existing)
                     (hashtable-set! hashcons canon root)]
                    [(not (= existing root))
                     (egraph-merge! eg existing root)])))
              (eclass-get-nodes store root))))
```

### 7b.7 Invariants

The e-graph maintains two key invariants:

1. **Canonical representatives**: All e-class IDs are canonical (via `uf-find!` before use)
2. **Hashcons uniqueness**: Each canonicalized e-node appears in exactly one e-class

After `egraph-merge!`, invariant #2 may be temporarily violated. `egraph-rebuild!` restores it.

### 7b.8 Thread Safety

The e-graph implementation is **not thread-safe**. Concurrent access to the same e-graph from multiple threads would cause data races on:

| Component | Mutable State |
|-----------|---------------|
| Union-find | `parent` and `rank` vectors |
| E-class store | Node lists, parent sets |
| Hashcons table | E-node → e-class mappings |
| Dirty worklist | Set of classes needing rebuild |
| Statistics | Counter updates |

For concurrent use cases:

1. **Separate e-graphs per thread**: Each thread operates on its own e-graph instance (preferred for embarrassingly parallel workloads)
2. **External synchronization**: Wrap e-graph operations in mutexes (simple but coarse-grained)
3. **Functional/persistent design**: A future enhancement could use persistent data structures for lock-free concurrent reads

The current design prioritizes single-threaded performance—mutation enables O(1) hashcons updates and O(α(n)) union-find operations. For CUDA codegen, where e-graphs optimize individual kernels, single-threaded operation is sufficient.

### 7b.9 Performance Characteristics

| Operation | Complexity |
|-----------|------------|
| `egraph-add-term!` | O(term size) |
| `egraph-merge!` | O(α(n)) amortized |
| `egraph-rebuild!` | O(dirty nodes) |

**Space complexity**: O(nodes + classes + hashcons entries)

**Fuel integration**: The e-graph respects the fuel system—each operation consumes fuel proportional to work done. This ensures bounded execution even with unbounded rewrite rules.

### 7b.10 Design Decisions

**Why in-house e-graph?**

Existing e-graph libraries (egg, egglog) are Rust-based and would violate The Fold's no-external-dependencies principle. Our implementation:
- Integrates with the CAS (e-nodes can reference block hashes)
- Uses the fuel system for bounded execution
- Is fully introspectable and debuggable

**Why separate e-classes and union-find?**

The union-find provides fast equivalence queries (O(α(n))), while the e-class store maintains the actual e-nodes. This separation enables:
- Efficient merge without copying node lists
- Fast canonical representative lookup
- Clean abstraction boundaries

---
