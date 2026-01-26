## 7b. Equality Saturation and E-Graph Optimization

The Fold includes a complete **equality saturation** system for finding optimal equivalent program forms. This capability is essential for CUDA codegen optimization, where algebraically equivalent expressions can have dramatically different GPU performance characteristics.

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

### 7b.2 E-Graph Data Structure

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

### 7b.3 Core Components

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

#### 7b.3.1 Union-Find

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

#### 7b.3.2 E-Nodes and E-Classes

An **e-node** represents a term constructor applied to e-class children:

```scheme
(make-enode '+                     ; operator
            (vector class-a class-b))  ; children (e-class IDs)
```

An **e-class** is a set of equivalent e-nodes. The e-class store provides O(1) lookup and supports incremental updates during saturation.

#### 7b.3.3 E-Graph Operations

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

**Rebuild** is critical: after merging, some e-nodes may become duplicates (same operator, same canonicalized children). Rebuild detects and merges these, propagating equivalences until fixpoint.

### 7b.4 Pattern Matching and Rewrite Rules

Patterns use variables prefixed with `?`:

```scheme
;; Pattern: (+ ?x 0)  matches any addition with 0
;; ?x binds to the e-class ID of the first child

(define identity-rule (make-rule '(+ ?x 0) '?x))
```

**Key insight**: In an e-graph, a pattern can match *multiple ways* because each e-class may contain multiple e-nodes. The matcher returns *all* valid substitutions.

```scheme
(ematch eg '(+ ?x ?y) class-id)
;; Returns: list of substitutions like ((?x . 3) (?y . 4))
```

### 7b.5 Equality Saturation

The saturation loop repeatedly applies rules until no new equivalences are found:

```scheme
(define (saturate eg rules config)
  (let loop ([iteration 0] [applied 0])
    (let ([new-applied (saturate-iteration eg rules)])
      (egraph-rebuild! eg)
      (cond
        [(zero? new-applied) 'saturated]      ; Fixpoint reached
        [(> applied fuel) 'fuel-exhausted]    ; Resource limit
        [else (loop (+ iteration 1) (+ applied new-applied))]))))
```

**Resource limits** prevent unbounded growth:
- **Fuel**: Maximum total rule applications
- **Node limit**: Maximum e-graph size
- **Iteration limit**: Maximum applications per iteration

**Predefined rule sets**:

```scheme
arith-identity-rules   ; (+ x 0)→x, (* x 1)→x, (* x 0)→0
arith-comm-rules       ; (+ x y)→(+ y x), (* x y)→(* y x)
arith-assoc-rules      ; ((+ x y) z)→(+ x (+ y z))
arith-distrib-rules    ; (* x (+ y z))→(+ (* x y) (* x z))
```

### 7b.6 Cost Models

Cost models assign numeric costs to e-nodes, enabling extraction of optimal forms.

#### 7b.6.1 CUDA Cost Model

The CUDA cost model optimizes for GPU execution:

| Operation | Cost | Rationale |
|-----------|------|-----------|
| `+`, `-` | 1 | Fast ALU operations |
| `*` | 2 | Slightly more expensive |
| `/`, `mod` | 10 | Division is costly on GPU |
| `sqrt` | 15 | Special function unit |
| `rsqrt` | 8 | Optimized on NVIDIA hardware |
| `fma` | 3 | Fused multiply-add (preferred) |
| `load`, `store` | 100 | Memory bandwidth dominates |

```scheme
(optimize '(/ 1 (sqrt x))          ; Original
          cuda-rewrite-rules       ; Try rsqrt equivalence
          cuda-cost)               ; CUDA cost model
;; => (rsqrt x)                    ; Faster on GPU
```

#### 7b.6.2 Cost Computation

Costs are computed via dynamic programming:
1. Initialize all classes with infinite cost
2. Iterate until fixpoint:
   - For each e-class, compute minimum cost across all e-nodes
   - E-node cost = base cost + sum of child costs

```scheme
(define (compute-costs eg cost-model)
  (let ([costs (make-hashtable)])
    ;; Initialize to infinity
    (for-each (lambda (root) (hashtable-set! costs root +inf.0))
              (uf-roots (egraph-uf eg)))
    ;; Iterate until fixpoint
    (let loop ([changed #t])
      (when changed
        (set! changed #f)
        (for-each (lambda (root)
                    (for-each (lambda (node)
                                (let ([c (node-cost node costs)])
                                  (when (< c (hashtable-ref costs root))
                                    (hashtable-set! costs root c)
                                    (set! changed #t))))
                              (class-nodes root)))
                  (uf-roots uf)))
        (loop changed)))
    costs))
```

### 7b.7 Extraction

Extraction recovers a concrete term from the e-graph by selecting the minimum-cost e-node from each e-class:

```scheme
(define (extract state class-id)
  (let ([best-node (hashtable-ref best-nodes class-id)])
    (if (leaf? best-node)
        (enode-op best-node)
        (cons (enode-op best-node)
              (map (lambda (child) (extract state child))
                   (enode-children best-node))))))
```

**High-level API**:

```scheme
(optimize term rules cost-model)
;; 1. Build e-graph from term
;; 2. Saturate with rules
;; 3. Extract minimum-cost equivalent
```

### 7b.8 Rule Scheduling

Naive saturation applies all rules to all classes every iteration—expensive for large e-graphs. **Scheduling strategies** improve performance:

#### 7b.8.1 Backoff Scheduler

Tracks per-rule statistics and reduces priority for unproductive rules:

```scheme
(define (update-stats! stats matches)
  (if (zero? matches)
      (begin
        (inc! zero-streak)
        (when (>= zero-streak threshold)
          (set! priority (* priority 0.5))))  ; Back off
      (begin
        (set! zero-streak 0)
        (set! priority (* priority 1.5)))))   ; Boost
```

#### 7b.8.2 Priority Scheduler

Sorts rules by recent productivity, applying most-likely-to-match rules first.

#### 7b.8.3 Worklist Scheduler

Only processes e-classes that changed in the previous iteration—essential for large e-graphs where most classes are stable.

### 7b.9 Integration with CUDA Codegen

The e-graph system integrates with CUDA code generation:

```scheme
;; 1. Parse CUDA kernel expression
(define expr (parse-cuda-kernel source))

;; 2. Optimize using e-graph
(define optimized
  (optimize expr
            (append cuda-algebraic-rules
                    cuda-intrinsic-rules)  ; rsqrt, fma, etc.
            cuda-cost))

;; 3. Generate CUDA code from optimized form
(define cuda-code (emit-cuda optimized))
```

**CUDA-specific rules** include:
- `(/ 1 (sqrt x))` → `(rsqrt x)` (fast reciprocal square root)
- `(+ (* a b) c)` → `(fma a b c)` (fused multiply-add)
- `(* x x)` → `(sq x)` (optimized squaring)

### 7b.10 Design Decisions

**Why in-house e-graph?**

Existing e-graph libraries (egg, egglog) are Rust-based and would violate The Fold's no-external-dependencies principle. Our implementation:
- Integrates with the CAS (e-nodes can reference block hashes)
- Uses the fuel system for bounded execution
- Is fully introspectable and debuggable

**Why separate cost models?**

Different targets need different optimizations:
- CUDA: Minimize memory bandwidth, prefer FMA
- CPU: Minimize division, exploit caching
- Code size: Minimize instruction count

The cost model abstraction enables target-specific optimization without changing the saturation logic.

**Why scheduling?**

Without scheduling, saturation is O(rules × classes × matches) per iteration. For large e-graphs:
- Backoff prevents wasting time on dormant rules
- Worklist avoids redundant work on stable classes
- Priority focuses on productive rules first

### 7b.11 Performance Characteristics

| Operation | Complexity |
|-----------|------------|
| `egraph-add-term!` | O(term size) |
| `egraph-merge!` | O(α(n)) amortized |
| `egraph-rebuild!` | O(dirty nodes) |
| `ematch` | O(pattern size × class nodes) |
| `saturate` | O(fuel) bounded |
| `compute-costs` | O(classes × nodes) |
| `extract` | O(term size) |

**Space complexity**: O(nodes + classes + hashcons entries)

The system handles e-graphs with thousands of nodes efficiently, sufficient for kernel-level optimization.

---
