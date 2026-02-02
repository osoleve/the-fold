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

### 10.5 Search Quality

The lattice search uses BM25 with term coverage boosting for multi-word queries. We evaluated precision on representative queries:

**Precision@10 for Sample Queries**:

| Query | Expected | Found in Top 10 | Notes |
|-------|----------|-----------------|-------|
| "identity matrix" | identity-matrix | ✓ | Exact match |
| "random float" | random-float | ✓ | Exact match |
| "merge sort" | data skill | ✓ | Skill keyword match |
| "graph shortest path" | data skill, dijkstra-path | ✓ | Multi-term coverage boost |
| "matrix vector" | linalg skill | ✓ | Both terms in description |

**Term Coverage Boost Effect**:

Multi-word queries use a `(matched/total)²` penalty for partial matches:

| Query | Result Type | Terms Matched | Score Multiplier |
|-------|-------------|---------------|------------------|
| "merge sort" | data skill | 2/2 | 1.0x (full match) |
| "merge sort" | dict-merge | 1/2 | 0.25x (partial) |
| "merge sort" | oquery-sort-by | 1/2 | 0.25x (partial) |

This ensures results matching all query terms rank significantly higher than partial matches.

**Limitations**:

1. **Single-character tokens dropped**: Function names like `vec+` don't match "vec add" because `+` is filtered by tokenization
2. **No phrase matching**: "merge sort" matches terms independently, not as a phrase
3. **No typo tolerance**: Queries must match indexed terms exactly

For a ~3,300 export index, these limitations are acceptable. Prefix search (`lfp`) and substring search (`lfs`) provide alternatives when exact BM25 matching fails.

### 10.6 CAS Performance Under Load

We benchmarked CAS operations to verify O(1) lookup performance regardless of store size.

**Fetch Latency vs Store Size**:

| Store Size | Fetch Time (mean) | Notes |
|------------|-------------------|-------|
| 10 blocks | 89 ns | Baseline |
| 100 blocks | 88 ns | No degradation |
| 1,000 blocks | 87 ns | Consistent |
| 10,000 blocks | 88 ns | O(1) confirmed |

The CAS achieves constant-time lookups through hash-indexed storage. Fetch time is dominated by memory access, not search.

**Operation Latencies** (1000 iterations):

| Operation | Mean | P99 | Notes |
|-----------|------|-----|-------|
| `fetch` (small block) | 89 ns | 128 ns | Hot path |
| `store!` (small block) | 2 μs | 2 μs | Includes hash computation |
| `stored?` (hit) | 90 ns | 128 ns | Hash table lookup |
| `stored?` (miss) | 85 ns | 112 ns | Early exit on miss |
| Serialize small block | 149 ns | 208 ns | Tag + payload encoding |
| Hash small block | 2 μs | 2 μs | SHA-256 dominates |

**Block Size Impact**:

| Block Size | Serialize | Hash | Store |
|------------|-----------|------|-------|
| Small (5B) | 149 ns | 2 μs | 2 μs |
| Medium (1KB) | 158 ns | 3 μs | 4 μs |
| Large (10KB) | 561 ns | 20 μs | 25 μs |

Hash computation dominates for larger blocks, but remains practical for typical code blocks (< 500 bytes).

### 10.7 Fuel Consumption Model

The fuel system assigns costs to primitive operations to enable termination guarantees and resource budgeting.

**Primitive Cost Tiers**:

| Tier | Cost | Operations | Rationale |
|------|------|------------|-----------|
| 1 | 1 | Type predicates, `car`, `cdr` | Pure inspection |
| 2 | 2 | Arithmetic, `cons`, indexing | Allocation or computation |
| 3 | 3 | Division, bitwise, string compare | Multi-cycle operations |
| 4 | 5 | `length`, `reverse`, conversions | Linear traversal |
| 5 | 10 | String operations, slicing | Allocation + traversal |
| 6 | 15 | `number->string` | Complex formatting |
| 7 | 100 | `sha256` | Cryptographic hash |
| 8 | 110 | `hash-block` | Hash + serialization |

**Predicted vs Actual Cost Correlation**:

For common operations on list of length n:

| Operation | Predicted O(...) | Actual Growth | Match |
|-----------|------------------|---------------|-------|
| `(length lst)` | O(n) | ~5n fuel | ✓ |
| `(reverse lst)` | O(n) | ~5n fuel | ✓ |
| `(append a b)` | O(len a) | ~5·len(a) fuel | ✓ |
| `(map f lst)` | O(n·f) | ~n·(cost f + 7) fuel | ✓ |
| `(fold-left f i lst)` | O(n·f) | ~n·(cost f + 5) fuel | ✓ |

The fuel model accurately predicts asymptotic behavior. Constant factors vary by ~10-15% depending on intermediate allocations.

**Fuel Budget Validation**:

We compared declared fuel bounds against actual consumption for lattice functions:

| Function | Declared | Actual (n=100) | Actual (n=1000) | Ratio |
|----------|----------|----------------|-----------------|-------|
| `vec+` | O(n) | 520 | 5,200 | 1.0x |
| `dot` | O(n) | 710 | 7,100 | 1.0x |
| `mat-mul` (10×10) | O(n³) | 8,200 | — | ✓ |
| `qr-decompose` (10×10) | O(n³) | 12,400 | — | ✓ |

Declared bounds are conservative upper bounds; actual consumption is typically 0.8–1.0x of declared.

---
