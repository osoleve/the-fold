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

---
