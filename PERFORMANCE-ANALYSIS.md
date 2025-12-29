# Performance Analysis - Core System Benchmarks

**Date:** 2025-12-29
**System:** The Fold - Content-Addressed Homoiconic Universe

## Executive Summary

Comprehensive benchmarking of core system components reveals that **SHA256 hashing is the dominant performance bottleneck** across all operations involving block storage. Fetch operations are 300-50000x faster than store operations due to this cryptographic overhead.

## Key Findings

### 1. Block Hashing is the Primary Bottleneck

**SHA256 Hashing Performance:**
- Small block: 59μs mean
- Medium block (1KB): 533μs mean
- Large block (10KB): 6ms mean

**Comparison with Serialization:**
- Hashing is **115x slower** than serialization for small blocks
- Hashing is **1037x slower** than serialization for medium blocks
- Hashing is **11945x slower** than serialization for large blocks

**Implication:** Any operation that stores blocks will be dominated by hashing cost.

### 2. CAS Operations Show Excellent O(1) Performance

**Store Operations (includes hashing):**
- Small: 56μs
- Medium (1KB): 1ms
- Large (10KB): 8ms

**Fetch Operations (hashtable lookup):**
- Small: 191ns
- Medium (1KB): 176ns
- Large (10KB): 176ns

**Scalability Test Results:**
| Store Size | Fetch Time |
|------------|------------|
| 10 blocks  | 200ns      |
| 100 blocks | 208ns      |
| 1000 blocks| 656ns      |
| 10000 blocks| 122ns     |

**Finding:** Fetch time remains essentially constant (O(1)) regardless of:
- Block size (176-191ns for any size)
- Store size (122-656ns for 10-10000 blocks)

This confirms the hashtable implementation is highly efficient.

### 3. Serialization is Very Fast

**Block Serialization:**
- Small: 515ns
- Medium (1KB): 585ns
- Large (10KB): 6μs

**Scaling:** Serialization scales reasonably well with size (~12x for 20x size increase).

### 4. Normalize/Expand Operations are Efficient

**Normalization (S-expr → Canonical):**
- Simple expr: 155ns
- Nested expr: 171ns
- Deep expr: 232ns

**Expansion (Canonical → S-expr):**
- Simple: 117ns
- Nested: 125ns
- Deep: 133ns

**Round-trip (Normalize + Expand):**
- Simple: 182ns
- Nested: 283ns

**Finding:** Expression transformation is very fast, even for deeply nested structures.

### 5. Serialization Round-trips are Efficient

**Serialize + Deserialize:**
- Small block: 654ns
- Medium block: 715ns

Only ~9% slower than a single serialization, indicating efficient deserialization.

## Performance Hierarchy (Fastest to Slowest)

1. **Normalize/Expand** - 100-300ns - 🟢 Excellent
2. **Fetch from CAS** - 170-200ns - 🟢 Excellent
3. **Block Serialization** - 500-6000ns - 🟢 Good
4. **Hash Small Blocks** - 50-60μs - 🟡 Moderate
5. **Store Small Blocks** - 50-60μs - 🟡 Moderate
6. **Hash Medium Blocks** - 500μs - 🟠 Slow
7. **Hash Large Blocks** - 6ms - 🔴 Very Slow

## Bottleneck Analysis

### Primary Bottleneck: SHA256

SHA256 hashing is **2-4 orders of magnitude slower** than other operations.

**Impact Areas:**
1. `store!` operations
2. `hash-block` calls
3. Content deduplication
4. Block verification

**Mitigation Strategies:**
1. ✅ **Lazy hashing** - Only hash when necessary
2. ✅ **Hash caching** - Cache computed hashes (already done via CAS)
3. ❌ **Faster hash** - Can't use weaker crypto (content addressing requires collision resistance)
4. ✅ **Batch operations** - Amortize hash cost across multiple operations
5. ✅ **Parallel hashing** - Hash large blocks in parallel (future: requires par/pseq)

### Secondary Bottleneck: Large Payload Serialization

For blocks >10KB, serialization starts to show linear scaling costs.

**Mitigation:**
- Stream-based serialization for very large payloads
- Chunk large data across multiple blocks

## Optimization Opportunities

### High-Impact (Implemented)
- ✅ **Eliminate redundant store lookups** in graph traversal (33-51% speedup achieved)
- ✅ **Efficient visited sets** using hash tables (O(1) vs O(n))
- ✅ **Okasaki queues** for BFS (amortized O(1))

### High-Impact (Not Yet Implemented)
- ⚠️ **Memoize hash-block results** for frequently accessed blocks
- ⚠️ **Parallel block hashing** for large blocks (split into chunks)
- ⚠️ **Incremental hashing** for blocks with similar content
- ⚠️ **Background hash pre-computation** for known blocks

### Medium-Impact
- Consider **streaming serialization** for blocks >100KB
- Implement **hash pooling** to reuse hash computation across similar blocks
- Add **compression** for large payloads before hashing

### Low-Impact
- Normalize/expand are already very fast - no optimization needed
- Fetch operations are already O(1) - no optimization needed

## Recommendations

### For Application Developers

1. **Minimize store! calls** - Batch creates when possible
2. **Reuse hashes** - Don't re-hash the same content
3. **Keep blocks small** - <1KB when possible
4. **Use fetch freely** - It's very fast (170ns)

### For System Developers

1. **Consider content-defined chunking** for large data
2. **Implement hash memoization** in hot paths
3. **Profile real workloads** to identify actual bottlenecks
4. **Investigate SIMD SHA256** implementations

## Benchmarking Methodology

**Hardware:** debian-8gb-ash-1 (first production server)
**Iterations:** 1000 per test
**Statistical Measures:** Mean, Median, Std Dev, Percentiles (P50, P90, P99)

**Test Data:**
- Small: 5 bytes
- Medium: 1KB
- Large: 10KB

**Expressions:**
- Simple: `(lambda (x) (+ x 1))`
- Nested: `(lambda (f) (lambda (x) (f (f x))))`
- Deep: 4-level nested lambdas

## Related Documents

- `fabric/patterns/GRAPH-OPTIMIZATIONS.md` - Graph algorithm optimizations
- `fabric/stitches/bench-prim.ss` - Primitive fuel cost benchmarks
- `fabric/stitches/bench-core.ss` - Core system benchmarks
- `fabric/patterns/bench-graph-algorithms.ss` - Graph algorithm benchmarks

## Future Work

1. **Memory profiling** - Understand allocation patterns
2. **GC impact analysis** - Measure GC overhead
3. **Concurrency benchmarks** - Test parallel operations
4. **Network I/O benchmarks** - Test remote fetch performance
5. **Disk I/O benchmarks** - Test persistent store performance
