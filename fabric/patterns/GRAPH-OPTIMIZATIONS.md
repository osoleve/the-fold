# Graph Algorithm Optimizations

## Performance Improvements

### Optimization 1: Eliminate Redundant Store Lookups in Traversal

**Problem**: Both `bfs-traverse` and `dfs-traverse` were making redundant `store-get` calls:
1. First `store-get` to retrieve the block
2. Second `store-get` inside `get-outgoing-hashes` for the same hash

**Solution**: Use `block-refs` directly from the already-fetched block instead of calling `get-outgoing-hashes`.

**Code Change**:
```scheme
;; Before:
(let ([block (store-get fs current)])
  (when block
    (visit-fn current block)
    (let* ([neighbors (get-outgoing-hashes fs current)] ; Redundant store-get!
           ...)))

;; After:
(let ([block (store-get fs current)])
  (when block
    (visit-fn current block)
    (let* ([neighbors (vector->list (block-refs block))] ; Direct access
           ...)))
```

**Impact** (Chain Graph, 100 nodes):
- **BFS**: 24μs → 16μs (33% faster, 1.5x speedup)
- **DFS**: 33μs → 16μs (51% faster, 2.06x speedup)

**Affected Functions**:
- `bfs-traverse`
- `dfs-traverse`

## Benchmark Results

### Before Optimizations
```
Chain-100 - BFS: Mean 24μs, Median 21μs
Chain-100 - DFS: Mean 33μs, Median 32μs (1.38x slower than BFS)
```

### After Optimizations
```
Chain-100 - BFS: Mean 16μs, Median 16μs
Chain-100 - DFS: Mean 16μs, Median 15μs (same performance as BFS)
```

## Analysis

### Why DFS Improved More

The DFS optimization showed greater improvement (51% vs 33%) because:
1. DFS makes more repeated visits to the store during deep recursion
2. Eliminating store lookups in a deep call stack compounds the savings
3. Stack operations are already O(1), so the store lookup was the bottleneck

### Expected Scaling

For larger graphs, the improvement should be even more pronounced because:
- The number of store lookups eliminated scales linearly with nodes visited
- Memory allocation is reduced (fewer intermediate data structures)
- Cache locality improves (block is already in memory)

## Future Optimization Opportunities

1. **Memoization**: Cache results of expensive graph analysis operations
   - `connected-components` could cache component membership
   - `topological-sort` could cache sort results

2. **Lazy Evaluation**: Don't compute full result sets when partial results suffice
   - `path-exists?` already does this (stops at first path)
   - Could apply to other search operations

3. **Parallel Traversal**: Independent graph components could be processed concurrently
   - Requires `par` and `pseq` primitives (tracked in the-fold-qe9r)

4. **Better Data Structures**:
   - ✅ Already using hash tables for visited sets (O(1) lookup)
   - ✅ Already using Okasaki queues for BFS (amortized O(1))
   - Could use finger trees for more complex operations

## Testing

All optimizations verified with:
- Unit tests in `fabric/patterns/test-graph-algorithms.ss`
- Benchmarks in `fabric/patterns/bench-graph-algorithms.ss`
- No regressions in functionality
