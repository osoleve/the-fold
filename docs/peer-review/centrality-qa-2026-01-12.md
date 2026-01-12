# Centrality Module QA Review

**Date:** 2026-01-12
**Reviewer:** Gemini 3 Pro
**Module:** `lattice/data/centrality.ss`

## Summary

Review of matrix-based graph centrality measures implementation.

## Findings

### 1. Mathematical Correctness & Algorithms

#### Betweenness Centrality (Weighted vs. Unweighted) — HIGH
- **Issue:** Documentation states the function handles weighted graphs with complexity O(nm + n² log n) typical for Dijkstra. However, implementation uses **BFS** which treats all edge weights as 1 (unweighted), only checking `> 0`.
- **Impact:** Running on weighted graph yields incorrect "hop-based" betweenness instead of "weight-based".
- **Recommendation:** Either implement Dijkstra or update docs to specify "Unweighted Only".

#### Betweenness Normalization — MEDIUM
- **Issue:** Normalization factor is `(* 2.0 (- n 1) (- n 2))`. Brandes' algorithm on undirected graphs produces raw sums up to (n-1)(n-2). Current code divides by 2(n-1)(n-2), resulting in scores in [0, 0.5] instead of [0, 1].
- **Note:** Depends on specific definition, but standard is [0, 1].

#### Closeness Centrality (Disconnected Graphs) — HIGH
- **Issue:** `standard-closeness-node` calculates score as (N-1) / Σdist. For disconnected graphs, it sums distances only for *reachable* nodes but normalizes by total graph size (N-1).
- **Result:** Nodes in small, isolated components with short internal distances receive artificially inflated scores.
- **Recommendation:** Prefer harmonic centrality for disconnected graphs, or normalize by component size ratio.

#### Oscillation Detection — CORRECT
- **Status:** The `eigenvector-average-oscillation` correctly handles period-2 oscillation (common in bipartite graphs) by averaging states k and k+1 when v_{k+1} ≈ v_{k-1}.

### 2. Performance Concerns

#### Betweenness Queue Operations — HIGH (Critical)
- **Issue:** BFS loop uses `(set! rest-q (append rest-q (list w)))`. `append` is O(k) where k is list length. Inside a loop, this turns BFS queue management from O(N) to O(N²).
- **Impact:** Performance degrades significantly on large graphs.
- **Fix:** Use proper queue data structure or `cons` to list and `reverse` for next level.

#### Closeness Complexity — MEDIUM
- **Issue:** `closeness-centrality-from-adj` calls `(floyd-warshall adj)`, forcing O(N³) complexity.
- **Impact:** For sparse graphs, N×BFS would be O(N(N+M)), much faster than O(N³).

### 3. Missing Error Handling

- Algorithms assume non-negative weights (implied by centrality definitions)
- No checks for negative weights in `katz` or `eigenvector`
- `betweenness` checks `> 0` for edges, treating negative weights as no edge

## Action Items

1. **Refactor `betweenness-centrality`**:
   - Replace `append` with efficient queue operations
   - Update docs to specify "Unweighted Only" (or implement Dijkstra)
   - Consider fixing normalization factor for [0,1] range

2. **Document closeness behavior**: Add note about harmonic variant for disconnected graphs (already implemented)

3. **Optional optimization**: Use N×BFS for sparse unweighted graphs instead of Floyd-Warshall
