# Graph Community Module QA Review

**Date:** 2026-01-12
**Reviewer:** Gemini 3 Pro
**Module:** `lattice/data/graph-community.ss`

## Summary

Review of community detection and MST algorithms implementation.

## Findings

### 1. Label Propagation and Modularity

#### Modularity Calculation — CORRECT
- **Status:** Mathematical implementation of Q metric is correct.

#### Tie-Breaking Behavior — MEDIUM
- **Issue:** Docstring claimed "random seed for tie-breaking", but implementation is deterministic when neighbor label counts are equal (favors lower index).
- **Fix Applied:** Updated docstring to clarify: "Seed controls node visit order; ties broken by lowest-index neighbor"

#### Complexity Mismatch — MEDIUM
- **Issue:** Docstring claimed O(km) complexity but implementation uses adjacency matrix with O(n²) neighbor iteration.
- **Fix Applied:** Updated complexity to O(k·n²) with note about adjacency list alternative.

### 2. MST Algorithms

#### Prim's Algorithm — CORRECT
- **Status:** Logic correct for finding MST of connected component.

#### Prim's Docstring Error — MEDIUM
- **Issue:** Docstring stated "Returns empty list if graph is disconnected" which is incorrect. It returns MST for the component containing the start node.
- **Fix Applied:** Updated to "For disconnected graphs, returns MST of the component containing start"

#### Kruskal's Algorithm — CORRECT
- **Status:** Properly sorts edges and uses union-find.

### 3. Union-Find Implementation — CORRECT

- **Path Compression:** Present in `uf-find`
- **Union by Rank:** Present in `uf-union`

### 4. Performance Issues

#### BFS Queue in connected-components — HIGH (Fixed)
- **Issue:** BFS used `(append rest-q new-nodes)` which is O(n), making overall BFS O(n²).
- **Additional Issue:** Nodes could be added to queue multiple times before being processed.
- **Fix Applied:** Refactored to level-by-level BFS with O(1) cons operations. Nodes now marked visited when queued (not when processed) to prevent duplicates.

## Action Items Completed

1. Fixed BFS performance in `connected-components` (level-by-level traversal)
2. Fixed Prim's docstring about disconnected graphs
3. Updated label-propagation complexity documentation
4. Clarified tie-breaking behavior in label-propagation

## Tests

All 54 tests passing after fixes.
