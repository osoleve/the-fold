# Graph Algorithms Library Guide

The Fold's graph algorithms library provides comprehensive tools for analyzing directed graphs, including traversal, shortest paths, centrality measures, and community detection.

## Quick Start

```scheme
;; For adjacency matrix-based algorithms
(load "lattice/data/graph-matrix.ss")
(load "lattice/data/centrality.ss")

;; Create a simple graph (adjacency matrix)
(define g (graph-from-edges 5 '((0 1) (0 2) (1 2) (2 3) (3 4))))

;; Find shortest paths
(dijkstra g 0)        ; Single-source shortest paths from node 0
(floyd-warshall g)    ; All-pairs shortest paths

;; Compute centrality
(eigenvector-centrality g)
(betweenness-centrality g)
```

---

## 1. Graph Representations

The library supports two main representations:

### Adjacency Matrix (graph-matrix.ss)

Best for dense graphs and algorithms requiring all-pairs operations.

```scheme
(load "lattice/data/graph-matrix.ss")

;; Create from edge list
(define g (graph-from-edges 4 '((0 1) (1 2) (2 3) (3 0))))

;; Create weighted graph
(define gw (graph-from-weighted-edges 4 '((0 1 2.5) (1 2 1.0) (2 3 3.0))))

;; Standard test graphs
(define k5 (complete-graph 5))      ; Complete graph K5
(define star (star-graph 5))        ; Star graph (center = 0)
(define path (path-graph 5))        ; Path 0-1-2-3-4
(define cycle (cycle-graph 5))      ; Cycle 0-1-2-3-4-0
```

### Hash-Based Graphs (graph-primitives.ss)

For The Fold's content-addressed block graphs.

```scheme
(load "lattice/data/graph/graph-primitives.ss")

;; Works with fs-capability and block hashes
(define fs (make-fs-capability ".store"))
(dfs-traverse fs start-hash visit-fn)
(shortest-path fs from-hash to-hash)
```

---

## 2. Graph Traversal

### Breadth-First Search (BFS)

Explores nodes level by level. Good for finding shortest paths in unweighted graphs.

```scheme
;; BFS traversal with visitor function
(bfs-traverse fs start-hash
              (lambda (hash block)
                (printf "Visiting: ~a~n" (hash->string hash))))

;; BFS in reverse (following incoming edges)
(bfs-traverse-reverse fs start-hash visit-fn)
```

### Depth-First Search (DFS)

Explores as deep as possible before backtracking. Good for detecting cycles, topological sorting.

```scheme
;; DFS traversal
(dfs-traverse fs start-hash visit-fn)
```

---

## 3. Shortest Paths

### Single-Source: Dijkstra's Algorithm

O((V+E) log V) using priority queue. For non-negative edge weights.

```scheme
(load "lattice/data/graph-matrix.ss")

(define g (graph-from-weighted-edges 5
           '((0 1 4) (0 2 1) (1 3 1) (2 1 2) (2 3 5) (3 4 3))))

;; Shortest paths from node 0
(dijkstra g 0)
;; Returns: #(0 3 1 4 7) - distances to each node
```

### All-Pairs: Floyd-Warshall

O(V³). Computes shortest paths between all pairs of nodes.

```scheme
(define dist-matrix (floyd-warshall g))

;; Distance from node 1 to node 4
(matrix-ref dist-matrix 1 4)
```

### Hash-Based Shortest Path

For block reference graphs:

```scheme
;; Find shortest path between two hashes
(shortest-path fs from-hash to-hash)
;; Returns: list of hashes forming the path, or #f if no path

;; Check if path exists
(path-exists? fs from-hash to-hash)  ; => #t or #f

;; Find all paths up to length k
(all-paths fs from-hash to-hash max-length)
```

---

## 4. Graph Properties

### Degree Analysis

```scheme
;; For hash-based graphs
(in-degree fs hash)      ; Number of incoming edges
(out-degree fs hash)     ; Number of outgoing edges
(total-degree fs hash)   ; in + out

;; Find nodes with highest degree
(find-hubs fs k)         ; Top k nodes by degree
```

### Reachability

```scheme
;; All nodes reachable from a starting node
(reachable-from fs start-hash)

;; All ancestors (nodes that can reach target)
(ancestors-of fs target-hash)

;; k-hop neighborhood
(neighborhood fs center-hash k)
```

### Connected Components

```scheme
;; Find strongly connected components
(strongly-connected-components g)

;; Find weakly connected components
(weakly-connected-components g)
```

### Roots and Leaves

```scheme
(find-roots fs)   ; Nodes with no incoming edges
(find-leaves fs)  ; Nodes with no outgoing edges
```

---

## 5. Centrality Measures

Centrality measures identify important nodes in a graph.

```scheme
(load "lattice/data/centrality.ss")
```

### Eigenvector Centrality

Node importance based on connections to other important nodes.

```scheme
(eigenvector-centrality g)
;; Returns: vector of scores, higher = more central

;; With custom parameters
(eigenvector-centrality g max-iterations tolerance)
```

**Interpretation:** High score means connected to other high-scoring nodes. Good for finding influential nodes in social networks.

### Katz Centrality

Like eigenvector centrality but with damping factor.

```scheme
(katz-centrality g)
(katz-centrality g alpha)  ; alpha = damping (default 0.1)
```

**Interpretation:** Counts all paths to a node, with longer paths weighted less. More robust than eigenvector for disconnected graphs.

### Closeness Centrality

Based on average distance to all other nodes.

```scheme
;; Requires distance matrix from Floyd-Warshall
(define dist (floyd-warshall g))
(closeness-centrality dist)
```

**Interpretation:** High closeness = can reach all other nodes quickly. Good for finding optimal "broadcast" locations.

### Betweenness Centrality

Fraction of shortest paths passing through a node.

```scheme
(betweenness-centrality g)
```

**Interpretation:** High betweenness = lies on many shortest paths (gatekeeper). Good for finding critical infrastructure.

### Comparing Centralities

```scheme
;; Rank nodes by centrality score
(rank-by-centrality (eigenvector-centrality g))

;; Get top k most central nodes
(top-k-central (katz-centrality g) 5)

;; Correlation between measures
(centrality-correlation eig-scores katz-scores)

;; Compute all four centralities at once
(all-centralities g)
```

---

## 6. Community Detection

Finding groups of densely connected nodes.

```scheme
(load "lattice/data/graph-community.ss")
```

### Label Propagation

Fast, iterative algorithm for community detection.

```scheme
(label-propagation g)
;; Returns: vector where v[i] = community label for node i
```

### Modularity

Quality measure for community assignments.

```scheme
(modularity g communities)
;; Returns: number in [-0.5, 1], higher = better communities
```

### Modularity Maximization (ILP)

Optimal community detection via integer linear programming.

```scheme
(modularity-ilp g k)  ; Find exactly k communities
```

---

## 7. Minimum Spanning Trees

For weighted undirected graphs:

```scheme
(load "lattice/data/graph-community.ss")

;; Prim's algorithm - O((V+E) log V)
(prim-mst g)

;; Kruskal's algorithm - O(E log E)
(kruskal-mst g)
```

Both return a list of edges in the MST.

---

## 8. Cycle Detection

```scheme
;; Find all cycles containing a specific node
(find-cycles fs hash max-length)

;; Detect if graph has any cycles
(has-cycle? g)

;; Topological sort (only works for DAGs)
(topological-sort g)
```

---

## 9. Graph Statistics

```scheme
;; Summary statistics for hash-based graph
(graph-stats fs)
;; Returns: ((nodes . N) (edges . M) (roots . R) (leaves . L) ...)
```

---

## 10. Worked Examples

### Example: Social Network Analysis

```scheme
;; Create a small social network
(define social (graph-from-edges 6
  '((0 1) (0 2) (1 2) (1 3) (2 3) (3 4) (3 5) (4 5))))

;; Find the most influential person
(define eig (eigenvector-centrality social))
(rank-by-centrality eig)
;; Node 3 likely ranks highest (central connector)

;; Find communities
(define communities (label-propagation social))
;; Might find {0,1,2} and {3,4,5} as separate groups

;; Who bridges communities?
(betweenness-centrality social)
;; Node 3 has highest betweenness (connects the two groups)
```

### Example: Finding Critical Infrastructure

```scheme
;; Network of servers
(define network (graph-from-edges 8
  '((0 1) (1 2) (2 3) (3 4) (4 5) (5 6) (6 7)  ; Main chain
    (0 7)                                       ; Shortcut
    (2 5))))                                    ; Another shortcut

;; Find bottleneck nodes
(define btw (betweenness-centrality network))
(top-k-central btw 3)
;; Identifies nodes whose failure would most disrupt paths
```

### Example: PageRank-style Importance

```scheme
(load "lattice/data/pagerank.ss")

;; Web-like link structure
(define web (graph-from-edges 5
  '((0 1) (0 2) (1 2) (2 0) (2 3) (3 4) (4 2))))

;; Compute PageRank
(pagerank web)
;; Node 2 likely has highest rank (most incoming links, including from important nodes)
```

---

## 11. Performance Characteristics

| Algorithm | Time Complexity | Space | Notes |
|-----------|----------------|-------|-------|
| BFS/DFS | O(V + E) | O(V) | |
| Dijkstra | O((V+E) log V) | O(V) | Non-negative weights |
| Floyd-Warshall | O(V³) | O(V²) | All-pairs |
| Eigenvector centrality | O(k × E) | O(V) | k = iterations |
| Betweenness | O(V × E) | O(V) | |
| Prim MST | O((V+E) log V) | O(V) | |
| Kruskal MST | O(E log E) | O(V) | |
| Label propagation | O(k × E) | O(V) | k = iterations |
| SCC (Tarjan) | O(V + E) | O(V) | |

---

## 12. Module Reference

| Module | Contents |
|--------|----------|
| `graph-algorithms.ss` | BFS, DFS, shortest paths, reachability, cycles (hash-based) |
| `graph-matrix.ss` | Adjacency matrix, Dijkstra, Floyd-Warshall |
| `centrality.ss` | Eigenvector, Katz, closeness, betweenness centrality |
| `graph-community.ss` | Label propagation, modularity, Prim/Kruskal MST |
| `pagerank.ss` | PageRank importance scoring |
| `graph-layout.ss` | Force-directed layout for visualization |
| `graph-render.ss` | ASCII graph rendering |

---

## Tips

1. **Choose representation wisely**: Adjacency matrix for dense graphs (> 10% edges), hash-based for sparse.

2. **Centrality measures complement each other**:
   - Eigenvector: "who knows important people"
   - Betweenness: "who controls information flow"
   - Closeness: "who can reach everyone quickly"

3. **For large graphs**: Use iterative algorithms (label propagation, power iteration) rather than exact methods.

4. **Verify results**: Use `modularity` to check if detected communities are meaningful.
