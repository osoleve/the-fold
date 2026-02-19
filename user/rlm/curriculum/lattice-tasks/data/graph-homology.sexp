;;; graph-homology.sexp — Development tasks for lattice/data/graph/graph-homology.ss
;;;
;;; Module: graph-homology (tier 1, depends on prelude, homology)
;;; 150 lines, ~10 exported functions
;;; Algebraic topology applied to graphs: Betti numbers (connected components,
;;; independent cycles), Euler characteristic, tree/forest detection,
;;; cycle basis via Z_2 null space.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement graph-euler-characteristic
  ;; ──────────────────────────────────────────────
  ;; The simplest topological invariant of a graph.

  (task
    (id "data/graph-homology/euler-characteristic-impl")
    (module graph-homology)
    (tier 0)
    (type implement)
    (description "Implement graph-euler-characteristic: compute the Euler characteristic of a graph. For a graph, chi = V - E where V is the number of vertices and E is the number of edges. This is one of the oldest topological invariants (Euler 1736). For connected graphs, chi = 1 - beta_1 where beta_1 is the number of independent cycles. A tree always has chi = 1.")
    (context "Available: length, -. Two lengths and a subtraction.")
    (before "(define (graph-euler-characteristic edges vertices)
  ;; TODO: chi = V - E
  0)")
    (test-file "lattice/data/graph/test-graph-homology.ss")
    (test-forms (";; Tree: V=3, E=2, chi=1
(assert-equal 1 (graph-euler-characteristic '((1 . 2) (2 . 3)) '(1 2 3)))"
                 ";; Triangle: V=3, E=3, chi=0
(assert-equal 0 (graph-euler-characteristic '((1 . 2) (2 . 3) (1 . 3)) '(1 2 3)))"
                 ";; Single vertex: V=1, E=0, chi=1
(assert-equal 1 (graph-euler-characteristic '() '(1)))"
                 ";; Disconnected: V=4, E=1, chi=3
(assert-equal 3 (graph-euler-characteristic '((1 . 2)) '(1 2 3 4)))"))
    (available-modules (prelude homology))
    (hints ("V = (length vertices)"
            "E = (length edges)"
            "Return (- V E)"))
    (reference-solution "(define (graph-euler-characteristic edges vertices)
  (- (length vertices) (length edges)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement graph-betti-numbers
  ;; ──────────────────────────────────────────────
  ;; Connected components and independent cycles via homology.

  (task
    (id "data/graph-homology/betti-numbers-impl")
    (module graph-homology)
    (tier 0)
    (type implement)
    (description "Implement graph-betti-numbers: compute the Betti numbers of a graph using Z_2 simplicial homology. Beta_0 = number of connected components, beta_1 = number of independent cycles. Steps: (1) Convert the graph to a simplicial complex using graph->simplicial-complex. (2) If no edges, return (components . 0) where components = number of vertices. (3) Otherwise compute Betti numbers via sc-betti-numbers. (4) Extract beta_0 and beta_1 from the result, defaulting to 0 if missing. Return as (beta_0 . beta_1).")
    (context "Available: graph->simplicial-complex, sc-betti-numbers, null?, length, car, cdr, cadr, pair?, cons, if, and, let. Two function calls plus result extraction.")
    (before "(define (graph-betti-numbers edges vertices)
  ;; TODO: compute (beta_0 . beta_1) via simplicial homology
  (cons 0 0))")
    (test-file "lattice/data/graph/test-graph-homology.ss")
    (test-forms (";; Triangle: 1 component, 1 cycle
(assert-equal '(1 . 1) (graph-betti-numbers '((1 . 2) (2 . 3) (1 . 3)) '(1 2 3)))"
                 ";; Tree: 1 component, 0 cycles
(assert-equal '(1 . 0) (graph-betti-numbers '((1 . 2) (2 . 3)) '(1 2 3)))"
                 ";; Disconnected: 3 components, 0 cycles
(assert-equal '(3 . 0) (graph-betti-numbers '((1 . 2)) '(1 2 3 4)))"
                 ";; No edges: each vertex is its own component
(assert-equal '(3 . 0) (graph-betti-numbers '() '(1 2 3)))"))
    (available-modules (prelude homology))
    (hints ("Convert: (graph->simplicial-complex edges vertices)"
            "Edge case: (null? edges) → (cons (length vertices) 0)"
            "Compute: (sc-betti-numbers sc)"
            "Extract: beta_0 = (car betti), beta_1 = (cadr betti) with safety checks"))
    (reference-solution "(define (graph-betti-numbers edges vertices)
  (let ([sc (graph->simplicial-complex edges vertices)])
    (if (null? edges)
        (cons (length vertices) 0)
        (let ([betti (sc-betti-numbers sc)])
          (cons (if (pair? betti) (car betti) 0)
                (if (and (pair? betti) (pair? (cdr betti)))
                    (cadr betti)
                    0))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement graph-is-tree?
  ;; ──────────────────────────────────────────────
  ;; The topological definition of a tree.

  (task
    (id "data/graph-homology/is-tree-impl")
    (module graph-homology)
    (tier 0)
    (type implement)
    (description "Implement graph-is-tree?: determine if a graph is a tree using its Betti numbers. A graph is a tree if and only if it is connected (beta_0 = 1) AND acyclic (beta_1 = 0). This is the topological characterization — equivalent to the combinatorial definition (connected + V = E + 1) but derived from homology. Compute the Betti numbers via graph-betti-numbers, then check both conditions.")
    (context "Available: graph-betti-numbers, car, cdr, and, =. One function call plus two comparisons.")
    (before "(define (graph-is-tree? edges vertices)
  ;; TODO: connected (beta_0 = 1) and acyclic (beta_1 = 0)
  #f)")
    (test-file "lattice/data/graph/test-graph-homology.ss")
    (test-forms (";; Path graph is a tree
(assert-true (graph-is-tree? '((1 . 2) (2 . 3)) '(1 2 3)))"
                 ";; Triangle is NOT a tree (has cycle)
(assert-false (graph-is-tree? '((1 . 2) (2 . 3) (1 . 3)) '(1 2 3)))"
                 ";; Disconnected is NOT a tree
(assert-false (graph-is-tree? '((1 . 2)) '(1 2 3 4)))"
                 ";; Single vertex is a tree
(assert-true (graph-is-tree? '() '(1)))"))
    (available-modules (prelude homology))
    (hints ("Compute: (graph-betti-numbers edges vertices)"
            "Check: (= (car betti) 1) for connectivity"
            "Check: (= (cdr betti) 0) for acyclicity"
            "Return: (and connected acyclic)"))
    (reference-solution "(define (graph-is-tree? edges vertices)
  (let ([betti (graph-betti-numbers edges vertices)])
    (and (= (car betti) 1)
         (= (cdr betti) 0))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement graph-is-forest?
  ;; ──────────────────────────────────────────────
  ;; Forest = acyclic (possibly disconnected).

  (task
    (id "data/graph-homology/is-forest-impl")
    (module graph-homology)
    (tier 0)
    (type implement)
    (description "Implement graph-is-forest?: determine if a graph is a forest using its Betti numbers. A forest is an acyclic graph (beta_1 = 0) — it may have multiple connected components (each component is a tree). The difference from graph-is-tree? is that we only check acyclicity, not connectivity. A forest with one component is a tree; a forest with multiple components is a collection of trees.")
    (context "Available: graph-betti-numbers, cdr, =. One function call plus one comparison.")
    (before "(define (graph-is-forest? edges vertices)
  ;; TODO: acyclic (beta_1 = 0)
  #f)")
    (test-file "lattice/data/graph/test-graph-homology.ss")
    (test-forms (";; Tree is a forest
(assert-true (graph-is-forest? '((1 . 2) (2 . 3)) '(1 2 3)))"
                 ";; Disconnected acyclic graph is a forest
(assert-true (graph-is-forest? '((1 . 2)) '(1 2 3 4)))"
                 ";; Graph with cycle is NOT a forest
(assert-false (graph-is-forest? '((1 . 2) (2 . 3) (1 . 3)) '(1 2 3)))"
                 ";; Isolated vertices are a forest
(assert-true (graph-is-forest? '() '(1 2 3)))"))
    (available-modules (prelude homology))
    (hints ("Compute: (graph-betti-numbers edges vertices)"
            "Only check: (= (cdr betti) 0)"))
    (reference-solution "(define (graph-is-forest? edges vertices)
  (let ([betti (graph-betti-numbers edges vertices)])
    (= (cdr betti) 0)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement edges-from-z2-null-vector
  ;; ──────────────────────────────────────────────
  ;; Converting algebraic null vectors to graph edges.

  (task
    (id "data/graph-homology/edges-from-z2-null-impl")
    (module graph-homology)
    (tier 0)
    (type implement)
    (description "Implement edges-from-z2-null-vector: convert a Z_2 null space vector into a list of edges that form a cycle. The null space of the boundary matrix represents cycles in Z_2 homology. Each element in the coefficient vector is 0 or 1 — a 1 means that edge is part of the cycle. Walk through the coefficients and edge simplices in parallel: when coeff = 1, extract the two vertices from the simplex using simplex-vertices, form an edge pair (v0 . v1), and collect. Return edges in order (use reverse at the end for correct ordering).")
    (context "Available: null?, or, car, cdr, cadr, cons, reverse, =, simplex-vertices, let, let*, if. Named let with three accumulators.")
    (before "(define (edges-from-z2-null-vector coeffs edge-simplices)
  ;; TODO: collect edges where coefficient = 1
  '())")
    (test-file "lattice/data/graph/test-graph-homology.ss")
    (test-forms (";; Triangle cycle: all edges present
(let* ([sc (graph->simplicial-complex '((1 . 2) (2 . 3) (1 . 3)) '(1 2 3))]
       [edges (sc-edges sc)]
       [result (edges-from-z2-null-vector '(1 1 1) edges)])
  (assert-equal 3 (length result)))"
                 ";; Partial: only some edges
(let* ([sc (graph->simplicial-complex '((1 . 2) (2 . 3) (1 . 3)) '(1 2 3))]
       [edges (sc-edges sc)]
       [result (edges-from-z2-null-vector '(1 0 1) edges)])
  (assert-equal 2 (length result)))"
                 ";; Empty: no edges selected
(let* ([sc (graph->simplicial-complex '((1 . 2) (2 . 3)) '(1 2 3))]
       [edges (sc-edges sc)]
       [result (edges-from-z2-null-vector '(0 0) edges)])
  (assert-equal 0 (length result)))"))
    (available-modules (prelude homology))
    (hints ("Named let loop with cs, edges, result accumulators"
            "Base: (or (null? cs) (null? edges)) → (reverse result)"
            "If (= (car cs) 1): extract vertices, cons (v0 . v1) to result"
            "Else: skip, recurse with (cdr cs) and (cdr edges)"))
    (reference-solution "(define (edges-from-z2-null-vector coeffs edge-simplices)
  (let loop ([cs coeffs] [edges edge-simplices] [result '()])
    (if (or (null? cs) (null? edges))
        (reverse result)
        (let ([coeff (car cs)]
              [edge (car edges)])
          (if (= coeff 1)
              (let* ([vs (simplex-vertices edge)]
                     [v0 (car vs)]
                     [v1 (cadr vs)])
                (loop (cdr cs) (cdr edges) (cons (cons v0 v1) result)))
              (loop (cdr cs) (cdr edges) result))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

) ;; end tasks
