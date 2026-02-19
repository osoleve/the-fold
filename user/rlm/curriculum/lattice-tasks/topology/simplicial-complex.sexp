;;; simplicial-complex.sexp — Development tasks for lattice/topology/simplicial-complex.ss
;;;
;;; Module: simplicial-complex (tier 1, depends on set, sort)
;;; 575 lines, ~35 exported functions
;;; Simplicial complexes: simplex construction, face enumeration,
;;; boundary operator, chains, Euler characteristic, star/link,
;;; f-vectors, filtration support.
;;;
;;; Git history:
;;;   5d9fd947 feat(module): migrate lattice/topology/ to require
;;;   555b079a docs(lattice): Convert topology to (doc ...) forms

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement combinations
  ;; ──────────────────────────────────────────────
  ;; Classic recursive algorithm. Foundation for face enumeration.

  (task
    (id "topology/simplicial-complex/combinations-impl")
    (module simplicial-complex)
    (tier 1)
    (type implement)
    (description "Implement combinations: generate all k-element subsets of a list. This is the standard 'choose k from n' algorithm. Two base cases: if k = 0, return a list containing just the empty list (there's exactly one way to choose nothing). If the input list is empty and k > 0, return an empty list (can't choose from nothing). Recursive case: for the first element, either include it or don't. Include: prepend it to each (k-1)-combination of the rest. Exclude: take all k-combinations of the rest. Append both results.")
    (context "Available: prelude (null?, car, cdr, cons, =, -, append, map). This is a pure combinatorial function with no dependencies on the topology module.")
    (before "(define (combinations lst k)
  (doc 'type '(-> (List a) Integer (List (List a))))
  (doc 'description \"Generate all k-combinations of elements from a list\")
  ;; TODO: recursive include/exclude on first element
  '())")
    (test-file "lattice/topology/test-simplicial-complex.ss")
    (test-forms (";; C(4,2) = 6
(assert-equal 6 (length (combinations '(1 2 3 4) 2)))"
                 ";; C(5,3) = 10
(assert-equal 10 (length (combinations '(1 2 3 4 5) 3)))"
                 ";; C(n,0) = 1 — one way to choose nothing
(assert-equal 1 (length (combinations '(1 2 3) 0)))"
                 ";; C(n,n) = 1 — one way to choose everything
(assert-equal 1 (length (combinations '(1 2 3) 3)))"
                 ";; Choosing from empty list gives nothing
(assert-equal '() (combinations '() 2))"))
    (available-modules (prelude))
    (hints ("Base case k=0: return '(()) — a list containing the empty list"
            "Base case empty list: return '()"
            "Recursive: (append include-first exclude-first)"
            "include-first: (map (lambda (c) (cons (car lst) c)) (combinations (cdr lst) (- k 1)))"
            "exclude-first: (combinations (cdr lst) k)"))
    (reference-solution "(define (combinations lst k)
  (cond
    [(= k 0) '(())]
    [(null? lst) '()]
    [else
     (append
       (map (lambda (c) (cons (car lst) c))
            (combinations (cdr lst) (- k 1)))
       (combinations (cdr lst) k))]))")
    (alternatives ())
    (source-commit "5d9fd947")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement simplex-facets
  ;; ──────────────────────────────────────────────
  ;; Codimension-1 faces — remove one vertex at a time.

  (task
    (id "topology/simplicial-complex/facets-impl")
    (module simplicial-complex)
    (tier 1)
    (type implement)
    (description "Implement simplex-facets: get all facets (codimension-1 faces) of a simplex. Each facet is obtained by removing exactly one vertex from the simplex. A triangle [a,b,c] has three facets: [b,c], [a,c], [a,b]. A vertex (0-simplex) has no facets. Use a loop from i=0 to n-1, where n is the number of vertices: at each step, create a new simplex from the vertex list with element at index i removed. Use (make-simplex (remove-at verts i)) where remove-at returns a list with the element at index i deleted. Collect results in a list.")
    (context "Available: prelude (length, >=, +, cons, reverse), make-simplex (creates a simplex from vertex list), simplex-vertices (gets sorted vertex list). You also have remove-at which removes element at index i from a list.")
    (before "(define (simplex-facets s)
  (doc 'type '(-> Simplex (List Simplex)))
  (doc 'description \"Get all facets (codimension-1 faces) of a simplex\")
  ;; TODO: for each vertex, create simplex with that vertex removed
  '())")
    (test-file "lattice/topology/test-simplicial-complex.ss")
    (test-forms (";; Triangle has 3 facets (edges)
(let ([facets (simplex-facets (triangle 'a 'b 'c))])
  (assert-equal 3 (length facets)))"
                 ";; Edge has 2 facets (vertices)
(assert-equal 2 (length (simplex-facets (edge 'a 'b))))"
                 ";; Vertex has no facets
(assert-equal '() (simplex-facets (vertex 'a)))"
                 ";; Tetrahedron has 4 facets (triangles)
(assert-equal 4 (length (simplex-facets (tetrahedron 'a 'b 'c 'd))))"))
    (available-modules (prelude set sort))
    (hints ("Get vertices with (simplex-vertices s) and count with (length verts)"
            "If n <= 1, return '() — vertices and empty simplices have no facets"
            "Loop from i=0 to n-1, accumulating (make-simplex (remove-at verts i))"
            "Reverse the accumulator at the end to preserve order"))
    (reference-solution "(define (simplex-facets s)
  (let* ([verts (simplex-vertices s)]
         [n (length verts)])
    (if (<= n 1)
        '()
        (let loop ([i 0] [acc '()])
          (if (>= i n)
              (reverse acc)
              (loop (+ i 1)
                    (cons (make-simplex (remove-at verts i)) acc)))))))")
    (alternatives ())
    (source-commit "5d9fd947")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement simplex-boundary
  ;; ──────────────────────────────────────────────
  ;; The boundary operator — heart of algebraic topology.

  (task
    (id "topology/simplicial-complex/boundary-impl")
    (module simplicial-complex)
    (tier 1)
    (type implement)
    (description "Implement simplex-boundary: compute the boundary of a simplex as a chain (formal linear combination of simplices with integer coefficients). The boundary formula is: d[v0, v1, ..., vn] = sum from i=0 to n of (-1)^i * [v0, ..., v_hat_i, ..., vn], where v_hat_i means vertex i is omitted. Each term has coefficient +1 if i is even, -1 if i is odd. A chain is a list of (coefficient . simplex) pairs. The boundary of a vertex (0-simplex) is the empty chain. Use chain-add-term to build up the result — it handles combining coefficients when the same simplex appears twice.")
    (context "Available: prelude (length, >=, +, even?), simplex-vertices, make-simplex, remove-at, chain-empty (the empty chain '()), chain-add-term (adds a (coeff . simplex) pair to a chain, combining if already present).")
    (before "(define (simplex-boundary s)
  (doc 'type '(-> Simplex Chain))
  (doc 'description \"Compute the boundary of a simplex as a chain\")
  ;; TODO: alternating signed sum of codimension-1 faces
  chain-empty)")
    (test-file "lattice/topology/test-simplicial-complex.ss")
    (test-forms (";; Boundary of vertex is empty
(assert-equal chain-empty (simplex-boundary (vertex 'a)))"
                 ";; Boundary of edge has two terms
(assert-equal 2 (length (simplex-boundary (edge 'a 'b))))"
                 ";; Boundary of triangle has three terms
(assert-equal 3 (length (simplex-boundary (triangle 'a 'b 'c))))"
                 ";; The fundamental theorem: boundary squared is zero!
(let* ([t (triangle 'a 'b 'c)]
       [bd1 (simplex-boundary t)]
       [bd2 (chain-boundary bd1)])
  (assert-equal chain-empty bd2))"))
    (available-modules (prelude set sort))
    (hints ("Get vertices and count n = (length verts)"
            "If n <= 1, return chain-empty (boundary of vertex is empty)"
            "Loop from i=0 to n: sign = (if (even? i) 1 -1)"
            "Face at step i = (make-simplex (remove-at verts i))"
            "Accumulate: chain = (chain-add-term chain sign face)"
            "The alternating signs are what make boundary-squared-is-zero work"))
    (reference-solution "(define (simplex-boundary s)
  (let* ([verts (simplex-vertices s)]
         [n (length verts)])
    (if (<= n 1)
        chain-empty
        (let loop ([i 0] [chain chain-empty])
          (if (>= i n)
              chain
              (let* ([sign (if (even? i) 1 -1)]
                     [face (make-simplex (remove-at verts i))])
                (loop (+ i 1) (chain-add-term chain sign face))))))))")
    (alternatives ())
    (source-commit "5d9fd947")
    (difficulty 4))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement sc-euler
  ;; ──────────────────────────────────────────────
  ;; The Euler characteristic — a topological invariant.

  (task
    (id "topology/simplicial-complex/euler-impl")
    (module simplicial-complex)
    (tier 1)
    (type implement)
    (description "Implement sc-euler: compute the Euler characteristic of a simplicial complex. The Euler characteristic is chi = sum from k=0 to d of (-1)^k * f_k, where f_k is the number of k-dimensional simplices and d is the maximum dimension. For a surface: chi = V - E + F (vertices minus edges plus faces). This is a topological invariant: chi(sphere) = 2, chi(torus) = 0, chi(filled triangle) = 1. Loop from k=0 to sc-max-dim, accumulating the alternating sum.")
    (context "Available: prelude (>, +, *, even?), sc-max-dim (maximum dimension in complex), sc-count-dim (count simplices of dimension k).")
    (before "(define (sc-euler sc)
  (doc 'type '(-> SC Integer))
  (doc 'description \"Compute Euler characteristic: chi = sum (-1)^k f_k\")
  ;; TODO: alternating sum of simplex counts by dimension
  0)")
    (test-file "lattice/topology/test-simplicial-complex.ss")
    (test-forms (";; Triangle: V - E + F = 3 - 3 + 1 = 1
(assert-equal 1 (sc-euler (sc-from-simplices (list (triangle 'a 'b 'c)))))"
                 ";; Tetrahedron: 4 - 6 + 4 - 1 = 1
(assert-equal 1 (sc-euler (sc-from-simplices (list (tetrahedron 'a 'b 'c 'd)))))"
                 ";; Boundary of tetrahedron = 2-sphere: chi = 2
(assert-equal 2 (sc-euler (sc-boundary-of-simplex 3)))"
                 ";; Boundary of triangle = circle: chi = 0
(assert-equal 0 (sc-euler (sc-boundary-of-simplex 2)))"))
    (available-modules (prelude set sort))
    (hints ("Get max-d = (sc-max-dim sc)"
            "Loop from k=0 to max-d with accumulator sum"
            "At each step: sign = (if (even? k) 1 -1), count = (sc-count-dim sc k)"
            "sum = sum + sign * count"
            "Return sum when k > max-d"))
    (reference-solution "(define (sc-euler sc)
  (let ([max-d (sc-max-dim sc)])
    (let loop ([k 0] [sum 0])
      (if (> k max-d)
          sum
          (let ([sign (if (even? k) 1 -1)]
                [count (sc-count-dim sc k)])
            (loop (+ k 1) (+ sum (* sign count))))))))")
    (alternatives ())
    (source-commit "5d9fd947")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement sc-f-vector
  ;; ──────────────────────────────────────────────
  ;; The f-vector summarizes a complex's structure.

  (task
    (id "topology/simplicial-complex/f-vector-impl")
    (module simplicial-complex)
    (tier 1)
    (type implement)
    (description "Implement sc-f-vector: compute the f-vector of a simplicial complex. The f-vector is (f_0, f_1, ..., f_d) where f_k is the number of k-dimensional simplices. For a triangle: f-vector is (3, 3, 1) meaning 3 vertices, 3 edges, 1 face. For a tetrahedron: (4, 6, 4, 1). If the complex is empty (max-dim < 0), return the empty list. Otherwise, map sc-count-dim over dimensions 0 through max-dim.")
    (context "Available: prelude (map, <, +), iota (generates (0 1 2 ... n-1)), sc-max-dim, sc-count-dim.")
    (before "(define (sc-f-vector sc)
  (doc 'type '(-> SC (List Integer)))
  (doc 'description \"Get the f-vector: (f_0, f_1, ..., f_d) counting simplices per dimension\")
  ;; TODO: map count-dim over all dimensions
  '())")
    (test-file "lattice/topology/test-simplicial-complex.ss")
    (test-forms ("(assert-equal '(3 3 1) (sc-f-vector (sc-from-simplices (list (triangle 'a 'b 'c)))))"
                 "(assert-equal '(4 6 4 1) (sc-f-vector (sc-from-simplices (list (tetrahedron 'a 'b 'c 'd)))))"
                 ";; Discrete complex: only vertices
(assert-equal '(3) (sc-f-vector (sc-discrete '(a b c))))"
                 ";; Empty complex
(assert-equal '() (sc-f-vector sc-empty))"))
    (available-modules (prelude set sort))
    (hints ("Get max-d = (sc-max-dim sc)"
            "If max-d < 0, return '() (empty complex)"
            "Use iota to generate (0 1 2 ... max-d): (iota (+ max-d 1))"
            "Map (lambda (k) (sc-count-dim sc k)) over that list"))
    (reference-solution "(define (sc-f-vector sc)
  (let ([max-d (sc-max-dim sc)])
    (if (< max-d 0)
        '()
        (map (lambda (k) (sc-count-dim sc k))
             (iota (+ max-d 1))))))")
    (alternatives ())
    (source-commit "5d9fd947")
    (difficulty 2))

) ;; end tasks
