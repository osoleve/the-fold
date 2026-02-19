;;; homology.sexp — Development tasks for lattice/topology/homology.ss
;;;
;;; Module: homology (tier 1, depends on simplicial-complex)
;;; 521 lines, ~20 exported functions
;;; Computes homology groups and Betti numbers for simplicial complexes
;;; over Z_2 coefficients. Gaussian elimination on boundary matrices,
;;; rank-nullity theorem, standard spaces (sphere, torus, Klein bottle,
;;; projective plane), connected components via union-find.
;;;
;;; The key identity: d*d = 0 (boundary of a boundary is empty).
;;; H_k = ker(d_k) / im(d_{k+1}), Betti number B_k = nullity(d_k) - rank(d_{k+1}).
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   2696481c feat(module): migrate info/, automata/, number-theory/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement z2-matrix-zero
  ;; ──────────────────────────────────────────────
  ;; Create a zero matrix over Z_2 — the simplest constructor.

  (task
    (id "topology/homology/z2-matrix-zero-impl")
    (module homology)
    (tier 1)
    (type implement)
    (description "Implement z2-matrix-zero: create a rows x cols matrix over Z_2 (integers mod 2) initialized to all zeros. The matrix is represented as a tagged list: (list 'z2-matrix rows cols data) where data is a flat vector of rows*cols entries (row-major order). Use make-vector to allocate the flat storage initialized to 0. This is the foundation for all Z_2 linear algebra in computational topology.")
    (context "Available: list, make-vector, *. Pure data construction. The z2-matrix constructor is: (z2-matrix rows cols data) = (list 'z2-matrix rows cols data).")
    (before "(define (z2-matrix-zero rows cols)
  (doc 'export #t)
  (doc 'type '(-> Integer Integer Z2Matrix))
  (doc 'description \"Create a zero matrix\")
  ;; TODO: create tagged list with zero-filled vector
  '())")
    (test-file "lattice/topology/test-homology.ss")
    (test-forms (";; Correct tag
(assert-equal 'z2-matrix (car (z2-matrix-zero 3 4)))"
                 ";; Correct dimensions
(let ([m (z2-matrix-zero 3 4)])
  (assert-equal 3 (z2-matrix-rows m))
  (assert-equal 4 (z2-matrix-cols m)))"
                 ";; All entries are zero
(let ([m (z2-matrix-zero 2 2)])
  (assert-equal 0 (z2-matrix-ref m 0 0))
  (assert-equal 0 (z2-matrix-ref m 1 1)))"))
    (available-modules (prelude))
    (hints ("Call z2-matrix constructor: (z2-matrix rows cols data)"
            "Data is: (make-vector (* rows cols) 0)"
            "The z2-matrix function is just: (list 'z2-matrix rows cols data)"))
    (reference-solution "(define (z2-matrix-zero rows cols)
  (z2-matrix rows cols (make-vector (* rows cols) 0)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement z2-nullity
  ;; ──────────────────────────────────────────────
  ;; Kernel dimension via rank-nullity — one line but deep meaning.

  (task
    (id "topology/homology/z2-nullity-impl")
    (module homology)
    (tier 1)
    (type implement)
    (description "Implement z2-nullity: compute the nullity (dimension of the kernel/null space) of a Z_2 matrix. By the rank-nullity theorem, nullity = cols - rank. The kernel of the boundary matrix d_k gives the space of k-cycles (chains with no boundary), and its dimension directly enters the Betti number formula: B_k = nullity(d_k) - rank(d_{k+1}). Use the existing z2-rank function and z2-matrix-cols accessor.")
    (context "Available: z2-rank (computes rank via Gaussian elimination), z2-matrix-cols (gets column count), -. One-line computation backed by deep linear algebra.")
    (before "(define (z2-nullity m)
  (doc 'export #t)
  (doc 'type '(-> Z2Matrix Integer))
  (doc 'description \"Compute the nullity (dimension of kernel) of a Z_2 matrix\")
  ;; TODO: rank-nullity theorem
  0)")
    (test-file "lattice/topology/test-homology.ss")
    (test-forms (";; Identity matrix: nullity = 0 (trivial kernel)
(assert-equal 0 (z2-nullity (z2-matrix 3 3 (vector 1 0 0  0 1 0  0 0 1))))"
                 ";; Zero matrix: nullity = cols (everything is in the kernel)
(assert-equal 3 (z2-nullity (z2-matrix 2 3 (vector 0 0 0  0 0 0))))"
                 ";; Rank 2, 3 cols → nullity 1
(assert-equal 1 (z2-nullity (z2-matrix 2 3 (vector 1 0 1  0 1 1))))"))
    (available-modules (prelude))
    (hints ("The rank-nullity theorem: nullity = cols - rank"
            "Get cols with (z2-matrix-cols m)"
            "Get rank with (z2-rank m)"
            "Return (- cols rank)"))
    (reference-solution "(define (z2-nullity m)
  (- (z2-matrix-cols m) (z2-rank m)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement z2-rank
  ;; ──────────────────────────────────────────────
  ;; Gaussian elimination over Z_2 — the computational workhorse.

  (task
    (id "topology/homology/z2-rank-impl")
    (module homology)
    (tier 1)
    (type implement)
    (description "Implement z2-rank: compute the rank of a Z_2 matrix using Gaussian elimination. Work on a copy of the input (non-destructive). Maintain a pivot-row and pivot-col cursor. For each column: scan downward to find a row with a 1 in the pivot column. If found, swap it to the pivot position, then eliminate all other 1s in that column by adding (XOR) the pivot row to each row that has a 1 there. Advance both cursors. If no pivot found in a column, advance only the column cursor. The rank equals the number of pivots found. Over Z_2, addition is XOR (same as subtraction), so row reduction is simpler than over the reals.")
    (context "Available: z2-matrix-copy (non-destructive copy), z2-matrix-rows, z2-matrix-cols, z2-matrix-ref, z2-matrix-swap-rows!, z2-matrix-add-row! (row i += row j mod 2), >=, =, not, when, do. Nested named let for the main loop and pivot search.")
    (before "(define (z2-rank matrix)
  (doc 'export #t)
  (doc 'type '(-> Z2Matrix Integer))
  (doc 'description \"Compute the rank of a Z_2 matrix using Gaussian elimination\")
  ;; TODO: Gaussian elimination, count pivots
  0)")
    (test-file "lattice/topology/test-homology.ss")
    (test-forms (";; Identity matrix: rank = n
(assert-equal 3 (z2-rank (z2-matrix 3 3 (vector 1 0 0  0 1 0  0 0 1))))"
                 ";; Zero matrix: rank = 0
(assert-equal 0 (z2-rank (z2-matrix 2 3 (vector 0 0 0  0 0 0))))"
                 ";; Full-rank 2x3 matrix
(assert-equal 2 (z2-rank (z2-matrix 2 3 (vector 1 0 1  0 1 1))))"
                 ";; Duplicate rows reduce rank
(assert-equal 1 (z2-rank (z2-matrix 2 2 (vector 1 1  1 1))))"))
    (available-modules (prelude))
    (hints ("Copy first: (z2-matrix-copy matrix)"
            "Main loop with pivot-row and pivot-col, both starting at 0"
            "Termination: pivot-row >= rows OR pivot-col >= cols → return pivot-row"
            "Find pivot: scan rows r from pivot-row to rows-1 for z2-matrix-ref m r pivot-col = 1"
            "If found: swap rows, then eliminate by adding pivot row to all other rows with 1 in that column"
            "If not found in column: increment pivot-col only"
            "The rank = number of pivots = final value of pivot-row"))
    (reference-solution "(define (z2-rank matrix)
  (let* ([m (z2-matrix-copy matrix)]
         [rows (z2-matrix-rows m)]
         [cols (z2-matrix-cols m)])
    (let loop ([pivot-row 0] [pivot-col 0])
      (if (or (>= pivot-row rows) (>= pivot-col cols))
          pivot-row
          (let find-pivot ([r pivot-row])
            (cond
              [(>= r rows)
               (loop pivot-row (+ pivot-col 1))]
              [(= (z2-matrix-ref m r pivot-col) 1)
               (when (not (= r pivot-row))
                 (z2-matrix-swap-rows! m r pivot-row))
               (do ([i 0 (+ i 1)])
                   [(= i rows)]
                 (when (and (not (= i pivot-row))
                            (= (z2-matrix-ref m i pivot-col) 1))
                   (z2-matrix-add-row! m i pivot-row)))
               (loop (+ pivot-row 1) (+ pivot-col 1))]
              [else
               (find-pivot (+ r 1))]))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement sc-betti
  ;; ──────────────────────────────────────────────
  ;; The Betti number — where algebra meets topology.

  (task
    (id "topology/homology/sc-betti-impl")
    (module homology)
    (tier 1)
    (type implement)
    (description "Implement sc-betti: compute the k-th Betti number of a simplicial complex. The Betti number B_k = dim(ker d_k) - dim(im d_{k+1}) = nullity(d_k) - rank(d_{k+1}), where d_k is the k-th boundary matrix. B_0 counts connected components, B_1 counts 1-dimensional holes (loops), B_2 counts 2-dimensional voids, etc. Handle edge cases: k < 0 or k > max dimension returns 0. Use sc-boundary-matrix to build the boundary matrices, z2-nullity for ker dimension, and z2-rank for image dimension.")
    (context "Available: sc-max-dim (highest simplex dimension), sc-boundary-matrix (builds d_k), z2-nullity (kernel dimension), z2-rank (image dimension), <, >, -, +, cond, let*.")
    (before "(define (sc-betti sc k)
  (doc 'export #t)
  (doc 'type '(-> SC Integer Integer))
  (doc 'description \"Compute the k-th Betti number of a simplicial complex\")
  ;; TODO: B_k = nullity(d_k) - rank(d_{k+1})
  0)")
    (test-file "lattice/topology/test-homology.ss")
    (test-forms (";; S^1 (circle): B_0=1 (connected), B_1=1 (one loop)
(let ([s1 (make-sphere 1)])
  (assert-equal 1 (sc-betti s1 0))
  (assert-equal 1 (sc-betti s1 1)))"
                 ";; S^2 (sphere): B_0=1, B_1=0, B_2=1
(let ([s2 (make-sphere 2)])
  (assert-equal 1 (sc-betti s2 0))
  (assert-equal 0 (sc-betti s2 1))
  (assert-equal 1 (sc-betti s2 2)))"
                 ";; Negative dimension → 0
(assert-equal 0 (sc-betti (make-sphere 1) -1))"
                 ";; Above max dimension → 0
(assert-equal 0 (sc-betti (make-sphere 1) 5))"))
    (available-modules (prelude))
    (hints ("Get max dimension: (sc-max-dim sc)"
            "Guard: k < 0 or k > max-d → return 0"
            "Build d_k: (sc-boundary-matrix sc k)"
            "Build d_{k+1}: (sc-boundary-matrix sc (+ k 1))"
            "Compute: (- (z2-nullity d_k) (z2-rank d_{k+1}))"))
    (reference-solution "(define (sc-betti sc k)
  (let ([max-d (sc-max-dim sc)])
    (cond
      [(< k 0) 0]
      [(> k max-d) 0]
      [else
       (let* ([bk (sc-boundary-matrix sc k)]
              [bk+1 (sc-boundary-matrix sc (+ k 1))]
              [ker-dim (z2-nullity bk)]
              [im-dim (z2-rank bk+1)])
         (- ker-dim im-dim))])))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement sc-betti-numbers
  ;; ──────────────────────────────────────────────
  ;; All Betti numbers at once — the complete topological signature.

  (task
    (id "topology/homology/sc-betti-numbers-impl")
    (module homology)
    (tier 1)
    (type implement)
    (description "Implement sc-betti-numbers: compute all Betti numbers B_0, B_1, ..., B_d where d is the maximum simplex dimension. Return them as a list. This gives the complete topological signature of the space: the list (1 1) for a circle, (1 0 1) for a sphere, (1 2 1) for a torus (over Z_2). Use sc-max-dim to find d, then map sc-betti over dimensions 0 to d using iota to generate the index list.")
    (context "Available: sc-max-dim, sc-betti, map, iota (generates 0..n-1), +, <. Uses the previously-implemented sc-betti for each dimension.")
    (before "(define (sc-betti-numbers sc)
  (doc 'export #t)
  (doc 'type '(-> SC (List Integer)))
  (doc 'description \"Compute all Betti numbers from dimension 0 to max-dim\")
  ;; TODO: map sc-betti over 0..max-dim
  '())")
    (test-file "lattice/topology/test-homology.ss")
    (test-forms (";; Circle S^1: (1 1)
(assert-equal '(1 1) (sc-betti-numbers (make-sphere 1)))"
                 ";; Sphere S^2: (1 0 1)
(assert-equal '(1 0 1) (sc-betti-numbers (make-sphere 2)))"
                 ";; Torus (over Z_2): (1 2 1)
(assert-equal '(1 2 1) (sc-betti-numbers (make-torus)))"))
    (available-modules (prelude))
    (hints ("Get max dimension: (sc-max-dim sc)"
            "Guard: if max-d < 0, return '()"
            "Generate indices: (iota (+ max-d 1)) gives (0 1 2 ... max-d)"
            "Map: (map (lambda (k) (sc-betti sc k)) indices)"))
    (reference-solution "(define (sc-betti-numbers sc)
  (let ([max-d (sc-max-dim sc)])
    (if (< max-d 0)
        '()
        (map (lambda (k) (sc-betti sc k))
             (iota (+ max-d 1))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

) ;; end tasks
