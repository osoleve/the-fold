(load "lattice/topology/simplicial-complex.ss")

(doc 'module 'homology)
(doc 'description "Computes homology groups and Betti numbers for simplicial complexes")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'tier 1)
(doc 'dependencies '(topology/simplicial-complex))

(doc 'note "Homology measures holes in topological spaces:
  B_0 = number of connected components
  B_1 = number of 1-dimensional holes (loops that aren't boundaries)
  B_2 = number of 2-dimensional holes (voids)
  ...")

(doc 'note "We work with Z_2 coefficients (mod 2 arithmetic) which:
  - Avoids integer coefficient tracking (signs don't matter mod 2)
  - Simplifies Gaussian elimination (only 0 and 1)
  - Is standard in computational topology/TDA")

(doc 'note "The key insight: d*d = 0 (boundary of a boundary is empty)
This gives us a chain complex:
  ... -> C_k+1 --d_{k+1}--> C_k --d_k--> C_{k-1} -> ...")

(doc 'note "Homology H_k = ker(d_k) / im(d_{k+1})
Betti number B_k = dim(H_k) = nullity(d_k) - rank(d_{k+1})")

(doc 'section 'z2-matrix-operations)

(doc 'note "We represent Z_2 matrices as vectors of row vectors. Each entry is 0 or 1")

(define (z2-matrix rows cols data)
  (doc 'type '(-> Integer Integer Vector Z2Matrix))
  (doc 'description "Create a Z_2 matrix from row-major data")
  (list 'z2-matrix rows cols data))

(define (z2-matrix? x)
  (doc 'type '(-> α Boolean))
  (and (pair? x) (eq? (car x) 'z2-matrix)))

(define (z2-matrix-rows m)
  (doc 'type '(-> Z2Matrix Integer))
  (cadr m))

(define (z2-matrix-cols m)
  (doc 'type '(-> Z2Matrix Integer))
  (caddr m))

(define (z2-matrix-data m)
  (doc 'type '(-> Z2Matrix Vector))
  (cadddr m))

(define (z2-matrix-ref m i j)
  (doc 'type '(-> Z2Matrix Integer Integer Integer))
  (vector-ref (z2-matrix-data m) (+ (* i (z2-matrix-cols m)) j)))

(define (z2-matrix-set! m i j v)
  (doc 'type '(-> Z2Matrix Integer Integer Integer Void))
  (doc 'description "Mutate matrix entry (used during Gaussian elimination)")
  (vector-set! (z2-matrix-data m) (+ (* i (z2-matrix-cols m)) j) (mod v 2)))

(define (z2-matrix-zero rows cols)
  (doc 'type '(-> Integer Integer Z2Matrix))
  (doc 'description "Create a zero matrix")
  (z2-matrix rows cols (make-vector (* rows cols) 0)))

(define (z2-matrix-copy m)
  (doc 'type '(-> Z2Matrix Z2Matrix))
  (doc 'description "Create a copy of a matrix (for non-destructive operations)")
  (z2-matrix (z2-matrix-rows m)
             (z2-matrix-cols m)
             (vector-copy (z2-matrix-data m))))

(define (z2-matrix-swap-rows! m i j)
  (doc 'type '(-> Z2Matrix Integer Integer Void))
  (doc 'description "Swap two rows in place")
  (let ([cols (z2-matrix-cols m)])
    (do ([k 0 (+ k 1)])
        [(= k cols)]
      (let ([tmp (z2-matrix-ref m i k)])
        (z2-matrix-set! m i k (z2-matrix-ref m j k))
        (z2-matrix-set! m j k tmp)))))

(define (z2-matrix-add-row! m i j)
  (doc 'type '(-> Z2Matrix Integer Integer Void))
  (doc 'description "Add row j to row i (mod 2), storing result in row i")
  (let ([cols (z2-matrix-cols m)])
    (do ([k 0 (+ k 1)])
        [(= k cols)]
      (z2-matrix-set! m i k (+ (z2-matrix-ref m i k) (z2-matrix-ref m j k))))))

(doc 'section 'gaussian-elimination)

(define (z2-rank matrix)
  (doc 'type '(-> Z2Matrix Integer))
  (doc 'description "Compute the rank of a Z_2 matrix using Gaussian elimination")
  (doc 'note "This is the dimension of the image (column space)")
  (let* ([m (z2-matrix-copy matrix)]  ; Work on a copy
         [rows (z2-matrix-rows m)]
         [cols (z2-matrix-cols m)])
    (let loop ([pivot-row 0] [pivot-col 0])
      (if (or (>= pivot-row rows) (>= pivot-col cols))
          pivot-row  ; Rank = number of pivots found
          ; Find a row with 1 in pivot column
          (let find-pivot ([r pivot-row])
            (cond
              [(>= r rows)
               ; No pivot in this column, try next column
               (loop pivot-row (+ pivot-col 1))]
              [(= (z2-matrix-ref m r pivot-col) 1)
               ; Found pivot, swap to pivot position
               (when (not (= r pivot-row))
                 (z2-matrix-swap-rows! m r pivot-row))
               ; Eliminate all other 1s in this column
               (do ([i 0 (+ i 1)])
                   [(= i rows)]
                 (when (and (not (= i pivot-row))
                            (= (z2-matrix-ref m i pivot-col) 1))
                   (z2-matrix-add-row! m i pivot-row)))
               ; Move to next pivot position
               (loop (+ pivot-row 1) (+ pivot-col 1))]
              [else
               (find-pivot (+ r 1))]))))))

(define (z2-nullity m)
  (doc 'type '(-> Z2Matrix Integer))
  (doc 'description "Compute the nullity (dimension of kernel) of a Z_2 matrix")
  (doc 'note "nullity = cols - rank (by rank-nullity theorem)")
  (- (z2-matrix-cols m) (z2-rank m)))

(define (z2-rref matrix)
  (doc 'type '(-> Z2Matrix (values Z2Matrix (List Integer))))
  (doc 'description "Compute reduced row echelon form and return pivot columns")
  (doc 'returns "1) RREF matrix, 2) list of pivot column indices")
  (let* ([m (z2-matrix-copy matrix)]
         [rows (z2-matrix-rows m)]
         [cols (z2-matrix-cols m)]
         [pivots '()])  ; Will collect pivot columns in reverse
    (let loop ([pivot-row 0] [pivot-col 0])
      (if (or (>= pivot-row rows) (>= pivot-col cols))
          (values m (reverse pivots))
          ; Find a row with 1 in pivot column
          (let find-pivot ([r pivot-row])
            (cond
              [(>= r rows)
               ; No pivot in this column, try next column
               (loop pivot-row (+ pivot-col 1))]
              [(= (z2-matrix-ref m r pivot-col) 1)
               ; Found pivot, swap to pivot position
               (when (not (= r pivot-row))
                 (z2-matrix-swap-rows! m r pivot-row))
               ; Eliminate all other 1s in this column
               (do ([i 0 (+ i 1)])
                   [(= i rows)]
                 (when (and (not (= i pivot-row))
                            (= (z2-matrix-ref m i pivot-col) 1))
                   (z2-matrix-add-row! m i pivot-row)))
               ; Record this pivot column
               (set! pivots (cons pivot-col pivots))
               ; Move to next pivot position
               (loop (+ pivot-row 1) (+ pivot-col 1))]
              [else
               (find-pivot (+ r 1))]))))))

(define (z2-null-space matrix)
  (doc 'type '(-> Z2Matrix (List (List Integer))))
  (doc 'description "Compute a basis for the null space (kernel) of a Z_2 matrix")
  (doc 'returns "A list of vectors, each represented as a list of 0s and 1s")
  (doc 'note "Algorithm:
  1. Reduce matrix to RREF, tracking pivot columns
  2. Free columns (non-pivot) correspond to free variables
  3. For each free column, construct a basis vector:
     - Set free variable to 1, other free variables to 0
     - Back-substitute to find pivot variable values from RREF")
  (doc 'example "For a 1-cycle in a graph, the null space basis vectors indicate which edges participate in the cycle")
  (let-values ([(rref pivots) (z2-rref matrix)])
    (let* ([cols (z2-matrix-cols matrix)]
           [pivot-set (list->hashtable-set pivots)]
           ; Free columns are those not in pivot-set
           [free-cols (filter (lambda (c) (not (hashtable-set-member? pivot-set c)))
                              (iota cols))])
      (if (null? free-cols)
          '()  ; Trivial null space
          ; For each free column, construct a basis vector
          (map (lambda (free-col)
                 (z2-null-basis-vector rref pivots cols free-col))
               free-cols)))))

(define (z2-null-basis-vector rref pivots cols free-col)
  (doc 'type '(-> Z2Matrix (List Integer) Integer Integer (List Integer)))
  (doc 'description "Construct one null space basis vector for the given free column")
  (doc 'note "In RREF, for free column f, the basis vector has:
  - v[f] = 1 (the free variable)
  - v[p] = rref[row-of-p][f] for each pivot column p
  - v[other-free] = 0")
  (let ([vec (make-vector cols 0)])
    ; Set the free variable to 1
    (vector-set! vec free-col 1)
    ; For each pivot, read the value from the RREF matrix
    ; Pivot column p in row r means: x_p + ... + rref[r][f]*x_f + ... = 0
    ; So x_p = rref[r][f] (mod 2)
    (let loop ([ps pivots] [row 0])
      (unless (null? ps)
        (let ([pivot-col (car ps)])
          (vector-set! vec pivot-col (z2-matrix-ref rref row free-col))
          (loop (cdr ps) (+ row 1)))))
    (vector->list vec)))

(define (list->hashtable-set lst)
  (doc 'description "Helper: list->hashtable-set using hash table for O(1) membership")
  (let ([ht (make-hashtable equal-hash equal?)])
    (for-each (lambda (x) (hashtable-set! ht x #t)) lst)
    ht))

(define (hashtable-set-member? set x)
  (doc 'description "Helper: hashtable-set-member?")
  (hashtable-ref set x #f))

(doc 'section 'boundary-matrices)

(define (simplex-index simplices s)
  (doc 'type '(-> (List Simplex) Simplex (Or Integer #f)))
  (doc 'description "Find the index of a simplex in a list (for matrix indexing)")
  (doc 'note "O(n) linear scan - use build-simplex-index-table for bulk lookups")
  (let loop ([remaining simplices] [idx 0])
    (cond
      [(null? remaining) #f]
      [(simplex-equal? s (car remaining)) idx]
      [else (loop (cdr remaining) (+ idx 1))])))

(define (build-simplex-index-table simplices)
  (doc 'type '(-> (List Simplex) HashTable))
  (doc 'description "Build a hash table mapping simplex vertices -> index for O(1) lookup")
  (doc 'note "Key is the sorted vertex list (canonical form)")
  (let ([table (make-hashtable equal-hash equal?)])
    (let loop ([remaining simplices] [idx 0])
      (unless (null? remaining)
        (hashtable-set! table (simplex-vertices (car remaining)) idx)
        (loop (cdr remaining) (+ idx 1))))
    table))

(define (simplex-index-fast table s)
  (doc 'type '(-> HashTable Simplex (Or Integer #f)))
  (doc 'description "O(1) lookup using pre-built index table")
  (hashtable-ref table (simplex-vertices s) #f))

(define (sc-boundary-matrix sc k)
  (doc 'type '(-> SC Integer Z2Matrix))
  (doc 'description "Build the k-th boundary matrix d_k : C_k -> C_{k-1}")
  (doc 'note "Rows = (k-1)-simplices (codomain basis)
Cols = k-simplices (domain basis)
Entry (i,j) = 1 if sigma_i is a facet of tau_j

For Z_2 coefficients, we ignore signs (+/-1 = 1 mod 2)")
  (doc 'complexity "O(N * k) where N = number of k-simplices, k = simplex dimension. Uses hash table for O(1) facet index lookups")
  (let* ([domain (sc-simplices-dim sc k)]        ; k-simplices
         [codomain (sc-simplices-dim sc (- k 1))] ; (k-1)-simplices
         [num-rows (length codomain)]
         [num-cols (length domain)])
    (if (or (= num-rows 0) (= num-cols 0))
        (z2-matrix-zero num-rows num-cols)
        (let ([mat (z2-matrix-zero num-rows num-cols)]
              [codomain-index (build-simplex-index-table codomain)])  ; O(1) lookups
          ; Fill in the matrix
          (let col-loop ([dom domain] [j 0])
            (if (null? dom)
                mat
                (let* ([tau (car dom)]
                       [facets (simplex-facets tau)])
                  ; For each facet of tau, mark the corresponding row
                  (for-each
                    (lambda (sigma)
                      (let ([i (simplex-index-fast codomain-index sigma)])
                        (when i
                          (z2-matrix-set! mat i j 1))))
                    facets)
                  (col-loop (cdr dom) (+ j 1)))))))))

(doc 'section 'betti-numbers)

(define (sc-betti sc k)
  (doc 'type '(-> SC Integer Integer))
  (doc 'description "Compute the k-th Betti number of a simplicial complex")
  (doc 'note "B_k = dim(ker d_k) - dim(im d_{k+1}) = nullity(d_k) - rank(d_{k+1})")
  (doc 'note "Special cases: B_k = 0 for k > max-dim; B_0 counts connected components")
  (let ([max-d (sc-max-dim sc)])
    (cond
      [(< k 0) 0]
      [(> k max-d) 0]
      [else
       (let* ([bk (sc-boundary-matrix sc k)]           ; d_k
              [bk+1 (sc-boundary-matrix sc (+ k 1))]   ; d_{k+1}
              [ker-dim (z2-nullity bk)]                ; dim(ker d_k)
              [im-dim (z2-rank bk+1)])                 ; dim(im d_{k+1})
         (- ker-dim im-dim))])))

(define (sc-betti-numbers sc)
  (doc 'type '(-> SC (List Integer)))
  (doc 'description "Compute all Betti numbers from dimension 0 to max-dim")
  (let ([max-d (sc-max-dim sc)])
    (if (< max-d 0)
        '()
        (map (lambda (k) (sc-betti sc k))
             (iota (+ max-d 1))))))

(define (sc-homology-summary sc)
  (doc 'type '(-> SC Void))
  (doc 'description "Print a summary of the homology of a simplicial complex")
  (let* ([betti (sc-betti-numbers sc)]
         [euler (sc-euler sc)]
         [f-vec (sc-f-vector sc)])
    (printf "~n=== Homology Summary ===~n")
    (printf "f-vector:      ~a~n" f-vec)
    (printf "Betti numbers: ~a~n" betti)
    (printf "Euler char:    ~a~n" euler)
    ; Verify Euler characteristic via Betti numbers
    (let ([euler-from-betti
           (let loop ([bs betti] [k 0] [sum 0])
             (if (null? bs)
                 sum
                 (loop (cdr bs) (+ k 1)
                       (+ sum (* (if (even? k) 1 -1) (car bs))))))])
      (printf "X from B:      ~a~n" euler-from-betti)
      (unless (= euler euler-from-betti)
        (printf "WARNING: Euler characteristic mismatch!~n")))
    (printf "========================~n")))

(doc 'section 'standard-spaces)

(define (make-sphere n)
  (doc 'type '(-> Integer SC))
  (doc 'description "Create a triangulation of the n-sphere S^n")
  (doc 'note "We use the boundary of the (n+1)-simplex, which is homeomorphic to S^n")
  (doc 'note "Expected Betti numbers for S^n:
  B_0 = 1 (connected)
  B_k = 0 for 0 < k < n
  B_n = 1 (the fundamental class)")
  (sc-boundary-of-simplex (+ n 1)))

(define (make-torus)
  (doc 'type '(-> SC))
  (doc 'description "Create a triangulation of the torus T^2 = S^1 x S^1")
  (doc 'note "Uses a 3x3 grid with opposite edges identified. 9 vertices, 27 edges, 18 triangles")
  (doc 'note "Expected Betti numbers (over Z_2): (1 2 1)
  B_0 = 1 (connected)
  B_1 = 2 (two independent loops: meridian and longitude)
  B_2 = 1 (the surface bounds a void)")
  (doc 'note "Euler characteristic: X = 9 - 27 + 18 = 0 = 1 - 2 + 1")
  (doc 'note "Vertex layout (3x3 grid with wrap-around):
  0-1-2-0
  |\\|\\|\\|
  3-4-5-3
  |\\|\\|\\|
  6-7-8-6
  |\\|\\|\\|
  0-1-2-0")
  ; 3x3 grid torus: 9 vertices, 18 triangles
  ; Each unit square is split into 2 triangles
  ; Indices wrap: column mod 3, row mod 3
  (sc-from-simplices
    (map (lambda (vs) (make-simplex vs))
         '(; Row 0 squares (top row)
           (0 1 4) (0 3 4)      ; square (0,0)-(1,0)
           (1 2 5) (1 4 5)      ; square (1,0)-(2,0)
           (2 0 3) (2 5 3)      ; square (2,0)-(0,0) wraps right
           ; Row 1 squares (middle row)
           (3 4 7) (3 6 7)      ; square (0,1)-(1,1)
           (4 5 8) (4 7 8)      ; square (1,1)-(2,1)
           (5 3 6) (5 8 6)      ; square (2,1)-(0,1) wraps right
           ; Row 2 squares (bottom row, wraps to top)
           (6 7 1) (6 0 1)      ; square (0,2)-(1,0) wraps bottom
           (7 8 2) (7 1 2)      ; square (1,2)-(2,0) wraps bottom
           (8 6 0) (8 2 0)))))  ; corner wrap: (2,2) to (0,0)

(define (make-klein-bottle)
  (doc 'type '(-> SC))
  (doc 'description "Triangulation of the Klein bottle")
  (doc 'note "Expected Betti numbers (over Z_2): (1 2 1)")
  (doc 'note "Over Z, B_2 = 0 because Klein bottle is non-orientable. Over Z_2, we get B_2 = 1 because Z_2 doesn't see orientation")
  ; 9-vertex triangulation similar to torus but with twisted gluing
  ; Top edge glued to bottom with orientation reversal
  (sc-from-simplices
    (map (lambda (vs) (make-simplex vs))
         '(; Row 0 squares
           (0 1 4) (0 3 4)
           (1 2 5) (1 4 5)
           (2 0 3) (2 5 3)
           ; Row 1 squares
           (3 4 7) (3 6 7)
           (4 5 8) (4 7 8)
           (5 3 6) (5 8 6)
           ; Row 2 squares (twisted gluing: 6->2, 7->1, 8->0)
           (6 7 1) (6 2 1)
           (7 8 0) (7 1 0)
           (8 6 2) (8 0 2)))))

(define (make-projective-plane)
  (doc 'type '(-> SC))
  (doc 'description "Triangulation of the real projective plane RP^2")
  (doc 'note "Expected Betti numbers (over Z_2): (1 1 1). Over Z: (1 0 0) because RP^2 is non-orientable")
  ; 6-vertex triangulation (minimal)
  (sc-from-simplices
    (map (lambda (vs) (make-simplex vs))
         '((0 1 2) (0 2 3) (0 3 4) (0 4 5) (0 1 5)
           (1 2 5) (2 3 5) (3 4 5) (1 3 4) (1 2 4)))))

(doc 'section 'connected-components)

(define (sc-connected-components sc)
  (doc 'type '(-> SC Integer))
  (doc 'description "Count connected components using union-find")
  (doc 'note "This gives the same result as B_0 but via direct computation")
  (let* ([vertices (sc-vertices sc)]
         [n (length vertices)]
         [parent (make-vector n)]
         [rank-vec (make-vector n 0)])
    ; Initialize: each vertex is its own component
    (do ([i 0 (+ i 1)])
        [(= i n)]
      (vector-set! parent i i))
    ; Define find and union using letrec
    (letrec
      ([find (lambda (x)
               (if (= (vector-ref parent x) x)
                   x
                   (let ([root (find (vector-ref parent x))])
                     (vector-set! parent x root)
                     root)))]
       [union (lambda (x y)
                (let ([px (find x)]
                      [py (find y)])
                  (unless (= px py)
                    (let ([rx (vector-ref rank-vec px)]
                          [ry (vector-ref rank-vec py)])
                      (cond
                        [(< rx ry) (vector-set! parent px py)]
                        [(> rx ry) (vector-set! parent py px)]
                        [else
                         (vector-set! parent py px)
                         (vector-set! rank-vec px (+ rx 1))])))))])
      ; Process edges
      (for-each
        (lambda (edge)
          (let* ([vs (simplex-vertices edge)]
                 [v1-idx (list-index (car vs) vertices)]
                 [v2-idx (list-index (cadr vs) vertices)])
            (when (and v1-idx v2-idx)
              (union v1-idx v2-idx))))
        (sc-edges sc))
      ; Count distinct roots
      (let ([roots (make-vector n #f)])
        (do ([i 0 (+ i 1)])
            [(= i n)]
          (vector-set! roots (find i) #t))
        (let loop ([i 0] [count 0])
          (if (= i n)
              count
              (loop (+ i 1) (if (vector-ref roots i) (+ count 1) count))))))))

(define (list-index x lst)
  (doc 'type '(-> α (List α) (Or Integer #f)))
  (let loop ([remaining lst] [idx 0])
    (cond
      [(null? remaining) #f]
      [(equal? x (car remaining)) idx]
      [else (loop (cdr remaining) (+ idx 1))])))

(doc 'section 'debugging-visualization)

(define (z2-matrix-print m)
  (doc 'type '(-> Z2Matrix Void))
  (doc 'description "Print a Z_2 matrix for debugging")
  (let ([rows (z2-matrix-rows m)]
        [cols (z2-matrix-cols m)])
    (printf "~ax~a Z_2 matrix:~n" rows cols)
    (do ([i 0 (+ i 1)])
        [(= i rows)]
      (printf "  [")
      (do ([j 0 (+ j 1)])
          [(= j cols)]
        (printf " ~a" (z2-matrix-ref m i j)))
      (printf " ]~n"))))

(define (sc-boundary-matrix-print sc k)
  (doc 'type '(-> SC Integer Void))
  (doc 'description "Print a boundary matrix with simplex labels")
  (let* ([domain (sc-simplices-dim sc k)]
         [codomain (sc-simplices-dim sc (- k 1))]
         [mat (sc-boundary-matrix sc k)])
    (printf "~nd_~a : C_~a -> C_~a~n" k k (- k 1))
    (printf "Domain (~a-simplices):   ~a~n" k
            (map simplex-vertices domain))
    (printf "Codomain (~a-simplices): ~a~n" (- k 1)
            (map simplex-vertices codomain))
    (z2-matrix-print mat)))
