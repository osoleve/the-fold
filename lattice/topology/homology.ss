;;; lattice/topology/homology.ss -- Homology Computation
;;;
;;; Computes homology groups and Betti numbers for simplicial complexes.
;;;
;;; Homology measures "holes" in topological spaces:
;;;   B_0 = number of connected components
;;;   B_1 = number of "1-dimensional holes" (loops that aren't boundaries)
;;;   B_2 = number of "2-dimensional holes" (voids)
;;;   ...
;;;
;;; We work with Z_2 coefficients (mod 2 arithmetic) which:
;;;   - Avoids integer coefficient tracking (signs don't matter mod 2)
;;;   - Simplifies Gaussian elimination (only 0 and 1)
;;;   - Is standard in computational topology/TDA
;;;
;;; The key insight: d*d = 0 (boundary of a boundary is empty)
;;; This gives us a chain complex:
;;;   ... -> C_k+1 --d_{k+1}--> C_k --d_k--> C_{k-1} -> ...
;;;
;;; Homology H_k = ker(d_k) / im(d_{k+1})
;;; Betti number B_k = dim(H_k) = nullity(d_k) - rank(d_{k+1})
;;;
;;; TIER: 1 (depends on topology/simplicial-complex)

(load "lattice/topology/simplicial-complex.ss")

;;; ============================================================
;;; Z_2 MATRIX OPERATIONS (mod 2 arithmetic)
;;; ============================================================

;;; We represent Z_2 matrices as vectors of row vectors.
;;; Each entry is 0 or 1.

;;; z2-matrix : rows x cols x data -> Z2Matrix
;;; Create a Z_2 matrix from row-major data.
(define (z2-matrix rows cols data)
  (list 'z2-matrix rows cols data))

;;; z2-matrix? : a -> Boolean
(define (z2-matrix? x)
  (and (pair? x) (eq? (car x) 'z2-matrix)))

;;; z2-matrix-rows : Z2Matrix -> Integer
(define (z2-matrix-rows m)
  (cadr m))

;;; z2-matrix-cols : Z2Matrix -> Integer
(define (z2-matrix-cols m)
  (caddr m))

;;; z2-matrix-data : Z2Matrix -> Vector
(define (z2-matrix-data m)
  (cadddr m))

;;; z2-matrix-ref : Z2Matrix x Integer x Integer -> {0, 1}
(define (z2-matrix-ref m i j)
  (vector-ref (z2-matrix-data m) (+ (* i (z2-matrix-cols m)) j)))

;;; z2-matrix-set! : Z2Matrix x Integer x Integer x {0, 1} -> Void
;;; Mutate matrix entry (used during Gaussian elimination).
(define (z2-matrix-set! m i j v)
  (vector-set! (z2-matrix-data m) (+ (* i (z2-matrix-cols m)) j) (mod v 2)))

;;; z2-matrix-zero : Integer x Integer -> Z2Matrix
;;; Create a zero matrix.
(define (z2-matrix-zero rows cols)
  (z2-matrix rows cols (make-vector (* rows cols) 0)))

;;; z2-matrix-copy : Z2Matrix -> Z2Matrix
;;; Create a copy of a matrix (for non-destructive operations).
(define (z2-matrix-copy m)
  (z2-matrix (z2-matrix-rows m)
             (z2-matrix-cols m)
             (vector-copy (z2-matrix-data m))))

;;; z2-matrix-swap-rows! : Z2Matrix x Integer x Integer -> Void
;;; Swap two rows in place.
(define (z2-matrix-swap-rows! m i j)
  (let ([cols (z2-matrix-cols m)])
    (do ([k 0 (+ k 1)])
        [(= k cols)]
      (let ([tmp (z2-matrix-ref m i k)])
        (z2-matrix-set! m i k (z2-matrix-ref m j k))
        (z2-matrix-set! m j k tmp)))))

;;; z2-matrix-add-row! : Z2Matrix x Integer x Integer -> Void
;;; Add row j to row i (mod 2), storing result in row i.
(define (z2-matrix-add-row! m i j)
  (let ([cols (z2-matrix-cols m)])
    (do ([k 0 (+ k 1)])
        [(= k cols)]
      (z2-matrix-set! m i k (+ (z2-matrix-ref m i k) (z2-matrix-ref m j k))))))

;;; ============================================================
;;; GAUSSIAN ELIMINATION OVER Z_2
;;; ============================================================

;;; z2-rank : Z2Matrix -> Integer
;;; Compute the rank of a Z_2 matrix using Gaussian elimination.
;;; This is the dimension of the image (column space).
(define (z2-rank matrix)
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

;;; z2-nullity : Z2Matrix -> Integer
;;; Compute the nullity (dimension of kernel) of a Z_2 matrix.
;;; nullity = cols - rank (by rank-nullity theorem)
(define (z2-nullity m)
  (- (z2-matrix-cols m) (z2-rank m)))

;;; ============================================================
;;; BOUNDARY MATRICES
;;; ============================================================

;;; simplex-index : (List Simplex) x Simplex -> Integer | #f
;;; Find the index of a simplex in a list (for matrix indexing).
;;; NOTE: O(n) linear scan - use build-simplex-index-table for bulk lookups.
(define (simplex-index simplices s)
  (let loop ([remaining simplices] [idx 0])
    (cond
      [(null? remaining) #f]
      [(simplex-equal? s (car remaining)) idx]
      [else (loop (cdr remaining) (+ idx 1))])))

;;; build-simplex-index-table : (List Simplex) -> HashTable
;;; Build a hash table mapping simplex vertices -> index for O(1) lookup.
;;; Key is the sorted vertex list (canonical form).
(define (build-simplex-index-table simplices)
  (let ([table (make-hashtable equal-hash equal?)])
    (let loop ([remaining simplices] [idx 0])
      (unless (null? remaining)
        (hashtable-set! table (simplex-vertices (car remaining)) idx)
        (loop (cdr remaining) (+ idx 1))))
    table))

;;; simplex-index-fast : HashTable x Simplex -> Integer | #f
;;; O(1) lookup using pre-built index table.
(define (simplex-index-fast table s)
  (hashtable-ref table (simplex-vertices s) #f))

;;; sc-boundary-matrix : SC x Integer -> Z2Matrix
;;; Build the k-th boundary matrix d_k : C_k -> C_{k-1}.
;;;
;;; Rows = (k-1)-simplices (codomain basis)
;;; Cols = k-simplices (domain basis)
;;; Entry (i,j) = 1 if sigma_i is a facet of tau_j
;;;
;;; For Z_2 coefficients, we ignore signs (+/-1 = 1 mod 2).
;;;
;;; Complexity: O(N * k) where N = number of k-simplices, k = simplex dimension
;;; Uses hash table for O(1) facet index lookups (was O(M) linear scan).
(define (sc-boundary-matrix sc k)
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

;;; ============================================================
;;; BETTI NUMBERS
;;; ============================================================

;;; sc-betti : SC x Integer -> Integer
;;; Compute the k-th Betti number of a simplicial complex.
;;;
;;; B_k = dim(ker d_k) - dim(im d_{k+1})
;;;     = nullity(d_k) - rank(d_{k+1})
;;;
;;; Special cases:
;;;   - B_k = 0 for k > max-dim
;;;   - B_0 counts connected components
(define (sc-betti sc k)
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

;;; sc-betti-numbers : SC -> (List Integer)
;;; Compute all Betti numbers from dimension 0 to max-dim.
(define (sc-betti-numbers sc)
  (let ([max-d (sc-max-dim sc)])
    (if (< max-d 0)
        '()
        (map (lambda (k) (sc-betti sc k))
             (iota (+ max-d 1))))))

;;; sc-homology-summary : SC -> Void
;;; Print a summary of the homology of a simplicial complex.
(define (sc-homology-summary sc)
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

;;; ============================================================
;;; STANDARD TOPOLOGICAL SPACES
;;; ============================================================

;;; make-sphere : Integer -> SC
;;; Create a triangulation of the n-sphere S^n.
;;; We use the boundary of the (n+1)-simplex, which is homeomorphic to S^n.
;;;
;;; Expected Betti numbers for S^n:
;;;   B_0 = 1 (connected)
;;;   B_k = 0 for 0 < k < n
;;;   B_n = 1 (the fundamental class)
(define (make-sphere n)
  (sc-boundary-of-simplex (+ n 1)))

;;; make-torus : -> SC
;;; Create a triangulation of the torus T^2 = S^1 x S^1.
;;; Uses a 3x3 grid with opposite edges identified.
;;; 9 vertices, 27 edges, 18 triangles.
;;;
;;; Expected Betti numbers (over Z_2): (1 2 1)
;;;   B_0 = 1 (connected)
;;;   B_1 = 2 (two independent loops: meridian and longitude)
;;;   B_2 = 1 (the surface bounds a void)
;;;
;;; Euler characteristic: X = 9 - 27 + 18 = 0 = 1 - 2 + 1
;;;
;;; Vertex layout (3x3 grid with wrap-around):
;;;   0-1-2-0
;;;   |\|\|\|
;;;   3-4-5-3
;;;   |\|\|\|
;;;   6-7-8-6
;;;   |\|\|\|
;;;   0-1-2-0
(define (make-torus)
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

;;; make-klein-bottle : -> SC
;;; Triangulation of the Klein bottle.
;;; Expected Betti numbers (over Z_2): (1 2 1)
;;; Note: Over Z, B_2 = 0 because Klein bottle is non-orientable.
;;; Over Z_2, we get B_2 = 1 because Z_2 doesn't see orientation.
(define (make-klein-bottle)
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

;;; make-projective-plane : -> SC
;;; Triangulation of the real projective plane RP^2.
;;; Expected Betti numbers (over Z_2): (1 1 1)
;;; Over Z: (1 0 0) because RP^2 is non-orientable.
(define (make-projective-plane)
  ; 6-vertex triangulation (minimal)
  (sc-from-simplices
    (map (lambda (vs) (make-simplex vs))
         '((0 1 2) (0 2 3) (0 3 4) (0 4 5) (0 1 5)
           (1 2 5) (2 3 5) (3 4 5) (1 3 4) (1 2 4)))))

;;; ============================================================
;;; CONNECTED COMPONENTS (B_0 alternative)
;;; ============================================================

;;; sc-connected-components : SC -> Integer
;;; Count connected components using union-find.
;;; This gives the same result as B_0 but via direct computation.
(define (sc-connected-components sc)
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

;;; list-index : a x (List a) -> Integer | #f
(define (list-index x lst)
  (let loop ([remaining lst] [idx 0])
    (cond
      [(null? remaining) #f]
      [(equal? x (car remaining)) idx]
      [else (loop (cdr remaining) (+ idx 1))])))

;;; ============================================================
;;; DEBUGGING AND VISUALIZATION
;;; ============================================================

;;; z2-matrix-print : Z2Matrix -> Void
;;; Print a Z_2 matrix for debugging.
(define (z2-matrix-print m)
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

;;; sc-boundary-matrix-print : SC x Integer -> Void
;;; Print a boundary matrix with simplex labels.
(define (sc-boundary-matrix-print sc k)
  (let* ([domain (sc-simplices-dim sc k)]
         [codomain (sc-simplices-dim sc (- k 1))]
         [mat (sc-boundary-matrix sc k)])
    (printf "~nd_~a : C_~a -> C_~a~n" k k (- k 1))
    (printf "Domain (~a-simplices):   ~a~n" k
            (map simplex-vertices domain))
    (printf "Codomain (~a-simplices): ~a~n" (- k 1)
            (map simplex-vertices codomain))
    (z2-matrix-print mat)))
