;;; lattice/linalg/integer-matrix.ss — Integer Matrix Normal Forms
;;; @module integer-matrix
;;; @requires prelude matrix modular
;;;
;;; Smith and Hermite normal forms for integer matrices.
;;; Essential for:
;;;   - Solving Diophantine equations
;;;   - Computing homology groups (computational topology)
;;;   - Integer linear programming
;;;   - Module theory over Z
;;;
;;; This is Lattice code: pure, total, decomposes to Fold primitives.

(require 'prelude)
(require 'matrix)
(require 'modular)

(doc 'module 'integer-matrix)
(doc 'description "Smith and Hermite normal forms for integer matrices")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ============================================================================
;;; Internal Matrix Operations (Mutable for Efficiency)
;;; ============================================================================

;;; These operations work on raw vectors for efficiency during the algorithm.
;;; The public API returns immutable matrix structures.

;;; int-mat-ref : Vec × Int × Int × Int → Int
;;; Get element at (i,j) from flat vector with given column count.
(define (int-mat-ref data cols i j)
  (vector-ref data (+ (* i cols) j)))

;;; int-mat-set! : Vec × Int × Int × Int × Int → Void
;;; Set element at (i,j) in flat vector.
(define (int-mat-set! data cols i j val)
  (vector-set! data (+ (* i cols) j) val))

;;; int-mat-copy : Vec → Vec
;;; Copy a vector.
(define (int-mat-copy v)
  (let* ([n (vector-length v)]
         [result (make-vector n 0)])
    (do ([i 0 (+ i 1)])
        ((= i n) result)
      (vector-set! result i (vector-ref v i)))))

;;; ============================================================================
;;; Row and Column Operations
;;; ============================================================================

;;; swap-rows! : Vec × Int × Int × Int × Int → Void
;;; Swap rows i1 and i2.
(define (swap-rows! data rows cols i1 i2)
  (when (not (= i1 i2))
    (do ([j 0 (+ j 1)])
        ((= j cols))
      (let ([tmp (int-mat-ref data cols i1 j)])
        (int-mat-set! data cols i1 j (int-mat-ref data cols i2 j))
        (int-mat-set! data cols i2 j tmp)))))

;;; swap-cols! : Vec × Int × Int × Int × Int → Void
;;; Swap columns j1 and j2.
(define (swap-cols! data rows cols j1 j2)
  (when (not (= j1 j2))
    (do ([i 0 (+ i 1)])
        ((= i rows))
      (let ([tmp (int-mat-ref data cols i j1)])
        (int-mat-set! data cols i j1 (int-mat-ref data cols i j2))
        (int-mat-set! data cols i j2 tmp)))))

;;; add-row-multiple! : Vec × Int × Int × Int × Int × Int → Void
;;; Row i1 += k * Row i2
(define (add-row-multiple! data rows cols i1 i2 k)
  (do ([j 0 (+ j 1)])
      ((= j cols))
    (int-mat-set! data cols i1 j
                  (+ (int-mat-ref data cols i1 j)
                     (* k (int-mat-ref data cols i2 j))))))

;;; add-col-multiple! : Vec × Int × Int × Int × Int × Int → Void
;;; Col j1 += k * Col j2
(define (add-col-multiple! data rows cols j1 j2 k)
  (do ([i 0 (+ i 1)])
      ((= i rows))
    (int-mat-set! data cols i j1
                  (+ (int-mat-ref data cols i j1)
                     (* k (int-mat-ref data cols i j2))))))

;;; negate-row! : Vec × Int × Int × Int → Void
;;; Negate all elements in row i.
(define (negate-row! data rows cols i)
  (do ([j 0 (+ j 1)])
      ((= j cols))
    (int-mat-set! data cols i j (- (int-mat-ref data cols i j)))))

;;; negate-col! : Vec × Int × Int × Int → Void
;;; Negate all elements in column j.
(define (negate-col! data rows cols j)
  (do ([i 0 (+ i 1)])
      ((= i rows))
    (int-mat-set! data cols i j (- (int-mat-ref data cols i j)))))

;;; ============================================================================
;;; Smith Normal Form
;;; ============================================================================

(doc 'section 'smith-normal-form)

(doc 'note "Smith Normal Form: Given m×n integer matrix A, find unimodular")
(doc 'note "matrices U (m×m) and V (n×n) such that U*A*V = D where D is diagonal")
(doc 'note "with d_1 | d_2 | ... | d_r (each divides the next), and d_i > 0.")
(doc 'note "")
(doc 'note "The diagonal entries are the invariant factors of A.")
(doc 'note "The rank of A equals the number of non-zero diagonal entries.")

;;; smith-normal-form : Matrix Int → (List Matrix Matrix Matrix)
;;; Returns (U, D, V) where U*A*V = D.
;;; D is diagonal with invariant factors d_1 | d_2 | ... | d_r > 0.
(define (smith-normal-form mat)
  (doc 'export #t)
  (doc 'type '(-> (Matrix Int) (List (Matrix Int) (Matrix Int) (Matrix Int))))
  (doc 'description "Compute Smith normal form: returns (U, D, V) where U*A*V = D diagonal")
  (let* ([m (matrix-rows mat)]
         [n (matrix-cols mat)]
         [r (min m n)]
         ;; Work on copies
         [D-data (int-mat-copy (matrix-data mat))]
         [U-data (make-identity-vec m)]
         [V-data (make-identity-vec n)])
    ;; Process each diagonal position
    (let loop ([k 0])
      (if (>= k r)
          ;; Done - construct result matrices
          (list (list 'matrix m m U-data)
                (list 'matrix m n D-data)
                (list 'matrix n n V-data))
          ;; Process position k
          (begin
            ;; Find pivot and reduce
            (smith-reduce-position! D-data U-data V-data m n k)
            (loop (+ k 1)))))))

;;; make-identity-vec : Int → Vec
;;; Create identity matrix as flat vector.
(define (make-identity-vec n)
  (let ([data (make-vector (* n n) 0)])
    (do ([i 0 (+ i 1)])
        ((= i n) data)
      (vector-set! data (+ (* i n) i) 1))))

;;; smith-reduce-position! : Vec × Vec × Vec × Int × Int × Int → Void
;;; Reduce matrix at diagonal position k using extended GCD operations.
(define (smith-reduce-position! D U V m n k)
  ;; Step 1: Find smallest non-zero element in submatrix and move to (k,k)
  (let ([found (smith-find-pivot D m n k)])
    (when found
      (let ([pi (car found)]
            [pj (cdr found)])
        ;; Move pivot to (k,k)
        (swap-rows! D m n k pi)
        (swap-rows! U m m k pi)
        (swap-cols! D m n k pj)
        (swap-cols! V n n k pj)

        ;; Make pivot positive
        (when (< (int-mat-ref D n k k) 0)
          (negate-row! D m n k)
          (negate-row! U m m k))

        ;; Step 2: Eliminate row k and column k using GCD operations
        (smith-eliminate-row-col! D U V m n k)))))

;;; smith-find-pivot : Vec × Int × Int × Int → (Int . Int) | #f
;;; Find position of smallest non-zero element in submatrix starting at (k,k).
(define (smith-find-pivot D m n k)
  (let loop-i ([i k] [best #f] [best-val #f])
    (if (>= i m)
        best
        (let loop-j ([j k] [best best] [best-val best-val])
          (if (>= j n)
              (loop-i (+ i 1) best best-val)
              (let ([val (int-mat-ref D n i j)])
                (if (and (not (= val 0))
                         (or (not best-val)
                             (< (abs val) best-val)))
                    (loop-j (+ j 1) (cons i j) (abs val))
                    (loop-j (+ j 1) best best-val))))))))

;;; smith-eliminate-row-col! : Vec × Vec × Vec × Int × Int × Int → Void
;;; Use pivot at (k,k) to eliminate rest of row k and column k.
;;; May need multiple passes if elements aren't divisible by pivot.
(define (smith-eliminate-row-col! D U V m n k)
  (let loop ()
    (let ([changed #f]
          [pivot (int-mat-ref D n k k)])
      ;; Eliminate column k (rows below k)
      (do ([i (+ k 1) (+ i 1)])
          ((= i m))
        (let ([target (int-mat-ref D n i k)])
          (when (not (= target 0))
            (if (= (abs pivot) 1)
                ;; Simple case: pivot is ±1, just subtract multiple of pivot row
                (let ([factor (if (= pivot 1) target (- target))])
                  (add-row-multiple! D m n i k (- factor))
                  (add-row-multiple! U m m i k (- factor))
                  (set! changed #t))
                ;; General case: use extended GCD
                (let* ([result (extended-gcd pivot target)]
                       [g (car result)]
                       [s (cadr result)]
                       [t (caddr result)]
                       [pivot-factor (quotient pivot g)]
                       [target-factor (quotient target g)])
                  (smith-row-gcd-transform! D U m n k i s t (- target-factor) pivot-factor)
                  (set! changed #t))))))

      ;; Eliminate row k (columns right of k)
      ;; Re-read pivot in case it changed
      (set! pivot (int-mat-ref D n k k))
      (do ([j (+ k 1) (+ j 1)])
          ((= j n))
        (let ([target (int-mat-ref D n k j)])
          (when (not (= target 0))
            (if (= (abs pivot) 1)
                ;; Simple case: pivot is ±1, just subtract multiple of pivot col
                (let ([factor (if (= pivot 1) target (- target))])
                  (add-col-multiple! D m n j k (- factor))
                  (add-col-multiple! V n n j k (- factor))
                  (set! changed #t))
                ;; General case: use extended GCD
                (let* ([result (extended-gcd pivot target)]
                       [g (car result)]
                       [s (cadr result)]
                       [t (caddr result)]
                       [pivot-factor (quotient pivot g)]
                       [target-factor (quotient target g)])
                  (smith-col-gcd-transform! D V m n k j s t (- target-factor) pivot-factor)
                  (set! changed #t))))))

      ;; Check if any element in submatrix is not divisible by pivot
      ;; If so, add that row/col to pivot position and repeat
      (let ([pivot (int-mat-ref D n k k)])
        (when (and (not (= pivot 0)) (smith-has-non-divisible? D m n k pivot))
          ;; Find a non-divisible element and use it
          (let ([pos (smith-find-non-divisible D m n k pivot)])
            (when pos
              (let ([i (car pos)]
                    [j (cdr pos)])
                ;; Add row i to row k to create a smaller GCD
                (add-row-multiple! D m n k i 1)
                (add-row-multiple! U m m k i 1)
                (set! changed #t))))))

      ;; Keep iterating until no changes
      (when changed (loop)))))

;;; smith-row-gcd-transform! : Vec × Vec × Int × Int × Int × Int × Int × Int × Int × Int → Void
;;; Apply GCD-based row transformation.
;;; [new_row_k]   [a  b] [row_k]
;;; [new_row_i] = [c  d] [row_i]
(define (smith-row-gcd-transform! D U m n k i a b c d)
  ;; Transform D
  (do ([j 0 (+ j 1)])
      ((= j n))
    (let ([rk (int-mat-ref D n k j)]
          [ri (int-mat-ref D n i j)])
      (int-mat-set! D n k j (+ (* a rk) (* b ri)))
      (int-mat-set! D n i j (+ (* c rk) (* d ri)))))
  ;; Transform U (same row ops)
  (do ([j 0 (+ j 1)])
      ((= j m))
    (let ([rk (int-mat-ref U m k j)]
          [ri (int-mat-ref U m i j)])
      (int-mat-set! U m k j (+ (* a rk) (* b ri)))
      (int-mat-set! U m i j (+ (* c rk) (* d ri))))))

;;; smith-col-gcd-transform! : Vec × Vec × Int × Int × Int × Int × Int × Int × Int × Int → Void
;;; Apply GCD-based column transformation.
;;; [new_col_k, new_col_j] = [col_k, col_j] [a  c]
;;;                                         [b  d]
(define (smith-col-gcd-transform! D V m n k j a b c d)
  ;; Transform D
  (do ([i 0 (+ i 1)])
      ((= i m))
    (let ([ck (int-mat-ref D n i k)]
          [cj (int-mat-ref D n i j)])
      (int-mat-set! D n i k (+ (* a ck) (* b cj)))
      (int-mat-set! D n i j (+ (* c ck) (* d cj)))))
  ;; Transform V (same col ops)
  (do ([i 0 (+ i 1)])
      ((= i n))
    (let ([ck (int-mat-ref V n i k)]
          [cj (int-mat-ref V n i j)])
      (int-mat-set! V n i k (+ (* a ck) (* b cj)))
      (int-mat-set! V n i j (+ (* c ck) (* d cj))))))

;;; smith-has-non-divisible? : Vec × Int × Int × Int × Int → Boolean
;;; Check if any element in submatrix (below and right of k,k) is not divisible by pivot.
(define (smith-has-non-divisible? D m n k pivot)
  (let loop-i ([i (+ k 1)])
    (if (>= i m)
        #f
        (let loop-j ([j (+ k 1)])
          (if (>= j n)
              (loop-i (+ i 1))
              (let ([val (int-mat-ref D n i j)])
                (if (not (= 0 (modulo val pivot)))
                    #t
                    (loop-j (+ j 1)))))))))

;;; smith-find-non-divisible : Vec × Int × Int × Int × Int → (Int . Int) | #f
;;; Find position of element not divisible by pivot.
(define (smith-find-non-divisible D m n k pivot)
  (let loop-i ([i (+ k 1)])
    (if (>= i m)
        #f
        (let loop-j ([j (+ k 1)])
          (if (>= j n)
              (loop-i (+ i 1))
              (let ([val (int-mat-ref D n i j)])
                (if (not (= 0 (modulo val pivot)))
                    (cons i j)
                    (loop-j (+ j 1)))))))))

;;; ============================================================================
;;; Hermite Normal Form
;;; ============================================================================

(doc 'section 'hermite-normal-form)

(doc 'note "Hermite Normal Form (HNF): Given m×n integer matrix A, find")
(doc 'note "unimodular U (m×m) such that H = U*A is in row echelon form:")
(doc 'note "  1. All pivot elements are positive")
(doc 'note "  2. Elements above each pivot are non-negative and < pivot")
(doc 'note "  3. Rows of zeros are at the bottom")
(doc 'note "")
(doc 'note "HNF is unique for a given matrix (unlike Smith form).")

;;; hermite-normal-form : Matrix Int → (List Matrix Matrix)
;;; Returns (U, H) where H = U*A is in Hermite normal form.
(define (hermite-normal-form mat)
  (doc 'export #t)
  (doc 'type '(-> (Matrix Int) (List (Matrix Int) (Matrix Int))))
  (doc 'description "Compute Hermite normal form: returns (U, H) where H = U*A")
  (let* ([m (matrix-rows mat)]
         [n (matrix-cols mat)]
         [H-data (int-mat-copy (matrix-data mat))]
         [U-data (make-identity-vec m)])
    ;; Process each column
    (let loop ([col 0] [pivot-row 0])
      (if (or (>= col n) (>= pivot-row m))
          ;; Done - construct result
          (list (list 'matrix m m U-data)
                (list 'matrix m n H-data))
          ;; Find pivot in current column
          (let ([pivot-idx (hermite-find-pivot H-data m n col pivot-row)])
            (if (not pivot-idx)
                ;; No pivot in this column, move to next
                (loop (+ col 1) pivot-row)
                (begin
                  ;; Move pivot to pivot-row
                  (swap-rows! H-data m n pivot-row pivot-idx)
                  (swap-rows! U-data m m pivot-row pivot-idx)

                  ;; Make pivot positive
                  (when (< (int-mat-ref H-data n pivot-row col) 0)
                    (negate-row! H-data m n pivot-row)
                    (negate-row! U-data m m pivot-row))

                  ;; Eliminate below pivot using GCD
                  (hermite-eliminate-below! H-data U-data m n col pivot-row)

                  ;; Reduce elements above pivot
                  (hermite-reduce-above! H-data U-data m n col pivot-row)

                  (loop (+ col 1) (+ pivot-row 1)))))))))

;;; hermite-find-pivot : Vec × Int × Int × Int × Int → Int | #f
;;; Find row index of first non-zero element in column col, starting from start-row.
(define (hermite-find-pivot H m n col start-row)
  (let loop ([i start-row])
    (cond
      [(>= i m) #f]
      [(not (= 0 (int-mat-ref H n i col))) i]
      [else (loop (+ i 1))])))

;;; hermite-eliminate-below! : Vec × Vec × Int × Int × Int × Int → Void
;;; Eliminate elements below the pivot using GCD-based row operations.
(define (hermite-eliminate-below! H U m n col pivot-row)
  (do ([i (+ pivot-row 1) (+ i 1)])
      ((= i m))
    (let ([pivot (int-mat-ref H n pivot-row col)]
          [target (int-mat-ref H n i col)])
      (when (not (= target 0))
        ;; Use extended GCD
        (let* ([result (extended-gcd pivot target)]
               [g (car result)]
               [s (cadr result)]
               [t (caddr result)]
               [pivot-factor (quotient pivot g)]
               [target-factor (quotient target g)])
          ;; Transform rows
          (smith-row-gcd-transform! H U m n pivot-row i s t (- target-factor) pivot-factor))))))

;;; hermite-reduce-above! : Vec × Vec × Int × Int × Int × Int → Void
;;; Reduce elements above pivot to be in range [0, pivot).
(define (hermite-reduce-above! H U m n col pivot-row)
  (let ([pivot (int-mat-ref H n pivot-row col)])
    (do ([i 0 (+ i 1)])
        ((= i pivot-row))
      (let* ([val (int-mat-ref H n i col)]
             [q (floor (/ val pivot))])
        (when (not (= q 0))
          ;; row_i -= q * row_pivot
          (add-row-multiple! H m n i pivot-row (- q))
          (add-row-multiple! U m m i pivot-row (- q)))))))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(doc 'section 'utilities)

;;; smith-rank : Matrix Int → Int
;;; Compute rank of integer matrix using Smith form.
(define (smith-rank mat)
  (doc 'export #t)
  (doc 'type '(-> (Matrix Int) Int))
  (doc 'description "Compute matrix rank via Smith normal form")
  (let* ([result (smith-normal-form mat)]
         [D (cadr result)]
         [m (matrix-rows D)]
         [n (matrix-cols D)]
         [r (min m n)]
         [data (matrix-data D)])
    (let loop ([i 0] [count 0])
      (if (>= i r)
          count
          (if (= 0 (vector-ref data (+ (* i n) i)))
              count  ; Found first zero on diagonal
              (loop (+ i 1) (+ count 1)))))))

;;; smith-invariant-factors : Matrix Int → (List Int)
;;; Extract the invariant factors (diagonal of Smith form).
(define (smith-invariant-factors mat)
  (doc 'export #t)
  (doc 'type '(-> (Matrix Int) (List Int)))
  (doc 'description "Extract invariant factors from Smith normal form")
  (let* ([result (smith-normal-form mat)]
         [D (cadr result)]
         [m (matrix-rows D)]
         [n (matrix-cols D)]
         [r (min m n)]
         [data (matrix-data D)])
    (let loop ([i 0] [factors '()])
      (if (>= i r)
          (reverse factors)
          (let ([d (vector-ref data (+ (* i n) i))])
            (if (= d 0)
                (reverse factors)
                (loop (+ i 1) (cons d factors))))))))

;;; solve-linear-diophantine : Matrix Int × Vec Int → (List Vec) | 'no-solution
;;; Solve A*x = b over integers using Smith normal form.
;;; Returns list of (particular solution, basis of null space) or 'no-solution.
(define (solve-linear-diophantine A b)
  (doc 'export #t)
  (doc 'type '(-> (Matrix Int) (Vec Int) (Union (List (Vec Int)) Symbol)))
  (doc 'description "Solve integer system Ax = b using Smith normal form")
  (let* ([result (smith-normal-form A)]
         [U (car result)]
         [D (cadr result)]
         [V (caddr result)]
         [m (matrix-rows A)]
         [n (matrix-cols A)]
         ;; Compute U*b
         [Ub (matrix-vec-mul U b)])
    ;; Check solvability: for each row i, d_i must divide (Ub)_i
    (if (diophantine-solvable? D Ub m n)
        ;; Compute particular solution
        (let* ([y (compute-diophantine-y D Ub m n)]
               [x (matrix-vec-mul V y)])
          (list x (compute-null-space V D n)))
        'no-solution)))

;;; diophantine-solvable? : Matrix × Vec × Int × Int → Boolean
(define (diophantine-solvable? D Ub m n)
  (let ([r (min m n)]
        [D-data (matrix-data D)])
    (let loop ([i 0])
      (cond
        [(>= i m) #t]
        [(>= i r)
         ;; For rows beyond diagonal, Ub must be 0
         (if (= 0 (vector-ref Ub i))
             (loop (+ i 1))
             #f)]
        [else
         (let ([d (vector-ref D-data (+ (* i n) i))]
               [ub (vector-ref Ub i)])
           (if (= d 0)
               (if (= ub 0)
                   (loop (+ i 1))
                   #f)
               (if (= 0 (modulo ub d))
                   (loop (+ i 1))
                   #f)))]))))

;;; compute-diophantine-y : Matrix × Vec × Int × Int → Vec
(define (compute-diophantine-y D Ub m n)
  (let ([y (make-vector n 0)]
        [r (min m n)]
        [D-data (matrix-data D)])
    (do ([i 0 (+ i 1)])
        ((= i r) y)
      (let ([d (vector-ref D-data (+ (* i n) i))])
        (when (not (= d 0))
          (vector-set! y i (quotient (vector-ref Ub i) d)))))))

;;; compute-null-space : Matrix × Matrix × Int → (List Vec)
;;; Compute null space basis from V and D.
(define (compute-null-space V D n)
  ;; Columns of V corresponding to zero diagonal entries form null space basis
  (let ([r (min (matrix-rows D) (matrix-cols D))]
        [D-data (matrix-data D)]
        [V-data (matrix-data V)]
        [D-cols (matrix-cols D)])
    (let loop ([j 0] [basis '()])
      (cond
        [(>= j n) (reverse basis)]
        ;; If j >= r or d_j = 0, column j of V is in null space
        [(or (>= j r) (= 0 (vector-ref D-data (+ (* j D-cols) j))))
         (loop (+ j 1) (cons (matrix-col V j) basis))]
        [else (loop (+ j 1) basis)]))))

;;; ============================================================================
;;; Integer Kernel and Image
;;; ============================================================================

;;; integer-kernel : Matrix Int → (List Vec Int)
;;; Compute a basis for the integer kernel of A (null space over Z).
(define (integer-kernel A)
  (doc 'export #t)
  (doc 'type '(-> (Matrix Int) (List (Vec Int))))
  (doc 'description "Compute basis for integer kernel (null space) of matrix")
  (let* ([result (smith-normal-form A)]
         [V (caddr result)]
         [D (cadr result)]
         [n (matrix-cols A)])
    (compute-null-space V D n)))

;;; integer-image : Matrix Int → (List Vec Int)
;;; Compute a basis for the integer image of A (column space over Z).
(define (integer-image A)
  (doc 'export #t)
  (doc 'type '(-> (Matrix Int) (List (Vec Int))))
  (doc 'description "Compute basis for integer image (column space) of matrix")
  (let* ([result (smith-normal-form A)]
         [U (car result)]
         [D (cadr result)]
         [m (matrix-rows A)]
         [U-inv (unimodular-inverse U)])
    ;; Columns of U^-1 corresponding to non-zero diagonal entries
    (let ([r (min (matrix-rows D) (matrix-cols D))]
          [D-data (matrix-data D)]
          [D-cols (matrix-cols D)])
      (let loop ([j 0] [basis '()])
        (cond
          [(>= j r) (reverse basis)]
          [(= 0 (vector-ref D-data (+ (* j D-cols) j)))
           (reverse basis)]  ; Reached zero diagonal
          [else
           (loop (+ j 1) (cons (matrix-col U-inv j) basis))])))))

;;; unimodular-inverse : Matrix Int → Matrix Int
;;; Compute inverse of unimodular matrix (det = ±1).
;;; For unimodular matrices, the inverse is also an integer matrix.
(define (unimodular-inverse U)
  (doc 'export #t)
  (doc 'type '(-> (Matrix Int) (Matrix Int)))
  (doc 'description "Compute inverse of unimodular matrix")
  ;; Use the adjugate formula: U^-1 = adj(U) / det(U)
  ;; For unimodular, det = ±1, so U^-1 = ±adj(U)
  (let* ([n (matrix-rows U)]
         [adj (matrix-adjugate U)]
         [det (matrix-determinant-int U)])
    (if (= det 1)
        adj
        (matrix-scale -1 adj))))

;;; matrix-determinant-int : Matrix Int → Int
;;; Compute determinant of integer matrix using Bareiss algorithm.
;;; This is a division-free algorithm that keeps intermediate results as integers.
(define (matrix-determinant-int mat)
  (doc 'export #t)
  (doc 'type '(-> (Matrix Int) Int))
  (doc 'description "Compute determinant using Bareiss algorithm")
  (let* ([n (matrix-rows mat)])
    (if (= n 0)
        1  ; Empty matrix has det = 1
        (let ([data (int-mat-copy (matrix-data mat))]
              [sign 1])
          (let loop ([k 0] [prev-pivot 1])
            (if (>= k (- n 1))
                ;; Final element is the determinant
                (* sign (int-mat-ref data n (- n 1) (- n 1)))
                ;; Find pivot
                (let ([pivot-idx (let search ([i k])
                                  (cond
                                    [(>= i n) #f]
                                    [(not (= 0 (int-mat-ref data n i k))) i]
                                    [else (search (+ i 1))]))])
                  (if (not pivot-idx)
                      0  ; Zero column means det = 0
                      (begin
                        (when (not (= pivot-idx k))
                          (swap-rows! data n n k pivot-idx)
                          (set! sign (- sign)))
                        ;; Bareiss elimination
                        (let ([pivot (int-mat-ref data n k k)])
                          (do ([i (+ k 1) (+ i 1)])
                              ((= i n))
                            (do ([j (+ k 1) (+ j 1)])
                                ((= j n))
                              (let ([new-val (quotient
                                              (- (* pivot (int-mat-ref data n i j))
                                                 (* (int-mat-ref data n i k)
                                                    (int-mat-ref data n k j)))
                                              prev-pivot)])
                                (int-mat-set! data n i j new-val))))
                          (loop (+ k 1) pivot)))))))))))

;;; matrix-adjugate : Matrix Int → Matrix Int
;;; Compute adjugate (classical adjoint) of matrix.
(define (matrix-adjugate mat)
  (let* ([n (matrix-rows mat)]
         [result (make-vector (* n n) 0)])
    (do ([i 0 (+ i 1)])
        ((= i n) (list 'matrix n n result))
      (do ([j 0 (+ j 1)])
          ((= j n))
        ;; Adjugate[i,j] = (-1)^(i+j) * det(minor(j,i))
        (let* ([minor (matrix-minor mat j i)]
               [minor-det (matrix-determinant-int minor)]
               [sign (if (even? (+ i j)) 1 -1)])
          (vector-set! result (+ (* i n) j) (* sign minor-det)))))))

;;; matrix-minor : Matrix × Int × Int → Matrix
;;; Return matrix with row i and column j removed.
(define (matrix-minor mat i j)
  (let* ([n (matrix-rows mat)]
         [data (matrix-data mat)]
         [new-n (- n 1)]
         [result (make-vector (* new-n new-n) 0)])
    (do ([row 0 (+ row 1)]
         [new-row 0 (if (= row i) new-row (+ new-row 1))])
        ((= row n) (list 'matrix new-n new-n result))
      (when (not (= row i))
        (do ([col 0 (+ col 1)]
             [new-col 0 (if (= col j) new-col (+ new-col 1))])
            ((= col n))
          (when (not (= col j))
            (vector-set! result (+ (* new-row new-n) new-col)
                        (vector-ref data (+ (* row n) col)))))))))
