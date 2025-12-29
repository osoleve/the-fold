;;; fabric/stitches/matrix.ss — Matrix Operations
;;;
;;; Core matrix operations for linear algebra.
;;;
;;; Matrices are represented as (matrix rows cols data) where data is a
;;; flat vector in row-major order for cache efficiency.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - vec.ss

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/vec.ss")

;;; ============================================================
;;; Matrix Representation
;;; ============================================================

;;; A matrix is: (matrix rows cols data)
;;; - rows: number of rows (Nat)
;;; - cols: number of columns (Nat)
;;; - data: flat vector of size rows*cols in row-major order

;;; matrix? : Any → Boolean
(define (matrix? m)
  (and (pair? m)
       (eq? (car m) 'matrix)
       (= (length m) 4)
       (integer? (cadr m))
       (integer? (caddr m))
       (vector? (cadddr m))))

;;; matrix-rows : Matrix → Nat
(define (matrix-rows m)
  (cadr m))

;;; matrix-cols : Matrix → Nat
(define (matrix-cols m)
  (caddr m))

;;; matrix-data : Matrix → Vec
(define (matrix-data m)
  (cadddr m))

;;; matrix-shape : Matrix → (Nat × Nat)
(define (matrix-shape m)
  (cons (matrix-rows m) (matrix-cols m)))

;;; ============================================================
;;; Matrix Construction
;;; ============================================================

;;; make-matrix : Nat × Nat × a → Matrix a
;;; Create an m×n matrix with all elements set to init.
(define (make-matrix rows cols init)
  (list 'matrix rows cols (make-vector (* rows cols) init)))

;;; matrix-from-lists : (List (List a)) → Matrix a
;;; Create matrix from list of row lists.
(define (matrix-from-lists rows)
  (if (null? rows)
      (list 'matrix 0 0 (vector))
      (let* ([m (length rows)]
             [n (length (car rows))]
             [data (make-vector (* m n) 0)])
            (do ([i 0 (+ i 1)]
                 [rs rows (cdr rs)])
                ((= i m))
                (do ([j 0 (+ j 1)]
                     [cs (car rs) (cdr cs)])
                    ((= j n))
                    (vector-set! data (+ (* i n) j) (car cs))))
            (list 'matrix m n data))))

;;; matrix-from-vec : Nat × Nat × Vec a → Matrix a
;;; Create matrix from flat vector (row-major order).
(define (matrix-from-vec rows cols data)
  (if (= (vector-length data) (* rows cols))
      (list 'matrix rows cols (vec-copy data))
      `(error dimension-mismatch ,(vector-length data) ,(* rows cols))))

;;; ============================================================
;;; Matrix Accessors
;;; ============================================================

;;; matrix-ref : Matrix a × Nat × Nat → a | Error
;;; Get element at position (i, j).
(define (matrix-ref m i j)
  (let ([rows (matrix-rows m)]
        [cols (matrix-cols m)]
        [data (matrix-data m)])
       (if (and (>= i 0) (< i rows) (>= j 0) (< j cols))
           (vector-ref data (+ (* i cols) j))
           `(error out-of-bounds (,i ,j) (,rows ,cols)))))

;;; matrix-row : Matrix a × Nat → Vec a | Error
;;; Extract row i as a vector.
(define (matrix-row m i)
  (let ([rows (matrix-rows m)]
        [cols (matrix-cols m)]
        [data (matrix-data m)])
       (if (and (>= i 0) (< i rows))
           (let ([result (make-vector cols 0)]
                 [start (* i cols)])
                (do ([j 0 (+ j 1)])
                    ((= j cols) result)
                    (vector-set! result j (vector-ref data (+ start j)))))
           `(error out-of-bounds ,i ,rows))))

;;; matrix-col : Matrix a × Nat → Vec a | Error
;;; Extract column j as a vector.
(define (matrix-col m j)
  (let ([rows (matrix-rows m)]
        [cols (matrix-cols m)]
        [data (matrix-data m)])
       (if (and (>= j 0) (< j cols))
           (let ([result (make-vector rows 0)])
                (do ([i 0 (+ i 1)])
                    ((= i rows) result)
                    (vector-set! result i (vector-ref data (+ (* i cols) j)))))
           `(error out-of-bounds ,j ,cols))))

;;; matrix-diagonal : Matrix a → Vec a
;;; Extract main diagonal as a vector.
(define (matrix-diagonal m)
  (let* ([rows (matrix-rows m)]
         [cols (matrix-cols m)]
         [n (min rows cols)]
         [data (matrix-data m)]
         [result (make-vector n 0)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (vector-set! result i (vector-ref data (+ (* i cols) i))))))

;;; ============================================================
;;; Matrix Transformations
;;; ============================================================

;;; matrix-transpose : Matrix a → Matrix a
;;; Transpose a matrix (swap rows and columns).
(define (matrix-transpose m)
  (let* ([rows (matrix-rows m)]
         [cols (matrix-cols m)]
         [data (matrix-data m)]
         [result (make-vector (* rows cols) 0)])
        (do ([i 0 (+ i 1)])
            ((= i rows))
            (do ([j 0 (+ j 1)])
                ((= j cols))
                (vector-set! result (+ (* j rows) i)
                             (vector-ref data (+ (* i cols) j)))))
        (list 'matrix cols rows result)))

;;; matrix-map : (a → b) × Matrix a → Matrix b
;;; Apply function to each element.
(define (matrix-map f m)
  (let* ([rows (matrix-rows m)]
         [cols (matrix-cols m)]
         [data (matrix-data m)]
         [n (* rows cols)]
         [result (make-vector n 0)])
        (do ([i 0 (+ i 1)])
            ((= i n) (list 'matrix rows cols result))
            (vector-set! result i (f (vector-ref data i))))))

;;; matrix-map2 : (a × b → c) × Matrix a × Matrix b → Matrix c | Error
;;; Apply function to corresponding elements.
(define (matrix-map2 f m1 m2)
  (let ([r1 (matrix-rows m1)] [c1 (matrix-cols m1)]
        [r2 (matrix-rows m2)] [c2 (matrix-cols m2)])
       (if (not (and (= r1 r2) (= c1 c2)))
           `(error dimension-mismatch (,r1 ,c1) (,r2 ,c2))
           (let* ([data1 (matrix-data m1)]
                  [data2 (matrix-data m2)]
                  [n (* r1 c1)]
                  [result (make-vector n 0)])
                 (do ([i 0 (+ i 1)])
                     ((= i n) (list 'matrix r1 c1 result))
                     (vector-set! result i
                                  (f (vector-ref data1 i)
                                     (vector-ref data2 i))))))))

;;; matrix-fold : (b × a → b) × b × Matrix a → b
;;; Fold over all elements (row-major order).
(define (matrix-fold f init m)
  (vec-fold f init (matrix-data m)))

;;; ============================================================
;;; Matrix Arithmetic
;;; ============================================================

;;; matrix-add : Matrix Num × Matrix Num → Matrix Num | Error
;;; Element-wise addition.
(define (matrix-add m1 m2)
  (matrix-map2 + m1 m2))

;;; matrix-sub : Matrix Num × Matrix Num → Matrix Num | Error
;;; Element-wise subtraction.
(define (matrix-sub m1 m2)
  (matrix-map2 - m1 m2))

;;; matrix-hadamard : Matrix Num × Matrix Num → Matrix Num | Error
;;; Element-wise multiplication (Hadamard product).
(define (matrix-hadamard m1 m2)
  (matrix-map2 * m1 m2))

;;; matrix-scale : Num × Matrix Num → Matrix Num
;;; Scalar multiplication.
(define (matrix-scale k m)
  (matrix-map (lambda (x) (* k x)) m))

;;; matrix-negate : Matrix Num → Matrix Num
;;; Negate all elements.
(define (matrix-negate m)
  (matrix-map - m))

;;; ============================================================
;;; Matrix Multiplication
;;; ============================================================

;;; matrix-mul : Matrix Num × Matrix Num → Matrix Num | Error
;;; Matrix multiplication: A(m×n) * B(n×p) = C(m×p)
(define (matrix-mul m1 m2)
  (let ([r1 (matrix-rows m1)] [c1 (matrix-cols m1)]
        [r2 (matrix-rows m2)] [c2 (matrix-cols m2)])
       (if (not (= c1 r2))
           `(error dimension-mismatch (,r1 ,c1) (,r2 ,c2))
           (let* ([data1 (matrix-data m1)]
                  [data2 (matrix-data m2)]
                  [result (make-vector (* r1 c2) 0)])
                 (do ([i 0 (+ i 1)])
                     ((= i r1))
                     (do ([j 0 (+ j 1)])
                         ((= j c2))
                         (do ([k 0 (+ k 1)]
                              [sum 0 (+ sum (* (vector-ref data1 (+ (* i c1) k))
                                               (vector-ref data2 (+ (* k c2) j))))])
                             ((= k c1)
                              (vector-set! result (+ (* i c2) j) sum)))))
                 (list 'matrix r1 c2 result)))))

;;; matrix-vec-mul : Matrix Num × Vec Num → Vec Num | Error
;;; Matrix-vector multiplication: A(m×n) * v(n) = result(m)
(define (matrix-vec-mul m v)
  (let ([rows (matrix-rows m)]
        [cols (matrix-cols m)]
        [n (vector-length v)])
       (if (not (= cols n))
           `(error dimension-mismatch ,cols ,n)
           (let ([data (matrix-data m)]
                 [result (make-vector rows 0)])
                (do ([i 0 (+ i 1)])
                    ((= i rows) result)
                    (do ([j 0 (+ j 1)]
                         [sum 0 (+ sum (* (vector-ref data (+ (* i cols) j))
                                          (vector-ref v j)))])
                        ((= j cols)
                         (vector-set! result i sum))))))))

;;; vec-matrix-mul : Vec Num × Matrix Num → Vec Num | Error
;;; Vector-matrix multiplication: v(m) * A(m×n) = result(n)
(define (vec-matrix-mul v m)
  (let ([rows (matrix-rows m)]
        [cols (matrix-cols m)]
        [n (vector-length v)])
       (if (not (= rows n))
           `(error dimension-mismatch ,n ,rows)
           (let ([data (matrix-data m)]
                 [result (make-vector cols 0)])
                (do ([j 0 (+ j 1)])
                    ((= j cols) result)
                    (do ([i 0 (+ i 1)]
                         [sum 0 (+ sum (* (vector-ref v i)
                                          (vector-ref data (+ (* i cols) j))))])
                        ((= i rows)
                         (vector-set! result j sum))))))))

;;; ============================================================
;;; Matrix Comparisons
;;; ============================================================

;;; matrix-equal? : Matrix a × Matrix a → Boolean
;;; Exact equality.
(define (matrix-equal? m1 m2)
  (and (= (matrix-rows m1) (matrix-rows m2))
       (= (matrix-cols m1) (matrix-cols m2))
       (vec-equal? (matrix-data m1) (matrix-data m2))))

;;; matrix-approx-equal? : Matrix Num × Matrix Num × [Num] → Boolean
;;; Approximate equality within tolerance.
(define (matrix-approx-equal? m1 m2 . epsilon-arg)
  (let ([epsilon (if (null? epsilon-arg) 1e-10 (car epsilon-arg))])
       (and (= (matrix-rows m1) (matrix-rows m2))
            (= (matrix-cols m1) (matrix-cols m2))
            (vec-approx-equal? (matrix-data m1) (matrix-data m2) epsilon))))

;;; ============================================================
;;; Matrix Properties
;;; ============================================================

;;; matrix-square? : Matrix → Boolean
(define (matrix-square? m)
  (= (matrix-rows m) (matrix-cols m)))

;;; matrix-symmetric? : Matrix Num → Boolean
;;; Check if matrix equals its transpose.
(define (matrix-symmetric? m)
  (and (matrix-square? m)
       (matrix-equal? m (matrix-transpose m))))

;;; trace : Matrix Num → Num
;;; Sum of diagonal elements.
(define (trace m)
  (vec-sum (matrix-diagonal m)))

;;; frobenius-norm : Matrix Num → Num
;;; Frobenius norm (sqrt of sum of squares).
(define (frobenius-norm m)
  (sqrt (vec-fold (lambda (acc x) (+ acc (* x x))) 0 (matrix-data m))))

;;; ============================================================
;;; Special Matrices
;;; ============================================================

;;; zeros : Nat × Nat → Matrix Num
;;; Matrix of all zeros.
(define (zeros rows cols)
  (make-matrix rows cols 0))

;;; ones : Nat × Nat → Matrix Num
;;; Matrix of all ones.
(define (ones rows cols)
  (make-matrix rows cols 1))

;;; identity : Nat → Matrix Num
;;; n×n identity matrix.
(define (identity n)
  (let ([data (make-vector (* n n) 0)])
       (do ([i 0 (+ i 1)])
           ((= i n) (list 'matrix n n data))
           (vector-set! data (+ (* i n) i) 1))))

;;; diagonal : Vec Num → Matrix Num
;;; Create diagonal matrix from vector.
(define (diagonal v)
  (let* ([n (vector-length v)]
         [data (make-vector (* n n) 0)])
        (do ([i 0 (+ i 1)])
            ((= i n) (list 'matrix n n data))
            (vector-set! data (+ (* i n) i) (vector-ref v i)))))

;;; ============================================================
;;; Matrix Slicing
;;; ============================================================

;;; matrix-submatrix : Matrix × Nat × Nat × Nat × Nat → Matrix | Error
;;; Extract submatrix from (r1,c1) to (r2,c2) exclusive.
(define (matrix-submatrix m r1 c1 r2 c2)
  (let ([rows (matrix-rows m)]
        [cols (matrix-cols m)]
        [data (matrix-data m)])
       (cond
        [(or (< r1 0) (< c1 0)) `(error out-of-bounds (,r1 ,c1))]
        [(or (> r2 rows) (> c2 cols)) `(error out-of-bounds (,r2 ,c2))]
        [(or (> r1 r2) (> c1 c2)) `(error invalid-range)]
        [else
         (let* ([new-rows (- r2 r1)]
                [new-cols (- c2 c1)]
                [result (make-vector (* new-rows new-cols) 0)])
               (do ([i 0 (+ i 1)])
                   ((= i new-rows) (list 'matrix new-rows new-cols result))
                   (do ([j 0 (+ j 1)])
                       ((= j new-cols))
                       (vector-set! result (+ (* i new-cols) j)
                                    (vector-ref data (+ (* (+ r1 i) cols) (+ c1 j)))))))])))

;;; ============================================================
;;; Matrix Concatenation
;;; ============================================================

;;; matrix-hstack : Matrix × Matrix → Matrix | Error
;;; Horizontal concatenation (side by side).
(define (matrix-hstack m1 m2)
  (let ([r1 (matrix-rows m1)] [c1 (matrix-cols m1)]
        [r2 (matrix-rows m2)] [c2 (matrix-cols m2)])
       (if (not (= r1 r2))
           `(error dimension-mismatch ,r1 ,r2)
           (let* ([new-cols (+ c1 c2)]
                  [data1 (matrix-data m1)]
                  [data2 (matrix-data m2)]
                  [result (make-vector (* r1 new-cols) 0)])
                 (do ([i 0 (+ i 1)])
                     ((= i r1) (list 'matrix r1 new-cols result))
                     ;; Copy from m1
                     (do ([j 0 (+ j 1)])
                         ((= j c1))
                         (vector-set! result (+ (* i new-cols) j)
                                      (vector-ref data1 (+ (* i c1) j))))
                     ;; Copy from m2
                     (do ([j 0 (+ j 1)])
                         ((= j c2))
                         (vector-set! result (+ (* i new-cols) (+ c1 j))
                                      (vector-ref data2 (+ (* i c2) j)))))))))

;;; matrix-vstack : Matrix × Matrix → Matrix | Error
;;; Vertical concatenation (one on top of another).
(define (matrix-vstack m1 m2)
  (let ([r1 (matrix-rows m1)] [c1 (matrix-cols m1)]
        [r2 (matrix-rows m2)] [c2 (matrix-cols m2)])
       (if (not (= c1 c2))
           `(error dimension-mismatch ,c1 ,c2)
           (let* ([new-rows (+ r1 r2)]
                  [data1 (matrix-data m1)]
                  [data2 (matrix-data m2)]
                  [result (make-vector (* new-rows c1) 0)])
                 ;; Copy m1
                 (do ([i 0 (+ i 1)])
                     ((= i (* r1 c1)))
                     (vector-set! result i (vector-ref data1 i)))
                 ;; Copy m2
                 (do ([i 0 (+ i 1)])
                     ((= i (* r2 c2)) (list 'matrix new-rows c1 result))
                     (vector-set! result (+ (* r1 c1) i) (vector-ref data2 i)))))))

;;; ============================================================
;;; Utilities
;;; ============================================================

;;; matrix->lists : Matrix → (List (List a))
;;; Convert matrix to list of row lists.
(define (matrix->lists m)
  (let ([rows (matrix-rows m)]
        [cols (matrix-cols m)]
        [data (matrix-data m)])
       (do ([i (- rows 1) (- i 1)]
            [result '() (cons (do ([j (- cols 1) (- j 1)]
                                   [row '() (cons (vector-ref data (+ (* i cols) j)) row)])
                                  ((< j 0) row))
                              result)])
           ((< i 0) result))))

;;; matrix-copy : Matrix → Matrix
;;; Create a copy of a matrix.
(define (matrix-copy m)
  (list 'matrix
        (matrix-rows m)
        (matrix-cols m)
        (vec-copy (matrix-data m))))
