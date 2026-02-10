;;; lattice/linalg/matrix-blocked.ss — Blocked/Tiled Matrix Algorithms
;;; @module matrix-blocked
;;; @requires prelude vec matrix

(require 'prelude)
(require 'vec)
(require 'matrix)

(doc 'module 'matrix-blocked)
(doc 'description "Cache-efficient blocked matrix algorithms: tiled multiply, tiled transpose, Strassen.")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Client must load prelude.ss, vec.ss, matrix.ss before this file.")

;;; ====
;;; Configuration
;;; ====

;;; Default block size: 32x32 tiles.
;;; 3 tiles of 32x32 flonums (8 bytes each) = 24KB, fits in GB10 L1D (64KB).
(define *default-block-size* 32)

;;; ====
;;; Blocked Matrix Multiplication
;;; ====

;;; matrix-mul-blocked : Matrix × Matrix × [Nat] → Matrix | Error
;;; Tiled i-k-j matrix multiplication for cache efficiency.
;;; Outer loops iterate over tile-triples, inner loops over elements within tiles.
(define (matrix-mul-blocked m1 m2 . bs-arg)
  (let ([r1 (matrix-rows m1)] [c1 (matrix-cols m1)]
        [r2 (matrix-rows m2)] [c2 (matrix-cols m2)])
    (if (not (= c1 r2))
        `(error dimension-mismatch (,r1 ,c1) (,r2 ,c2))
        (let* ([bs (if (null? bs-arg) *default-block-size* (car bs-arg))]
               [data1 (matrix-data m1)]
               [data2 (matrix-data m2)]
               [result (make-vector (* r1 c2) 0)])
          ;; Tile-level loops: I over row-tiles, K over shared-dim tiles, J over col-tiles
          (do ([I 0 (+ I bs)])
              ((>= I r1))
            (let ([i-end (min (+ I bs) r1)])
              (do ([K 0 (+ K bs)])
                  ((>= K c1))
                (let ([k-end (min (+ K bs) c1)])
                  (do ([J 0 (+ J bs)])
                      ((>= J c2))
                    (let ([j-end (min (+ J bs) c2)])
                      ;; Inner element-level i-k-j loop within tile
                      (do ([i I (+ i 1)])
                          ((= i i-end))
                        (do ([k K (+ k 1)])
                            ((= k k-end))
                          (let ([a-ik (vector-ref data1 (+ (* i c1) k))]
                                [b-row-k (* k c2)]
                                [c-row-i (* i c2)])
                            (do ([j J (+ j 1)])
                                ((= j j-end))
                              (let ([idx (+ c-row-i j)])
                                (vector-set! result idx
                                  (+ (vector-ref result idx)
                                     (* a-ik (vector-ref data2 (+ b-row-k j))))))))))))))))
          (list 'matrix r1 c2 result)))))

;;; matrix-mul-blocked-into! : Matrix × Matrix × Vector × Nat × Nat × [Nat] → Void
;;; Blocked matmul writing to a pre-allocated result vector for rows [row-from, row-to).
;;; Used by parallel dispatch to partition work across workers without allocation.
(define (matrix-mul-blocked-into! m1 m2 result row-from row-to . bs-arg)
  (let* ([c1 (matrix-cols m1)]
         [c2 (matrix-cols m2)]
         [bs (if (null? bs-arg) *default-block-size* (car bs-arg))]
         [data1 (matrix-data m1)]
         [data2 (matrix-data m2)])
    ;; Same tiling but I range is [row-from, row-to)
    (do ([I row-from (+ I bs)])
        ((>= I row-to))
      (let ([i-end (min (+ I bs) row-to)])
        (do ([K 0 (+ K bs)])
            ((>= K c1))
          (let ([k-end (min (+ K bs) c1)])
            (do ([J 0 (+ J bs)])
                ((>= J c2))
              (let ([j-end (min (+ J bs) c2)])
                (do ([i I (+ i 1)])
                    ((= i i-end))
                  (do ([k K (+ k 1)])
                      ((= k k-end))
                    (let ([a-ik (vector-ref data1 (+ (* i c1) k))]
                          [b-row-k (* k c2)]
                          [c-row-i (* i c2)])
                      (do ([j J (+ j 1)])
                          ((= j j-end))
                        (let ([idx (+ c-row-i j)])
                          (vector-set! result idx
                            (+ (vector-ref result idx)
                               (* a-ik (vector-ref data2 (+ b-row-k j))))))))))))))))))

;;; ====
;;; Blocked Matrix Transpose
;;; ====

;;; matrix-transpose-blocked : Matrix × [Nat] → Matrix
;;; Tiled transpose for cache efficiency on large matrices.
(define (matrix-transpose-blocked m . bs-arg)
  (let* ([rows (matrix-rows m)]
         [cols (matrix-cols m)]
         [bs (if (null? bs-arg) *default-block-size* (car bs-arg))]
         [data (matrix-data m)]
         [result (make-vector (* rows cols) 0)])
    ;; Tile-level loops over row-tiles and col-tiles
    (do ([I 0 (+ I bs)])
        ((>= I rows))
      (let ([i-end (min (+ I bs) rows)])
        (do ([J 0 (+ J bs)])
            ((>= J cols))
          (let ([j-end (min (+ J bs) cols)])
            ;; Inner element-level copy within tile
            (do ([i I (+ i 1)])
                ((= i i-end))
              (do ([j J (+ j 1)])
                  ((= j j-end))
                (vector-set! result (+ (* j rows) i)
                  (vector-ref data (+ (* i cols) j)))))))))
    (list 'matrix cols rows result)))

;;; matrix-transpose-blocked-into! : Matrix × Vector × Nat × Nat × [Nat] → Void
;;; Blocked transpose writing tile-rows [row-from, row-to) to pre-allocated result.
(define (matrix-transpose-blocked-into! m result row-from row-to . bs-arg)
  (let* ([rows (matrix-rows m)]
         [cols (matrix-cols m)]
         [bs (if (null? bs-arg) *default-block-size* (car bs-arg))]
         [data (matrix-data m)])
    (do ([I row-from (+ I bs)])
        ((>= I row-to))
      (let ([i-end (min (+ I bs) row-to)])
        (do ([J 0 (+ J bs)])
            ((>= J cols))
          (let ([j-end (min (+ J bs) cols)])
            (do ([i I (+ i 1)])
                ((= i i-end))
              (do ([j J (+ j 1)])
                  ((= j j-end))
                (vector-set! result (+ (* j rows) i)
                  (vector-ref data (+ (* i cols) j)))))))))))

;;; ====
;;; Strassen Matrix Multiplication
;;; ====

;;; Strassen threshold: below this size, fall back to blocked multiply.
(define *strassen-threshold* 128)

;;; next-pow2 : Nat → Nat
;;; Smallest power of 2 >= n.
(define (next-pow2 n)
  (let loop ([p 1])
    (if (>= p n) p (loop (* p 2)))))

;;; matrix-pad : Matrix × Nat × Nat → Matrix
;;; Pad matrix to target-rows x target-cols with zeros.
(define (matrix-pad m target-rows target-cols)
  (let* ([rows (matrix-rows m)]
         [cols (matrix-cols m)]
         [data (matrix-data m)]
         [result (make-vector (* target-rows target-cols) 0)])
    (do ([i 0 (+ i 1)])
        ((= i rows))
      (do ([j 0 (+ j 1)])
          ((= j cols))
        (vector-set! result (+ (* i target-cols) j)
          (vector-ref data (+ (* i cols) j)))))
    (list 'matrix target-rows target-cols result)))

;;; matrix-extract : Matrix × Nat × Nat → Matrix
;;; Extract top-left rows x cols submatrix.
(define (matrix-extract m rows cols)
  (matrix-submatrix m 0 0 rows cols))

;;; matrix-quadrant : Matrix × Symbol → Matrix
;;; Extract quadrant of a square matrix. Quadrant is 'tl, 'tr, 'bl, 'br.
(define (matrix-quadrant m which)
  (let* ([n (matrix-rows m)]
         [half (quotient n 2)])
    (case which
      [tl (matrix-submatrix m 0 0 half half)]
      [tr (matrix-submatrix m 0 half half n)]
      [bl (matrix-submatrix m half 0 n half)]
      [br (matrix-submatrix m half half n n)]
      [else `(error invalid-quadrant ,which)])))

;;; matrix-from-quadrants : Matrix × Matrix × Matrix × Matrix → Matrix
;;; Assemble 4 quadrants into one matrix.
;;; c11=top-left, c12=top-right, c21=bottom-left, c22=bottom-right.
(define (matrix-from-quadrants c11 c12 c21 c22)
  (let* ([half-r (matrix-rows c11)]
         [half-c (matrix-cols c11)]
         [n (* 2 half-r)]
         [m (* 2 half-c)]
         [result (make-vector (* n m) 0)]
         [d11 (matrix-data c11)]
         [d12 (matrix-data c12)]
         [d21 (matrix-data c21)]
         [d22 (matrix-data c22)])
    ;; Top-left
    (do ([i 0 (+ i 1)])
        ((= i half-r))
      (do ([j 0 (+ j 1)])
          ((= j half-c))
        (vector-set! result (+ (* i m) j)
          (vector-ref d11 (+ (* i half-c) j)))))
    ;; Top-right
    (do ([i 0 (+ i 1)])
        ((= i half-r))
      (do ([j 0 (+ j 1)])
          ((= j half-c))
        (vector-set! result (+ (* i m) (+ half-c j))
          (vector-ref d12 (+ (* i half-c) j)))))
    ;; Bottom-left
    (do ([i 0 (+ i 1)])
        ((= i half-r))
      (do ([j 0 (+ j 1)])
          ((= j half-c))
        (vector-set! result (+ (* (+ half-r i) m) j)
          (vector-ref d21 (+ (* i half-c) j)))))
    ;; Bottom-right
    (do ([i 0 (+ i 1)])
        ((= i half-r))
      (do ([j 0 (+ j 1)])
          ((= j half-c))
        (vector-set! result (+ (* (+ half-r i) m) (+ half-c j))
          (vector-ref d22 (+ (* i half-c) j)))))
    (list 'matrix n m result)))

;;; matrix-mul-strassen : Matrix × Matrix × [Nat] → Matrix | Error
;;; Strassen algorithm O(n^2.807). Pads non-pow2 / non-square to next pow2.
;;; Falls back to blocked multiply below threshold.
(define (matrix-mul-strassen m1 m2 . threshold-arg)
  (let ([r1 (matrix-rows m1)] [c1 (matrix-cols m1)]
        [r2 (matrix-rows m2)] [c2 (matrix-cols m2)])
    (if (not (= c1 r2))
        `(error dimension-mismatch (,r1 ,c1) (,r2 ,c2))
        (let* ([threshold (if (null? threshold-arg) *strassen-threshold* (car threshold-arg))]
               [max-dim (max r1 c1 r2 c2)]
               [n (next-pow2 max-dim)]
               [a (if (and (= r1 n) (= c1 n)) m1 (matrix-pad m1 n n))]
               [b (if (and (= r2 n) (= c2 n)) m2 (matrix-pad m2 n n))]
               [c (strassen-recurse a b threshold)])
          ;; Extract the actual result dimensions
          (if (and (= r1 n) (= c2 n))
              c
              (matrix-extract c r1 c2))))))

;;; strassen-recurse : Matrix × Matrix × Nat → Matrix
;;; Internal Strassen recursion on square pow2 matrices.
(define (strassen-recurse a b threshold)
  (let ([n (matrix-rows a)])
    (if (<= n threshold)
        (matrix-mul-blocked a b)
        ;; Strassen's 7 multiplications
        (let* ([a11 (matrix-quadrant a 'tl)]
               [a12 (matrix-quadrant a 'tr)]
               [a21 (matrix-quadrant a 'bl)]
               [a22 (matrix-quadrant a 'br)]
               [b11 (matrix-quadrant b 'tl)]
               [b12 (matrix-quadrant b 'tr)]
               [b21 (matrix-quadrant b 'bl)]
               [b22 (matrix-quadrant b 'br)]
               ;; M1 = (A11 + A22)(B11 + B22)
               [m1 (strassen-recurse (matrix-add a11 a22) (matrix-add b11 b22) threshold)]
               ;; M2 = (A21 + A22)B11
               [m2 (strassen-recurse (matrix-add a21 a22) b11 threshold)]
               ;; M3 = A11(B12 - B22)
               [m3 (strassen-recurse a11 (matrix-sub b12 b22) threshold)]
               ;; M4 = A22(B21 - B11)
               [m4 (strassen-recurse a22 (matrix-sub b21 b11) threshold)]
               ;; M5 = (A11 + A12)B22
               [m5 (strassen-recurse (matrix-add a11 a12) b22 threshold)]
               ;; M6 = (A21 - A11)(B11 + B12)
               [m6 (strassen-recurse (matrix-sub a21 a11) (matrix-add b11 b12) threshold)]
               ;; M7 = (A12 - A22)(B21 + B22)
               [m7 (strassen-recurse (matrix-sub a12 a22) (matrix-add b21 b22) threshold)]
               ;; Combine: C11 = M1 + M4 - M5 + M7
               [c11 (matrix-add (matrix-sub (matrix-add m1 m4) m5) m7)]
               ;; C12 = M3 + M5
               [c12 (matrix-add m3 m5)]
               ;; C21 = M2 + M4
               [c21 (matrix-add m2 m4)]
               ;; C22 = M1 - M2 + M3 + M6
               [c22 (matrix-add (matrix-add (matrix-sub m1 m2) m3) m6)])
          (matrix-from-quadrants c11 c12 c21 c22)))))
