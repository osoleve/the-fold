;;; matrix-blocked.sexp — Development tasks for lattice/linalg/matrix-blocked.ss
;;;
;;; Module: matrix-blocked (tier 0, depends on prelude, vec, matrix)
;;; 290 lines, ~10 exported functions
;;; Cache-efficient blocked matrix algorithms: tiled multiply, tiled transpose,
;;; and Strassen's O(n^2.807) multiplication with padding and quadrant
;;; decomposition. Includes parallel-friendly partitioned variants.
;;;
;;; Git history:
;;;   68780a81 feat(linalg): parallel matrix operations
;;;   9dd7f0b3 feat(module): migrate lattice/linalg/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement next-pow2
  ;; ──────────────────────────────────────────────
  ;; Find the smallest power of 2 >= n.

  (task
    (id "linalg/matrix-blocked/next-pow2-impl")
    (module matrix-blocked)
    (tier 0)
    (type implement)
    (description "Implement next-pow2: find the smallest power of 2 that is greater than or equal to n. Start with p = 1 and keep doubling until p >= n. Uses a named let loop with a single accumulator. This is needed by Strassen's algorithm to pad non-power-of-2 matrices to the next power of 2 before recursive quadrant decomposition. For example: 1->1, 3->4, 4->4, 5->8, 100->128.")
    (context "Available: >=, *, let loop. Simple doubling loop.")
    (before "(define (next-pow2 n)
  ;; TODO: smallest power of 2 >= n
  n)")
    (test-file "lattice/linalg/test-matrix-blocked.ss")
    (test-forms (";; Powers of 2 return themselves
(assert-equal 1 (next-pow2 1))
(assert-equal 4 (next-pow2 4))
(assert-equal 8 (next-pow2 8))"
                 ";; Non-powers round up
(assert-equal 4 (next-pow2 3))
(assert-equal 8 (next-pow2 5))
(assert-equal 8 (next-pow2 7))"
                 ";; Larger values
(assert-equal 128 (next-pow2 100))
(assert-equal 256 (next-pow2 200))"
                 ";; Edge case
(assert-equal 2 (next-pow2 2))"))
    (available-modules (prelude vec matrix))
    (hints ("Start: (let loop ([p 1]) ...)"
            "Test: (>= p n) -> return p"
            "Otherwise: (loop (* p 2))"
            "This is a 3-line function"))
    (reference-solution "(define (next-pow2 n)
  (let loop ([p 1])
    (if (>= p n) p (loop (* p 2)))))")
    (alternatives ())
    (source-commit "68780a81")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement matrix-pad
  ;; ──────────────────────────────────────────────
  ;; Zero-pad a matrix to larger dimensions.

  (task
    (id "linalg/matrix-blocked/matrix-pad-impl")
    (module matrix-blocked)
    (tier 0)
    (type implement)
    (description "Implement matrix-pad: pad a matrix to target-rows x target-cols with zeros. Allocate a new zero-filled vector of size target-rows * target-cols, then copy the original elements into the correct positions. Uses a nested do loop: outer loop over rows i in [0, rows), inner loop over columns j in [0, cols). The source index is (+ (* i cols) j) and the destination index is (+ (* i target-cols) j) — note the different stride because the padded matrix has more columns. Return a matrix struct (list 'matrix target-rows target-cols result). This is used by Strassen to pad non-square or non-power-of-2 matrices.")
    (context "Available: matrix-rows, matrix-cols, matrix-data, make-vector, vector-ref, vector-set!, do, +, *, =, list. Nested copy loop with stride conversion.")
    (before "(define (matrix-pad m target-rows target-cols)
  ;; TODO: pad matrix to target dimensions with zeros
  m)")
    (test-file "lattice/linalg/test-matrix-blocked.ss")
    (test-forms (";; Pad 2x2 to 3x4
(let* ([m (matrix-from-lists '((1 2) (3 4)))]
       [p (matrix-pad m 3 4)])
  (assert-equal '((1 2 0 0) (3 4 0 0) (0 0 0 0)) (matrix->lists p)))"
                 ";; Pad to same size is identity
(let* ([m (matrix-from-lists '((1 2) (3 4)))]
       [p (matrix-pad m 2 2)])
  (assert-equal '((1 2) (3 4)) (matrix->lists p)))"
                 ";; Pad single element
(let* ([m (matrix-from-lists '((5)))]
       [p (matrix-pad m 3 3)])
  (assert-equal '((5 0 0) (0 0 0) (0 0 0)) (matrix->lists p)))"
                 ";; Pad rectangular
(let* ([m (matrix-from-lists '((1 2 3)))]
       [p (matrix-pad m 2 4)])
  (assert-equal '((1 2 3 0) (0 0 0 0)) (matrix->lists p)))"))
    (available-modules (prelude vec matrix))
    (hints ("Allocate: (make-vector (* target-rows target-cols) 0)"
            "Outer loop: (do ([i 0 (+ i 1)]) ((= i rows)) ...)"
            "Inner loop: (do ([j 0 (+ j 1)]) ((= j cols)) ...)"
            "Source index: (+ (* i cols) j)"
            "Dest index: (+ (* i target-cols) j) — different stride!"
            "Return: (list 'matrix target-rows target-cols result)"))
    (reference-solution "(define (matrix-pad m target-rows target-cols)
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
    (list 'matrix target-rows target-cols result)))")
    (alternatives ())
    (source-commit "68780a81")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement matrix-quadrant
  ;; ──────────────────────────────────────────────
  ;; Extract a quadrant of a square matrix.

  (task
    (id "linalg/matrix-blocked/matrix-quadrant-impl")
    (module matrix-blocked)
    (tier 0)
    (type implement)
    (description "Implement matrix-quadrant: extract one of the four quadrants (top-left, top-right, bottom-left, bottom-right) of a square matrix. Compute half = n/2 (using quotient for integer division). Then use matrix-submatrix to extract the appropriate region based on the which symbol: 'tl -> rows [0, half), cols [0, half). 'tr -> rows [0, half), cols [half, n). 'bl -> rows [half, n), cols [0, half). 'br -> rows [half, n), cols [half, n). Uses case to dispatch on the quadrant symbol. This is a core building block of Strassen's algorithm, which decomposes matrices into 4 quadrants for the 7-multiply recursion.")
    (context "Available: matrix-rows, matrix-submatrix, quotient, case. Four-branch dispatch via case.")
    (before "(define (matrix-quadrant m which)
  ;; TODO: extract 'tl, 'tr, 'bl, or 'br quadrant
  m)")
    (test-file "lattice/linalg/test-matrix-blocked.ss")
    (test-forms (";; 4x4 matrix quadrants
(let ([m (matrix-from-lists '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16)))])
  (assert-equal '((1 2) (5 6)) (matrix->lists (matrix-quadrant m 'tl)))
  (assert-equal '((3 4) (7 8)) (matrix->lists (matrix-quadrant m 'tr)))
  (assert-equal '((9 10) (13 14)) (matrix->lists (matrix-quadrant m 'bl)))
  (assert-equal '((11 12) (15 16)) (matrix->lists (matrix-quadrant m 'br))))"
                 ";; 2x2 matrix -> scalar quadrants
(let ([m (matrix-from-lists '((1 2) (3 4)))])
  (assert-equal '((1)) (matrix->lists (matrix-quadrant m 'tl)))
  (assert-equal '((4)) (matrix->lists (matrix-quadrant m 'br))))"
                 ";; Roundtrip: extract all 4 and reassemble
(let* ([m (matrix-from-lists '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16)))]
       [tl (matrix-quadrant m 'tl)]
       [tr (matrix-quadrant m 'tr)]
       [bl (matrix-quadrant m 'bl)]
       [br (matrix-quadrant m 'br)]
       [reassembled (matrix-from-quadrants tl tr bl br)])
  (assert-true (matrix-equal? m reassembled)))"))
    (available-modules (prelude vec matrix))
    (hints ("Compute half: (quotient (matrix-rows m) 2)"
            "Use case on which: [tl ...] [tr ...] [bl ...] [br ...]"
            "'tl: (matrix-submatrix m 0 0 half half)"
            "'tr: (matrix-submatrix m 0 half half n)"
            "'bl: (matrix-submatrix m half 0 n half)"
            "'br: (matrix-submatrix m half half n n)"))
    (reference-solution "(define (matrix-quadrant m which)
  (let* ([n (matrix-rows m)]
         [half (quotient n 2)])
    (case which
      [tl (matrix-submatrix m 0 0 half half)]
      [tr (matrix-submatrix m 0 half half n)]
      [bl (matrix-submatrix m half 0 n half)]
      [br (matrix-submatrix m half half n n)]
      [else (error 'matrix-quadrant \"invalid quadrant\" which)])))")
    (alternatives ())
    (source-commit "68780a81")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement matrix-from-quadrants
  ;; ──────────────────────────────────────────────
  ;; Assemble 4 quadrants into one matrix.

  (task
    (id "linalg/matrix-blocked/matrix-from-quadrants-impl")
    (module matrix-blocked)
    (tier 0)
    (type implement)
    (description "Implement matrix-from-quadrants: assemble four equal-sized submatrices (c11=top-left, c12=top-right, c21=bottom-left, c22=bottom-right) into a single matrix. The result dimensions are n = 2*half-r rows and m = 2*half-c columns. Allocate a zero-filled result vector, then use four nested do loops to copy each quadrant's data into the correct position. The key is computing the correct offset for each quadrant in the result: top-left starts at (0,0), top-right at (0, half-c), bottom-left at (half-r, 0), bottom-right at (half-r, half-c). The result index for the top-right quadrant at local position (i,j) is (+ (* i m) (+ half-c j)). This is the inverse of matrix-quadrant and completes Strassen's combine step.")
    (context "Available: matrix-rows, matrix-cols, matrix-data, make-vector, vector-ref, vector-set!, do, +, *, =, list. Four nested copy loops with offset computation.")
    (before "(define (matrix-from-quadrants c11 c12 c21 c22)
  ;; TODO: assemble 4 quadrants into one matrix
  c11)")
    (test-file "lattice/linalg/test-matrix-blocked.ss")
    (test-forms (";; 2x2 from scalar quadrants
(let* ([c11 (matrix-from-lists '((1)))]
       [c12 (matrix-from-lists '((2)))]
       [c21 (matrix-from-lists '((3)))]
       [c22 (matrix-from-lists '((4)))])
  (assert-equal '((1 2) (3 4))
    (matrix->lists (matrix-from-quadrants c11 c12 c21 c22))))"
                 ";; 4x4 from 2x2 quadrants
(let* ([c11 (matrix-from-lists '((1 2) (5 6)))]
       [c12 (matrix-from-lists '((3 4) (7 8)))]
       [c21 (matrix-from-lists '((9 10) (13 14)))]
       [c22 (matrix-from-lists '((11 12) (15 16)))])
  (assert-equal '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16))
    (matrix->lists (matrix-from-quadrants c11 c12 c21 c22))))"
                 ";; Roundtrip with matrix-quadrant
(let* ([m (matrix-from-lists '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16)))]
       [reassembled (matrix-from-quadrants
                      (matrix-quadrant m 'tl) (matrix-quadrant m 'tr)
                      (matrix-quadrant m 'bl) (matrix-quadrant m 'br))])
  (assert-true (matrix-equal? m reassembled)))"))
    (available-modules (prelude vec matrix))
    (hints ("Get half dimensions: (matrix-rows c11), (matrix-cols c11)"
            "Full dimensions: n = 2*half-r, m = 2*half-c"
            "Allocate: (make-vector (* n m) 0)"
            "Four copy blocks, each a nested do loop"
            "Top-left: result index = (+ (* i m) j)"
            "Top-right: result index = (+ (* i m) (+ half-c j))"
            "Bottom-left: result index = (+ (* (+ half-r i) m) j)"
            "Bottom-right: result index = (+ (* (+ half-r i) m) (+ half-c j))"
            "Return (list 'matrix n m result)"))
    (reference-solution "(define (matrix-from-quadrants c11 c12 c21 c22)
  (let* ([half-r (matrix-rows c11)]
         [half-c (matrix-cols c11)]
         [n (* 2 half-r)]
         [m (* 2 half-c)]
         [result (make-vector (* n m) 0)]
         [d11 (matrix-data c11)]
         [d12 (matrix-data c12)]
         [d21 (matrix-data c21)]
         [d22 (matrix-data c22)])
    (do ([i 0 (+ i 1)]) ((= i half-r))
      (do ([j 0 (+ j 1)]) ((= j half-c))
        (vector-set! result (+ (* i m) j)
          (vector-ref d11 (+ (* i half-c) j)))))
    (do ([i 0 (+ i 1)]) ((= i half-r))
      (do ([j 0 (+ j 1)]) ((= j half-c))
        (vector-set! result (+ (* i m) (+ half-c j))
          (vector-ref d12 (+ (* i half-c) j)))))
    (do ([i 0 (+ i 1)]) ((= i half-r))
      (do ([j 0 (+ j 1)]) ((= j half-c))
        (vector-set! result (+ (* (+ half-r i) m) j)
          (vector-ref d21 (+ (* i half-c) j)))))
    (do ([i 0 (+ i 1)]) ((= i half-r))
      (do ([j 0 (+ j 1)]) ((= j half-c))
        (vector-set! result (+ (* (+ half-r i) m) (+ half-c j))
          (vector-ref d22 (+ (* i half-c) j)))))
    (list 'matrix n m result)))")
    (alternatives ())
    (source-commit "68780a81")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement matrix-transpose-blocked
  ;; ──────────────────────────────────────────────
  ;; Cache-efficient tiled matrix transpose.

  (task
    (id "linalg/matrix-blocked/matrix-transpose-blocked-impl")
    (module matrix-blocked)
    (tier 0)
    (type implement)
    (description "Implement matrix-transpose-blocked: transpose a matrix using tiled (blocked) iteration for cache efficiency. The result is a cols x rows matrix. Uses four nested do loops: two outer loops iterate over tile origins (I and J stepping by block size bs), two inner loops iterate over elements within each tile. For element (i,j) in the source, the transposed position is (j,i): source index = (+ (* i cols) j), destination index = (+ (* j rows) i). The block size bs defaults to *default-block-size* but can be overridden via an optional argument. The tiling ensures that both the source and destination memory accesses have good spatial locality within each tile, which matters for large matrices that don't fit in L1 cache.")
    (context "Available: matrix-rows, matrix-cols, matrix-data, make-vector, vector-ref, vector-set!, do, +, *, >=, =, min, list, null?, car, if. Four nested do loops with tile bounds.")
    (before "(define (matrix-transpose-blocked m . bs-arg)
  ;; TODO: tiled transpose for cache efficiency
  (matrix-transpose m))")
    (test-file "lattice/linalg/test-matrix-blocked.ss")
    (test-forms (";; Matches naive transpose on 2x3
(let* ([m (matrix-from-lists '((1 2 3) (4 5 6)))]
       [naive (matrix-transpose m)]
       [blocked (matrix-transpose-blocked m)])
  (assert-true (matrix-equal? naive blocked)))"
                 ";; Square matrix
(let* ([m (matrix-from-lists '((1 2 3) (4 5 6) (7 8 9)))]
       [blocked (matrix-transpose-blocked m)])
  (assert-equal '((1 4 7) (2 5 8) (3 6 9)) (matrix->lists blocked)))"
                 ";; Double transpose is identity
(let* ([m (matrix-from-lists '((1 2) (3 4) (5 6)))]
       [result (matrix-transpose-blocked (matrix-transpose-blocked m))])
  (assert-true (matrix-equal? m result)))"
                 ";; Custom block size
(let* ([m (matrix-from-lists '((1 2 3 4 5) (6 7 8 9 10)))]
       [naive (matrix-transpose m)]
       [blocked (matrix-transpose-blocked m 2)])
  (assert-true (matrix-equal? naive blocked)))"))
    (available-modules (prelude vec matrix))
    (hints ("Parse optional bs: (if (null? bs-arg) *default-block-size* (car bs-arg))"
            "Allocate result: (make-vector (* rows cols) 0)"
            "Outer tile loop I: (do ([I 0 (+ I bs)]) ((>= I rows)) ...)"
            "Outer tile loop J: (do ([J 0 (+ J bs)]) ((>= J cols)) ...)"
            "Tile bounds: i-end = (min (+ I bs) rows), j-end = (min (+ J bs) cols)"
            "Inner loops: i from I to i-end, j from J to j-end"
            "Copy: result[(j*rows + i)] = data[(i*cols + j)]"
            "Return: (list 'matrix cols rows result) — note swapped dimensions!"))
    (reference-solution "(define (matrix-transpose-blocked m . bs-arg)
  (let* ([rows (matrix-rows m)]
         [cols (matrix-cols m)]
         [bs (if (null? bs-arg) *default-block-size* (car bs-arg))]
         [data (matrix-data m)]
         [result (make-vector (* rows cols) 0)])
    (do ([I 0 (+ I bs)])
        ((>= I rows))
      (let ([i-end (min (+ I bs) rows)])
        (do ([J 0 (+ J bs)])
            ((>= J cols))
          (let ([j-end (min (+ J bs) cols)])
            (do ([i I (+ i 1)])
                ((= i i-end))
              (do ([j J (+ j 1)])
                  ((= j j-end))
                (vector-set! result (+ (* j rows) i)
                  (vector-ref data (+ (* i cols) j)))))))))
    (list 'matrix cols rows result)))")
    (alternatives ())
    (source-commit "68780a81")
    (difficulty 2))

) ;; end tasks
