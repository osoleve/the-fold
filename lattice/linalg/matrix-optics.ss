;;; lattice/linalg/matrix-optics.ss — Optics for Matrix Operations
;;; @module matrix-optics
;;; @requires prelude matrix optics
;;;
;;; Provides lenses and traversals for matrix manipulation.
;;; These compose with traced-optics for gradient computation.
;;;
;;; Key optics:
;;;   - matrix-cell-lens i j    Focus on single cell at (i,j)
;;;   - matrix-row-lens i       Focus on row i as a vector
;;;   - matrix-col-lens j       Focus on column j as a vector
;;;   - matrix-diagonal-lens    Focus on main diagonal as a vector
;;;   - matrix-cells-each       Traversal over all cells (row-major)
;;;   - matrix-rows-each        Traversal over all rows as vectors
;;;
;;; Example with autodiff:
;;;   (optic-gradient loss-fn (matrix-cell-lens 0 0) my-matrix)
;;;   => gradient of loss w.r.t. cell (0,0)

(load "core/base/prelude.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/fp/optics/optics.ss")

(doc 'module 'matrix-optics)
(doc 'description "Optics for matrix operations - composable data access and autodiff support")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================
;;; Section: Cell Lenses
;;; ============================================================

(doc 'section 'cell-lenses)

;;; matrix-cell-lens : Nat x Nat -> Lens Matrix a
;;; Focus on a single cell at position (i, j).
;;; Bounds checking is done at access time.
(define (matrix-cell-lens i j)
  (doc 'export #t)
  (doc 'type '(-> Nat Nat (Lens Matrix a)))
  (doc 'description "Lens focusing on cell (i,j) of a matrix")
  (make-lens
   ;; Getter: extract cell value
   (lambda (m)
     (matrix-ref m i j))
   ;; Setter: create new matrix with updated cell
   (lambda (val m)
     (let* ([rows (matrix-rows m)]
            [cols (matrix-cols m)]
            [old-data (matrix-data m)]
            [new-data (vec-copy old-data)]
            [idx (+ (* i cols) j)])
       (vector-set! new-data idx val)
       (list 'matrix rows cols new-data)))))

;;; matrix-cell-affine : Nat x Nat -> Affine Matrix a
;;; Like matrix-cell-lens but returns nothing for out-of-bounds access.
(define (matrix-cell-affine i j)
  (doc 'export #t)
  (doc 'type '(-> Nat Nat (Affine Matrix a)))
  (doc 'description "Affine focusing on cell (i,j), safe for out-of-bounds")
  (make-affine
   ;; Getter: Maybe a
   (lambda (m)
     (let ([rows (matrix-rows m)]
           [cols (matrix-cols m)])
       (if (and (>= i 0) (< i rows) (>= j 0) (< j cols))
           (just (matrix-ref m i j))
           nothing)))
   ;; Setter: no-op if out of bounds
   (lambda (val m)
     (let ([rows (matrix-rows m)]
           [cols (matrix-cols m)])
       (if (and (>= i 0) (< i rows) (>= j 0) (< j cols))
           (let* ([old-data (matrix-data m)]
                  [new-data (vec-copy old-data)]
                  [idx (+ (* i cols) j)])
             (vector-set! new-data idx val)
             (list 'matrix rows cols new-data))
           m)))))

;;; ============================================================
;;; Section: Row and Column Lenses
;;; ============================================================

(doc 'section 'row-col-lenses)

;;; matrix-row-lens : Nat -> Lens Matrix Vec
;;; Focus on row i as a vector.
(define (matrix-row-lens i)
  (doc 'export #t)
  (doc 'type '(-> Nat (Lens Matrix Vec)))
  (doc 'description "Lens focusing on row i as a vector")
  (make-lens
   ;; Getter: extract row as vector
   (lambda (m)
     (matrix-row m i))
   ;; Setter: replace row
   (lambda (new-row m)
     (let* ([rows (matrix-rows m)]
            [cols (matrix-cols m)]
            [old-data (matrix-data m)]
            [new-data (vec-copy old-data)]
            [start (* i cols)])
       (do ([j 0 (+ j 1)])
           ((= j cols) (list 'matrix rows cols new-data))
         (vector-set! new-data (+ start j) (vector-ref new-row j)))))))

;;; matrix-col-lens : Nat -> Lens Matrix Vec
;;; Focus on column j as a vector.
(define (matrix-col-lens j)
  (doc 'export #t)
  (doc 'type '(-> Nat (Lens Matrix Vec)))
  (doc 'description "Lens focusing on column j as a vector")
  (make-lens
   ;; Getter: extract column as vector
   (lambda (m)
     (matrix-col m j))
   ;; Setter: replace column
   (lambda (new-col m)
     (let* ([rows (matrix-rows m)]
            [cols (matrix-cols m)]
            [old-data (matrix-data m)]
            [new-data (vec-copy old-data)])
       (do ([i 0 (+ i 1)])
           ((= i rows) (list 'matrix rows cols new-data))
         (vector-set! new-data (+ (* i cols) j) (vector-ref new-col i)))))))

;;; matrix-diagonal-lens : Lens Matrix Vec
;;; Focus on the main diagonal as a vector.
;;; Only valid for square matrices or min(rows, cols) diagonal.
(doc matrix-diagonal-lens 'export #t)
(doc matrix-diagonal-lens 'type '(Lens Matrix Vec))
(doc matrix-diagonal-lens 'description "Lens focusing on the main diagonal")
(define matrix-diagonal-lens
  (make-lens
   ;; Getter: extract diagonal
   (lambda (m)
     (matrix-diagonal m))
   ;; Setter: replace diagonal
   (lambda (new-diag m)
     (let* ([rows (matrix-rows m)]
            [cols (matrix-cols m)]
            [n (min rows cols)]
            [old-data (matrix-data m)]
            [new-data (vec-copy old-data)])
       (do ([i 0 (+ i 1)])
           ((= i n) (list 'matrix rows cols new-data))
         (vector-set! new-data (+ (* i cols) i) (vector-ref new-diag i)))))))

;;; ============================================================
;;; Section: Traversals
;;; ============================================================

(doc 'section 'traversals)

;;; matrix-cells-each : Traversal Matrix a
;;; Traverse all cells in row-major order.
(doc matrix-cells-each 'export #t)
(doc matrix-cells-each 'type '(Traversal Matrix a))
(doc matrix-cells-each 'description "Traversal over all matrix cells in row-major order")
(define matrix-cells-each
  (make-traversal
   ;; Traverse: apply f to each cell
   (lambda (f m)
     (let* ([rows (matrix-rows m)]
            [cols (matrix-cols m)]
            [old-data (matrix-data m)]
            [n (* rows cols)]
            [new-data (make-vector n)])
       (do ([i 0 (+ i 1)])
           ((= i n) (list 'matrix rows cols new-data))
         (vector-set! new-data i (f (vector-ref old-data i))))))
   ;; Fold: get all cells as list
   (lambda (m)
     (vector->list (matrix-data m)))))

;;; matrix-rows-each : Traversal Matrix Vec
;;; Traverse all rows as vectors.
(doc matrix-rows-each 'export #t)
(doc matrix-rows-each 'type '(Traversal Matrix Vec))
(doc matrix-rows-each 'description "Traversal over all matrix rows as vectors")
(define matrix-rows-each
  (make-traversal
   ;; Traverse: apply f to each row
   (lambda (f m)
     (let* ([rows (matrix-rows m)]
            [cols (matrix-cols m)]
            [new-data (make-vector (* rows cols))])
       (do ([i 0 (+ i 1)])
           ((= i rows) (list 'matrix rows cols new-data))
         (let* ([old-row (matrix-row m i)]
                [new-row (f old-row)]
                [start (* i cols)])
           (do ([j 0 (+ j 1)])
               ((= j cols))
             (vector-set! new-data (+ start j) (vector-ref new-row j)))))))
   ;; Fold: get all rows as list of vectors
   (lambda (m)
     (let ([rows (matrix-rows m)])
       (let loop ([i 0] [acc '()])
         (if (= i rows)
             (reverse acc)
             (loop (+ i 1) (cons (matrix-row m i) acc))))))))

;;; matrix-cols-each : Traversal Matrix Vec
;;; Traverse all columns as vectors.
(doc matrix-cols-each 'export #t)
(doc matrix-cols-each 'type '(Traversal Matrix Vec))
(doc matrix-cols-each 'description "Traversal over all matrix columns as vectors")
(define matrix-cols-each
  (make-traversal
   ;; Traverse: apply f to each column
   (lambda (f m)
     (let* ([rows (matrix-rows m)]
            [cols (matrix-cols m)]
            [new-data (make-vector (* rows cols))])
       ;; First copy original data
       (do ([i 0 (+ i 1)])
           ((= i (* rows cols)))
         (vector-set! new-data i (vector-ref (matrix-data m) i)))
       ;; Then update each column
       (do ([j 0 (+ j 1)])
           ((= j cols) (list 'matrix rows cols new-data))
         (let* ([old-col (matrix-col m j)]
                [new-col (f old-col)])
           (do ([i 0 (+ i 1)])
               ((= i rows))
             (vector-set! new-data (+ (* i cols) j) (vector-ref new-col i)))))))
   ;; Fold: get all columns as list of vectors
   (lambda (m)
     (let ([cols (matrix-cols m)])
       (let loop ([j 0] [acc '()])
         (if (= j cols)
             (reverse acc)
             (loop (+ j 1) (cons (matrix-col m j) acc))))))))

;;; ============================================================
;;; Section: Submatrix Optics
;;; ============================================================

(doc 'section 'submatrix)

;;; matrix-submatrix-lens : Nat x Nat x Nat x Nat -> Lens Matrix Matrix
;;; Focus on a submatrix from (r1,c1) to (r2,c2) exclusive.
(define (matrix-submatrix-lens r1 c1 r2 c2)
  (doc 'export #t)
  (doc 'type '(-> Nat Nat Nat Nat (Lens Matrix Matrix)))
  (doc 'description "Lens focusing on a rectangular submatrix")
  (make-lens
   ;; Getter: extract submatrix
   (lambda (m)
     (matrix-submatrix m r1 c1 r2 c2))
   ;; Setter: replace submatrix region
   (lambda (sub m)
     (let* ([rows (matrix-rows m)]
            [cols (matrix-cols m)]
            [old-data (matrix-data m)]
            [new-data (vec-copy old-data)]
            [sub-rows (- r2 r1)]
            [sub-cols (- c2 c1)]
            [sub-data (matrix-data sub)])
       (do ([i 0 (+ i 1)])
           ((= i sub-rows) (list 'matrix rows cols new-data))
         (do ([j 0 (+ j 1)])
             ((= j sub-cols))
           (vector-set! new-data (+ (* (+ r1 i) cols) (+ c1 j))
                        (vector-ref sub-data (+ (* i sub-cols) j)))))))))

;;; ============================================================
;;; Section: Composed Optics
;;; ============================================================

(doc 'section 'composed)

;;; matrix-row-cell : Nat x Nat -> Lens Matrix a
;;; Shorthand for composing row and cell access.
;;; (matrix-row-cell i j) = (>>> (matrix-row-lens i) (vec-element-lens j))
(define (matrix-row-cell i j)
  (doc 'export #t)
  (matrix-cell-lens i j))  ; Direct access is more efficient

;;; Note: vec-element-lens and vec-elements-each are now in optics.ss

;;; ============================================================
;;; Section: Convenience Functions
;;; ============================================================

(doc 'section 'convenience)

;;; matrix-update-cell : Matrix x Nat x Nat x (a -> a) -> Matrix
;;; Functional update of a single cell using over.
(define (matrix-update-cell m i j f)
  (doc 'export #t)
  (over (matrix-cell-lens i j) f m))

;;; matrix-update-row : Matrix x Nat x (Vec -> Vec) -> Matrix
;;; Functional update of a row.
(define (matrix-update-row m i f)
  (doc 'export #t)
  (over (matrix-row-lens i) f m))

;;; matrix-update-col : Matrix x Nat x (Vec -> Vec) -> Matrix
;;; Functional update of a column.
(define (matrix-update-col m j f)
  (doc 'export #t)
  (over (matrix-col-lens j) f m))

;;; matrix-update-diagonal : Matrix x (Vec -> Vec) -> Matrix
;;; Functional update of the diagonal.
(define (matrix-update-diagonal m f)
  (doc 'export #t)
  (over matrix-diagonal-lens f m))

;;; matrix-scale-row : Matrix x Nat x Num -> Matrix
;;; Scale a row by a constant.
(define (matrix-scale-row m i k)
  (doc 'export #t)
  (matrix-update-row m i (lambda (row) (vec-scale k row))))

;;; matrix-scale-col : Matrix x Nat x Num -> Matrix
;;; Scale a column by a constant.
(define (matrix-scale-col m j k)
  (doc 'export #t)
  (matrix-update-col m j (lambda (col) (vec-scale k col))))

;;; ============================================================
;;; Section: Row Operations (for Gaussian elimination etc.)
;;; ============================================================

(doc 'section 'row-operations)

;;; matrix-swap-rows : Matrix x Nat x Nat -> Matrix
;;; Swap rows i and j.
(define (matrix-swap-rows m i j)
  (doc 'export #t)
  (if (= i j)
      m
      (let ([row-i (^. m (matrix-row-lens i))]
            [row-j (^. m (matrix-row-lens j))])
        (& (& m (.~ (matrix-row-lens i) row-j))
           (.~ (matrix-row-lens j) row-i)))))

;;; matrix-add-row-scaled : Matrix x Nat x Nat x Num -> Matrix
;;; Add k * row_src to row_dst: row_dst += k * row_src
(define (matrix-add-row-scaled m dst src k)
  (doc 'export #t)
  (let ([row-src (^. m (matrix-row-lens src))])
    (matrix-update-row m dst
      (lambda (row-dst)
        (vec-add row-dst (vec-scale k row-src))))))

;;; ============================================================
;;; Exports Summary
;;; ============================================================
;;;
;;; Cell access:
;;;   matrix-cell-lens, matrix-cell-affine
;;;
;;; Row/Column/Diagonal:
;;;   matrix-row-lens, matrix-col-lens, matrix-diagonal-lens
;;;
;;; Traversals:
;;;   matrix-cells-each, matrix-rows-each, matrix-cols-each
;;;
;;; Submatrix:
;;;   matrix-submatrix-lens
;;;
;;; Vector optics (from optics.ss):
;;;   vec-element-lens, vec-element-affine, vec-elements-each
;;;
;;; Convenience:
;;;   matrix-update-cell, matrix-update-row, matrix-update-col
;;;   matrix-update-diagonal, matrix-scale-row, matrix-scale-col
;;;
;;; Row operations:
;;;   matrix-swap-rows, matrix-add-row-scaled
