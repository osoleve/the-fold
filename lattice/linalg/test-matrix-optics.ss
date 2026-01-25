;;; lattice/linalg/test-matrix-optics.ss — Tests for Matrix Optics
;;; @module test-matrix-optics

(load "core/testing/test-framework.ss")
(load "lattice/linalg/matrix-optics.ss")

(test-group "matrix-optics"

  ;;; --- Cell Lenses ---

  (define-test "matrix-cell-lens view"
    (let ([m (matrix-from-lists '((1 2 3) (4 5 6) (7 8 9)))])
      (assert-equal 1 (^. m (matrix-cell-lens 0 0)))
      (assert-equal 5 (^. m (matrix-cell-lens 1 1)))
      (assert-equal 9 (^. m (matrix-cell-lens 2 2)))
      (assert-equal 6 (^. m (matrix-cell-lens 1 2)))))

  (define-test "matrix-cell-lens set"
    (let* ([m (matrix-from-lists '((1 2) (3 4)))]
           [m2 (& m (.~ (matrix-cell-lens 0 1) 99))])
      (assert-equal 99 (^. m2 (matrix-cell-lens 0 1)))
      (assert-equal 1 (^. m2 (matrix-cell-lens 0 0)))  ; unchanged
      (assert-equal 3 (^. m2 (matrix-cell-lens 1 0)))  ; unchanged
      (assert-equal 4 (^. m2 (matrix-cell-lens 1 1))))) ; unchanged

  (define-test "matrix-cell-lens over"
    (let* ([m (matrix-from-lists '((1 2) (3 4)))]
           [m2 (& m (%~ (matrix-cell-lens 1 1) (lambda (x) (* x 10))))])
      (assert-equal 40 (^. m2 (matrix-cell-lens 1 1)))))

  (define-test "matrix-cell-affine safe access"
    (let ([m (matrix-from-lists '((1 2) (3 4)))])
      (assert-true (just? (^? m (matrix-cell-affine 0 0))))
      (assert-true (nothing? (^? m (matrix-cell-affine 5 5))))))

  ;;; --- Row Lenses ---

  (define-test "matrix-row-lens view"
    (let ([m (matrix-from-lists '((1 2 3) (4 5 6)))])
      (assert-true (vec-equal? '#(1 2 3) (^. m (matrix-row-lens 0))))
      (assert-true (vec-equal? '#(4 5 6) (^. m (matrix-row-lens 1))))))

  (define-test "matrix-row-lens set"
    (let* ([m (matrix-from-lists '((1 2) (3 4)))]
           [m2 (& m (.~ (matrix-row-lens 0) '#(10 20)))])
      (assert-true (vec-equal? '#(10 20) (^. m2 (matrix-row-lens 0))))
      (assert-true (vec-equal? '#(3 4) (^. m2 (matrix-row-lens 1))))))

  ;;; --- Column Lenses ---

  (define-test "matrix-col-lens view"
    (let ([m (matrix-from-lists '((1 2 3) (4 5 6)))])
      (assert-true (vec-equal? '#(1 4) (^. m (matrix-col-lens 0))))
      (assert-true (vec-equal? '#(2 5) (^. m (matrix-col-lens 1))))
      (assert-true (vec-equal? '#(3 6) (^. m (matrix-col-lens 2))))))

  (define-test "matrix-col-lens set"
    (let* ([m (matrix-from-lists '((1 2) (3 4)))]
           [m2 (& m (.~ (matrix-col-lens 1) '#(99 88)))])
      (assert-equal 99 (matrix-ref m2 0 1))
      (assert-equal 88 (matrix-ref m2 1 1))
      (assert-equal 1 (matrix-ref m2 0 0))   ; unchanged
      (assert-equal 3 (matrix-ref m2 1 0)))) ; unchanged

  ;;; --- Diagonal Lens ---

  (define-test "matrix-diagonal-lens view"
    (let ([m (matrix-from-lists '((1 2 3) (4 5 6) (7 8 9)))])
      (assert-true (vec-equal? '#(1 5 9) (^. m matrix-diagonal-lens)))))

  (define-test "matrix-diagonal-lens set"
    (let* ([m (matrix-from-lists '((1 2) (3 4)))]
           [m2 (& m (.~ matrix-diagonal-lens '#(10 40)))])
      (assert-equal 10 (matrix-ref m2 0 0))
      (assert-equal 40 (matrix-ref m2 1 1))
      (assert-equal 2 (matrix-ref m2 0 1))   ; unchanged
      (assert-equal 3 (matrix-ref m2 1 0)))) ; unchanged

  ;;; --- Traversals ---

  (define-test "matrix-cells-each to-list"
    (let ([m (matrix-from-lists '((1 2) (3 4)))])
      (assert-equal '(1 2 3 4) (^.. m matrix-cells-each))))

  (define-test "matrix-cells-each over"
    (let* ([m (matrix-from-lists '((1 2) (3 4)))]
           [m2 (traversal-over matrix-cells-each (lambda (x) (* x 2)) m)])
      (assert-equal '(2 4 6 8) (^.. m2 matrix-cells-each))))

  (define-test "matrix-rows-each to-list"
    (let ([m (matrix-from-lists '((1 2) (3 4)))])
      (let ([rows (^.. m matrix-rows-each)])
        (assert-equal 2 (length rows))
        (assert-true (vec-equal? '#(1 2) (car rows)))
        (assert-true (vec-equal? '#(3 4) (cadr rows))))))

  (define-test "matrix-cols-each to-list"
    (let ([m (matrix-from-lists '((1 2) (3 4)))])
      (let ([cols (^.. m matrix-cols-each)])
        (assert-equal 2 (length cols))
        (assert-true (vec-equal? '#(1 3) (car cols)))
        (assert-true (vec-equal? '#(2 4) (cadr cols))))))

  ;;; --- Vector Optics ---

  (define-test "vec-element-lens"
    (let ([v '#(10 20 30)])
      (assert-equal 20 (^. v (vec-element-lens 1)))
      (let ([v2 (& v (.~ (vec-element-lens 1) 99))])
        (assert-equal 99 (vector-ref v2 1))
        (assert-equal 10 (vector-ref v2 0)))))

  (define-test "vec-elements-each"
    (let ([v '#(1 2 3)])
      (assert-equal '(1 2 3) (^.. v vec-elements-each))
      (let ([v2 (traversal-over vec-elements-each add1 v)])
        (assert-equal '(2 3 4) (vector->list v2)))))

  ;;; --- Submatrix ---

  (define-test "matrix-submatrix-lens view"
    (let ([m (matrix-from-lists '((1 2 3) (4 5 6) (7 8 9)))])
      (let ([sub (^. m (matrix-submatrix-lens 0 0 2 2))])
        (assert-equal 2 (matrix-rows sub))
        (assert-equal 2 (matrix-cols sub))
        (assert-equal 1 (matrix-ref sub 0 0))
        (assert-equal 5 (matrix-ref sub 1 1)))))

  (define-test "matrix-submatrix-lens set"
    (let* ([m (matrix-from-lists '((1 2 3) (4 5 6) (7 8 9)))]
           [new-sub (matrix-from-lists '((10 20) (40 50)))]
           [m2 (& m (.~ (matrix-submatrix-lens 0 0 2 2) new-sub))])
      (assert-equal 10 (matrix-ref m2 0 0))
      (assert-equal 20 (matrix-ref m2 0 1))
      (assert-equal 40 (matrix-ref m2 1 0))
      (assert-equal 50 (matrix-ref m2 1 1))
      (assert-equal 3 (matrix-ref m2 0 2))   ; unchanged
      (assert-equal 9 (matrix-ref m2 2 2)))) ; unchanged

  ;;; --- Convenience Functions ---

  (define-test "matrix-update-cell"
    (let* ([m (matrix-from-lists '((1 2) (3 4)))]
           [m2 (matrix-update-cell m 0 0 (lambda (x) (+ x 100)))])
      (assert-equal 101 (matrix-ref m2 0 0))))

  (define-test "matrix-scale-row"
    (let* ([m (matrix-from-lists '((1 2) (3 4)))]
           [m2 (matrix-scale-row m 0 10)])
      (assert-equal 10 (matrix-ref m2 0 0))
      (assert-equal 20 (matrix-ref m2 0 1))
      (assert-equal 3 (matrix-ref m2 1 0))))  ; unchanged

  ;;; --- Row Operations ---

  (define-test "matrix-swap-rows"
    (let* ([m (matrix-from-lists '((1 2) (3 4)))]
           [m2 (matrix-swap-rows m 0 1)])
      (assert-true (vec-equal? '#(3 4) (matrix-row m2 0)))
      (assert-true (vec-equal? '#(1 2) (matrix-row m2 1)))))

  (define-test "matrix-add-row-scaled"
    (let* ([m (matrix-from-lists '((1 2) (3 4)))]
           [m2 (matrix-add-row-scaled m 0 1 2)])  ; row0 += 2 * row1
      (assert-true (vec-equal? '#(7 10) (matrix-row m2 0)))  ; [1+6, 2+8]
      (assert-true (vec-equal? '#(3 4) (matrix-row m2 1)))))  ; unchanged

  ;;; --- Lens Laws ---

  (define-test "cell-lens get-put law"
    (let* ([m (matrix-from-lists '((1 2) (3 4)))]
           [lens (matrix-cell-lens 0 1)]
           [got (^. m lens)]
           [m2 (& m (.~ lens got))])
      (assert-true (matrix-equal? m m2))))

  (define-test "cell-lens put-get law"
    (let* ([m (matrix-from-lists '((1 2) (3 4)))]
           [lens (matrix-cell-lens 1 0)]
           [m2 (& m (.~ lens 99))])
      (assert-equal 99 (^. m2 lens))))

  (define-test "row-lens get-put law"
    (let* ([m (matrix-from-lists '((1 2) (3 4)))]
           [lens (matrix-row-lens 0)]
           [got (^. m lens)]
           [m2 (& m (.~ lens got))])
      (assert-true (matrix-equal? m m2))))

) ; end test-group

(run-all-tests)
