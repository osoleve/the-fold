;;; lattice/linalg/test-matrix-optics-properties.ss — QuickCheck properties for matrix optics

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'matrix-optics)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define (gen-matrix rows cols)
  (gen-map matrix-from-lists
           (gen-list-of rows (gen-list-of cols gen-entry))))

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-entry
  (gen-int-range -20 20))

(define gen-scalar
  (gen-int-range -6 6))

(define gen-small-dim
  (gen-int-range 0 4))

(define gen-pos-dim
  (gen-int-range 1 4))

(define gen-any-matrix
  (gen-bind gen-small-dim
    (lambda (rows)
      (gen-bind gen-small-dim
        (lambda (cols)
          (gen-matrix rows cols))))))

(define gen-cell-args
  (gen-bind gen-pos-dim
    (lambda (rows)
      (gen-bind gen-pos-dim
        (lambda (cols)
          (gen-bind (gen-matrix rows cols)
            (lambda (m)
              (gen-bind (gen-int-range 0 (- rows 1))
                (lambda (i)
                  (gen-map (lambda (j) (list m i j))
                           (gen-int-range 0 (- cols 1))))))))))))

(define gen-cell-set-args
  (gen-bind gen-cell-args
    (lambda (args)
      (gen-map (lambda (v)
                 (list (car args) (cadr args) (caddr args) v))
               gen-entry))))

(define gen-row-set-args
  (gen-bind gen-pos-dim
    (lambda (rows)
      (gen-bind gen-pos-dim
        (lambda (cols)
          (gen-bind (gen-matrix rows cols)
            (lambda (m)
              (gen-bind (gen-int-range 0 (- rows 1))
                (lambda (i)
                  (gen-map (lambda (xs)
                             (list m i (list->vector xs)))
                           (gen-list-of cols gen-entry)))))))))))

(define gen-col-set-args
  (gen-bind gen-pos-dim
    (lambda (rows)
      (gen-bind gen-pos-dim
        (lambda (cols)
          (gen-bind (gen-matrix rows cols)
            (lambda (m)
              (gen-bind (gen-int-range 0 (- cols 1))
                (lambda (j)
                  (gen-map (lambda (xs)
                             (list m j (list->vector xs)))
                           (gen-list-of rows gen-entry)))))))))))

(define gen-diagonal-set-args
  (gen-bind gen-pos-dim
    (lambda (rows)
      (gen-bind gen-pos-dim
        (lambda (cols)
          (let ([n (min rows cols)])
            (gen-bind (gen-matrix rows cols)
              (lambda (m)
                (gen-map (lambda (xs)
                           (list m (list->vector xs)))
                         (gen-list-of n gen-entry))))))))))

(define gen-submatrix-args
  (gen-bind gen-pos-dim
    (lambda (rows)
      (gen-bind gen-pos-dim
        (lambda (cols)
          (gen-bind (gen-matrix rows cols)
            (lambda (m)
              (gen-bind (gen-int-range 0 (- rows 1))
                (lambda (r1)
                  (gen-bind (gen-int-range (+ r1 1) rows)
                    (lambda (r2)
                      (gen-bind (gen-int-range 0 (- cols 1))
                        (lambda (c1)
                          (gen-map (lambda (c2)
                                     (list m r1 c1 r2 c2))
                                   (gen-int-range (+ c1 1) cols)))))))))))))))

(define gen-row-scale-args
  (gen-bind gen-pos-dim
    (lambda (rows)
      (gen-bind gen-pos-dim
        (lambda (cols)
          (gen-bind (gen-matrix rows cols)
            (lambda (m)
              (gen-bind (gen-int-range 0 (- rows 1))
                (lambda (i)
                  (gen-map (lambda (k) (list m i k))
                           gen-scalar))))))))))

(define gen-col-scale-args
  (gen-bind gen-pos-dim
    (lambda (rows)
      (gen-bind gen-pos-dim
        (lambda (cols)
          (gen-bind (gen-matrix rows cols)
            (lambda (m)
              (gen-bind (gen-int-range 0 (- cols 1))
                (lambda (j)
                  (gen-map (lambda (k) (list m j k))
                           gen-scalar))))))))))

(define gen-row-op-args
  (gen-bind gen-pos-dim
    (lambda (rows)
      (gen-bind gen-pos-dim
        (lambda (cols)
          (gen-bind (gen-matrix rows cols)
            (lambda (m)
              (gen-bind (gen-int-range 0 (- rows 1))
                (lambda (dst)
                  (gen-bind (gen-int-range 0 (- rows 1))
                    (lambda (src)
                      (gen-map (lambda (k)
                                 (list m dst src k))
                               gen-scalar))))))))))))

;;; ============================================================================
;;; Lens and affine properties
;;; ============================================================================

(test-group matrix-optics-lens-properties

  (define-property "cell lens get-put law"
    gen-cell-args
    (lambda (args)
      (let* ([m (car args)]
             [i (cadr args)]
             [j (caddr args)]
             [lens (matrix-cell-lens i j)]
             [got (^. m lens)]
             [m2 (& m (.~ lens got))])
        (matrix-equal? m m2)))
    'tests 220)

  (define-property "cell lens put-get law"
    gen-cell-set-args
    (lambda (args)
      (let* ([m (car args)]
             [i (cadr args)]
             [j (caddr args)]
             [val (cadddr args)]
             [lens (matrix-cell-lens i j)]
             [m2 (& m (.~ lens val))])
        (equal? (^. m2 lens) val)))
    'tests 220)

  (define-property "row lens get-put law"
    gen-row-set-args
    (lambda (args)
      (let* ([m (car args)]
             [i (cadr args)]
             [lens (matrix-row-lens i)]
             [got (^. m lens)]
             [m2 (& m (.~ lens got))])
        (matrix-equal? m m2)))
    'tests 180)

  (define-property "column lens put-get law"
    gen-col-set-args
    (lambda (args)
      (let* ([m (car args)]
             [j (cadr args)]
             [new-col (caddr args)]
             [lens (matrix-col-lens j)]
             [m2 (& m (.~ lens new-col))])
        (vec-equal? (^. m2 lens) new-col)))
    'tests 180)

  (define-property "diagonal lens get-put law"
    gen-diagonal-set-args
    (lambda (args)
      (let* ([m (car args)]
             [lens matrix-diagonal-lens]
             [got (^. m lens)]
             [m2 (& m (.~ lens got))])
        (matrix-equal? m m2)))
    'tests 180)

  (define-property "submatrix lens get-put law"
    gen-submatrix-args
    (lambda (args)
      (let* ([m (car args)]
             [r1 (cadr args)]
             [c1 (caddr args)]
             [r2 (cadddr args)]
             [c2 (car (cddddr args))]
             [lens (matrix-submatrix-lens r1 c1 r2 c2)]
             [sub (^. m lens)]
             [m2 (& m (.~ lens sub))])
        (matrix-equal? m m2)))
    'tests 150)

  (define-property "row-cell alias matches matrix-cell-lens"
    gen-cell-args
    (lambda (args)
      (let* ([m (car args)]
             [i (cadr args)]
             [j (caddr args)])
        (equal? (^. m (matrix-row-cell i j))
                (^. m (matrix-cell-lens i j)))))
    'tests 220)

  (define-property "cell affine in-bounds preview matches cell lens"
    gen-cell-args
    (lambda (args)
      (let* ([m (car args)]
             [i (cadr args)]
             [j (caddr args)]
             [v1 (^. m (matrix-cell-lens i j))]
             [v2 (^? m (matrix-cell-affine i j))])
        (and (just? v2)
             (equal? v1 (from-just v2)))))
    'tests 180)
)

;;; ============================================================================
;;; Traversal and convenience-operation properties
;;; ============================================================================

(test-group matrix-optics-traversal-properties

  (define-property "matrix-cells-each fold length is rows*cols"
    gen-any-matrix
    (lambda (m)
      (= (length (^.. m matrix-cells-each))
         (* (matrix-rows m) (matrix-cols m))))
    'tests 220)

  (define-property "matrix-cells-each identity traversal leaves matrix unchanged"
    gen-any-matrix
    (lambda (m)
      (matrix-equal? (traversal-over matrix-cells-each (lambda (x) x) m)
                     m))
    'tests 220)

  (define-property "matrix-rows-each identity traversal leaves matrix unchanged"
    gen-any-matrix
    (lambda (m)
      (matrix-equal? (traversal-over matrix-rows-each (lambda (row) row) m)
                     m))
    'tests 200)

  (define-property "matrix-cols-each identity traversal leaves matrix unchanged"
    gen-any-matrix
    (lambda (m)
      (matrix-equal? (traversal-over matrix-cols-each (lambda (col) col) m)
                     m))
    'tests 200)

  (define-property "matrix-update-cell equals direct cell-lens over"
    gen-cell-args
    (lambda (args)
      (let* ([m (car args)]
             [i (cadr args)]
             [j (caddr args)]
             [f (lambda (x) (+ x 1))])
        (matrix-equal? (matrix-update-cell m i j f)
                       (& m (%~ (matrix-cell-lens i j) f)))))
    'tests 220)

  (define-property "matrix-scale-row matches matrix-update-row definition"
    gen-row-scale-args
    (lambda (args)
      (let* ([m (car args)]
             [i (cadr args)]
             [k (caddr args)])
        (matrix-equal? (matrix-scale-row m i k)
                       (matrix-update-row m i (lambda (row) (vec-scale k row))))))
    'tests 180)

  (define-property "matrix-scale-col matches matrix-update-col definition"
    gen-col-scale-args
    (lambda (args)
      (let* ([m (car args)]
             [j (cadr args)]
             [k (caddr args)])
        (matrix-equal? (matrix-scale-col m j k)
                       (matrix-update-col m j (lambda (col) (vec-scale k col))))))
    'tests 180)

  (define-property "matrix-swap-rows is involutive"
    gen-row-op-args
    (lambda (args)
      (let* ([m (car args)]
             [i (cadr args)]
             [j (caddr args)]
             [m2 (matrix-swap-rows (matrix-swap-rows m i j) i j)])
        (matrix-equal? m m2)))
    'tests 200)

  (define-property "matrix-add-row-scaled with k=0 is identity"
    gen-row-op-args
    (lambda (args)
      (let* ([m (car args)]
             [dst (cadr args)]
             [src (caddr args)])
        (matrix-equal? (matrix-add-row-scaled m dst src 0)
                       m)))
    'tests 200)

  (define-property "matrix-add-row-scaled matches its row-update definition"
    gen-row-op-args
    (lambda (args)
      (let* ([m (car args)]
             [dst (cadr args)]
             [src (caddr args)]
             [k (cadddr args)]
             [row-src (^. m (matrix-row-lens src))]
             [expected (matrix-update-row
                        m dst
                        (lambda (row-dst)
                          (vec-add row-dst (vec-scale k row-src))))])
        (matrix-equal? (matrix-add-row-scaled m dst src k)
                       expected)))
    'tests 180)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
