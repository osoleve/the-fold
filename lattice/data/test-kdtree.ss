;;; lattice/data/test-kdtree.ss — K-D Tree Tests

(load "core/testing/test-framework.ss")
(load "lattice/data/kdtree.ss")

(define-test "kdtree-empty?"
  (assert-true (kdtree-empty? kdtree-empty))
  (assert-false (kdtree-empty? (kdtree-build '((0 0))))))

(define-test "kdtree-build single point"
  (let ([tree (kdtree-build '((1 2)))])
    (assert-false (kdtree-empty? tree))
    (assert-equal '(1 2) (kdtree-point tree))
    (assert-true (kdtree-empty? (kdtree-left tree)))
    (assert-true (kdtree-empty? (kdtree-right tree)))))

(define-test "kdtree-build multiple points"
  (let ([tree (kdtree-build '((2 3) (5 4) (9 6) (4 7) (8 1) (7 2)))])
    (assert-equal 6 (kdtree-size tree))
    (assert-true (kdtree-balanced? tree))))

(define-test "list->kdtree alias"
  ;; list->kdtree is alias for kdtree-build
  (let ([tree (list->kdtree '((1 2) (3 4) (5 6)))])
    (assert-equal 3 (kdtree-size tree))))

(define-test "kdtree-build 3D points"
  (let ([tree (kdtree-build '((1 2 3) (4 5 6) (7 8 9) (2 3 4)))])
    (assert-equal 4 (kdtree-size tree))
    (assert-true (kdtree-balanced? tree))))

(define-test "point-distance-sq"
  (assert-equal 0 (point-distance-sq '(0 0) '(0 0)))
  (assert-equal 2 (point-distance-sq '(0 0) '(1 1)))
  (assert-equal 25 (point-distance-sq '(0 0) '(3 4)))
  (assert-equal 3 (point-distance-sq '(1 1 1) '(2 2 2))))

(define-test "point-in-box?"
  (assert-true (point-in-box? '(5 5) '(0 0) '(10 10)))
  (assert-true (point-in-box? '(0 0) '(0 0) '(10 10)))  ; on boundary
  (assert-true (point-in-box? '(10 10) '(0 0) '(10 10)))  ; on boundary
  (assert-false (point-in-box? '(11 5) '(0 0) '(10 10)))
  (assert-false (point-in-box? '(-1 5) '(0 0) '(10 10))))

(define-test "kdtree-nearest simple"
  (let ([tree (kdtree-build '((0 0) (10 10) (5 5)))])
    (assert-equal '(0 0) (kdtree-nearest tree '(1 1)))
    (assert-equal '(10 10) (kdtree-nearest tree '(9 9)))
    (assert-equal '(5 5) (kdtree-nearest tree '(5 5)))))  ; exact match

(define-test "kdtree-nearest empty"
  (assert-false (kdtree-nearest kdtree-empty '(0 0))))

(define-test "kdtree-nearest with many points"
  (let* ([points '((2 3) (5 4) (9 6) (4 7) (8 1) (7 2))]
         [tree (kdtree-build points)])
    ;; Query close to (2,3)
    (assert-equal '(2 3) (kdtree-nearest tree '(2 3)))
    ;; Query close to (8,1)
    (assert-equal '(8 1) (kdtree-nearest tree '(8 0)))
    ;; Query in middle - should find closest
    (let ([result (kdtree-nearest tree '(6 5))])
      ;; (5,4) is distance sqrt(2), (7,2) is sqrt(10), (9,6) is sqrt(10)
      (assert-equal '(5 4) result))))

(define-test "kdtree-knn basic"
  (let ([tree (kdtree-build '((0 0) (1 1) (2 2) (10 10)))])
    ;; Find 2 nearest to origin
    (let ([result (kdtree-knn tree '(0 0) 2)])
      (assert-equal 2 (length result))
      (assert-equal '(0 0) (car result))
      (assert-equal '(1 1) (cadr result)))
    ;; Find 3 nearest
    (let ([result (kdtree-knn tree '(0 0) 3)])
      (assert-equal 3 (length result))
      (assert-equal '(2 2) (caddr result)))))

(define-test "kdtree-knn more than available"
  (let ([tree (kdtree-build '((0 0) (1 1)))])
    ;; Ask for 5, only 2 exist
    (let ([result (kdtree-knn tree '(0 0) 5)])
      (assert-equal 2 (length result)))))

(define-test "kdtree-range basic"
  (let ([tree (kdtree-build '((1 1) (5 5) (9 9) (3 3) (7 7)))])
    ;; Range that includes (3,3) and (5,5)
    (let ([result (kdtree-range tree '(2 2) '(6 6))])
      (assert-equal 2 (length result))
      (assert-true (not (not (member '(3 3) result))))
      (assert-true (not (not (member '(5 5) result)))))))

(define-test "kdtree-range empty result"
  (let ([tree (kdtree-build '((0 0) (10 10)))])
    (let ([result (kdtree-range tree '(4 4) '(6 6))])
      (assert-equal 0 (length result)))))

(define-test "kdtree-range all points"
  (let* ([points '((1 1) (2 2) (3 3))]
         [tree (kdtree-build points)])
    (let ([result (kdtree-range tree '(0 0) '(10 10))])
      (assert-equal 3 (length result)))))

(define-test "kdtree-radius"
  (let ([tree (kdtree-build '((0 0) (1 0) (0 1) (5 5)))])
    ;; Radius 1.5 from origin should get (0,0), (1,0), (0,1)
    (let ([result (kdtree-radius tree '(0 0) 1.5)])
      (assert-equal 3 (length result))
      (assert-true (not (not (member '(0 0) result))))
      (assert-true (not (not (member '(1 0) result))))
      (assert-true (not (not (member '(0 1) result))))
      (assert-false (member '(5 5) result)))))

(define-test "kdtree-insert"
  (let* ([tree (kdtree-build '((0 0) (10 10)))]
         [tree2 (kdtree-insert tree '(5 5) 2)])
    (assert-equal 3 (kdtree-size tree2))
    (assert-equal '(5 5) (kdtree-nearest tree2 '(5 5)))))

(define-test "kdtree-fold"
  (let ([tree (kdtree-build '((1 2) (3 4) (5 6)))])
    ;; Count points via fold (acc first, then point)
    (assert-equal 3 (kdtree-fold (lambda (acc pt) (+ 1 acc)) 0 tree))
    ;; Sum x coordinates
    (assert-equal 9 (kdtree-fold (lambda (acc pt) (+ (car pt) acc)) 0 tree))
    ;; Collect points
    (assert-equal 3 (length (kdtree-fold (lambda (acc pt) (cons pt acc)) '() tree)))))

(define-test "kdtree->list"
  (let* ([points '((2 3) (5 4) (9 6))]
         [tree (kdtree-build points)]
         [result (kdtree->list tree)])
    (assert-equal 3 (length result))
    ;; All original points should be in result
    (for-each (lambda (p) (assert-true (not (not (member p result))))) points)))

(define-test "kdtree 3D nearest"
  (let ([tree (kdtree-build '((0 0 0) (1 1 1) (2 2 2) (10 10 10)))])
    (assert-equal '(0 0 0) (kdtree-nearest tree '(0 0 0)))
    (assert-equal '(1 1 1) (kdtree-nearest tree '(1 1 0.9)))
    (assert-equal '(10 10 10) (kdtree-nearest tree '(9 9 9)))))

(define-test "kdtree height logarithmic"
  ;; Build tree with 1000 points, height should be ~10 (log2(1000) ≈ 10)
  (let* ([points (map (lambda (i) (list i (* 2 i))) (iota 100))]
         [tree (kdtree-build points)]
         [h (kdtree-height tree)])
    ;; Height should be reasonable for balanced tree
    (assert-true (<= h 15))))  ; 2*log2(100) ≈ 14

(define-test "kdtree stress test nearest"
  ;; Grid of points, verify nearest is correct for random queries
  (let* ([points (apply append
                        (map (lambda (x)
                               (map (lambda (y) (list x y))
                                    (iota 10)))
                             (iota 10)))]
         [tree (kdtree-build points)])
    ;; Query at each grid point should return itself
    (for-each (lambda (p)
                (assert-equal p (kdtree-nearest tree p)))
              points)))

(run-all-tests)
