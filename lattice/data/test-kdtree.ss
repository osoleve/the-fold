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

;;; ============================================================
;;; Delete Tests
;;; ============================================================

(define-test "kdtree-member? basic"
  (let ([tree (kdtree-build '((1 2) (3 4) (5 6)))])
    (assert-true (kdtree-member? tree '(1 2) 2))
    (assert-true (kdtree-member? tree '(3 4) 2))
    (assert-true (kdtree-member? tree '(5 6) 2))
    (assert-false (kdtree-member? tree '(0 0) 2))
    (assert-false (kdtree-member? tree '(1 3) 2))))

(define-test "kdtree-delete leaf"
  ;; Delete from single-node tree
  (let* ([tree (kdtree-build '((5 5)))]
         [tree2 (kdtree-delete-2d tree '(5 5))])
    (assert-true (kdtree-empty? tree2))))

(define-test "kdtree-delete non-existent"
  (let* ([tree (kdtree-build '((1 1) (5 5)))]
         [tree2 (kdtree-delete-2d tree '(99 99))])
    ;; Should be unchanged
    (assert-equal 2 (kdtree-size tree2))))

(define-test "kdtree-delete root"
  (let* ([tree (kdtree-build '((5 5) (2 2) (8 8)))]
         [root-point (kdtree-point tree)]
         [tree2 (kdtree-delete-2d tree root-point)])
    (assert-equal 2 (kdtree-size tree2))
    (assert-false (kdtree-member? tree2 root-point 2))))

(define-test "kdtree-delete maintains structure"
  ;; Delete from larger tree, verify remaining points still queryable
  (let* ([points '((2 3) (5 4) (9 6) (4 7) (8 1) (7 2))]
         [tree (kdtree-build points)]
         [tree2 (kdtree-delete-2d tree '(5 4))]
         [tree3 (kdtree-delete-2d tree2 '(8 1))])
    (assert-equal 4 (kdtree-size tree3))
    ;; Remaining points should still be findable
    (assert-equal '(2 3) (kdtree-nearest tree3 '(2 3)))
    (assert-equal '(9 6) (kdtree-nearest tree3 '(9 6)))
    (assert-equal '(4 7) (kdtree-nearest tree3 '(4 7)))
    (assert-equal '(7 2) (kdtree-nearest tree3 '(7 2)))))

(define-test "kdtree-delete all points"
  (let* ([points '((1 1) (2 2) (3 3))]
         [tree (kdtree-build points)]
         [tree2 (kdtree-delete-2d tree '(1 1))]
         [tree3 (kdtree-delete-2d tree2 '(2 2))]
         [tree4 (kdtree-delete-2d tree3 '(3 3))])
    (assert-true (kdtree-empty? tree4))))

(define-test "kdtree-delete 3d"
  (let* ([points '((1 2 3) (4 5 6) (7 8 9))]
         [tree (kdtree-build points)]
         [tree2 (kdtree-delete-3d tree '(4 5 6))])
    (assert-equal 2 (kdtree-size tree2))
    (assert-true (kdtree-member? tree2 '(1 2 3) 3))
    (assert-false (kdtree-member? tree2 '(4 5 6) 3))
    (assert-true (kdtree-member? tree2 '(7 8 9) 3))))

(define-test "kdtree-delete stress"
  ;; Build grid, delete half, verify queries still work
  (let* ([points (apply append
                        (map (lambda (x)
                               (map (lambda (y) (list x y))
                                    (iota 10)))
                             (iota 10)))]
         [tree (kdtree-build points)]
         ;; Delete all even-indexed points
         [even-points (filter (lambda (p) (even? (+ (car p) (cadr p)))) points)]
         [tree2 (fold-left (lambda (t pt) (kdtree-delete-2d t pt)) tree even-points)])
    ;; Should have about half remaining
    (assert-true (< (kdtree-size tree2) 60))
    ;; Odd-indexed points should still be findable
    (let ([odd-points (filter (lambda (p) (odd? (+ (car p) (cadr p)))) points)])
      (for-each (lambda (p)
                  (assert-equal p (kdtree-nearest tree2 p)))
                (take odd-points 10)))))  ; test subset for speed

(define-test "kdtree-find-min on split axis"
  ;; Test find-min when search axis matches split axis
  (let ([tree (kdtree-build '((5 5) (2 3) (8 7) (1 8) (3 2)))])
    ;; min on x-axis (axis 0) should be point with x=1
    (let ([min-pt (kdtree-find-min tree 0 2 0)])
      (assert-equal 1 (car min-pt)))
    ;; min on y-axis (axis 1) should be point with y=2
    (let ([min-pt (kdtree-find-min tree 1 2 0)])
      (assert-equal 2 (cadr min-pt)))))

(define-test "kdtree-delete with duplicate coordinates"
  ;; Test deletion when multiple points share same coordinate on split axis
  (let* ([points '((5 1) (5 2) (5 3) (5 4) (5 5))]  ; all same x
         [tree (kdtree-build points)])
    (assert-equal 5 (kdtree-size tree))
    ;; Delete one point
    (let ([tree2 (kdtree-delete-2d tree '(5 3))])
      (assert-equal 4 (kdtree-size tree2))
      (assert-false (kdtree-member? tree2 '(5 3) 2))
      ;; Other points still present
      (assert-true (kdtree-member? tree2 '(5 1) 2))
      (assert-true (kdtree-member? tree2 '(5 5) 2)))))

;; Helper for stress test
(define (take lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

(run-all-tests)
