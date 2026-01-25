;;; lattice/data/test-quadtree.ss — Quadtree Tests

(load "core/testing/test-framework.ss")
(load "lattice/data/quadtree.ss")

(define-test "bounds-contains?"
  (let ([b (make-bounds 0 0 10 10)])  ; box from -10,-10 to 10,10
    (assert-true (bounds-contains? b 0 0))
    (assert-true (bounds-contains? b 5 5))
    (assert-true (bounds-contains? b -10 -10))  ; edge
    (assert-true (bounds-contains? b 10 10))    ; edge
    (assert-false (bounds-contains? b 11 0))
    (assert-false (bounds-contains? b 0 11))))

(define-test "bounds-intersects?"
  (let ([b1 (make-bounds 0 0 10 10)]       ; -10,-10 to 10,10
        [b2 (make-bounds 50 50 10 10)]     ; 40,40 to 60,60 - truly disjoint
        [b3 (make-bounds 5 5 10 10)])      ; -5,-5 to 15,15 - overlaps b1
    (assert-false (bounds-intersects? b1 b2))  ; disjoint
    (assert-true (bounds-intersects? b1 b3))   ; overlap
    (assert-true (bounds-intersects? b1 b1)))) ; same

(define-test "quadtree-create"
  (let ([tree (quadtree-create (make-bounds 0 0 100 100))])
    (assert-true (quadtree-leaf? tree))
    (assert-equal 0 (quadtree-size tree))))

(define-test "quadtree-insert single"
  (let* ([tree (quadtree-create (make-bounds 0 0 100 100))]
         [tree2 (quadtree-insert tree 5 5 'point-a)])
    (assert-equal 1 (quadtree-size tree2))))

(define-test "quadtree-insert multiple no split"
  (let* ([tree (quadtree-create (make-bounds 0 0 100 100))]
         [tree2 (fold-left (lambda (t i)
                            (quadtree-insert t i i (list 'pt i)))
                          tree
                          '(1 2 3))])  ; 3 points, under capacity of 4
    (assert-equal 3 (quadtree-size tree2))
    (assert-true (quadtree-leaf? tree2))))

(define-test "quadtree-insert causes split"
  (let* ([tree (quadtree-create (make-bounds 0 0 100 100))]
         [tree2 (fold-left (lambda (t i)
                            (quadtree-insert t i i (list 'pt i)))
                          tree
                          '(1 2 3 4 5))])  ; 5 points, causes split
    (assert-equal 5 (quadtree-size tree2))
    (assert-true (quadtree-node? tree2))))

(define-test "quadtree-build-auto"
  (let* ([points '((0 0) (10 10) (5 5) (-3 7))]
         [tree (quadtree-build-auto points)])
    (assert-equal 4 (quadtree-size tree))))

(define-test "list->quadtree alias"
  ;; list->quadtree is alias for quadtree-build-auto
  (let* ([points '((1 2) (3 4) (5 6))]
         [tree (list->quadtree points)])
    (assert-equal 3 (quadtree-size tree))))

(define-test "quadtree-range-rect"
  (let* ([points '((1 1) (5 5) (9 9) (3 3) (7 7))]
         [tree (quadtree-build-auto points)]
         [result (quadtree-range-rect tree 2 2 6 6)])
    ;; Should find (3,3) and (5,5)
    (assert-equal 2 (length result))
    (assert-true (not (not (member '(3 3) (map (lambda (p) (list (car p) (cadr p))) result)))))
    (assert-true (not (not (member '(5 5) (map (lambda (p) (list (car p) (cadr p))) result)))))))

(define-test "quadtree-range-rect empty"
  (let* ([points '((0 0) (10 10))]
         [tree (quadtree-build-auto points)]
         [result (quadtree-range-rect tree 4 4 6 6)])
    (assert-equal 0 (length result))))

(define-test "quadtree-radius"
  (let* ([points '((0 0) (1 0) (0 1) (5 5))]
         [tree (quadtree-build-auto points)]
         [result (quadtree-radius tree 0 0 1.5)])
    ;; (0,0), (1,0), (0,1) are within radius 1.5
    (assert-equal 3 (length result))))

(define-test "quadtree-nearest basic"
  (let* ([points '((0 0) (10 10) (5 5))]
         [tree (quadtree-build-auto points)])
    ;; Each point's x,y stored with data
    (let ([nearest (quadtree-nearest tree 1 1)])
      (assert-equal 0 (car nearest))
      (assert-equal 0 (cadr nearest)))
    (let ([nearest (quadtree-nearest tree 9 9)])
      (assert-equal 10 (car nearest))
      (assert-equal 10 (cadr nearest)))))

(define-test "quadtree-nearest empty"
  (let ([tree (quadtree-create (make-bounds 0 0 10 10))])
    (assert-false (quadtree-nearest tree 5 5))))

(define-test "quadtree-fold"
  (let* ([points '((1 2) (3 4) (5 6))]
         [tree (quadtree-build-auto points)])
    ;; Count points via fold (acc first, then point)
    (assert-equal 3 (quadtree-fold (lambda (acc pt) (+ 1 acc)) 0 tree))
    ;; Sum x coordinates (points stored as (x y data) where data=(x y))
    (assert-equal 9 (quadtree-fold (lambda (acc pt) (+ (car pt) acc)) 0 tree))
    ;; Collect points
    (assert-equal 3 (length (quadtree-fold (lambda (acc pt) (cons pt acc)) '() tree)))))

(define-test "quadtree->list"
  (let* ([points '((1 2) (3 4) (5 6))]
         [tree (quadtree-build-auto points)]
         [result (quadtree->list tree)])
    (assert-equal 3 (length result))))

(define-test "quadtree-depth single leaf"
  (let* ([points '((1 1) (2 2))]
         [tree (quadtree-build-auto points)])
    (assert-equal 1 (quadtree-depth tree))))

(define-test "quadtree-depth with split"
  (let* ([points '((0 0) (10 10) (20 20) (30 30) (40 40))]
         [tree (quadtree-build-auto points)])
    (assert-true (> (quadtree-depth tree) 1))))

(define-test "quadtree duplicate points no infinite loop"
  ;; This would cause infinite recursion before the fix
  (let* ([points '((5 5) (5 5) (5 5) (5 5) (5 5) (5 5) (5 5) (5 5))]
         [tree (quadtree-build-auto points)])
    (assert-equal 8 (quadtree-size tree))
    ;; Should complete without stack overflow
    (assert-true (> (quadtree-depth tree) 0))))

(define-test "quadtree many points"
  ;; Grid of 100 points
  (let* ([points (apply append
                        (map (lambda (x)
                               (map (lambda (y) (list x y))
                                    (iota 10)))
                             (iota 10)))]
         [tree (quadtree-build-auto points)])
    (assert-equal 100 (quadtree-size tree))
    ;; Each point should find itself as nearest
    (for-each (lambda (pt)
                (let ([nearest (quadtree-nearest tree (car pt) (cadr pt))])
                  (assert-equal (car pt) (car nearest))
                  (assert-equal (cadr pt) (cadr nearest))))
              (take 10 points))))  ; test first 10 for speed

;; Note: Uses take from prelude.ss with signature (take n lst)

;;; ============================================================
;;; Delete Tests
;;; ============================================================

(define-test "quadtree-member? basic"
  (let* ([points '((1 1) (5 5) (9 9))]
         [tree (quadtree-build-auto points)])
    (assert-true (quadtree-member? tree 1 1))
    (assert-true (quadtree-member? tree 5 5))
    (assert-true (quadtree-member? tree 9 9))
    (assert-false (quadtree-member? tree 3 3))
    (assert-false (quadtree-member? tree 0 0))))

(define-test "quadtree-delete single point"
  (let* ([points '((1 1) (5 5) (9 9))]
         [tree (quadtree-build-auto points)]
         [tree2 (quadtree-delete tree 5 5)])
    (assert-equal 2 (quadtree-size tree2))
    (assert-true (quadtree-member? tree2 1 1))
    (assert-false (quadtree-member? tree2 5 5))
    (assert-true (quadtree-member? tree2 9 9))))

(define-test "quadtree-delete non-existent"
  (let* ([points '((1 1) (5 5))]
         [tree (quadtree-build-auto points)]
         [tree2 (quadtree-delete tree 99 99)])
    ;; Should be unchanged
    (assert-equal 2 (quadtree-size tree2))))

(define-test "quadtree-delete all points"
  (let* ([tree (quadtree-build-auto '((1 1) (2 2)))]
         [tree2 (quadtree-delete tree 1 1)]
         [tree3 (quadtree-delete tree2 2 2)])
    (assert-equal 0 (quadtree-size tree3))))

(define-test "quadtree-delete triggers merge"
  ;; Create tree that splits, then delete to trigger merge
  (let* ([points '((0 0) (10 10) (20 20) (30 30) (40 40))]  ; causes split
         [tree (quadtree-build-auto points)])
    (assert-true (quadtree-node? tree))
    ;; Delete enough points to allow merging
    (let* ([t2 (quadtree-delete tree 40 40)]
           [t3 (quadtree-delete t2 30 30)]
           [t4 (quadtree-delete t3 20 20)])
      ;; With only 2 points remaining, should have merged to leaf
      (assert-equal 2 (quadtree-size t4))
      (assert-true (quadtree-leaf? t4)))))

(define-test "quadtree-delete-if with predicate"
  ;; Store points with data: (x y data)
  (let* ([tree (quadtree-create (make-bounds 50 50 100 100))]
         [tree (quadtree-insert tree 10 10 'a)]
         [tree (quadtree-insert tree 10 10 'b)]  ; duplicate coords, different data
         [tree (quadtree-insert tree 20 20 'c)])
    (assert-equal 3 (quadtree-size tree))
    ;; Delete only the 'b point at (10,10)
    (let ([tree2 (quadtree-delete-if tree 10 10
                                     (lambda (pt) (eq? (caddr pt) 'b)))])
      (assert-equal 2 (quadtree-size tree2))
      ;; 'a at (10,10) should still exist
      (assert-true (quadtree-member? tree2 10 10)))))

(define-test "quadtree-delete point on boundary"
  ;; Test deletion of point exactly on quadrant boundaries
  (let* ([tree (quadtree-create (make-bounds 0 0 10 10))]  ; bounds from -10,-10 to 10,10
         [tree (quadtree-insert tree 0 0 'origin)]         ; exactly at center
         [tree (quadtree-insert tree 0 5 'on-y-axis)]      ; on y-axis
         [tree (quadtree-insert tree 5 0 'on-x-axis)])     ; on x-axis
    (assert-equal 3 (quadtree-size tree))
    ;; Delete origin point
    (let ([tree2 (quadtree-delete tree 0 0)])
      (assert-equal 2 (quadtree-size tree2))
      (assert-false (quadtree-member? tree2 0 0))
      (assert-true (quadtree-member? tree2 0 5))
      (assert-true (quadtree-member? tree2 5 0)))))

(run-all-tests)
