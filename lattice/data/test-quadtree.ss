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
              (take points 10))))  ; test first 10 for speed

;; Helper
(define (take lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

(run-all-tests)
