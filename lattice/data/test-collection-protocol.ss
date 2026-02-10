;;; lattice/data/test-collection-protocol.ss — Tests for Collection Protocol
;;; @module test-collection-protocol

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/data/collection-impl.ss")

(display "\n=== Testing Collection Protocol System ===\n\n")

;;; ============================================================
;;; Test Data Setup
;;; ============================================================

;; AVL tree with some key-value pairs
(define test-avl
  (fold-left (lambda (tree pair)
               (avl-insert (car pair) (cdr pair) tree))
             avl-empty
             '((3 . "three") (1 . "one") (4 . "four") (1 . "uno") (5 . "five") (9 . "nine"))))

;; Heap with some numbers
(define test-heap
  (list->heap '(3 1 4 1 5 9 2 6)))

;; KD-tree with 2D points
(define test-kdtree
  (kdtree-build '((2 3) (5 4) (9 6) (4 7) (8 1) (7 2))))

;; Quadtree with 2D points
(define test-quadtree
  (quadtree-build-auto '((2 3) (5 4) (9 6) (4 7) (8 1) (7 2))))

;;; ============================================================
;;; Core Protocol Tests
;;; ============================================================

(test-group "Core Protocols"

  (define-test "coll-empty? on non-empty collections"
    (assert-false (coll-empty? test-avl))
    (assert-false (coll-empty? test-heap))
    (assert-false (coll-empty? test-kdtree))
    (assert-false (coll-empty? test-quadtree)))

  (define-test "coll-size returns correct counts"
    (assert-equal 5 (coll-size test-avl))    ; 5 unique keys (1 is duplicated, updates)
    (assert-equal 8 (coll-size test-heap))   ; 8 elements
    (assert-equal 6 (coll-size test-kdtree)) ; 6 points
    (assert-equal 6 (coll-size test-quadtree))) ; 6 points

  (define-test "coll-fold accumulates correctly"
    ;; AVL: fold over (key . value) pairs, count pairs
    (assert-equal 5 (coll-fold test-avl (lambda (acc elem) (+ acc 1)) 0))
    ;; Heap: fold over elements, sum them
    (assert-equal 31 (coll-fold test-heap (lambda (acc elem) (+ acc elem)) 0))
    ;; KD-tree: fold over points, count them
    (assert-equal 6 (coll-fold test-kdtree (lambda (acc pt) (+ acc 1)) 0))
    ;; Quadtree: fold over points, count them
    (assert-equal 6 (coll-fold test-quadtree (lambda (acc pt) (+ acc 1)) 0)))

  (define-test "coll-to-list converts to list"
    (assert-equal 5 (length (coll-to-list test-avl)))
    (assert-equal 8 (length (coll-to-list test-heap)))
    (assert-equal 6 (length (coll-to-list test-kdtree)))
    (assert-equal 6 (length (coll-to-list test-quadtree)))))

;;; ============================================================
;;; Empty Collection Tests
;;; ============================================================

(test-group "Empty Collections"

  (define-test "coll-empty? on empty collections"
    (assert-true (coll-empty? avl-empty))
    (assert-true (coll-empty? heap-empty))
    (assert-true (coll-empty? kdtree-empty))
    (assert-true (coll-empty? quadtree-empty)))

  (define-test "coll-size on empty collections"
    (assert-equal 0 (coll-size avl-empty))
    (assert-equal 0 (coll-size heap-empty))
    (assert-equal 0 (coll-size kdtree-empty))
    (assert-equal 0 (coll-size quadtree-empty)))

  (define-test "coll-fold on empty collections"
    (assert-equal 42 (coll-fold avl-empty (lambda (acc x) (+ acc 1)) 42))
    (assert-equal 42 (coll-fold heap-empty (lambda (acc x) (+ acc 1)) 42))
    (assert-equal 42 (coll-fold kdtree-empty (lambda (acc x) (+ acc 1)) 42))
    (assert-equal 42 (coll-fold quadtree-empty (lambda (acc x) (+ acc 1)) 42)))

  (define-test "coll-to-list on empty collections"
    (assert-equal '() (coll-to-list avl-empty))
    (assert-equal '() (coll-to-list heap-empty))
    (assert-equal '() (coll-to-list kdtree-empty))
    (assert-equal '() (coll-to-list quadtree-empty)))

  (define-test "keyed protocols on empty AVL"
    (assert-false (keyed-lookup avl-empty 'anything))
    (assert-false (keyed-contains? avl-empty 'anything))
    (assert-equal '() (keyed-keys avl-empty))
    (assert-equal '() (keyed-values avl-empty))
    ;; keyed-insert on empty creates new tree
    (let ([new-avl (keyed-insert avl-empty 1 "one")])
      (assert-equal "one" (keyed-lookup new-avl 1))))

  (define-test "priority protocols on empty heap"
    ;; prio-insert on empty creates new heap
    (let ([new-heap (prio-insert heap-empty 42)])
      (assert-equal 42 (prio-peek new-heap))
      (assert-equal 1 (coll-size new-heap)))
    ;; prio-merge with empty returns the other
    (let ([h (list->heap '(1 2 3))])
      (assert-equal 3 (coll-size (prio-merge heap-empty h)))))

  (define-test "spatial protocols on empty kdtree"
    (assert-false (spatial-nearest kdtree-empty '(0 0)))
    (assert-equal '() (spatial-knn kdtree-empty '(0 0) 5))
    (assert-equal '() (spatial-range kdtree-empty '(0 0) '(10 10)))
    (assert-equal '() (spatial-radius kdtree-empty '(0 0) 100))
    (assert-false (spatial-contains? kdtree-empty '(0 0))))

  (define-test "spatial protocols on empty quadtree"
    (assert-false (spatial-nearest quadtree-empty '(0 0)))
    (assert-equal '() (spatial-range quadtree-empty '(0 0) '(10 10)))
    (assert-equal '() (spatial-radius quadtree-empty '(0 0) 100))
    (assert-false (spatial-contains? quadtree-empty '(0 0)))))

;;; ============================================================
;;; Generic Operations Tests
;;; ============================================================

(test-group "Generic Operations"

  (define-test "coll-count counts matching elements"
    ;; Count heap elements > 5
    (assert-equal 2 (coll-count test-heap (lambda (x) (> x 5))))
    ;; Count AVL pairs where key > 3
    (assert-equal 3 (coll-count test-avl (lambda (pair) (> (car pair) 3)))))

  (define-test "coll-any? finds matches"
    (assert-true (coll-any? test-heap (lambda (x) (= x 9))))
    (assert-false (coll-any? test-heap (lambda (x) (= x 100)))))

  (define-test "coll-all? checks all elements"
    (assert-true (coll-all? test-heap (lambda (x) (> x 0))))
    (assert-false (coll-all? test-heap (lambda (x) (> x 5)))))

  (define-test "coll-filter-list filters correctly"
    (let ([big-nums (coll-filter-list test-heap (lambda (x) (> x 5)))])
      (assert-equal 2 (length big-nums))
      (assert-true (and (member 9 big-nums) #t))
      (assert-true (and (member 6 big-nums) #t))))

  (define-test "coll-map-list transforms elements"
    (let ([doubled (coll-map-list test-heap (lambda (x) (* 2 x)))])
      (assert-equal 8 (length doubled))
      (assert-equal 62 (apply + doubled))))  ; 31 * 2

  (define-test "coll-sum and coll-product"
    (assert-equal 31 (coll-sum test-heap))
    ;; Small heap for product test
    (let ([small-heap (list->heap '(2 3 4))])
      (assert-equal 24 (coll-product small-heap)))))

;;; ============================================================
;;; Keyed Protocol Tests (AVL)
;;; ============================================================

(test-group "Keyed Protocols"

  (define-test "keyed-lookup finds values"
    (assert-equal "five" (keyed-lookup test-avl 5))
    (assert-equal "uno" (keyed-lookup test-avl 1))  ; Updated value
    (assert-false (keyed-lookup test-avl 100)))

  (define-test "keyed-insert adds pairs"
    (let ([new-avl (keyed-insert test-avl 2 "two")])
      (assert-equal "two" (keyed-lookup new-avl 2))
      (assert-equal 6 (coll-size new-avl))))

  (define-test "keyed-delete removes pairs"
    (let ([smaller-avl (keyed-delete test-avl 5)])
      (assert-false (keyed-lookup smaller-avl 5))
      (assert-equal 4 (coll-size smaller-avl))))

  (define-test "keyed-contains? checks existence"
    (assert-true (keyed-contains? test-avl 5))
    (assert-false (keyed-contains? test-avl 100)))

  (define-test "keyed-keys returns all keys"
    (let ([keys (keyed-keys test-avl)])
      (assert-equal 5 (length keys))
      (assert-true (and (member 1 keys) #t))
      (assert-true (and (member 9 keys) #t))))

  (define-test "keyed-values returns all values"
    (let ([vals (keyed-values test-avl)])
      (assert-equal 5 (length vals))
      (assert-true (and (member "five" vals) #t))
      (assert-true (and (member "nine" vals) #t)))))

;;; ============================================================
;;; Spatial Protocol Tests (KDTree, Quadtree)
;;; ============================================================

(test-group "Spatial Protocols - KDTree"

  (define-test "spatial-nearest finds closest point"
    (let ([nearest (spatial-nearest test-kdtree '(5 5))])
      (assert-true (pair? nearest))
      ;; Should be (5 4) - closest to (5,5)
      (assert-equal '(5 4) nearest)))

  (define-test "spatial-knn finds k nearest"
    (let ([knn (spatial-knn test-kdtree '(5 5) 3)])
      (assert-equal 3 (length knn))
      ;; (5 4) should be first (closest)
      (assert-equal '(5 4) (car knn))))

  (define-test "spatial-range finds points in box"
    (let ([in-range (spatial-range test-kdtree '(3 2) '(6 5))])
      ;; Points with 3<=x<=6 and 2<=y<=5: (5 4), (4 ?)
      (assert-true (and (member '(5 4) in-range) #t))))

  (define-test "spatial-radius finds points in circle"
    (let ([nearby (spatial-radius test-kdtree '(5 4) 2)])
      ;; Points within distance 2 of (5,4)
      (assert-true (and (member '(5 4) nearby) #t))))

  (define-test "spatial-contains? checks membership"
    (assert-true (spatial-contains? test-kdtree '(5 4)))
    (assert-false (spatial-contains? test-kdtree '(100 100)))))

(test-group "Spatial Protocols - Quadtree"

  (define-test "spatial-nearest finds closest point"
    (let ([nearest (spatial-nearest test-quadtree '(5 5))])
      ;; Should return the point data (x y data)
      (assert-true (pair? nearest))))

  (define-test "spatial-knn finds k nearest"
    (let ([knn (spatial-knn test-quadtree '(5 5) 3)])
      (assert-equal 3 (length knn))
      ;; First result should be closest - point at or near (5,4)
      (let* ([first-pt (car knn)]
             [x (car first-pt)]
             [y (cadr first-pt)])
        ;; (5 4) is in our test data and closest to (5,5)
        (assert-true (and (= x 5) (= y 4))))))

  (define-test "spatial-range finds points in box"
    (let ([in-range (spatial-range test-quadtree '(3 2) '(6 5))])
      (assert-true (>= (length in-range) 1))))

  (define-test "spatial-radius finds points in circle"
    (let ([nearby (spatial-radius test-quadtree '(5 4) 3)])
      (assert-true (>= (length nearby) 1))))

  (define-test "spatial-contains? checks membership"
    (assert-true (spatial-contains? test-quadtree '(5 4)))
    (assert-false (spatial-contains? test-quadtree '(100 100)))))

;;; ============================================================
;;; Priority Protocol Tests (Heap)
;;; ============================================================

(test-group "Priority Protocols"

  (define-test "prio-peek returns minimum"
    (assert-equal 1 (prio-peek test-heap)))

  (define-test "prio-pop removes minimum"
    (let-values ([(new-heap elem) (prio-pop test-heap)])
      (assert-equal 1 elem)
      (assert-equal 7 (coll-size new-heap))
      (assert-equal 1 (prio-peek new-heap))))  ; Still 1 (there are two 1s)

  (define-test "prio-insert adds element"
    (let ([new-heap (prio-insert test-heap 0)])
      (assert-equal 9 (coll-size new-heap))
      (assert-equal 0 (prio-peek new-heap))))

  (define-test "prio-merge combines heaps"
    (let* ([h1 (list->heap '(1 3 5))]
           [h2 (list->heap '(2 4 6))]
           [merged (prio-merge h1 h2)])
      (assert-equal 6 (coll-size merged))
      (assert-equal 1 (prio-peek merged)))))

;;; ============================================================
;;; Cross-Collection Generic Tests
;;; ============================================================

(test-group "Cross-Collection Generics"

  (define-test "same generic works on all collections"
    ;; coll-size works uniformly
    (let ([collections (list test-avl test-heap test-kdtree test-quadtree)]
          [expected-sizes (list 5 8 6 6)])
      (for-each
       (lambda (coll expected)
         (assert-equal expected (coll-size coll)))
       collections expected-sizes)))

  (define-test "coll-fold signature is uniform"
    ;; All collections support (coll-fold coll fn init)
    ;; Count elements using same pattern
    (let ([count-fn (lambda (acc elem) (+ acc 1))])
      (assert-equal 5 (coll-fold test-avl count-fn 0))
      (assert-equal 8 (coll-fold test-heap count-fn 0))
      (assert-equal 6 (coll-fold test-kdtree count-fn 0))
      (assert-equal 6 (coll-fold test-quadtree count-fn 0)))))

;;; ============================================================
;;; Introspection Tests
;;; ============================================================

(test-group "Introspection"

  (define-test "coll-protocols returns implemented protocols"
    (let ([avl-protos (coll-protocols 'avl-node)])
      (assert-true (and (member 'coll-size avl-protos) #t))
      (assert-true (and (member 'keyed-lookup avl-protos) #t))
      (assert-false (member 'spatial-nearest avl-protos)))

    (let ([heap-protos (coll-protocols 'heap-node)])
      (assert-true (and (member 'coll-size heap-protos) #t))
      (assert-true (and (member 'prio-peek heap-protos) #t))
      (assert-false (member 'keyed-lookup heap-protos)))

    (let ([kdtree-protos (coll-protocols 'kdtree-node)])
      (assert-true (and (member 'coll-size kdtree-protos) #t))
      (assert-true (and (member 'spatial-nearest kdtree-protos) #t))
      (assert-false (member 'prio-peek kdtree-protos)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests)
