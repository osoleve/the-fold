;;; fabric/stitches/fp/test-heap.ss — Tests for Heap Library
;;;
;;; NOTE: Run from fabric/stitches directory

(load "core/test-framework.ss")
(load "core/fp/data/heap.ss")

(display "
")
(display "==============================================================
")
(display "         HEAP (PRIORITY QUEUE) TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Basic Heap Tests
;;; ============================================================

(test-group heap-basics
            (define-test empty-heap-test
              (assert-true (heap-empty? heap-empty))
              (assert-true (heap-empty? (empty-min-heap)))
              (assert-equal 0 (heap-size heap-empty)))
            
            (define-test singleton-heap-test
              (let ([h (heap-singleton 42 min-cmp)])
                   (assert-true (heap? h))
                   (assert-false (heap-empty? h))
                   (assert-equal 1 (heap-size h))
                   (assert-equal 42 (from-just (heap-find-min h)))))
            
            (define-test heap-insert-test
              (let* ([h (heap-singleton 5 min-cmp)]
                     [h (heap-insert h 3)]
                     [h (heap-insert h 7)])
                    (assert-equal 3 (heap-size h))
                    (assert-equal 3 (from-just (heap-find-min h)))))
            
            (define-test heap-insert-order-test
              (let* ([h heap-empty]
                     [h (heap-insert* h 10 min-cmp)]
                     [h (heap-insert h 5)]
                     [h (heap-insert h 15)]
                     [h (heap-insert h 1)])
                    (assert-equal 1 (from-just (heap-find-min h))))))

;;; ============================================================
;;; Heap Operations Tests
;;; ============================================================

(test-group heap-operations
            (define-test delete-min-test
              (let* ([h (list->min-heap '(3 1 4 1 5 9 2 6))]
                     [h2 (heap-delete-min h)])
                    (assert-equal 1 (from-just (heap-find-min h)))
                    (assert-equal 1 (from-just (heap-find-min h2)))))
            
            (define-test extract-min-test
              (let* ([h (list->min-heap '(5 3 7 1))]
                     [result (heap-extract-min h)])
                    (assert-true (not (not result)))
                    (assert-equal 1 (car result))
                    (assert-equal 3 (from-just (heap-find-min (cdr result))))))
            
            (define-test extract-all-test
              (let* ([h (list->min-heap '(3 1 4))]
                     [r1 (heap-extract-min h)]
                     [r2 (heap-extract-min (cdr r1))]
                     [r3 (heap-extract-min (cdr r2))])
                    (assert-equal 1 (car r1))
                    (assert-equal 3 (car r2))
                    (assert-equal 4 (car r3))
                    (assert-true (heap-empty? (cdr r3)))))
            
            (define-test heap-merge-test
              (let* ([h1 (list->min-heap '(1 3 5))]
                     [h2 (list->min-heap '(2 4 6))]
                     [merged (heap-merge h1 h2)])
                    (assert-equal 6 (heap-size merged))
                    (assert-equal 1 (from-just (heap-find-min merged)))))
            
            (define-test heap-contains-test
              (let ([h (list->min-heap '(1 2 3 4 5))])
                   (assert-true (heap-contains? h 3))
                   (assert-true (heap-contains? h 1))
                   (assert-true (heap-contains? h 5))
                   (assert-false (heap-contains? h 6))
                   (assert-false (heap-contains? h 0)))))

;;; ============================================================
;;; Max Heap Tests
;;; ============================================================

(test-group max-heap
            (define-test max-heap-basic-test
              (let ([h (list->max-heap '(1 5 3 9 2))])
                   (assert-equal 9 (from-just (heap-find-min h)))))
            
            (define-test max-heap-extract-test
              (let* ([h (list->max-heap '(1 5 3 9 2))]
                     [r1 (heap-extract-min h)]
                     [r2 (heap-extract-min (cdr r1))])
                    (assert-equal 9 (car r1))
                    (assert-equal 5 (car r2)))))

;;; ============================================================
;;; Heap Builders Tests
;;; ============================================================

(test-group heap-builders
            (define-test list-to-min-heap-test
              (let ([h (list->min-heap '(5 2 8 1 9))])
                   (assert-equal 5 (heap-size h))
                   (assert-equal 1 (from-just (heap-find-min h)))))
            
            (define-test list-to-max-heap-test
              (let ([h (list->max-heap '(5 2 8 1 9))])
                   (assert-equal 5 (heap-size h))
                   (assert-equal 9 (from-just (heap-find-min h)))))
            
            (define-test heap-to-list-test
              (let* ([h (list->min-heap '(3 1 4 1 5))]
                     [lst (heap->list h)])
                    (assert-equal 5 (length lst))
                    (assert-equal '(1 1 3 4 5) lst)))
            
            (define-test empty-list-to-heap-test
              (let ([h (list->min-heap '())])
                   (assert-true (heap-empty? h)))))

;;; ============================================================
;;; Heap Sort Tests
;;; ============================================================

(test-group heap-sort
            (define-test heap-sort-test
              (assert-equal '(1 2 3 4 5) (heap-sort '(3 1 4 5 2))))
            
            (define-test heap-sort-desc-test
              (assert-equal '(5 4 3 2 1) (heap-sort-desc '(3 1 4 5 2))))
            
            (define-test heap-sort-empty-test
              (assert-equal '() (heap-sort '())))
            
            (define-test heap-sort-single-test
              (assert-equal '(42) (heap-sort '(42))))
            
            (define-test heap-sort-duplicates-test
              (assert-equal '(1 1 2 2 3) (heap-sort '(2 1 2 3 1))))
            
            (define-test heap-sort-by-test
              (let ([result (heap-sort-by (lambda (a b) (> a b)) '(3 1 4 5 2))])
                   (assert-equal '(5 4 3 2 1) result))))

;;; ============================================================
;;; Top-K Tests
;;; ============================================================

(test-group top-k
            (define-test top-k-smallest-test
              (let ([result (top-k-smallest 3 '(5 2 8 1 9 3 7))])
                   (assert-equal 3 (length result))
                   (assert-equal '(1 2 3) result)))
            
            (define-test top-k-largest-test
              (let ([result (top-k-largest 3 '(5 2 8 1 9 3 7))])
                   (assert-equal 3 (length result))
                   (assert-equal '(9 8 7) result)))
            
            (define-test top-k-all-test
              (let ([result (top-k-smallest 10 '(3 1 2))])
                   (assert-equal 3 (length result))))
            
            (define-test heap-top-k-test
              (let* ([h (list->min-heap '(5 2 8 1 9))]
                     [result (heap-top-k h 2)])
                    (assert-equal '(1 2) result))))

;;; ============================================================
;;; Priority Queue Tests
;;; ============================================================

(test-group priority-queue
            (define-test pq-empty-test
              (let ([pq (pq-empty)])
                   (assert-true (pq-empty? pq))
                   (assert-equal 0 (pq-size pq))))
            
            (define-test pq-insert-peek-test
              (let* ([pq (pq-empty)]
                     [pq (pq-insert pq 1 "high")]
                     [pq (pq-insert pq 3 "low")]
                     [pq (pq-insert pq 2 "medium")])
                    (assert-equal 3 (pq-size pq))
                    (assert-equal "high" (from-just (pq-peek pq)))
                    (assert-equal 1 (from-just (pq-peek-priority pq)))))
            
            (define-test pq-pop-test
              (let* ([pq (pq-empty)]
                     [pq (pq-insert pq 2 "second")]
                     [pq (pq-insert pq 1 "first")]
                     [pq (pq-insert pq 3 "third")]
                     [result (pq-pop pq)])
                    (assert-true (not (not result)))
                    (assert-equal "first" (car result))
                    (assert-equal "second" (from-just (pq-peek (cdr result))))))
            
            (define-test list-to-pq-test
              (let ([pq (list->pq '((3 . "c") (1 . "a") (2 . "b")))])
                   (assert-equal 3 (pq-size pq))
                   (assert-equal "a" (from-just (pq-peek pq))))))

;;; ============================================================
;;; Merge Operations Tests
;;; ============================================================

(test-group merge-operations
            (define-test heap-merge-all-test
              (let* ([h1 (list->min-heap '(1 4))]
                     [h2 (list->min-heap '(2 5))]
                     [h3 (list->min-heap '(3 6))]
                     [merged (heap-merge-all (list h1 h2 h3))])
                    (assert-equal 6 (heap-size merged))
                    (assert-equal '(1 2 3 4 5 6) (heap->list merged))))
            
            (define-test merge-sorted-test
              (let ([result (merge-sorted '(1 3 5) '(2 4 6) <)])
                   (assert-equal '(1 2 3 4 5 6) result)))
            
            (define-test merge-sorted-empty-test
              (assert-equal '(1 2 3) (merge-sorted '(1 2 3) '() <))
              (assert-equal '(1 2 3) (merge-sorted '() '(1 2 3) <)))
            
            (define-test k-way-merge-test
              (let ([result (k-way-merge '((1 4 7) (2 5 8) (3 6 9)))])
                   (assert-equal '(1 2 3 4 5 6 7 8 9) result)))
            
            (define-test k-way-merge-uneven-test
              (let ([result (k-way-merge '((1 2) (3) (4 5 6)))])
                   (assert-equal '(1 2 3 4 5 6) result))))

;;; ============================================================
;;; Transformation Tests
;;; ============================================================

(test-group transformations
            (define-test heap-map-test
              (let* ([h (list->min-heap '(1 2 3))]
                     [mapped (heap-map (lambda (x) (* x 10)) h)])
                    (assert-equal '(10 20 30) (heap->list mapped))))
            
            (define-test heap-filter-test
              (let* ([h (list->min-heap '(1 2 3 4 5 6))]
                     [filtered (heap-filter even? h)])
                    (assert-equal '(2 4 6) (heap->list filtered))))
            
            (define-test heap-fold-test
              (let* ([h (list->min-heap '(1 2 3 4 5))]
                     [sum (heap-fold + 0 h)])
                    (assert-equal 15 sum))))

;;; ============================================================
;;; Median Tracker Tests
;;; ============================================================

(test-group median-tracker
            (define-test median-empty-test
              (let ([tracker (make-median-tracker)])
                   (assert-true (nothing? (median-get tracker)))))
            
            (define-test median-single-test
              (let* ([tracker (make-median-tracker)]
                     [tracker (median-insert tracker 5)])
                    (assert-equal 5 (from-just (median-get tracker)))))
            
            (define-test median-two-test
              (let* ([tracker (make-median-tracker)]
                     [tracker (median-insert tracker 5)]
                     [tracker (median-insert tracker 10)])
                    ;; With two elements, returns lower (or average depending on impl)
                    (let ([med (median-get tracker)])
                         (assert-true (just? med)))))
            
            (define-test median-sequence-test
              (let* ([tracker (make-median-tracker)]
                     [tracker (median-insert tracker 3)]
                     [tracker (median-insert tracker 1)]
                     [tracker (median-insert tracker 5)]
                     [tracker (median-insert tracker 2)]
                     [tracker (median-insert tracker 4)])
                    ;; Median of 1,2,3,4,5 is 3
                    (assert-equal 3 (from-just (median-get tracker))))))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(test-group edge-cases
            (define-test extract-from-empty-test
              (assert-false (heap-extract-min heap-empty)))
            
            (define-test find-min-empty-test
              (assert-true (nothing? (heap-find-min heap-empty))))
            
            (define-test delete-min-empty-test
              (assert-true (heap-empty? (heap-delete-min heap-empty))))
            
            (define-test single-element-operations-test
              (let* ([h (heap-singleton 42 min-cmp)]
                     [h2 (heap-delete-min h)])
                    (assert-true (heap-empty? h2))))
            
            (define-test large-heap-test
              (let* ([nums (map (lambda (x) (modulo (* x 7) 100)) (iota 100))]
                     [h (list->min-heap nums)]
                     [sorted (heap->list h)])
                    (assert-equal 100 (length sorted))
                    ;; First should be minimum
                    (assert-equal (apply min nums) (car sorted)))))

;;; ============================================================
;;; Visualization Tests
;;; ============================================================

(test-group visualization
            (define-test heap-to-string-test
              (let ([h (list->min-heap '(3 1 2))])
                   (assert-true (string? (heap->string h)))
                   (assert-true (> (string-length (heap->string h)) 0))))
            
            (define-test empty-heap-to-string-test
              (assert-equal "[]" (heap->string heap-empty))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "
")
(display "==============================================================
")
(printf "Tests passed: ~a
" *tests-passed*)
(printf "Tests failed: ~a
" *tests-failed*)
(printf "Total tests:  ~a
" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All heap tests passed.
")
    (display "
[FAILURE] Some heap tests failed.
"))
