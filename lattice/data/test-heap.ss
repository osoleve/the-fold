;;; lattice/data/test-heap.ss — Tests for Leftist Heap / Priority Queue
;;;
;;; Comprehensive tests for heap operations, priority queue interface,
;;; and heapsort.
;;;
;;; Dependencies:
;;;   - heap.ss

(load "lattice/data/heap.ss")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (display "  ✓ ") (display name) (newline))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (display "  ✗ ") (display name)
       (display " — expected ") (write expected)
       (display ", got ") (write actual)
       (newline))))

(define (test-error name thunk)
  (guard (ex
          [(serious-condition? ex)
           (set! tests-passed (+ tests-passed 1))
           (display "  ✓ ") (display name) (display " (error raised)") (newline)]
          [else
           (set! tests-failed (+ tests-failed 1))
           (display "  ✗ ") (display name) (display " (wrong exception type)") (newline)])
         (thunk)
         (set! tests-failed (+ tests-failed 1))
         (display "  ✗ ") (display name) (display " (no error raised)") (newline)))

(display "Testing Heap / Priority Queue\n")
(display "====\n\n")

;;; ====
;;; Basic Min-Heap Tests
;;; ====

(display "Min-Heap Basics:\n")
(display "----\n")

(test "heap-empty? on empty heap" #t (heap-empty? heap-empty))

(define h1 (heap-insert 5 heap-empty))
(test "heap-empty? after insert" #f (heap-empty? h1))
(test "heap-min after one insert" 5 (heap-min h1))
(test "heap-size after one insert" 1 (heap-size h1))

(define h2 (heap-insert 3 h1))
(test "heap-min finds smaller" 3 (heap-min h2))
(test "heap-size after two inserts" 2 (heap-size h2))

(define h3 (heap-insert 7 h2))
(test "heap-min unchanged after larger insert" 3 (heap-min h3))
(test "heap-size after three inserts" 3 (heap-size h3))

(define h4 (heap-insert 1 h3))
(test "heap-min updated after smaller insert" 1 (heap-min h4))

;;; ====
;;; Delete-Min Tests
;;; ====

(display "\nDelete-Min:\n")
(display "----\n")

(define hd1 (list->heap '(5 3 8 1 4)))
(test "initial min is 1" 1 (heap-min hd1))

(define hd2 (heap-delete-min hd1))
(test "after delete-min, min is 3" 3 (heap-min hd2))
(test "size after delete-min" 4 (heap-size hd2))

(define hd3 (heap-delete-min hd2))
(test "after second delete-min, min is 4" 4 (heap-min hd3))

;;; ====
;;; Heap Pop Tests
;;; ====

(display "\nHeap Pop:\n")
(display "----\n")

(define hp (list->heap '(10 2 7 5)))
(define-values (hp2 val1) (heap-pop hp))
(test "first pop returns min" 2 val1)
(test "heap after pop has 3 elements" 3 (heap-size hp2))

(define-values (hp3 val2) (heap-pop hp2))
(test "second pop returns next min" 5 val2)

(define-values (hp4 val3) (heap-pop hp3))
(test "third pop returns 7" 7 val3)

(define-values (hp5 val4) (heap-pop hp4))
(test "fourth pop returns 10" 10 val4)
(test "heap now empty" #t (heap-empty? hp5))

;;; ====
;;; Max-Heap Tests
;;; ====

(display "\nMax-Heap:\n")
(display "----\n")

(define mx1 (heap-insert-max 5 heap-empty))
(test "max-heap: first insert" 5 (heap-max mx1))

(define mx2 (heap-insert-max 10 mx1))
(test "max-heap: finds larger" 10 (heap-max mx2))

(define mx3 (heap-insert-max 3 mx2))
(test "max-heap: ignores smaller" 10 (heap-max mx3))

(define mx4 (heap-delete-max mx3))
(test "max-heap: after delete-max" 5 (heap-max mx4))

(define-values (mx5 maxval) (heap-pop-max mx4))
(test "max-heap: pop returns max" 5 maxval)

;;; ====
;;; List Conversions
;;; ====

(display "\nList Conversions:\n")
(display "----\n")

(define conv-heap (list->heap '(9 1 5 3 7)))
(test "list->heap preserves elements" 5 (heap-size conv-heap))
(test "list->heap: min is correct" 1 (heap-min conv-heap))

(define sorted (heap->list conv-heap))
(test "heap->list gives sorted list" '(1 3 5 7 9) sorted)

(define max-heap (list->heap-max '(9 1 5 3 7)))
(define max-sorted (heap->list-max max-heap))
(test "heap->list-max gives reverse sorted" '(9 7 5 3 1) max-sorted)

;;; ====
;;; Heapsort Tests
;;; ====

(display "\nHeapsort:\n")
(display "----\n")

(test "heapsort empty list" '() (heapsort '()))
(test "heapsort single element" '(42) (heapsort '(42)))
(test "heapsort already sorted" '(1 2 3 4 5) (heapsort '(1 2 3 4 5)))
(test "heapsort reverse sorted" '(1 2 3 4 5) (heapsort '(5 4 3 2 1)))
(test "heapsort random order" '(1 2 3 4 5 6 7 8 9) (heapsort '(5 2 8 1 9 3 7 4 6)))
(test "heapsort with duplicates" '(1 2 2 3 3 3 4) (heapsort '(3 2 1 3 2 4 3)))

(test "heapsort-desc empty" '() (heapsort-desc '()))
(test "heapsort-desc" '(9 8 7 6 5 4 3 2 1) (heapsort-desc '(5 2 8 1 9 3 7 4 6)))

;;; Custom comparator sort
(test "heapsort-by ascending" '(1 1 3 4 5) (heapsort-by <= '(3 1 4 1 5)))
(test "heapsort-by descending" '(5 4 3 1 1) (heapsort-by >= '(3 1 4 1 5)))

;;; ====
;;; Priority Queue Interface Tests
;;; ====

(display "\nPriority Queue:\n")
(display "----\n")

(test "pq-empty? on empty" #t (pq-empty? pq-empty))

(define pq1 (pq-insert 50 pq-empty))
(test "pq-empty? after insert" #f (pq-empty? pq1))
(test "pq-peek after insert" 50 (pq-peek pq1))
(test "pq-size after insert" 1 (pq-size pq1))

(define pq2 (pq-insert 20 pq1))
(define pq3 (pq-insert 80 pq2))
(define pq4 (pq-insert 10 pq3))
(test "pq-peek finds highest priority" 10 (pq-peek pq4))
(test "pq-size with multiple" 4 (pq-size pq4))

(define-values (pq5 top) (pq-pop pq4))
(test "pq-pop returns highest priority" 10 top)
(test "pq-peek after pop" 20 (pq-peek pq5))

;;; ====
;;; Generic Comparator Tests
;;; ====

(display "\nCustom Comparator:\n")
(display "----\n")

;;; Max-heap using generic interface
(define gh1 (heap-insert-by >= 5 heap-empty))
(define gh2 (heap-insert-by >= 10 gh1))
(define gh3 (heap-insert-by >= 3 gh2))
(test "custom >= comparator: max at top" 10 (heap-value gh3))

;;; String length comparator (shorter strings have priority)
(define (shorter? a b) (<= (string-length a) (string-length b)))
(define sh (list->heap-by shorter? '("hello" "hi" "hey" "a")))
(test "custom string-length comparator" "a" (heap-value sh))

;;; ====
;;; Edge Cases
;;; ====

(display "\nEdge Cases:\n")
(display "----\n")

(test "heapsort preserves equal elements" '(5 5 5) (heapsort '(5 5 5)))
(test "heapsort large list" 100 (length (heapsort (iota 100))))
(test "heap merge with empty" 5 (heap-min (heap-merge (list->heap '(5)) heap-empty)))
(test "heap merge two empty" #t (heap-empty? (heap-merge heap-empty heap-empty)))

;;; Merge two non-empty heaps
(define mh1 (list->heap '(1 5 9)))
(define mh2 (list->heap '(2 6 10)))
(define merged (heap-merge mh1 mh2))
(test "merge preserves min" 1 (heap-min merged))
(test "merge size" 6 (heap-size merged))
(test "merged heap->list" '(1 2 5 6 9 10) (heap->list merged))

;;; ====
;;; Error Cases
;;; ====

(display "\nError Cases:\n")
(display "----\n")

(test-error "heap-min on empty" (lambda () (heap-min heap-empty)))
(test-error "heap-delete-min on empty" (lambda () (heap-delete-min heap-empty)))
(test-error "heap-pop on empty" (lambda () (heap-pop heap-empty)))
(test-error "heap-delete-max on empty" (lambda () (heap-delete-max heap-empty)))
(test-error "heap-pop-max on empty" (lambda () (heap-pop-max heap-empty)))

;;; ====
;;; Leftist Property Verification
;;; ====

(display "\nLeftist Property:\n")
(display "----\n")

;;; Verify that rightmost path is always short
(define (verify-leftist heap)
  (if (heap-empty? heap)
      #t
      (and (>= (heap-rank (heap-left heap))
               (heap-rank (heap-right heap)))
           (verify-leftist (heap-left heap))
           (verify-leftist (heap-right heap)))))

(test "leftist property: simple" #t (verify-leftist (list->heap '(5 3 8 1 4))))
(test "leftist property: sequential" #t (verify-leftist (list->heap (iota 20))))
(test "leftist property: reverse" #t (verify-leftist (list->heap (reverse (iota 20)))))

;;; ====
;;; Summary
;;; ====

(display "\n")
(display "====\n")
(display "Tests passed: ") (display tests-passed) (newline)
(display "Tests failed: ") (display tests-failed) (newline)

(if (= tests-failed 0)
    (begin
     (display "\n✓ All tests passed!\n")
     (exit 0))
    (begin
     (display "\n✗ Some tests failed.\n")
     (exit 1)))
