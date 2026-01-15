;;; lattice/data/test-sort.ss — Tests for Sorting Algorithms
;;;
;;; Comprehensive tests for merge sort, quicksort, insertion sort,
;;; and utility functions.
;;;
;;; Dependencies:
;;;   - sort.ss

(load "lattice/data/sort.ss")

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

(display "Testing Sorting Algorithms\n")
(display "====\n\n")

;;; ====
;;; Merge Sort Tests
;;; ====

(display "Merge Sort:\n")
(display "----\n")

(test "merge-sort empty" '() (merge-sort '()))
(test "merge-sort single" '(42) (merge-sort '(42)))
(test "merge-sort two elements" '(1 2) (merge-sort '(2 1)))
(test "merge-sort already sorted" '(1 2 3 4 5) (merge-sort '(1 2 3 4 5)))
(test "merge-sort reverse" '(1 2 3 4 5) (merge-sort '(5 4 3 2 1)))
(test "merge-sort random" '(1 2 3 4 5 6 7 8 9) (merge-sort '(5 2 8 1 9 3 7 4 6)))
(test "merge-sort duplicates" '(1 2 2 3 3 3 4) (merge-sort '(3 2 1 3 2 4 3)))

;;; Test stability
(define items '((a . 2) (b . 1) (c . 2) (d . 1)))
(define sorted-items (merge-sort-by (lambda (x y) (< (cdr x) (cdr y))) items))
(test "merge-sort stability: b before d" 'b (caar sorted-items))
(test "merge-sort stability: d second" 'd (car (cadr sorted-items)))
(test "merge-sort stability: a before c" 'a (car (caddr sorted-items)))
(test "merge-sort stability: c fourth" 'c (car (cadddr sorted-items)))

;;; ====
;;; Quicksort Tests
;;; ====

(display "\nQuicksort:\n")
(display "----\n")

(test "quicksort empty" '() (quicksort '()))
(test "quicksort single" '(42) (quicksort '(42)))
(test "quicksort two elements" '(1 2) (quicksort '(2 1)))
(test "quicksort already sorted" '(1 2 3 4 5) (quicksort '(1 2 3 4 5)))
(test "quicksort reverse" '(1 2 3 4 5) (quicksort '(5 4 3 2 1)))
(test "quicksort random" '(1 2 3 4 5 6 7 8 9) (quicksort '(5 2 8 1 9 3 7 4 6)))
(test "quicksort all same" '(5 5 5 5 5) (quicksort '(5 5 5 5 5)))
(test "quicksort duplicates" '(1 2 2 3 3 3 4) (quicksort '(3 2 1 3 2 4 3)))

;;; ====
;;; Insertion Sort Tests
;;; ====

(display "\nInsertion Sort:\n")
(display "----\n")

(test "insertion-sort empty" '() (insertion-sort '()))
(test "insertion-sort single" '(7) (insertion-sort '(7)))
(test "insertion-sort two" '(1 2) (insertion-sort '(2 1)))
(test "insertion-sort random" '(1 1 3 4 5) (insertion-sort '(3 1 4 1 5)))

;;; Insertion sort is stable
(define is-items '((a . 2) (b . 1) (c . 2) (d . 1)))
(define is-sorted (insertion-sort-by (lambda (x y) (< (cdr x) (cdr y))) is-items))
(test "insertion-sort stability" '(b d a c) (map car is-sorted))

;;; ====
;;; Sort by Key Tests
;;; ====

(display "\nSort by Key:\n")
(display "----\n")

(test "sort-by-key string-length" '("a" "bb" "ccc" "dddd")
      (sort-by-key string-length '("ccc" "a" "dddd" "bb")))
(test "sort-by-key abs" '(1 -2 3 -4 5)
      (sort-by-key (lambda (x) (abs x)) '(5 -2 3 -4 1)))
(test "sort-by-key-desc" '("dddd" "ccc" "bb" "a")
      (sort-by-key-desc string-length '("ccc" "a" "dddd" "bb")))

;;; ====
;;; Descending Order Tests
;;; ====

(display "\nDescending Order:\n")
(display "----\n")

(test "sort-desc" '(9 8 7 6 5 4 3 2 1) (sort-desc '(5 2 8 1 9 3 7 4 6)))
(test "quicksort-desc" '(5 4 3 1 1) (quicksort-desc '(3 1 4 1 5)))
(test "sort-desc empty" '() (sort-desc '()))

;;; ====
;;; Custom Comparator Tests
;;; ====

(display "\nCustom Comparators:\n")
(display "----\n")

(test "sort-by descending" '(5 4 3 1 1) (sort-by > '(3 1 4 1 5)))
(test "quicksort-by strings" '("apple" "banana" "cherry")
      (quicksort-by string<? '("cherry" "apple" "banana")))

;;; ====
;;; Sorted Predicate Tests
;;; ====

(display "\nSorted Predicates:\n")
(display "----\n")

(test "sorted? empty" #t (sorted? '()))
(test "sorted? single" #t (sorted? '(5)))
(test "sorted? ascending" #t (sorted? '(1 2 3 4 5)))
(test "sorted? with equals" #t (sorted? '(1 2 2 3 3)))
(test "sorted? unsorted" #f (sorted? '(1 3 2 4 5)))
(test "sorted-desc? descending" #t (sorted-desc? '(5 4 3 2 1)))
(test "sorted-desc? ascending" #f (sorted-desc? '(1 2 3 4 5)))

;;; ====
;;; Unique and Dedup Tests
;;; ====

(display "\nUnique/Dedup:\n")
(display "----\n")

(test "unique-sorted" '(1 2 3 4 5) (unique-sorted '(1 1 2 2 2 3 4 4 5)))
(test "unique-sorted no dups" '(1 2 3) (unique-sorted '(1 2 3)))
(test "unique-sorted empty" '() (unique-sorted '()))
(test "sort-unique" '(1 2 3 4 5) (sort-unique '(3 1 4 1 5 2 3)))

;;; ====
;;; Min/Max Tests
;;; ====

(display "\nMin/Max:\n")
(display "----\n")

(test "min-element" 1 (min-element '(5 2 8 1 9 3 7)))
(test "min-element single" 42 (min-element '(42)))
(test "max-element" 9 (max-element '(5 2 8 1 9 3 7)))
(test "max-element single" 42 (max-element '(42)))
(test "min-element with negatives" -5 (min-element '(3 -2 -5 1 4)))
(test "max-element with negatives" 4 (max-element '(3 -2 -5 1 4)))

(test-error "min-element empty" (lambda () (min-element '())))
(test-error "max-element empty" (lambda () (max-element '())))

;;; ====
;;; Min-by/Max-by Tests
;;; ====

(display "\nMin-by/Max-by:\n")
(display "----\n")

(test "min-by string-length" "a" (min-by string-length '("ccc" "a" "bb")))
(test "max-by string-length" "ccc" (max-by string-length '("ccc" "a" "bb")))
(test "min-by abs" 1 (min-by abs '(-5 3 1 -4)))
(test "max-by abs" -5 (max-by abs '(-5 3 1 -4)))

(test-error "min-by empty" (lambda () (min-by abs '())))
(test-error "max-by empty" (lambda () (max-by abs '())))

;;; ====
;;; Nth-Smallest and Median Tests
;;; ====

(display "\nNth-Smallest/Median:\n")
(display "----\n")

(test "nth-smallest 0" 1 (nth-smallest 0 '(5 2 8 1 9 3 7)))
(test "nth-smallest 1" 2 (nth-smallest 1 '(5 2 8 1 9 3 7)))
(test "nth-smallest 2" 3 (nth-smallest 2 '(5 2 8 1 9 3 7)))
(test "nth-smallest last" 9 (nth-smallest 6 '(5 2 8 1 9 3 7)))
(test "nth-smallest with dups" 2 (nth-smallest 2 '(3 1 2 1 3)))

(test "median odd length" 5 (median '(5 2 8 1 9 3 7)))  ; sorted: 1,2,3,5,7,8,9 -> 5
(test "median even length" 4 (median '(3 1 4 2 5 6)))   ; sorted: 1,2,3,4,5,6 -> 4 (index 3)

(test-error "nth-smallest empty" (lambda () (nth-smallest 0 '())))
(test-error "median empty" (lambda () (median '())))

;;; ====
;;; Top-K and Bottom-K Tests
;;; ====

(display "\nTop-K/Bottom-K:\n")
(display "----\n")

(test "top-k 3" '(9 8 7) (top-k 3 '(5 2 8 1 9 3 7)))
(test "top-k 0" '() (top-k 0 '(5 2 8 1 9)))
(test "top-k more than length" '(5 4 3 1 1) (top-k 10 '(3 1 4 1 5)))

(test "bottom-k 3" '(1 2 3) (bottom-k 3 '(5 2 8 1 9 3 7)))
(test "bottom-k 0" '() (bottom-k 0 '(5 2 8 1 9)))
(test "bottom-k more than length" '(1 1 3 4 5) (bottom-k 10 '(3 1 4 1 5)))

;;; ====
;;; Large List Tests
;;; ====

(display "\nLarge Lists:\n")
(display "----\n")

(define large-list (reverse (iota 100)))
(test "merge-sort 100 elements" (iota 100) (merge-sort large-list))
(test "quicksort 100 elements" (iota 100) (quicksort large-list))
(test "sorted? after sort" #t (sorted? (merge-sort large-list)))

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
