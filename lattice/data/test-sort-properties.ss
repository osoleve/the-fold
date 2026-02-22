;;; lattice/data/test-sort-properties.ss — QuickCheck property tests for sorting

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'sort)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define (same-elements? xs ys)
  ;; Check that xs and ys are permutations of each other
  ;; by sorting both and comparing
  (equal? (merge-sort xs) (merge-sort ys)))

(define (all-satisfy? pred lst)
  (or (null? lst)
      (and (pred (car lst))
           (all-satisfy? pred (cdr lst)))))

;;; ============================================================================
;;; Sort output properties (merge-sort)
;;; ============================================================================

(test-group sort-properties

  (define-property "merge-sort output is sorted"
    (gen-list gen-int)
    (lambda (lst) (sorted? (merge-sort lst)))
    'tests 300)

  (define-property "merge-sort preserves length"
    (gen-list gen-int)
    (lambda (lst) (= (length lst) (length (merge-sort lst))))
    'tests 300)

  (define-property "merge-sort output is a permutation of input"
    (gen-list gen-int)
    (lambda (lst) (same-elements? lst (merge-sort lst)))
    'tests 300)

  (define-property "merge-sort is idempotent"
    (gen-list gen-int)
    (lambda (lst)
      (let ([sorted (merge-sort lst)])
        (equal? sorted (merge-sort sorted))))
    'tests 200)

  (define-property "merge-sort min is first element"
    (gen-list gen-nat)
    (lambda (lst)
      (or (null? lst)
          (let ([sorted (merge-sort lst)])
            (all-satisfy? (lambda (x) (>= x (car sorted)))
                          (cdr sorted)))))
    'tests 200)

  (define-property "merge-sort max is last element"
    (gen-list gen-nat)
    (lambda (lst)
      (or (null? lst)
          (let* ([sorted (merge-sort lst)]
                 [last-elem (list-ref sorted (- (length sorted) 1))])
            (all-satisfy? (lambda (x) (<= x last-elem)) sorted))))
    'tests 200)
)

;;; ============================================================================
;;; Quicksort agrees with merge-sort
;;; ============================================================================

(test-group sort-agreement

  (define-property "quicksort agrees with merge-sort"
    (gen-list gen-int)
    (lambda (lst) (equal? (merge-sort lst) (quicksort lst)))
    'tests 300)

  (define-property "insertion-sort agrees with merge-sort"
    (gen-list gen-int)
    (lambda (lst) (equal? (merge-sort lst) (insertion-sort lst)))
    'tests 200
    'max-size 30)  ; insertion-sort is O(n^2)

  ;; heapsort tested in test-heap-properties.ss to avoid load-order issues
)

;;; ============================================================================
;;; Descending sort
;;; ============================================================================

(test-group sort-descending

  (define-property "sort-desc output is reverse-sorted"
    (gen-list gen-int)
    (lambda (lst) (sorted-desc? (sort-desc lst)))
    'tests 200)

  (define-property "sort-desc is reverse of sort"
    (gen-list gen-int)
    (lambda (lst) (equal? (sort-desc lst) (reverse (merge-sort lst))))
    'tests 200)
)

;;; ============================================================================
;;; Custom comparator sort
;;; ============================================================================

(test-group sort-by-properties

  (define-property "sort-by with > gives descending"
    (gen-list gen-int)
    (lambda (lst) (sorted-desc? (merge-sort-by > lst)))
    'tests 200)

  (define-property "sort-by with < gives ascending"
    (gen-list gen-int)
    (lambda (lst) (sorted? (merge-sort-by < lst)))
    'tests 200)
)

;;; ============================================================================
;;; sort-unique properties
;;; ============================================================================

(test-group sort-unique-properties

  (define-property "sort-unique output is sorted"
    (gen-list gen-int)
    (lambda (lst) (sorted? (sort-unique lst)))
    'tests 200)

  (define-property "sort-unique has no duplicates"
    (gen-list gen-int)
    (lambda (lst)
      (let ([su (sort-unique lst)])
        (or (null? su)
            (null? (cdr su))
            (let loop ([prev (car su)] [rest (cdr su)])
              (or (null? rest)
                  (and (not (= prev (car rest)))
                       (loop (car rest) (cdr rest))))))))
    'tests 200)

  (define-property "sort-unique subset of original"
    (gen-list gen-int)
    (lambda (lst)
      (let ([su (sort-unique lst)]
            [sorted-orig (merge-sort lst)])
        (all-satisfy? (lambda (x) (pair? (memv x sorted-orig))) su)))
    'tests 200)
)

;;; ============================================================================
;;; nth-smallest / median properties
;;; ============================================================================

(test-group selection-properties

  (define-property "nth-smallest 0 equals min of sorted"
    (gen-list gen-int)
    (lambda (lst)
      (or (null? lst)
          (= (nth-smallest 0 lst) (car (merge-sort lst)))))
    'tests 200)

  (define-property "min-element equals first of sorted"
    (gen-list gen-int)
    (lambda (lst)
      (or (null? lst)
          (= (min-element lst) (car (merge-sort lst)))))
    'tests 200)

  (define-property "max-element equals last of sorted"
    (gen-list gen-int)
    (lambda (lst)
      (or (null? lst)
          (let ([sorted (merge-sort lst)])
            (= (max-element lst) (list-ref sorted (- (length sorted) 1))))))
    'tests 200)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
