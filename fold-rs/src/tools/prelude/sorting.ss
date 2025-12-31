; sorting.ss - Sorting algorithms and utilities
; Part of The Fold prelude

; ============================================
; Insertion sort utilities
; ============================================

; insert-sorted: Insert element into sorted list
; (insert-sorted 3 '(1 2 4 5)) => (1 2 3 4 5)
(define (insert-sorted x lst)
  (if (null? lst)
      (list x)
      (if (<= x (car lst))
          (cons x lst)
          (cons (car lst) (insert-sorted x (cdr lst))))))

; insert-sorted-by: Insert element using comparison function
; (insert-sorted-by > 3 '(5 4 2 1)) => (5 4 3 2 1)
(define (insert-sorted-by cmp x lst)
  (if (null? lst)
      (list x)
      (if (cmp x (car lst))
          (cons x lst)
          (cons (car lst) (insert-sorted-by cmp x (cdr lst))))))

; ============================================
; Merge sort utilities
; ============================================

; merge-sorted: Merge two sorted lists
; (merge-sorted '(1 3 5) '(2 4 6)) => (1 2 3 4 5 6)
(define (merge-sorted xs ys)
  (if (null? xs)
      ys
      (if (null? ys)
          xs
          (if (<= (car xs) (car ys))
              (cons (car xs) (merge-sorted (cdr xs) ys))
              (cons (car ys) (merge-sorted xs (cdr ys)))))))

; merge-sorted-by: Merge two lists using comparison function
; (merge-sorted-by > '(5 3 1) '(6 4 2)) => (6 5 4 3 2 1)
(define (merge-sorted-by cmp xs ys)
  (if (null? xs)
      ys
      (if (null? ys)
          xs
          (if (cmp (car xs) (car ys))
              (cons (car xs) (merge-sorted-by cmp (cdr xs) ys))
              (cons (car ys) (merge-sorted-by cmp xs (cdr ys)))))))

; ============================================
; Module exports
; ============================================

(module-exports
  insert-sorted
  insert-sorted-by
  merge-sorted
  merge-sorted-by)
