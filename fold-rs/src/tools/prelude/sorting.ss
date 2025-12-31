; ============================================================
; Sorting Algorithms and Utilities
; Insertion sort and merge sort helpers
; ============================================================

; insert-sorted: Insert element into sorted list
(insert-sorted (fix insert-sorted
                   (fn (x lst)
                       (if (null? lst)
                           (list x)
                           (if (<= x (car lst))
                               (cons x lst)
                               (cons (car lst) (insert-sorted x (cdr lst))))))))

; insert-sorted-by: Insert element using comparison function
(insert-sorted-by (fix insert-sorted-by
                      (fn (cmp x lst)
                          (if (null? lst)
                              (list x)
                              (if (cmp x (car lst))
                                  (cons x lst)
                                  (cons (car lst) (insert-sorted-by cmp x (cdr lst))))))))

; merge-sorted: Merge two sorted lists
(merge-sorted (fix merge-sorted
                  (fn (xs ys)
                      (if (null? xs)
                          ys
                          (if (null? ys)
                              xs
                              (if (<= (car xs) (car ys))
                                  (cons (car xs) (merge-sorted (cdr xs) ys))
                                  (cons (car ys) (merge-sorted xs (cdr ys)))))))))

; merge-sorted-by: Merge two lists using comparison function
(merge-sorted-by (fix merge-sorted-by
                     (fn (cmp xs ys)
                         (if (null? xs)
                             ys
                             (if (null? ys)
                                 xs
                                 (if (cmp (car xs) (car ys))
                                     (cons (car xs) (merge-sorted-by cmp (cdr xs) ys))
                                     (cons (car ys) (merge-sorted-by cmp xs (cdr ys)))))))))
