; ============================================================
; Comparison and Sorting Functions
; Ordering, searching, and selection utilities
; ============================================================

; -- Extremum functions --

; max-by: Find maximum element by key function
(max-by (fix max-by
             (fn (key-fn lst)
                 (if (null? lst)
                     #f
                     (if (null? (cdr lst))
                         (car lst)
                         (let ((rest-max (max-by key-fn (cdr lst))))
                              (if (> (key-fn (car lst)) (key-fn rest-max))
                                  (car lst)
                                  rest-max)))))))

; min-by: Find minimum element by key function
(min-by (fix min-by
             (fn (key-fn lst)
                 (if (null? lst)
                     #f
                     (if (null? (cdr lst))
                         (car lst)
                         (let ((rest-min (min-by key-fn (cdr lst))))
                              (if (< (key-fn (car lst)) (key-fn rest-min))
                                  (car lst)
                                  rest-min)))))))

; max-list: Maximum of a list
(max-list (fn (lst)
              (if (null? lst)
                  #f
                  (foldl max (car lst) (cdr lst)))))

; min-list: Minimum of a list
(min-list (fn (lst)
              (if (null? lst)
                  #f
                  (foldl min (car lst) (cdr lst)))))

; -- Comparison utilities --

; compare-by: Compare two values by applying key function
; Returns -1, 0, or 1
(compare-by (fn (key-fn a b)
                (let ((ka (key-fn a))
                      (kb (key-fn b)))
                     (if (< ka kb)
                         (- 0 1)
                         (if (> ka kb) 1 0)))))

; comparing: Create comparator from key function
(comparing (fn (key-fn)
               (fn (a b) (< (key-fn a) (key-fn b)))))

; reverse-compare: Reverse a comparator
(reverse-compare (fn (cmp)
                     (fn (a b) (cmp b a))))

; -- Sorting functions (using insertion sort) --

; sort-by: Sort list by key function
(sort-by (fix sort-by
              (fn (key-fn lst)
                  (if (null? lst)
                      '()
                      (let ((x (car lst))
                            (rest (sort-by key-fn (cdr lst))))
                           (let ((insert-sorted (fix insert-sorted
                                                     (fn (elem sorted)
                                                         (if (null? sorted)
                                                             (list elem)
                                                             (if (<= (key-fn elem) (key-fn (car sorted)))
                                                                 (cons elem sorted)
                                                                 (cons (car sorted) (insert-sorted elem (cdr sorted)))))))))
                                (insert-sorted x rest)))))))

; insertion-sort: Insertion sort with default comparison
(insertion-sort (fn (lst) (sort-by id lst)))

; insertion-sort-cmp: Insertion sort with custom comparator
(insertion-sort-cmp (fix insertion-sort-cmp
                         (fn (cmp lst)
                             (if (null? lst)
                                 '()
                                 (let ((insert (fix insert
                                                    (fn (x sorted)
                                                        (if (null? sorted)
                                                            (list x)
                                                            (if (cmp x (car sorted))
                                                                (cons x sorted)
                                                                (cons (car sorted) (insert x (cdr sorted)))))))))
                                      (insert (car lst) (insertion-sort-cmp cmp (cdr lst))))))))

; merge-sort: Merge sort (more efficient for larger lists)
(merge-sort (fix merge-sort
                 (fn (lst)
                     (let ((merge (fix merge
                                       (fn (xs ys)
                                           (if (null? xs) ys
                                               (if (null? ys) xs
                                                   (if (<= (car xs) (car ys))
                                                       (cons (car xs) (merge (cdr xs) ys))
                                                       (cons (car ys) (merge xs (cdr ys))))))))))
                          (if (null? lst) '()
                              (if (null? (cdr lst)) lst
                                  (let ((mid (/ (length lst) 2)))
                                       (merge (merge-sort (take lst mid))
                                              (merge-sort (drop lst mid))))))))))

; merge-sort-cmp: Merge sort with custom comparator
(merge-sort-cmp (fix merge-sort-cmp
                     (fn (cmp lst)
                         (let ((merge (fix merge
                                           (fn (xs ys)
                                               (if (null? xs) ys
                                                   (if (null? ys) xs
                                                       (if (cmp (car xs) (car ys))
                                                           (cons (car xs) (merge (cdr xs) ys))
                                                           (cons (car ys) (merge xs (cdr ys))))))))))
                              (if (null? lst) '()
                                  (if (null? (cdr lst)) lst
                                      (let ((mid (/ (length lst) 2)))
                                           (merge (merge-sort-cmp cmp (take lst mid))
                                                  (merge-sort-cmp cmp (drop lst mid))))))))))

; sorted?: Check if list is sorted (ascending)
(sorted? (fix sorted?
              (fn (lst)
                  (if (null? lst) #t
                      (if (null? (cdr lst)) #t
                          (if (<= (car lst) (cadr lst))
                              (sorted? (cdr lst))
                              #f))))))

; sorted-cmp?: Check if list is sorted by comparator
(sorted-cmp? (fix sorted-cmp?
                  (fn (cmp lst)
                      (if (null? lst) #t
                          (if (null? (cdr lst)) #t
                              (if (or (cmp (car lst) (cadr lst))
                                      (equal? (car lst) (cadr lst)))
                                  (sorted-cmp? cmp (cdr lst))
                                  #f))))))

; sort-descending: Sort in descending order
(sort-descending (fn (lst) (reverse (merge-sort lst))))

; sort-by-descending: Sort by key in descending order
(sort-by-descending (fn (key-fn lst)
                        (reverse (sort-by key-fn lst))))

; -- Selection --

; select-nth: Select nth smallest element (0-indexed)
(select-nth (fn (n lst)
                (nth (merge-sort lst) n)))

; select-top: Select top n elements (largest)
(select-top (fn (n lst)
                (take (reverse (merge-sort lst)) n)))

; select-bottom: Select bottom n elements (smallest)
(select-bottom (fn (n lst)
                   (take (merge-sort lst) n)))

; -- Binary search --

; binary-search: Find element in sorted list
(binary-search (fix binary-search
                    (fn (target lst)
                        (if (null? lst)
                            #f
                            (let ((mid (/ (length lst) 2))
                                  (mid-val (nth lst (/ (length lst) 2))))
                                 (if (= target mid-val)
                                     mid
                                     (if (< target mid-val)
                                         (binary-search target (take lst mid))
                                         (let ((result (binary-search target (drop lst (+ mid 1)))))
                                              (if result (+ result mid 1) #f)))))))))

; lower-bound: Find first position where element could be inserted
(lower-bound (fix lower-bound
                  (fn (target lst)
                      (let ((go (fix go
                                     (fn (lo hi)
                                         (if (>= lo hi)
                                             lo
                                             (let ((mid (/ (+ lo hi) 2)))
                                                  (if (< (nth lst mid) target)
                                                      (go (+ mid 1) hi)
                                                      (go lo mid))))))))
                           (go 0 (length lst))))))

; upper-bound: Find first position where element is greater
(upper-bound (fix upper-bound
                  (fn (target lst)
                      (let ((go (fix go
                                     (fn (lo hi)
                                         (if (>= lo hi)
                                             lo
                                             (let ((mid (/ (+ lo hi) 2)))
                                                  (if (<= (nth lst mid) target)
                                                      (go (+ mid 1) hi)
                                                      (go lo mid))))))))
                           (go 0 (length lst))))))

; -- Predicates --

; prefix?: Check if xs is a prefix of ys
(prefix? (fix prefix?
              (fn (xs ys)
                  (if (null? xs)
                      #t
                      (if (null? ys)
                          #f
                          (if (eq? (car xs) (car ys))
                              (prefix? (cdr xs) (cdr ys))
                              #f))))))

; suffix?: Check if xs is a suffix of ys
(suffix? (fn (xs ys)
             (prefix? (reverse xs) (reverse ys))))

; sublist?: Check if xs is a sublist of ys
(sublist? (fix sublist?
               (fn (xs ys)
                   (if (null? xs)
                       #t
                       (if (null? ys)
                           #f
                           (if (eq? (car xs) (car ys))
                               (sublist? (cdr xs) (cdr ys))
                               (sublist? xs (cdr ys))))))))

; monotonic-increasing?: Check if list is monotonically increasing
(monotonic-increasing? (fn (lst)
                           (if (null? lst) #t
                               (if (null? (cdr lst)) #t
                                   (and (<= (car lst) (cadr lst))
                                        (monotonic-increasing? (cdr lst)))))))

; monotonic-decreasing?: Check if list is monotonically decreasing
(monotonic-decreasing? (fn (lst)
                           (if (null? lst) #t
                               (if (null? (cdr lst)) #t
                                   (and (>= (car lst) (cadr lst))
                                        (monotonic-decreasing? (cdr lst)))))))

; strictly-increasing?: Check if list is strictly increasing
(strictly-increasing? (fn (lst)
                          (if (null? lst) #t
                              (if (null? (cdr lst)) #t
                                  (and (< (car lst) (cadr lst))
                                       (strictly-increasing? (cdr lst)))))))

; strictly-decreasing?: Check if list is strictly decreasing
(strictly-decreasing? (fn (lst)
                          (if (null? lst) #t
                              (if (null? (cdr lst)) #t
                                  (and (> (car lst) (cadr lst))
                                       (strictly-decreasing? (cdr lst)))))))
