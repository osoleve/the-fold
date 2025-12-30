; ============================================================
; List Search Operations
; Finding, filtering, counting elements
; Part of list.ss module
; ============================================================

; before: Get elements before first match
(before (fn (f lst)
            (car (span (complement f) lst))))

; after: Get elements after first match (excluding match)
(after (fn (f lst)
           (let ((tail (drop-while (complement f) lst)))
                (if (null? tail) '() (cdr tail)))))

; find-if: Find first element matching predicate, or #f
(find-if (fix find-if
              (fn (p lst)
                  (if (null? lst)
                      #f
                      (if (p (car lst))
                          (car lst)
                          (find-if p (cdr lst)))))))

; remove-if: Remove elements matching predicate (opposite of filter)
(remove-if (fn (p lst) (filter (complement p) lst)))

; remove: Remove first occurrence of element (uses eq?)
(remove (fix remove
             (fn (x lst)
                 (if (null? lst)
                     '()
                     (if (eq? x (car lst))
                         (cdr lst)
                         (cons (car lst) (remove x (cdr lst))))))))

; count-if: Count elements matching predicate
(count-if (fn (p lst) (foldl (fn (acc x) (if (p x) (+ acc 1) acc)) 0 lst)))

; elem?: Check if element is in list
(elem? (fn (x lst) (if (member x lst) #t #f)))

; find-indices: Find all indices where predicate holds
(find-indices (fn (p lst)
                  (filter-indexed (fn (i x) (p x)) (map-indexed (fn (i x) i) lst))))

; count-occurrences: Count occurrences of element
(count-occurrences (fn (x lst)
                       (count-if (fn (y) (eq? x y)) lst)))

; index-where: Find index of first element matching predicate, or #f
(index-where (fix index-where
                  (fn (p lst)
                      (let ((go (fix go
                                     (fn (i xs)
                                         (if (null? xs)
                                             #f
                                             (if (p (car xs))
                                                 i
                                                 (go (+ i 1) (cdr xs))))))))
                           (go 0 lst)))))

; indices-where: Find all indices of elements matching predicate
(indices-where (fn (p lst)
                   (map-maybe (fn (pair) (if (p (cdr pair)) (car pair) #f))
                              (map-indexed (fn (i x) (cons i x)) lst))))

; last-where: Find last element matching predicate
(last-where (fn (p lst)
                (foldr (fn (x acc) (if (and (not acc) (p x)) x acc)) #f lst)))

; --- Module Exports ---
; (see exports.ss for exported symbols)
