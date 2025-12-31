; ============================================================
; Search Algorithm Implementations
; Binary search and bounds algorithms
; ============================================================

; binary-search: Find index of target in sorted list
(binary-search (fix binary-search
                   (fn (target lst)
                       (if (null? lst)
                           #f
                           (let ((mid (floor (/ (length lst) 2))))
                                (let ((mid-val (nth mid lst)))
                                     (if (= target mid-val)
                                         mid
                                         (if (< target mid-val)
                                             (binary-search target (take mid lst))
                                             (let ((result (binary-search target (drop (+ mid 1) lst))))
                                                  (if result (+ result mid 1) #f))))))))))

; lower-bound: Find index of first element >= target
(lower-bound (fn (target lst)
                (let ((go (fix go
                              (fn (lo hi)
                                  (if (>= lo hi)
                                      lo
                                      (let ((mid (floor (/ (+ lo hi) 2))))
                                           (if (< (nth mid lst) target)
                                               (go (+ mid 1) hi)
                                               (go lo mid))))))))
                     (go 0 (length lst)))))

; upper-bound: Find index of first element > target
(upper-bound (fn (target lst)
                (let ((go (fix go
                              (fn (lo hi)
                                  (if (>= lo hi)
                                      lo
                                      (let ((mid (floor (/ (+ lo hi) 2))))
                                           (if (<= (nth mid lst) target)
                                               (go (+ mid 1) hi)
                                               (go lo mid))))))))
                     (go 0 (length lst)))))
