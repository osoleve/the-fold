; search-algorithms.ss - Search algorithm implementations
; Part of The Fold prelude

; ============================================
; Binary search algorithms
; ============================================

; binary-search: Find index of target in sorted list
; (binary-search 3 '(1 2 3 4 5)) => 2
; (binary-search 10 '(1 2 3 4 5)) => #f
(define (binary-search target lst)
  (if (null? lst)
      #f
      (let ((mid (floor (/ (length lst) 2))))
        (let ((mid-val (nth mid lst)))
          (if (= target mid-val)
              mid
              (if (< target mid-val)
                  (binary-search target (take lst mid))
                  (let ((result (binary-search target (drop lst (+ mid 1)))))
                    (if result (+ result mid 1) #f))))))))

; lower-bound: Find index of first element >= target
; (lower-bound 3 '(1 2 3 3 4 5)) => 2
(define (lower-bound target lst)
  (let ((go (fn (lo hi)
              (if (>= lo hi)
                  lo
                  (let ((mid (floor (/ (+ lo hi) 2))))
                    (if (< (nth mid lst) target)
                        (go (+ mid 1) hi)
                        (go lo mid)))))))
    (go 0 (length lst))))

; upper-bound: Find index of first element > target
; (upper-bound 3 '(1 2 3 3 4 5)) => 4
(define (upper-bound target lst)
  (let ((go (fn (lo hi)
              (if (>= lo hi)
                  lo
                  (let ((mid (floor (/ (+ lo hi) 2))))
                    (if (<= (nth mid lst) target)
                        (go (+ mid 1) hi)
                        (go lo mid)))))))
    (go 0 (length lst))))

; ============================================
; Module exports
; ============================================

(module-exports
  binary-search
  lower-bound
  upper-bound)
