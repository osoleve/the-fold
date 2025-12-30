; ============================================================
; Set Operations (Lists as Sets)
; Canonical versions - no duplicates
; ============================================================

; union: Combine two lists, removing duplicates
; (union '(1 2 3) '(2 3 4)) => (1 2 3 4)
(union (fn (xs ys)
           (nub (append xs ys))))

; intersection: Elements in both lists
; (intersection '(1 2 3) '(2 3 4)) => (2 3)
(intersection (fn (xs ys)
                  (filter (fn (x) (elem? x ys)) xs)))

; difference: Elements in first list but not second
; (difference '(1 2 3) '(2 3 4)) => (1)
(difference (fn (xs ys)
                (filter (fn (x) (not (elem? x ys))) xs)))

; symmetric-difference: Elements in either list but not both
; (symmetric-difference '(1 2 3) '(2 3 4)) => (1 4)
(symmetric-difference (fn (xs ys)
                          (append (difference xs ys) (difference ys xs))))

; subset?: Check if first list is subset of second
; (subset? '(1 2) '(1 2 3)) => #t
(subset? (fn (xs ys)
             (all (fn (x) (elem? x ys)) xs)))

; disjoint?: Check if lists have no common elements
; (disjoint? '(1 2) '(3 4)) => #t
(disjoint? (fn (xs ys)
               (null? (intersection xs ys))))

; set-union: Alias for union
(set-union union)

; set-intersection: Alias for intersection
(set-intersection intersection)

; set-difference: Alias for difference
(set-difference difference)

; set-symmetric-difference: Alias for symmetric-difference
(set-symmetric-difference symmetric-difference)

; set-subset?: Alias for subset?
(set-subset? subset?)

; set-equal?: Check if two sets have same elements
; (set-equal? '(1 2 3) '(3 2 1)) => #t
(set-equal? (fn (s1 s2)
                (and (set-subset? s1 s2) (set-subset? s2 s1))))

; set-superset?: Check if first set is superset of second
; (set-superset? '(1 2 3) '(1 2)) => #t
(set-superset? (fn (s1 s2)
                   (set-subset? s2 s1)))
