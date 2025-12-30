; ============================================================
; Collection - Set Operations
; List-based set manipulation and operations
; Part of collection.ss module
; ============================================================

; ============================================
; Set Operations (list-based)
; ============================================

; set-new: Create empty set
(set-new (fn () '()))

; set-add: Add element to set
(set-add (fn (s x)
             (if (member? x s) s (cons x s))))

; set-remove: Remove element from set
(set-remove remove)

; set-member?: Check if element is in set
(set-member? member?)

; set-size: Get number of elements
(set-size length)

; set-empty?: Check if set is empty
(set-empty? null?)

; set-from-list: Create set from list (removes duplicates)
(set-from-list nub)

; set-to-list: Convert set to list
(set-to-list id)

; ============================================
; Core Set Operations
; ============================================

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

; ============================================
; Set-Prefixed Aliases
; ============================================

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

; set-disjoint?: Check if two sets have no common elements
(set-disjoint? (fn (s1 s2)
                   (null? (set-intersection s1 s2))))

; --- Module Exports ---
; (see exports.ss for exported symbols)
