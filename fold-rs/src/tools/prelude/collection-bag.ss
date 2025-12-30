; ============================================================
; Collection - Bag/Multiset Operations
; Multisets (bags) with counted elements
; Part of collection.ss module
; ============================================================

; ============================================
; Multiset/Bag Operations (alist of element -> count)
; ============================================

; multiset-new: Create empty multiset
(multiset-new (fn () '()))

; multiset-add: Add element to multiset (alist of counts)
(multiset-add (fn (ms x)
                  (let ((found (assoc-ref x ms)))
                       (let ((count (if found found 0)))
                            (assoc-set x (+ count 1) ms)))))

; multiset-add-n: Add n copies of element to multiset
(multiset-add-n (fn (ms x n)
                    (let ((found (assoc-ref x ms)))
                         (let ((count (if found found 0)))
                              (assoc-set x (+ count n) ms)))))

; multiset-remove: Remove one occurrence from multiset
(multiset-remove (fn (ms x)
                     (let ((found (assoc-ref x ms)))
                          (let ((count (if found found 0)))
                               (if (<= count 1)
                                   (assoc-remove x ms)
                                   (assoc-set x (- count 1) ms))))))

; multiset-remove-all: Remove all occurrences of element
(multiset-remove-all (fn (ms x)
                         (assoc-remove x ms)))

; multiset-count: Get count of element in multiset
(multiset-count (fn (ms x)
                    (let ((found (assoc-ref x ms)))
                         (if found found 0))))

; multiset-member?: Check if element is in multiset
(multiset-member? (fn (ms x)
                      (> (multiset-count ms x) 0)))

; multiset-elements: Get unique elements (keys)
(multiset-elements (fn (ms) (map car ms)))

; multiset-size: Get total count (sum of all counts)
(multiset-size (fn (ms)
                   (foldl (fn (acc pair) (+ acc (cdr pair))) 0 ms)))

; multiset-from-list: Create multiset from list
(multiset-from-list (fn (lst)
                        (foldl multiset-add '() lst)))

; multiset-to-list: Convert multiset to list (with repetitions)
(multiset-to-list (fn (ms)
                      (flat-map (fn (pair)
                                    (replicate (cdr pair) (car pair)))
                                ms)))

; ============================================
; Bag Operations (multiset synonyms)
; ============================================

; bag-new: Create empty bag
(bag-new multiset-new)

; bag-add: Add element to bag
(bag-add multiset-add)

; bag-remove: Remove one occurrence from bag
(bag-remove multiset-remove)

; bag-count: Get count of element in bag
(bag-count multiset-count)

; bag-from-list: Create bag from list
(bag-from-list multiset-from-list)

; bag-to-list: Convert bag to list with repetitions
(bag-to-list multiset-to-list)

; bag-union: Union of two multisets (sum counts)
(bag-union (fn (ms1 ms2)
               (foldl (fn (acc pair)
                          (let ((count (+ (multiset-count acc (car pair)) (cdr pair))))
                               (assoc-set (car pair) count acc)))
                      ms1
                      ms2)))

; bag-intersection: Intersection of two multisets (min counts)
(bag-intersection (fn (ms1 ms2)
                      (foldl (fn (acc pair)
                                 (let ((c1 (cdr pair))
                                       (c2 (multiset-count ms2 (car pair))))
                                      (if (> c2 0)
                                          (assoc-set (car pair) (min c1 c2) acc)
                                          acc)))
                             '()
                             ms1)))

; bag-difference: Difference of two multisets (subtract counts)
(bag-difference (fn (ms1 ms2)
                    (foldl (fn (acc pair)
                               (let ((c1 (cdr pair))
                                     (c2 (multiset-count ms2 (car pair)))
                                     (diff (- c1 c2)))
                                    (if (> diff 0)
                                        (assoc-set (car pair) diff acc)
                                        acc)))
                           '()
                           ms1)))

; bag-sub?: Check if ms1 is sub-bag of ms2 (all counts <=)
(bag-sub? (fn (ms1 ms2)
              (all (fn (pair)
                       (<= (cdr pair) (multiset-count ms2 (car pair))))
                   ms1)))

; bag-equal?: Check if two bags have same elements with same counts
(bag-equal? (fn (ms1 ms2)
                (and (bag-sub? ms1 ms2) (bag-sub? ms2 ms1))))

; ============================================
; Frequency/Run-Length Operations
; ============================================

; frequencies: Count occurrences of each element
; (frequencies '(a b a c a b)) => ((a . 3) (b . 2) (c . 1))
(frequencies multiset-from-list)

; run-length-encode: Encode consecutive runs
; (run-length-encode '(a a a b b c)) => ((3 . a) (2 . b) (1 . c))
(run-length-encode (fn (lst)
                       (if (null? lst)
                           '()
                           ((fix encode
                                 (fn (xs current count)
                                     (if (null? xs)
                                         (list (cons count current))
                                         (if (eq? (car xs) current)
                                             (encode (cdr xs) current (+ count 1))
                                             (cons (cons count current)
                                                   (encode (cdr xs) (car xs) 1))))))
                            (cdr lst) (car lst) 1))))

; run-length-decode: Decode run-length encoding
; (run-length-decode '((3 . a) (2 . b) (1 . c))) => (a a a b b c)
(run-length-decode (fn (rle)
                       (flat-map (fn (pair)
                                     (replicate (car pair) (cdr pair)))
                                 rle)))

; --- Module Exports ---
; (see exports.ss for exported symbols)
