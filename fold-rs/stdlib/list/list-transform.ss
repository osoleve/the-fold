; ============================================================
; List Transformation Operations
; Mapping, zipping, transposing, interspersing, rotating
; Part of list.ss module
; ============================================================

; adjacent-pairs: Get all adjacent pairs
(adjacent-pairs (fn (lst)
                    (if (null? lst)
                        '()
                        (if (null? (cdr lst))
                            '()
                            (zip lst (cdr lst))))))

; concat: Concatenate a list of lists
(concat (fn (lists) (foldl append '() lists)))

; flat-map: Map then concatenate results
(flat-map (fn (f lst) (concat (map f lst))))

; zip: Pair up elements from two lists
(zip (fn (lst1 lst2) (zip-with cons lst1 lst2)))

; unzip: Split list of pairs into (cars, cdrs)
(unzip (fn (pairs)
           (foldr (fn (pair acc)
                      (cons (cons (car pair) (car acc))
                            (cons (cdr pair) (cdr acc))))
                  (cons '() '())
                  pairs)))

; intersperse: Insert separator between elements
(intersperse (fix intersperse
                  (fn (sep lst)
                      (if (null? lst)
                          '()
                          (if (null? (cdr lst))
                              (list (car lst))
                              (cons (car lst) (cons sep (intersperse sep (cdr lst)))))))))

; map-indexed: Map with index (0-based)
(map-indexed (fix map-indexed
                  (fn (f lst)
                      (let ((go (fix go
                                     (fn (i xs)
                                         (if (null? xs)
                                             '()
                                             (cons (f i (car xs)) (go (+ i 1) (cdr xs))))))))
                           (go 0 lst)))))

; filter-indexed: Filter with index access
(filter-indexed (fix filter-indexed
                     (fn (p lst)
                         (let ((go (fix go
                                        (fn (i xs)
                                            (if (null? xs)
                                                '()
                                                (if (p i (car xs))
                                                    (cons (car xs) (go (+ i 1) (cdr xs)))
                                                    (go (+ i 1) (cdr xs))))))))
                              (go 0 lst)))))

; transpose: Transpose list of lists (matrix transpose)
(transpose (fix transpose
                (fn (lists)
                    (if (or (null? lists) (any null? lists))
                        '()
                        (cons (map car lists) (transpose (map cdr lists)))))))

; even-indices: Get elements at even indices (0, 2, 4, ...)
(even-indices (fn (lst)
                  (filter-indexed (fn (i x) (even? i)) lst)))

; odd-indices: Get elements at odd indices (1, 3, 5, ...)
(odd-indices (fn (lst)
                 (filter-indexed (fn (i x) (odd? i)) lst)))

; rotate-list: Rotate list by n positions
; Note: Using nested let since n-norm depends on len
(rotate-list (fn (n lst)
                 (if (null? lst)
                     lst
                     (let ((len (length lst)))
                          (let ((n-norm (mod (+ (mod n len) len) len)))
                               (append (drop lst n-norm) (take lst n-norm)))))))

; rotate-left: Rotate list left by 1
(rotate-left (fn (lst)
                 (if (null? lst) '() (append (cdr lst) (list (car lst))))))

; rotate-right: Rotate list right by 1
(rotate-right (fn (lst)
                  (if (null? lst) '() (cons (last lst) (init lst)))))

; interleave: Interleave two lists
(interleave (fix interleave
                 (fn (xs ys)
                     (if (null? xs)
                         ys
                         (cons (car xs) (interleave ys (cdr xs)))))))

; take-nth: Take every nth element
(take-nth (fn (n lst)
              (filter-indexed (fn (i x) (= (mod i n) 0)) lst)))

; drop-nth: Drop every nth element
(drop-nth (fn (n lst)
              (filter-indexed (fn (i x) (not (= (mod i n) 0))) lst)))

; intercalate: Insert list between lists and concat
(intercalate (fn (sep lists) (concat (intersperse sep lists))))

; pairs: Consecutive pairs (sliding window of 2)
(pairs (fn (lst) (sliding 2 lst)))

; replace-at: Replace element at index
(replace-at (fn (i val lst)
                (map-indexed (fn (j x) (if (= i j) val x)) lst)))

; update-at: Update element at index with function
(update-at (fn (idx f lst)
               (map-indexed (fn (i x) (if (= i idx) (f x) x)) lst)))

; insert-at: Insert element at index
(insert-at (fn (i val lst)
               (append (take lst i) (cons val (drop lst i)))))

; remove-at: Remove element at index
(remove-at (fn (i lst)
               (append (take lst i) (drop lst (+ i 1)))))

; swap-at: Swap elements at two indices
(swap-at (fn (i j lst)
             (let ((vi (nth lst i))
                   (vj (nth lst j)))
                  (replace-at j vi (replace-at i vj lst)))))

; cartesian-product: Cartesian product of two lists
(cartesian-product (fn (xs ys)
                       (concat (map (fn (x) (map (fn (y) (list x y)) ys)) xs))))

; list-product: Cartesian product of list of lists
(list-product (fix list-product
                   (fn (lists)
                       (if (null? lists)
                           (list '())
                           (concat (map (fn (x)
                                            (map (fn (rest) (cons x rest))
                                                 (list-product (cdr lists))))
                                        (car lists)))))))

; --- Module Exports ---
; (see exports.ss for exported symbols)
