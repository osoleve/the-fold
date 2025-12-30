; ============================================================
; List Manipulation Functions
; Comprehensive list operations beyond core HOFs
; ============================================================

; sum-list: Sum a list of numbers
(sum-list (fn (lst) (foldl + 0 lst)))

; product-list: Product of a list of numbers
(product-list (fn (lst) (foldl * 1 lst)))

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

; partition: Split list into (matches, non-matches) based on predicate
(partition (fix partition
                (fn (p lst)
                    (if (null? lst)
                        (list '() '())
                        (let ((rest-result (partition p (cdr lst)))
                              (x (car lst)))
                             (if (p x)
                                 (list (cons x (car rest-result)) (cadr rest-result))
                                 (list (car rest-result) (cons x (cadr rest-result)))))))))

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

; replicate: Create list of n copies of x
(replicate (fix replicate
                (fn (n x)
                    (if (<= n 0)
                        '()
                        (cons x (replicate (- n 1) x))))))

; iterate: Generate list by applying f n times: (x (f x) (f (f x)) ...)
(iterate (fix iterate
              (fn (f n x)
                  (if (<= n 0)
                      '()
                      (cons x (iterate f (- n 1) (f x)))))))

; scanl: Like foldl but returns list of intermediate results
(scanl (fix scanl
            (fn (f acc lst)
                (if (null? lst)
                    (list acc)
                    (cons acc (scanl f (f acc (car lst)) (cdr lst)))))))

; intersperse: Insert separator between elements
(intersperse (fix intersperse
                  (fn (sep lst)
                      (if (null? lst)
                          '()
                          (if (null? (cdr lst))
                              (list (car lst))
                              (cons (car lst) (cons sep (intersperse sep (cdr lst)))))))))

; span: Split at first element not matching predicate
(span (fn (p lst) (cons (take-while p lst) (drop-while p lst))))

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

; nth-safe: Safe list access (returns #f if out of bounds)
(nth-safe (fix nth-safe
               (fn (n lst)
                   (if (null? lst)
                       #f
                       (if (= n 0)
                           (car lst)
                           (nth-safe (- n 1) (cdr lst)))))))

; unfold: Generate list from seed (dual of fold)
(unfold (fix unfold
             (fn (stop? extract next seed)
                 (if (stop? seed)
                     '()
                     (cons (extract seed) (unfold stop? extract next (next seed)))))))

; tails: All suffixes of a list
(tails (fix tails
            (fn (lst)
                (if (null? lst)
                    (list '())
                    (cons lst (tails (cdr lst)))))))

; inits: All prefixes of a list
(inits (fix inits
            (fn (lst)
                (if (null? lst)
                    (list '())
                    (cons '() (map (fn (t) (cons (car lst) t)) (inits (cdr lst))))))))

; group-consecutive: Group consecutive equal elements
(group-consecutive (fix group-consecutive
                        (fn (lst)
                            (if (null? lst)
                                '()
                                (let ((x (car lst)))
                                     (let ((result (span (fn (y) (= x y)) lst)))
                                          (cons (car result) (group-consecutive (cdr result)))))))))

; range-list: Generate list of numbers (uses primitive range)
(range-list (fn (start end) (range start end)))

; repeat-fn: Apply function n times to initial value, return final result
(repeat-fn (fix repeat-fn
                (fn (f n x)
                    (if (<= n 0)
                        x
                        (repeat-fn f (- n 1) (f x))))))

; chunks: Split list into chunks of size n
(chunks (fix chunks
             (fn (n lst)
                 (if (null? lst)
                     '()
                     (cons (take lst n) (chunks n (drop lst n)))))))

; sliding: Sliding window of size n
(sliding (fix sliding
              (fn (n lst)
                  (if (< (length lst) n)
                      '()
                      (cons (take lst n) (sliding n (cdr lst)))))))

; pairs: Consecutive pairs (sliding window of 2)
(pairs (fn (lst) (sliding 2 lst)))

; split-at: Split list at index n
(split-at (fn (n lst) (cons (take lst n) (drop lst n))))

; elem?: Check if element is in list
(elem? (fn (x lst) (if (member x lst) #t #f)))

; nub: Remove duplicates (keep first occurrence)
(nub (fix nub
          (fn (lst)
              (if (null? lst)
                  '()
                  (cons (car lst) (nub (filter (fn (x) (not (eq? x (car lst)))) (cdr lst))))))))

; intercalate: Insert list between lists and concat
(intercalate (fn (sep lists) (concat (intersperse sep lists))))

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
(rotate-list (fn (n lst)
                 (if (null? lst)
                     lst
                     (let ((len (length lst))
                           (n-norm (mod (+ (mod n len) len) len)))
                          (append (drop lst n-norm) (take lst n-norm))))))

; frequencies: Count occurrences of each element
(frequencies (fn (lst)
                 (foldl (fn (acc x)
                            (let ((entry (assoc x acc)))
                                 (if entry
                                     (map (fn (p) (if (eq? (car p) x)
                                                      (list (car p) (+ (cadr p) 1))
                                                      p))
                                          acc)
                                     (cons (list x 1) acc))))
                        '()
                        lst)))

; dedupe: Remove consecutive duplicates
(dedupe (fix dedupe
             (fn (lst)
                 (if (null? lst)
                     '()
                     (if (null? (cdr lst))
                         lst
                         (if (eq? (car lst) (cadr lst))
                             (dedupe (cdr lst))
                             (cons (car lst) (dedupe (cdr lst)))))))))

; dedupe-by: Remove consecutive duplicates by key function
(dedupe-by (fix dedupe-by
                (fn (f lst)
                    (if (null? lst)
                        '()
                        (if (null? (cdr lst))
                            lst
                            (if (eq? (f (car lst)) (f (cadr lst)))
                                (dedupe-by f (cdr lst))
                                (cons (car lst) (dedupe-by f (cdr lst)))))))))

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

; rotate-left: Rotate list left by 1
(rotate-left (fn (lst)
                 (if (null? lst) '() (append (cdr lst) (list (car lst))))))

; rotate-right: Rotate list right by 1
(rotate-right (fn (lst)
                  (if (null? lst) '() (cons (last lst) (init lst)))))

; split-when: Split list when predicate becomes true
(split-when (fix split-when
                 (fn (p lst)
                     (if (null? lst)
                         (list '())
                         (if (p (car lst))
                             (cons '() (split-when p (cdr lst)))
                             (let ((rest (split-when p (cdr lst))))
                                  (cons (cons (car lst) (car rest)) (cdr rest))))))))

; split-with: Split at first element not satisfying predicate
(split-with (fn (p lst)
                (list (take-while p lst) (drop-while p lst))))

; group-runs: Group elements into runs where predicate holds between consecutive elements
(group-runs (fix group-runs
                 (fn (p lst)
                     (if (null? lst)
                         '()
                         (let ((go (fix go
                                        (fn (current rest)
                                            (if (null? rest)
                                                (list (reverse current))
                                                (if (p (car current) (car rest))
                                                    (go (cons (car rest) current) (cdr rest))
                                                    (cons (reverse current)
                                                          (group-runs p rest))))))))
                              (go (list (car lst)) (cdr lst)))))))

; map-runs: Apply function to runs of elements
(map-runs (fn (p f lst)
              (map f (group-runs p lst))))

; find-indices: Find all indices where predicate holds
(find-indices (fn (p lst)
                  (filter-indexed (fn (i x) (p x)) (map-indexed (fn (i x) i) lst))))

; replace-at: Replace element at index
(replace-at (fn (i val lst)
                (map-indexed (fn (j x) (if (= i j) val x)) lst)))

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

; count-occurrences: Count occurrences of element
(count-occurrences (fn (x lst)
                       (count-if (fn (y) (eq? x y)) lst)))

; windowed: All windows of size n
(windowed sliding)

; cartesian-product: Cartesian product of two lists
(cartesian-product (fn (xs ys)
                       (concat (map (fn (x) (map (fn (y) (list x y)) ys)) xs))))

; power-set: All subsets of a list
(power-set (fix power-set
                (fn (lst)
                    (if (null? lst)
                        (list '())
                        (let ((rest (power-set (cdr lst))))
                             (append rest (map (fn (s) (cons (car lst) s)) rest)))))))

; permutations: All permutations of a list
(permutations (fix permutations
                   (fn (lst)
                       (if (null? lst)
                           (list '())
                           (concat (map (fn (x)
                                            (map (fn (p) (cons x p))
                                                 (permutations (remove x lst))))
                                        lst))))))

; combinations: All k-combinations of a list
(combinations (fix combinations
                   (fn (k lst)
                       (if (= k 0)
                           (list '())
                           (if (null? lst)
                               '()
                               (append
                                (map (fn (c) (cons (car lst) c))
                                     (combinations (- k 1) (cdr lst)))
                                (combinations k (cdr lst))))))))

; list-product: Cartesian product of list of lists
(list-product (fix list-product
                   (fn (lists)
                       (if (null? lists)
                           (list '())
                           (concat (map (fn (x)
                                            (map (fn (rest) (cons x rest))
                                                 (list-product (cdr lists))))
                                        (car lists)))))))

; subsets: Alias for power-set
(subsets power-set)
