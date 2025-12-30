; ============================================================
; List Partitioning Operations
; Partitioning, grouping, chunking, splitting, deduplication
; Part of list.ss module
; ============================================================

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

; span: Split at first element not matching predicate
(span (fn (p lst) (cons (take-while p lst) (drop-while p lst))))

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

; split-at: Split list at index n
(split-at (fn (n lst) (cons (take lst n) (drop lst n))))

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

; group-consecutive: Group consecutive equal elements
(group-consecutive (fix group-consecutive
                        (fn (lst)
                            (if (null? lst)
                                '()
                                (let ((x (car lst)))
                                     (let ((result (span (fn (y) (= x y)) lst)))
                                          (cons (car result) (group-consecutive (cdr result)))))))))

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

; nub: Remove duplicates (keep first occurrence)
(nub (fix nub
          (fn (lst)
              (if (null? lst)
                  '()
                  (cons (car lst) (nub (filter (fn (x) (not (eq? x (car lst)))) (cdr lst))))))))

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

; windowed: All windows of size n
(windowed sliding)

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

; subsets: Alias for power-set
(subsets power-set)

; insert-sorted-by: Insert element into sorted list using comparison function
(insert-sorted-by (fix insert-sorted-by
                       (fn (cmp x lst)
                           (if (null? lst)
                               (list x)
                               (if (<= (cmp x) (cmp (car lst)))
                                   (cons x lst)
                                   (cons (car lst) (insert-sorted-by cmp x (cdr lst))))))))

; --- Module Exports ---
(module-exports
 partition span chunks sliding split-at split-when split-with
 group-consecutive group-runs map-runs
 nub dedupe dedupe-by frequencies windowed
 power-set permutations combinations subsets insert-sorted-by)
