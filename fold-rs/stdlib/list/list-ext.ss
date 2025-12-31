; ============================================================
; Extended List Functions
; Canonical versions - no duplicates
; ============================================================

; frequencies: Count occurrences of each unique element
; (frequencies '(a b a c b a)) => ((a 3) (b 2) (c 1))
(frequencies (fn (lst)
                 (let ((unique (nub lst)))
                      (map (fn (x) (list x (count-if (fn (y) (eq? x y)) lst))) unique))))

; interleave: Interleave elements from two lists
; (interleave '(1 2 3) '(a b c)) => (1 a 2 b 3 c)
(interleave (fix interleave
                 (fn (xs ys)
                     (if (null? xs)
                         ys
                         (if (null? ys)
                             xs
                             (cons (car xs)
                                   (cons (car ys)
                                         (interleave (cdr xs) (cdr ys)))))))))

; insert-at: Insert element at index
; (insert-at 1 'x '(a b c)) => (a x b c)
(insert-at (fix insert-at
                (fn (idx elem lst)
                    (if (= idx 0)
                        (cons elem lst)
                        (if (null? lst)
                            (list elem)
                            (cons (car lst) (insert-at (- idx 1) elem (cdr lst))))))))

; remove-at: Remove element at index
; (remove-at 1 '(a b c)) => (a c)
(remove-at (fn (idx lst)
               (map-maybe (fn (pair) (if (= (car pair) idx) #f (cdr pair)))
                          (map-indexed (fn (i x) (cons i x)) lst))))

; iterate-n: Apply function n times to value
; (iterate-n 3 inc 5) => 8
(iterate-n (fix iterate-n
                (fn (n f x)
                    (if (<= n 0)
                        x
                        (iterate-n (- n 1) f (f x))))))

; powers-of: Generate list of powers [base^0, base^1, ..., base^(n-1)]
; (powers-of 2 5) => (1 2 4 8 16)
(powers-of (fn (base n)
               (map (fn (i) (pow-int base i)) (iota n 0))))

; triangular-numbers: Generate first n triangular numbers
; (triangular-numbers 5) => (1 3 6 10 15)
(triangular-numbers (fn (n)
                        (map triangular (iota n 1))))

; index-where: Find index of first element matching predicate, or #f
; (index-where even? '(1 3 4 5)) => 2
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
; (indices-where even? '(1 2 3 4 5 6)) => (1 3 5)
(indices-where (fn (p lst)
                   (map-maybe (fn (pair) (if (p (cdr pair)) (car pair) #f))
                              (map-indexed (fn (i x) (cons i x)) lst))))

; last-where: Find last element matching predicate
; (last-where even? '(1 2 3 4 5)) => 4
(last-where (fn (p lst)
                (foldr (fn (x acc) (if (and (not acc) (p x)) x acc)) #f lst)))

; update-at: Update element at index with function
; (update-at 1 inc '(1 2 3)) => (1 3 3)
(update-at (fn (idx f lst)
               (map-indexed (fn (i x) (if (= i idx) (f x) x)) lst)))

; generate: Generate list using function and seed
; (generate 5 inc 0) => (0 1 2 3 4)
(generate (fix generate
               (fn (n f seed)
                   (if (<= n 0)
                       '()
                       (cons seed (generate (- n 1) f (f seed)))))))

; unfold-right: Right unfold (build list from seed)
(unfold-right (fix unfold-right
                   (fn (stop? f seed)
                       (if (stop? seed)
                           '()
                           (snoc (unfold-right stop? f (f seed)) seed)))))

; scan-right: Right scan (like scanl but from right)
(scan-right (fix scan-right
                 (fn (f init lst)
                     (if (null? lst)
                         (list init)
                         (let ((rest (scan-right f init (cdr lst))))
                              (cons (f (car lst) (car rest)) rest))))))

; running-max: Running maximum
(running-max (fn (lst)
                 (if (null? lst)
                     '()
                     (scanl max (car lst) (cdr lst)))))

; running-min: Running minimum
(running-min (fn (lst)
                 (if (null? lst)
                     '()
                     (scanl min (car lst) (cdr lst)))))
