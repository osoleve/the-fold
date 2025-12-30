; ============================================================
; List Core Operations
; Basic list manipulation: take, drop, cons variants, replicate
; Part of list.ss module
; ============================================================

; sum-list: Sum a list of numbers
(sum-list (fn (lst) (foldl + 0 lst)))

; product-list: Product of a list of numbers
(product-list (fn (lst) (foldl * 1 lst)))

; take-n: Take first n elements (HOF version)
(take-n (fix take-n
             (fn (n lst)
                 (if (<= n 0)
                     '()
                     (if (null? lst)
                         '()
                         (cons (car lst) (take-n (- n 1) (cdr lst))))))))

; drop-n: Drop first n elements (HOF version)
(drop-n (fix drop-n
             (fn (n lst)
                 (if (<= n 0)
                     lst
                     (if (null? lst)
                         '()
                         (drop-n (- n 1) (cdr lst)))))))

; take-right: Take n elements from end
(take-right (fn (n lst)
                (drop-n (- (length lst) n) lst)))

; drop-right: Drop n elements from end
(drop-right (fn (n lst)
                (take-n (- (length lst) n) lst)))

; take-last: Take n elements from end (alias)
(take-last take-right)

; drop-last: Drop n elements from end (alias)
(drop-last drop-right)

; butlast: Remove last n elements
(butlast (fn (n lst)
             (let ((len (length lst)))
                  (take-n (- len n) lst))))

; snoc: Append element to end of list
(snoc (fn (lst x) (append lst (list x))))

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

; range-list: Generate list of numbers (uses primitive range)
(range-list (fn (start end) (range start end)))

; repeat-fn: Apply function n times to initial value, return final result
(repeat-fn (fix repeat-fn
                (fn (f n x)
                    (if (<= n 0)
                        x
                        (repeat-fn f (- n 1) (f x))))))

; iterate-n: Apply function n times to value (alias for repeat-fn)
(iterate-n (fix iterate-n
                (fn (n f x)
                    (if (<= n 0)
                        x
                        (iterate-n (- n 1) f (f x))))))

; generate: Generate list using function and seed
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

; --- Module Exports ---
; (see exports.ss for exported symbols)
