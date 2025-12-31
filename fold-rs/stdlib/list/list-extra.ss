; ============================================================
; Additional List Utilities
; Scanning, searching, partitioning, and zipping
; ============================================================

; scanl: Left-to-right scan
(scanl (fix scanl
           (fn (f init lst)
               (if (null? lst)
                   (list init)
                   (cons init (scanl f (f init (car lst)) (cdr lst)))))))

; scanr: Right-to-left scan
(scanr (fix scanr
           (fn (f init lst)
               (if (null? lst)
                   (list init)
                   (let ((rest (scanr f init (cdr lst))))
                        (cons (f (car lst) (car rest)) rest))))))

; reduce: Like foldl but uses first element as init
(reduce (fn (f lst)
           (if (null? lst)
               '()
               (foldl f (car lst) (cdr lst)))))

; reduce-right: Like foldr but uses last element as init
(reduce-right (fn (f lst)
                 (if (null? lst)
                     '()
                     (foldr f (last lst) (init lst)))))

; member-of: Find element in list (returns tail or #f)
(member-of (fix member-of
               (fn (x lst)
                   (if (null? lst)
                       #f
                       (if (equal? x (car lst))
                           lst
                           (member-of x (cdr lst)))))))

; memq-fn: Like member but uses eq?
(memq-fn (fix memq-fn
             (fn (x lst)
                 (if (null? lst)
                     #f
                     (if (eq? x (car lst))
                         lst
                         (memq-fn x (cdr lst)))))))

; member-of?: Boolean membership check
(member-of? (fn (x lst)
               (if (member-of x lst) #t #f)))

; position-of: Find index of element
(position-of (fn (x lst)
                (let ((helper (fix helper
                                  (fn (lst idx)
                                      (if (null? lst)
                                          #f
                                          (if (equal? x (car lst))
                                              idx
                                              (helper (cdr lst) (+ idx 1))))))))
                     (helper lst 0))))

; list-index-fn: Find index where predicate holds
(list-index-fn (fn (f lst)
                  (let ((helper (fix helper
                                    (fn (lst idx)
                                        (if (null? lst)
                                            #f
                                            (if (f (car lst))
                                                idx
                                                (helper (cdr lst) (+ idx 1))))))))
                       (helper lst 0))))

; find-last: Find last element satisfying predicate
(find-last (fix find-last
               (fn (p lst)
                   (if (null? lst)
                       #f
                       (let ((rest (find-last p (cdr lst))))
                            (if rest
                                rest
                                (if (p (car lst)) (car lst) #f)))))))

; partition-all: Split list into chunks of n
(partition-all (fix partition-all
                   (fn (n lst)
                       (if (null? lst)
                           '()
                           (cons (take n lst)
                                 (partition-all n (drop n lst)))))))

; chunks: Alias for partition-all
(chunks partition-all)

; split-at-pred: Split list at first element satisfying predicate
(split-at-pred (fn (p lst)
                  (let ((split-helper (fix split-helper
                                          (fn (remaining before)
                                              (if (null? remaining)
                                                  (list (reverse before) '())
                                                  (if (p (car remaining))
                                                      (list (reverse before) remaining)
                                                      (split-helper (cdr remaining) (cons (car remaining) before))))))))
                       (split-helper lst '()))))

; zip3: Zip three lists together
(zip3 (fix zip3
          (fn (xs ys zs)
              (if (null? xs)
                  '()
                  (if (null? ys)
                      '()
                      (if (null? zs)
                          '()
                          (cons (list (car xs) (car ys) (car zs))
                                (zip3 (cdr xs) (cdr ys) (cdr zs)))))))))

; zip-with-idx: Pair each element with its index
(zip-with-idx (fn (lst)
                 (zip (range 0 (length lst)) lst)))

; enumerate: Alias for zip-with-idx
(enumerate zip-with-idx)
