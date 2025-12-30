; ============================================================
; Combinatorics Functions
; Canonical versions - no duplicates
; ============================================================

; rotate-left: Rotate list left by n positions
; (rotate-left 2 '(1 2 3 4 5)) => (3 4 5 1 2)
(rotate-left (fn (n lst)
                 (if (null? lst)
                     '()
                     (let ((len (length lst))
                           (n-mod (mod n (length lst))))
                          (append (drop lst n-mod) (take lst n-mod))))))

; rotate-right: Rotate list right by n positions
; (rotate-right 2 '(1 2 3 4 5)) => (4 5 1 2 3)
(rotate-right (fn (n lst)
                  (rotate-left (- (length lst) (mod n (length lst))) lst)))

; factorial: n! = 1 * 2 * ... * n
; (factorial 5) => 120
(factorial (fix factorial
                (fn (n)
                    (if (<= n 1)
                        1
                        (* n (factorial (- n 1)))))))

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

; list-product: Cartesian product of list of lists
(list-product (fix list-product
                   (fn (lists)
                       (if (null? lists)
                           (list '())
                           (concat (map (fn (x)
                                            (map (fn (rest) (cons x rest))
                                                 (list-product (cdr lists))))
                                        (car lists)))))))
