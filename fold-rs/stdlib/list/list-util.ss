; ============================================================
; List Predicates and Utility Functions
; List analysis and manipulation utilities
; ============================================================

; all-equal?: Check if all elements are equal
(all-equal? (fn (lst)
               (if (null? lst)
                   #t
                   (all (fn (x) (equal? x (car lst))) (cdr lst)))))

; sorted?: Check if list is sorted (ascending)
(sorted? (fix sorted?
             (fn (lst)
                 (if (null? lst)
                     #t
                     (if (null? (cdr lst))
                         #t
                         (if (<= (car lst) (cadr lst))
                             (sorted? (cdr lst))
                             #f))))))

; sorted-by?: Check if list is sorted by key function
(sorted-by? (fn (key-fn lst)
               (sorted? (map key-fn lst))))

; palindrome?: Check if list is a palindrome
(palindrome? (fix palindrome?
                 (fn (lst)
                     (if (null? lst)
                         #t
                         (if (null? (cdr lst))
                             #t
                             (if (equal? (car lst) (last lst))
                                 (palindrome? (cdr (init lst)))
                                 #f))))))

; interpose: Insert separator between elements
(interpose (fix interpose
               (fn (sep lst)
                   (if (null? lst)
                       '()
                       (if (null? (cdr lst))
                           lst
                           (cons (car lst)
                                 (cons sep (interpose sep (cdr lst)))))))))

; init-list: All but last element
(init-list (fix init-list
               (fn (lst)
                   (if (null? lst)
                       '()
                       (if (null? (cdr lst))
                           '()
                           (cons (car lst) (init-list (cdr lst))))))))

; nth-elem: Get nth element (0-indexed)
(nth-elem (fix nth-elem
              (fn (n lst)
                  (if (= n 0)
                      (car lst)
                      (nth-elem (- n 1) (cdr lst))))))
