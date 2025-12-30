; ============================================================
; Equality Predicates
; Value and structural equality operations
; ============================================================

; eqv?: Value equality (same as eq? for symbols, = for numbers)
(eqv? (fn (a b)
          (if (number? a)
              (if (number? b) (= a b) #f)
              (if (string? a)
                  (if (string? b) (string=? a b) #f)
                  (if (char? a)
                      (if (char? b) (char=? a b) #f)
                      (eq? a b))))))

; equal?: Deep structural equality
(equal? (fix equal-rec
             (fn (a b)
                 (if (pair? a)
                     (if (pair? b)
                         (if (equal-rec (car a) (car b))
                             (equal-rec (cdr a) (cdr b))
                             #f)
                         #f)
                     (if (null? a)
                         (null? b)
                         (eqv? a b))))))

; equal-by: Check if two values are equal after applying function
(equal-by (fn (f a b) (equal? (f a) (f b))))

; member?: Check if element is in list (using equal?)
(member? (fn (x lst)
             (if (null? lst)
                 #f
                 (if (equal? x (car lst))
                     #t
                     (member? x (cdr lst))))))

; all-equal?: Check if all elements in list are equal
(all-equal? (fn (lst)
                (if (null? lst)
                    #t
                    (all (fn (x) (equal? x (car lst))) (cdr lst)))))
