; ============================================================
; Core Higher-Order Functions
; Foundational building blocks used by all other modules
; ============================================================

; map: Apply function to each element of a list
(map (fix map
          (fn (f lst)
              (if (null? lst)
                  '()
                  (cons (f (car lst)) (map f (cdr lst)))))))

; filter: Keep elements satisfying predicate
(filter (fix filter
             (fn (p lst)
                 (if (null? lst)
                     '()
                     (if (p (car lst))
                         (cons (car lst) (filter p (cdr lst)))
                         (filter p (cdr lst)))))))

; foldl: Left fold (tail-recursive)
(foldl (fix foldl
            (fn (f acc lst)
                (if (null? lst)
                    acc
                    (foldl f (f acc (car lst)) (cdr lst))))))

; foldr: Right fold
(foldr (fix foldr
            (fn (f acc lst)
                (if (null? lst)
                    acc
                    (f (car lst) (foldr f acc (cdr lst)))))))

; any: Check if any element satisfies predicate
(any (fix any
          (fn (p lst)
              (if (null? lst)
                  #f
                  (if (p (car lst))
                      #t
                      (any p (cdr lst)))))))

; all: Check if all elements satisfy predicate
(all (fix all
          (fn (p lst)
              (if (null? lst)
                  #t
                  (if (p (car lst))
                      (all p (cdr lst))
                      #f)))))

; take-while: Take elements while predicate holds
(take-while (fix take-while
                 (fn (p lst)
                     (if (null? lst)
                         '()
                         (if (p (car lst))
                             (cons (car lst) (take-while p (cdr lst)))
                             '())))))

; drop-while: Drop elements while predicate holds
(drop-while (fix drop-while
                 (fn (p lst)
                     (if (null? lst)
                         '()
                         (if (p (car lst))
                             (drop-while p (cdr lst))
                             lst)))))

; zip-with: Combine two lists with a function
(zip-with (fix zip-with
               (fn (f lst1 lst2)
                   (if (or (null? lst1) (null? lst2))
                       '()
                       (cons (f (car lst1) (car lst2))
                             (zip-with f (cdr lst1) (cdr lst2)))))))
