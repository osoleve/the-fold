; list-extra.ss - Additional list utilities
; Part of The Fold prelude

; ============================================
; Scanning and reduction
; ============================================

; scanl: Left-to-right scan (like foldl but keeps intermediate results)
; (scanl + 0 '(1 2 3 4)) => (0 1 3 6 10)
(define (scanl f init lst)
  (if (null? lst)
      (list init)
      (cons init (scanl f (f init (car lst)) (cdr lst)))))

; scanr: Right-to-left scan
; (scanr + 0 '(1 2 3 4)) => (10 9 7 4 0)
(define (scanr f init lst)
  (if (null? lst)
      (list init)
      (let ((rest (scanr f init (cdr lst))))
        (cons (f (car lst) (car rest)) rest))))

; reduce: Like foldl but uses first element as init
; (reduce + '(1 2 3 4)) => 10
(define (reduce f lst)
  (if (null? lst)
      '()
      (foldl f (car lst) (cdr lst))))

; reduce-right: Like foldr but uses last element as init
(define (reduce-right f lst)
  (if (null? lst)
      '()
      (foldr f (last lst) (init lst))))

; ============================================
; Searching and indexing
; ============================================

; member: Find element in list (returns tail or #f)
; (member 3 '(1 2 3 4)) => (3 4)
(define (member x lst)
  (if (null? lst)
      #f
      (if (equal? x (car lst))
          lst
          (member x (cdr lst)))))

; memq: Like member but uses eq?
(define (memq x lst)
  (if (null? lst)
      #f
      (if (eq? x (car lst))
          lst
          (memq x (cdr lst)))))

; member?: Boolean membership check
(define (member? x lst)
  (if (member x lst) #t #f))

; position: Find index of element (or #f)
; (position 3 '(1 2 3 4)) => 2
(define (position x lst)
  (let ((helper (fn (lst idx)
                  (if (null? lst)
                      #f
                      (if (equal? x (car lst))
                          idx
                          (helper (cdr lst) (+ idx 1)))))))
    (helper lst 0)))

; list-index: Find index where predicate holds
; (list-index even? '(1 3 4 5)) => 2
(define (list-index f lst)
  (let ((helper (fn (lst idx)
                  (if (null? lst)
                      #f
                      (if (f (car lst))
                          idx
                          (helper (cdr lst) (+ idx 1)))))))
    (helper lst 0)))

; find-last: Find last element satisfying predicate
; (find-last odd? '(1 2 3 4 5)) => 5
(define (find-last p lst)
  (if (null? lst)
      #f
      (let ((rest (find-last p (cdr lst))))
        (if rest
            rest
            (if (p (car lst)) (car lst) #f)))))

; ============================================
; Partitioning and splitting
; ============================================

; partition-all: Split list into chunks of n
; (partition-all 2 '(1 2 3 4 5)) => ((1 2) (3 4) (5))
(define (partition-all n lst)
  (if (null? lst)
      '()
      (cons (take n lst)
            (partition-all n (drop n lst)))))

; chunks: Alias for partition-all
(define chunks partition-all)

; split-at-pred: Split list at first element satisfying predicate
; (split-at-pred even? '(1 3 4 5)) => ((1 3) (4 5))
(define (split-at-pred p lst)
  (let ((split-helper (fn (remaining before)
                        (if (null? remaining)
                            (list (reverse before) '())
                            (if (p (car remaining))
                                (list (reverse before) remaining)
                                (split-helper (cdr remaining) (cons (car remaining) before)))))))
    (split-helper lst '())))

; ============================================
; Zipping utilities
; ============================================

; zip3: Zip three lists together
; (zip3 '(1 2) '(a b) '(x y)) => ((1 a x) (2 b y))
(define (zip3 xs ys zs)
  (if (null? xs)
      '()
      (if (null? ys)
          '()
          (if (null? zs)
              '()
              (cons (list (car xs) (car ys) (car zs))
                    (zip3 (cdr xs) (cdr ys) (cdr zs)))))))

; zip-with-index: Pair each element with its index
; (zip-with-index '(a b c)) => ((0 a) (1 b) (2 c))
(define (zip-with-index lst)
  (zip (range 0 (length lst)) lst))

; enumerate: Alias for zip-with-index
(define enumerate zip-with-index)

; ============================================
; Module exports
; ============================================

(module-exports
  scanl
  scanr
  reduce
  reduce-right
  member
  memq
  member?
  position
  list-index
  find-last
  partition-all
  chunks
  split-at-pred
  zip3
  zip-with-index
  enumerate)
