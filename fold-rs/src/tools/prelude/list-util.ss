; list-util.ss - List predicates and utility functions
; Part of The Fold prelude

; ============================================
; List predicates
; ============================================

; all-equal?: Check if all elements are equal
; (all-equal? '(1 1 1)) => #t
(define (all-equal? lst)
  (if (null? lst)
      #t
      (all (fn (x) (equal? x (car lst))) (cdr lst))))

; sorted?: Check if list is sorted (ascending)
; (sorted? '(1 2 3)) => #t
(define (sorted? lst)
  (if (null? lst)
      #t
      (if (null? (cdr lst))
          #t
          (if (<= (car lst) (cadr lst))
              (sorted? (cdr lst))
              #f))))

; sorted-by?: Check if list is sorted by key function
; (sorted-by? abs '(-1 -2 -3)) => #t
(define (sorted-by? key-fn lst)
  (sorted? (map key-fn lst)))

; palindrome?: Check if list is a palindrome
; (palindrome? '(1 2 1)) => #t
(define (palindrome? lst)
  (if (null? lst)
      #t
      (if (null? (cdr lst))
          #t
          (if (equal? (car lst) (last lst))
              (palindrome? (cdr (init lst)))
              #f))))

; ============================================
; List utilities
; ============================================

; interpose: Insert separator between elements
; (interpose 0 '(1 2 3)) => (1 0 2 0 3)
(define (interpose sep lst)
  (if (null? lst)
      '()
      (if (null? (cdr lst))
          lst
          (cons (car lst)
                (cons sep (interpose sep (cdr lst)))))))

; init: All but last element
; (init '(1 2 3)) => (1 2)
(define (init lst)
  (if (null? lst)
      '()
      (if (null? (cdr lst))
          '()
          (cons (car lst) (init (cdr lst))))))

; nth: Get nth element (0-indexed)
; (nth 2 '(a b c d)) => c
(define (nth n lst)
  (if (= n 0)
      (car lst)
      (nth (- n 1) (cdr lst))))

; ============================================
; Module exports
; ============================================

(module-exports
  all-equal?
  sorted?
  sorted-by?
  palindrome?
  interpose
  init
  nth)
