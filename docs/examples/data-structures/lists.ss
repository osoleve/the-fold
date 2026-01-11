;;; lists.ss --- List Construction and Manipulation
;;;
;;; Lists are the fundamental data structure in Scheme/The Fold.
;;;
;;; Run with: scheme --script docs/examples/data-structures/lists.ss

;;; ============================================================
;;; Creating Lists
;;; ============================================================

(display "=== Creating Lists ===\n")

;;; Quote syntax - the most common way
(define colors '(red green blue))
(format #t "colors = ~a~%" colors)

;;; list function - when you need to evaluate arguments
(define nums (list 1 2 (+ 1 2)))
(format #t "nums = ~a~%" nums)

;;; cons builds lists from head and tail
(define more-colors (cons 'yellow colors))
(format #t "more-colors = ~a~%" more-colors)

;;; The empty list
(format #t "empty list: ~a~%" '())

;;; ============================================================
;;; Accessing Elements
;;; ============================================================

(display "\n=== Accessing Elements ===\n")

;;; car gets the first element (head)
(format #t "car of ~a = ~a~%" colors (car colors))

;;; cdr gets everything except the first (tail)
(format #t "cdr of ~a = ~a~%" colors (cdr colors))

;;; Combinations: cadr, caddr, etc.
(format #t "cadr (second) of ~a = ~a~%" colors (cadr colors))
(format #t "caddr (third) of ~a = ~a~%" colors (caddr colors))

;;; list-ref for arbitrary index (0-based)
(format #t "list-ref 1 of ~a = ~a~%" colors (list-ref colors 1))

;;; ============================================================
;;; List Properties
;;; ============================================================

(display "\n=== List Properties ===\n")

(format #t "length of ~a = ~a~%" colors (length colors))
(format #t "null? '() = ~a~%" (null? '()))
(format #t "null? ~a = ~a~%" colors (null? colors))
(format #t "list? ~a = ~a~%" colors (list? colors))
(format #t "pair? ~a = ~a~%" colors (pair? colors))

;;; ============================================================
;;; Transforming Lists
;;; ============================================================

(display "\n=== Transforming Lists ===\n")

;;; reverse
(format #t "reverse ~a = ~a~%" colors (reverse colors))

;;; append joins lists
(format #t "append ~a and ~a = ~a~%"
        '(1 2) '(3 4) (append '(1 2) '(3 4)))

;;; map applies a function to each element
(define squares (map (lambda (x) (* x x)) '(1 2 3 4 5)))
(format #t "squares of (1 2 3 4 5) = ~a~%" squares)

;;; filter keeps elements matching a predicate
(define evens (filter even? '(1 2 3 4 5 6)))
(format #t "evens from (1 2 3 4 5 6) = ~a~%" evens)

;;; ============================================================
;;; Folding (Reducing)
;;; ============================================================

(display "\n=== Folding ===\n")

;;; fold-left accumulates left-to-right
(define sum (fold-left + 0 '(1 2 3 4 5)))
(format #t "sum of (1 2 3 4 5) = ~a~%" sum)

(define product (fold-left * 1 '(1 2 3 4 5)))
(format #t "product of (1 2 3 4 5) = ~a~%" product)

;;; Building a string
(define greeting
  (fold-left string-append "" '("Hello" " " "World" "!")))
(format #t "concatenated: ~a~%" greeting)

;;; ============================================================
;;; Searching
;;; ============================================================

(display "\n=== Searching ===\n")

;;; member checks if element exists
(format #t "member 'green in ~a = ~a~%" colors (member 'green colors))
(format #t "member 'purple in ~a = ~a~%" colors (member 'purple colors))

;;; find returns first match or #f
(format #t "find even in (1 3 4 5) = ~a~%" (find even? '(1 3 4 5)))

;;; assoc for association lists
(define ages '((alice . 30) (bob . 25) (carol . 35)))
(format #t "assoc 'bob in ~a = ~a~%" ages (assoc 'bob ages))

;;; ============================================================
;;; Building Lists Recursively
;;; ============================================================

(display "\n=== Recursive List Building ===\n")

;;; iota-like: build list from 0 to n-1
(define (range n)
  (let loop ([i 0] [acc '()])
    (if (>= i n)
        (reverse acc)
        (loop (+ i 1) (cons i acc)))))

(format #t "range(5) = ~a~%" (range 5))

(display "\nDone!\n")
