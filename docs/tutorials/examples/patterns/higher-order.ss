;;; higher-order.ss --- Functions That Take or Return Functions
;;;
;;; Higher-order functions are the power tools of functional programming.
;;;
;;; Run with: scheme --script docs/tutorials/examples/patterns/higher-order.ss

;;; ====
;;; map - Transform Every Element
;;; ====

(display "=== map ===\n")

;;; map applies a function to each element
(define nums '(1 2 3 4 5))

(format #t "nums: ~a~%" nums)
(format #t "squared: ~a~%" (map (lambda (x) (* x x)) nums))
(format #t "doubled: ~a~%" (map (lambda (x) (* x 2)) nums))
(format #t "as strings: ~a~%" (map number->string nums))

;;; map with multiple lists (parallel iteration)
(format #t "sum pairs: ~a~%" (map + '(1 2 3) '(10 20 30)))
(format #t "products: ~a~%" (map * '(1 2 3) '(4 5 6)))

;;; ====
;;; filter - Keep Matching Elements
;;; ====

(display "\n=== filter ===\n")

(define mixed '(1 -2 3 -4 5 -6 7))
(format #t "mixed: ~a~%" mixed)
(format #t "positives: ~a~%" (filter positive? mixed))
(format #t "negatives: ~a~%" (filter negative? mixed))
(format #t "evens: ~a~%" (filter even? mixed))

;;; Custom predicates
(define (big? x) (> (abs x) 3))
(format #t "abs > 3: ~a~%" (filter big? mixed))

;;; ====
;;; fold - Reduce to a Single Value
;;; ====

(display "\n=== fold ===\n")

;;; fold-left: (((init op e1) op e2) op e3) ...
(format #t "sum: ~a~%" (fold-left + 0 nums))
(format #t "product: ~a~%" (fold-left * 1 nums))
(format #t "max: ~a~%" (fold-left max -inf.0 nums))

;;; Building collections
(format #t "reversed: ~a~%" (fold-left (lambda (acc x) (cons x acc)) '() nums))

;;; fold-right: e1 op (e2 op (e3 op ... init))
(format #t "fold-right cons: ~a~%" (fold-right cons '() nums))

;;; ====
;;; compose - Chain Functions
;;; ====

(display "\n=== compose ===\n")

;;; compose creates a function pipeline
(define (compose . fns)
  (lambda (x)
    (fold-right (lambda (f acc) (f acc)) x fns)))

(define add1 (lambda (x) (+ x 1)))
(define double (lambda (x) (* x 2)))
(define square (lambda (x) (* x x)))

;;; (compose f g h) means f(g(h(x)))
(define pipeline (compose add1 double square))

(format #t "square then double then add1:~%")
(format #t "  pipeline(3) = add1(double(square(3))) = add1(double(9)) = add1(18) = ~a~%" (pipeline 3))

;;; ====
;;; curry - Partial Application
;;; ====

(display "\n=== Partial Application ===\n")

;;; A function that returns a function
(define (add n)
  (lambda (x) (+ n x)))

(define add5 (add 5))
(define add10 (add 10))

(format #t "add5(3) = ~a~%" (add5 3))
(format #t "add10(3) = ~a~%" (add10 3))
(format #t "map add5 over (1 2 3): ~a~%" (map add5 '(1 2 3)))

;;; General curry for two arguments
(define (curry2 f)
  (lambda (x)
    (lambda (y)
      (f x y))))

(define curried-* (curry2 *))
(define times3 ((curry2 *) 3))
(format #t "times3(7) = ~a~%" (times3 7))

;;; ====
;;; Practical Combinations
;;; ====

(display "\n=== Combining Patterns ===\n")

(define data '(1 -2 3 -4 5 -6 7 -8 9 -10))
(format #t "data: ~a~%~%" data)

;;; Sum of squares of positive numbers
(define result
  (fold-left +
             0
             (map square
                  (filter positive? data))))

(format #t "sum of squares of positives: ~a~%" result)

;;; Count negatives
(format #t "count of negatives: ~a~%"
        (length (filter negative? data)))

;;; Average of absolutes
(let* ([absolutes (map abs data)]
       [sum (fold-left + 0 absolutes)]
       [count (length absolutes)])
  (format #t "average of absolutes: ~a~%" (/ sum count)))

;;; ====
;;; for-each - Side Effects
;;; ====

(display "\n=== for-each (Side Effects) ===\n")

;;; for-each is like map but doesn't collect results
(display "printing each: ")
(for-each (lambda (x) (format #t "~a " x)) '(a b c d e))
(newline)

;;; ====
;;; Building Your Own
;;; ====

(display "\n=== Building Your Own ===\n")

;;; my-map implementation
(define (my-map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst))
            (my-map f (cdr lst)))))

;;; my-filter implementation
(define (my-filter pred lst)
  (cond
    [(null? lst) '()]
    [(pred (car lst))
     (cons (car lst) (my-filter pred (cdr lst)))]
    [else (my-filter pred (cdr lst))]))

;;; my-fold-left implementation
(define (my-fold-left f init lst)
  (if (null? lst)
      init
      (my-fold-left f (f init (car lst)) (cdr lst))))

(format #t "my-map square: ~a~%" (my-map square '(1 2 3)))
(format #t "my-filter even: ~a~%" (my-filter even? '(1 2 3 4 5 6)))
(format #t "my-fold-left +: ~a~%" (my-fold-left + 0 '(1 2 3 4 5)))

(display "\nDone!\n")
