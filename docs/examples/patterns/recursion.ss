;;; recursion.ss --- Recursive Thinking
;;;
;;; Recursion is the natural way to process recursive data structures.
;;;
;;; Run with: scheme --script docs/examples/patterns/recursion.ss

;;; ============================================================
;;; The Classic: Factorial
;;; ============================================================

(display "=== Factorial ===\n")

;;; factorial(n) = n * factorial(n-1), factorial(0) = 1
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(format #t "factorial(0) = ~a~%" (factorial 0))
(format #t "factorial(5) = ~a~%" (factorial 5))
(format #t "factorial(10) = ~a~%" (factorial 10))

;;; Trace the recursion:
;;;   factorial(4)
;;;   = 4 * factorial(3)
;;;   = 4 * 3 * factorial(2)
;;;   = 4 * 3 * 2 * factorial(1)
;;;   = 4 * 3 * 2 * 1
;;;   = 24

;;; ============================================================
;;; Fibonacci
;;; ============================================================

(display "\n=== Fibonacci ===\n")

;;; The naive recursive definition (exponential time!)
(define (fib-naive n)
  (cond
    [(= n 0) 0]
    [(= n 1) 1]
    [else (+ (fib-naive (- n 1))
             (fib-naive (- n 2)))]))

(format #t "fib(0..10): ")
(let loop ([i 0])
  (when (<= i 10)
    (format #t "~a " (fib-naive i))
    (loop (+ i 1))))
(newline)

;;; Efficient version with accumulator (linear time)
(define (fib n)
  (let loop ([i n] [a 0] [b 1])
    (if (= i 0)
        a
        (loop (- i 1) b (+ a b)))))

(format #t "fib(50) = ~a~%" (fib 50))

;;; ============================================================
;;; List Recursion
;;; ============================================================

(display "\n=== List Recursion ===\n")

;;; Sum of a list
(define (sum-list lst)
  (if (null? lst)
      0
      (+ (car lst) (sum-list (cdr lst)))))

(format #t "sum (1 2 3 4 5) = ~a~%" (sum-list '(1 2 3 4 5)))

;;; Length of a list
(define (my-length lst)
  (if (null? lst)
      0
      (+ 1 (my-length (cdr lst)))))

(format #t "length of (a b c d) = ~a~%" (my-length '(a b c d)))

;;; Reverse a list
(define (my-reverse lst)
  (let loop ([remaining lst] [acc '()])
    (if (null? remaining)
        acc
        (loop (cdr remaining) (cons (car remaining) acc)))))

(format #t "reverse (1 2 3) = ~a~%" (my-reverse '(1 2 3)))

;;; ============================================================
;;; Tree Recursion
;;; ============================================================

(display "\n=== Tree Recursion ===\n")

;;; A tree is either a leaf (atom) or a list of subtrees
(define tree '(1 (2 (3 4)) (5 6 (7))))

(format #t "tree: ~a~%~%" tree)

;;; Count all leaves in a tree
(define (count-leaves tree)
  (cond
    [(null? tree) 0]
    [(not (pair? tree)) 1]  ; leaf
    [else (+ (count-leaves (car tree))
             (count-leaves (cdr tree)))]))

(format #t "leaves in tree: ~a~%" (count-leaves tree))

;;; Sum all numbers in a tree
(define (tree-sum tree)
  (cond
    [(null? tree) 0]
    [(number? tree) tree]
    [(pair? tree) (+ (tree-sum (car tree))
                     (tree-sum (cdr tree)))]
    [else 0]))

(format #t "sum of tree: ~a~%" (tree-sum tree))

;;; Flatten a tree to a list
(define (flatten tree)
  (cond
    [(null? tree) '()]
    [(not (pair? tree)) (list tree)]
    [else (append (flatten (car tree))
                  (flatten (cdr tree)))]))

(format #t "flattened: ~a~%" (flatten tree))

;;; ============================================================
;;; Mutual Recursion
;;; ============================================================

(display "\n=== Mutual Recursion ===\n")

;;; even? and odd? defined in terms of each other
(define (my-even? n)
  (if (= n 0)
      #t
      (my-odd? (- n 1))))

(define (my-odd? n)
  (if (= n 0)
      #f
      (my-even? (- n 1))))

(format #t "even?(4) = ~a~%" (my-even? 4))
(format #t "odd?(4) = ~a~%" (my-odd? 4))
(format #t "even?(7) = ~a~%" (my-even? 7))

;;; ============================================================
;;; The Recursion Pattern
;;; ============================================================

(display "\n=== The Pattern ===\n")

(display "
Every recursive function has:

1. BASE CASE(S)
   - When to stop recursing
   - Returns a value directly
   - Examples: empty list, zero, leaf node

2. RECURSIVE CASE(S)
   - Break problem into smaller subproblems
   - Call self on subproblems
   - Combine results

3. PROGRESS TOWARD BASE
   - Each recursive call must get 'smaller'
   - Otherwise: infinite recursion
")

(display "\nDone!\n")
