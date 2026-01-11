;;; tail-recursion.ss --- Efficient Iteration via Tail Calls
;;;
;;; Tail recursion = recursion that doesn't grow the stack.
;;; Scheme guarantees tail call optimization (TCO).
;;;
;;; Run with: scheme --script docs/examples/patterns/tail-recursion.ss

;;; ============================================================
;;; The Problem with Naive Recursion
;;; ============================================================

(display "=== Stack Growth Problem ===\n")

;;; Non-tail recursive factorial
(define (factorial-bad n)
  (if (<= n 1)
      1
      (* n (factorial-bad (- n 1)))))  ; multiplication AFTER the call

;;; Each call must wait for the recursive call to return:
;;;   factorial(4)
;;;   4 * factorial(3)          <- waiting
;;;   4 * (3 * factorial(2))    <- waiting
;;;   4 * (3 * (2 * factorial(1)))
;;;   4 * (3 * (2 * 1))
;;;   4 * (3 * 2)
;;;   4 * 6
;;;   24

(format #t "factorial-bad(10) = ~a~%" (factorial-bad 10))
(display "Works, but builds up stack frames.\n")

;;; ============================================================
;;; Tail Recursion Solution
;;; ============================================================

(display "\n=== Tail Recursive Factorial ===\n")

;;; Tail recursive: the recursive call IS the return value
(define (factorial n)
  (let loop ([i n] [acc 1])
    (if (<= i 1)
        acc
        (loop (- i 1) (* acc i)))))  ; loop is in tail position

;;; The accumulator carries the computation:
;;;   loop(4, 1)
;;;   loop(3, 4)     <- no waiting, just jump
;;;   loop(2, 12)    <- no waiting, just jump
;;;   loop(1, 24)    <- no waiting, just jump
;;;   24

(format #t "factorial(10) = ~a~%" (factorial 10))
(format #t "factorial(100) = ~a~%" (factorial 100))
(display "No stack growth - can handle huge inputs.\n")

;;; ============================================================
;;; The named let Pattern
;;; ============================================================

(display "\n=== Named let Pattern ===\n")

;;; (let name ([var init] ...) body)
;;; Creates a local function 'name' that can be called recursively

;;; Sum from 1 to n
(define (sum-to n)
  (let loop ([i 1] [acc 0])
    (if (> i n)
        acc
        (loop (+ i 1) (+ acc i)))))

(format #t "sum 1 to 100 = ~a~%" (sum-to 100))

;;; Build a list of squares
(define (squares-up-to n)
  (let loop ([i n] [acc '()])
    (if (< i 1)
        acc
        (loop (- i 1) (cons (* i i) acc)))))

(format #t "squares 1-5 = ~a~%" (squares-up-to 5))

;;; ============================================================
;;; Converting Non-Tail to Tail Recursive
;;; ============================================================

(display "\n=== Conversion Recipe ===\n")

(display "
To convert non-tail recursive to tail recursive:

1. Identify what 'waits' after the recursive call
2. Add an accumulator parameter for that computation
3. Move the computation INTO the recursive call
4. Return the accumulator in the base case

")

;;; Example: reverse
;;; Non-tail: builds result on the way back
(define (reverse-bad lst)
  (if (null? lst)
      '()
      (append (reverse-bad (cdr lst))
              (list (car lst)))))  ; append AFTER call

;;; Tail recursive: builds result as we go
(define (reverse-good lst)
  (let loop ([remaining lst] [acc '()])
    (if (null? remaining)
        acc
        (loop (cdr remaining)
              (cons (car remaining) acc)))))  ; cons BEFORE call

(format #t "reverse-good (1 2 3 4 5) = ~a~%" (reverse-good '(1 2 3 4 5)))

;;; ============================================================
;;; Multiple Accumulators
;;; ============================================================

(display "\n=== Multiple Accumulators ===\n")

;;; Fibonacci with two accumulators
(define (fib n)
  (let loop ([i n] [a 0] [b 1])
    (if (= i 0)
        a
        (loop (- i 1) b (+ a b)))))

(format #t "fib(10) = ~a~%" (fib 10))
(format #t "fib(50) = ~a~%" (fib 50))

;;; Compute both sum and product in one pass
(define (sum-and-product lst)
  (let loop ([remaining lst] [sum 0] [prod 1])
    (if (null? remaining)
        (cons sum prod)
        (let ([x (car remaining)])
          (loop (cdr remaining)
                (+ sum x)
                (* prod x))))))

(format #t "sum-and-product (1 2 3 4) = ~a~%" (sum-and-product '(1 2 3 4)))

;;; ============================================================
;;; Tail Recursion = Iteration
;;; ============================================================

(display "\n=== Tail Recursion = Iteration ===\n")

(display "
In Scheme, tail recursion IS the loop construct.
There's no need for while/for loops because:

  (let loop ([i 0])           ; 'loop' is like a goto label
    (when (< i 10)
      (display i)
      (loop (+ i 1))))        ; tail call = jump to label

The compiler transforms this into a simple jump.
No stack frames, no overhead.
")

;;; Classic 'for loop' pattern
(display "Counting 0-9: ")
(let loop ([i 0])
  (when (< i 10)
    (format #t "~a " i)
    (loop (+ i 1))))
(newline)

;;; While-loop pattern
(display "Powers of 2 under 1000: ")
(let loop ([n 1])
  (when (< n 1000)
    (format #t "~a " n)
    (loop (* n 2))))
(newline)

;;; ============================================================
;;; Performance
;;; ============================================================

(display "\n=== Performance ===\n")

;;; This would overflow the stack without TCO:
(define (count-down n)
  (let loop ([i n])
    (if (= i 0)
        'done
        (loop (- i 1)))))

(format #t "count-down from 1,000,000: ~a~%" (count-down 1000000))
(display "No stack overflow - TCO in action!\n")

(display "\nDone!\n")
