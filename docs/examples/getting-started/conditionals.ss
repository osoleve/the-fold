;;; conditionals.ss --- Control Flow and Boolean Logic
;;;
;;; Making decisions in The Fold.
;;;
;;; Run with: scheme --script docs/examples/getting-started/conditionals.ss

;;; ====
;;; Boolean Values
;;; ====

(display "=== Boolean Values ===\n")

;;; #t is true, #f is false
(format #t "#t = ~a~%" #t)
(format #t "#f = ~a~%" #f)

;;; In Scheme, only #f is false; everything else is truthy
(format #t "0 is truthy: ~a~%" (if 0 "yes" "no"))
(format #t "\"\" is truthy: ~a~%" (if "" "yes" "no"))
(format #t "'() is truthy: ~a~%" (if '() "yes" "no"))

;;; ====
;;; Boolean Operations
;;; ====

(display "\n=== Boolean Operations ===\n")

(format #t "(and #t #t) = ~a~%" (and #t #t))
(format #t "(and #t #f) = ~a~%" (and #t #f))
(format #t "(or #f #t) = ~a~%" (or #f #t))
(format #t "(not #t) = ~a~%" (not #t))
(format #t "(not #f) = ~a~%" (not #f))

;;; and/or are short-circuiting and return the deciding value
(format #t "(and 1 2 3) = ~a~%" (and 1 2 3))      ; returns 3
(format #t "(or #f 'found) = ~a~%" (or #f 'found)) ; returns 'found

;;; ====
;;; if Expressions
;;; ====

(display "\n=== if Expressions ===\n")

;;; if is an expression that returns a value
(define (abs-value x)
  (if (< x 0)
      (- x)
      x))

(format #t "abs-value(-5) = ~a~%" (abs-value -5))
(format #t "abs-value(5) = ~a~%" (abs-value 5))

;;; Nested if
(define (sign x)
  (if (< x 0)
      'negative
      (if (> x 0)
          'positive
          'zero)))

(format #t "sign(-10) = ~a~%" (sign -10))
(format #t "sign(10) = ~a~%" (sign 10))
(format #t "sign(0) = ~a~%" (sign 0))

;;; ====
;;; cond Expressions
;;; ====

(display "\n=== cond (Multi-way Branch) ===\n")

;;; cond is cleaner for multiple conditions
(define (grade score)
  (cond
    [(>= score 90) 'A]
    [(>= score 80) 'B]
    [(>= score 70) 'C]
    [(>= score 60) 'D]
    [else 'F]))

(format #t "grade(95) = ~a~%" (grade 95))
(format #t "grade(82) = ~a~%" (grade 82))
(format #t "grade(55) = ~a~%" (grade 55))

;;; ====
;;; case Expressions
;;; ====

(display "\n=== case (Value Matching) ===\n")

;;; case matches against specific values
(define (day-type day)
  (case day
    [(saturday sunday) 'weekend]
    [(monday tuesday wednesday thursday friday) 'weekday]
    [else 'unknown]))

(format #t "day-type('saturday) = ~a~%" (day-type 'saturday))
(format #t "day-type('wednesday) = ~a~%" (day-type 'wednesday))

;;; ====
;;; when and unless
;;; ====

(display "\n=== when and unless ===\n")

;;; when executes body only if condition is true
(when (> 5 3)
  (display "5 is greater than 3\n"))

;;; unless executes body only if condition is false
(unless (< 5 3)
  (display "5 is not less than 3\n"))

;;; ====
;;; Practical Example: FizzBuzz
;;; ====

(display "\n=== FizzBuzz (1-15) ===\n")

(define (fizzbuzz n)
  (cond
    [(= (modulo n 15) 0) "FizzBuzz"]
    [(= (modulo n 3) 0) "Fizz"]
    [(= (modulo n 5) 0) "Buzz"]
    [else n]))

(let loop ([i 1])
  (when (<= i 15)
    (format #t "~a " (fizzbuzz i))
    (loop (+ i 1))))
(newline)

(display "\nDone!\n")
