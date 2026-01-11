;;; functions.ss --- Defining and Calling Functions
;;;
;;; Functions are the heart of functional programming.
;;;
;;; Run with: scheme --script docs/examples/getting-started/functions.ss

;;; ============================================================
;;; Basic Function Definition
;;; ============================================================

(display "=== Basic Functions ===\n")

;;; define creates a named function
(define (square x)
  (* x x))

(format #t "square(5) = ~a~%" (square 5))
(format #t "square(12) = ~a~%" (square 12))

;;; Multiple parameters
(define (add-three a b c)
  (+ a b c))

(format #t "add-three(1, 2, 3) = ~a~%" (add-three 1 2 3))

;;; ============================================================
;;; Lambda Expressions
;;; ============================================================

(display "\n=== Lambda (Anonymous Functions) ===\n")

;;; lambda creates an anonymous function
(define cube (lambda (x) (* x x x)))

(format #t "cube(3) = ~a~%" (cube 3))

;;; Lambdas can be used inline
(format #t "((lambda (x) (+ x 10)) 5) = ~a~%" ((lambda (x) (+ x 10)) 5))

;;; ============================================================
;;; Functions Returning Functions
;;; ============================================================

(display "\n=== Higher-Order Functions ===\n")

;;; A function that returns a function
(define (make-adder n)
  (lambda (x) (+ x n)))

(define add-10 (make-adder 10))
(define add-100 (make-adder 100))

(format #t "add-10(5) = ~a~%" (add-10 5))
(format #t "add-100(5) = ~a~%" (add-100 5))

;;; ============================================================
;;; Functions as Arguments
;;; ============================================================

(display "\n=== Functions as Arguments ===\n")

;;; apply-twice takes a function and applies it twice
(define (apply-twice f x)
  (f (f x)))

(format #t "apply-twice(square, 2) = ~a~%" (apply-twice square 2))
;; square(square(2)) = square(4) = 16

(format #t "apply-twice(add-10, 0) = ~a~%" (apply-twice add-10 0))
;; add-10(add-10(0)) = add-10(10) = 20

;;; ============================================================
;;; Local Definitions with let
;;; ============================================================

(display "\n=== Local Definitions ===\n")

(define (circle-area radius)
  (let ([pi 3.14159])
    (* pi radius radius)))

(format #t "circle-area(5) = ~a~%" (circle-area 5))

;;; let* for sequential bindings
(define (quadratic a b c x)
  (let* ([x2 (* x x)]
         [ax2 (* a x2)]
         [bx (* b x)])
    (+ ax2 bx c)))

(format #t "quadratic(1, 2, 3, 4) = 1*16 + 2*4 + 3 = ~a~%" (quadratic 1 2 3 4))

;;; ============================================================
;;; Recursive Functions
;;; ============================================================

(display "\n=== Recursion ===\n")

(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(format #t "factorial(5) = ~a~%" (factorial 5))
(format #t "factorial(10) = ~a~%" (factorial 10))

(display "\nDone!\n")
