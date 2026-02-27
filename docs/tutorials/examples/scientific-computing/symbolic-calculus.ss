;;; docs/tutorials/examples/scientific-computing/symbolic-calculus.ss
;;;
;;; Demonstrates symbolic computation: differentiation, simplification,
;;; and integration working together for calculus tasks.
;;;
;;; Run: scheme --script docs/tutorials/examples/scientific-computing/symbolic-calculus.ss

(load "lattice/symbolic/integrate.ss")

(display "=== Symbolic Calculus Examples ===\n\n")

;;; ====
;;; Example 1: Derivative Tables
;;; ====

(display "--- Example 1: Building a Derivative Table ---\n\n")

(define functions
  (list (cons "x^n" (power (var 'x) (var 'n)))
        (cons "sin(x)" (sym-sin (var 'x)))
        (cons "cos(x)" (sym-cos (var 'x)))
        (cons "e^x" (sym-exp (var 'x)))
        (cons "ln(x)" (sym-log (var 'x)))
        (cons "x*sin(x)" (product (var 'x) (sym-sin (var 'x))))))

(for-each
 (lambda (pair)
         (let* ([name (car pair)]
                [f (cdr pair)]
                [df (simplify (deriv f 'x))])
               (display (format "d/dx[~a] = ~a\n" name (expr->string df)))))
 functions)

(newline)

;;; ====
;;; Example 2: Integration and Verification
;;; ====

(display "--- Example 2: Integration with Verification ---\n\n")

(define (verify-integral f var-sym)
  (let* ([F (integrate f var-sym)]
         [verified (and F
                        (let* ([dF (deriv F var-sym)]
                               [simplified (simplify dF)])
                              (expr=? simplified f)))])
        (display (format "integral of ~a:\n" (expr->string f)))
        (if F
            (begin
             (display (format "  F(x) = ~a\n" (expr->string F)))
             (display (format "  Verified: ~a\n\n" (if verified "YES" "check simplification"))))
            (display "  Could not integrate\n\n"))))

(verify-integral (power (var 'x) (num 3)) 'x)
(verify-integral (sym-sin (var 'x)) 'x)
(verify-integral (sym-exp (var 'x)) 'x)
(verify-integral (quotient (num 1) (var 'x)) 'x)

;;; ====
;;; Example 3: Simplification Showcase
;;; ====

(display "--- Example 3: Algebraic Simplification ---\n\n")

(define simplification-examples
  (list
   (cons "x + x + x"
         (sum (var 'x) (sum (var 'x) (var 'x))))
   (cons "x * x * x"
         (product (var 'x) (product (var 'x) (var 'x))))
   (cons "sin^2(x) + cos^2(x)"
         (sum (power (sym-sin (var 'x)) (num 2))
              (power (sym-cos (var 'x)) (num 2))))
   (cons "exp(log(x))"
         (sym-exp (sym-log (var 'x))))
   (cons "(x^2)^3"
         (power (power (var 'x) (num 2)) (num 3)))))

(for-each
 (lambda (pair)
         (let* ([name (car pair)]
                [expr (cdr pair)]
                [simplified (full-simplify expr)])
               (display (format "~a => ~a\n" name (expr->string simplified)))))
 simplification-examples)

(newline)

;;; ====
;;; Example 4: Definite Integrals
;;; ====

(display "--- Example 4: Definite Integrals ---\n\n")

;; Area under x^2 from 0 to 1
(let ([result (definite-integral (power (var 'x) (num 2)) 'x (num 0) (num 1))])
     (display (format "integral[0,1] x^2 dx = ~a\n"
                      (if (num? result)
                          (num-val result)
                          (expr->string result)))))

;; Area under sin(x) from 0 to pi (using symbolic pi)
(let ([result (definite-integral (sym-sin (var 'x)) 'x (num 0) (var 'pi))])
     (display (format "integral[0,pi] sin(x) dx = ~a\n"
                      (expr->string result))))

;; Parametric: integral of x from a to b
(let ([result (definite-integral (var 'x) 'x (var 'a) (var 'b))])
     (display (format "integral[a,b] x dx = ~a\n"
                      (expr->string (simplify result)))))

(newline)

;;; ====
;;; Example 5: Gradient Computation
;;; ====

(display "--- Example 5: Gradient and Hessian ---\n\n")

;; f(x,y) = x^2 + 2xy + y^2 = (x+y)^2
(define f
  (sum (power (var 'x) (num 2))
       (sum (product (num 2) (product (var 'x) (var 'y)))
            (power (var 'y) (num 2)))))

(display (format "f(x,y) = ~a\n" (expr->string f)))

;; Gradient
(let ([grad (map simplify (gradient f '(x y)))])
     (display "Gradient:\n")
     (display (format "  df/dx = ~a\n" (expr->string (car grad))))
     (display (format "  df/dy = ~a\n" (expr->string (cadr grad)))))

;; Hessian
(let ([hess (hessian f '(x y))])
     (display "Hessian:\n")
     (for-each
      (lambda (row)
              (display "  ")
              (for-each
               (lambda (entry)
                       (display (format "~a  " (expr->string (simplify entry)))))
               row)
              (newline))
      hess))

(newline)

;;; ====
;;; Example 6: Chain Rule in Action
;;; ====

(display "--- Example 6: Chain Rule ---\n\n")

;; Compose functions and differentiate
(define (compose-and-diff outer inner var-sym)
  (let* ([composed (subst outer 'u inner)]
         [d-composed (simplify (deriv composed var-sym))])
        (display (format "f(u) = ~a, u = ~a\n"
                         (expr->string outer)
                         (expr->string inner)))
        (display (format "d/d~a[f(u)] = ~a\n\n" var-sym (expr->string d-composed)))))

;; d/dx[sin(x^2)]
(compose-and-diff (sym-sin (var 'u))
                  (power (var 'x) (num 2))
                  'x)

;; d/dx[e^(sin(x))]
(compose-and-diff (sym-exp (var 'u))
                  (sym-sin (var 'x))
                  'x)

;; d/dx[log(1 + x^2)]
(compose-and-diff (sym-log (var 'u))
                  (sum (num 1) (power (var 'x) (num 2)))
                  'x)

;;; ====
;;; Example 7: Product Expansion
;;; ====

(display "--- Example 7: Product Expansion ---\n\n")

;; (x + 1)^2
(let* ([expr (power (sum (var 'x) (num 1)) (num 2))]
       [expanded (expand expr)]
       [simplified (simplify expanded)])
      (display (format "(x + 1)^2 = ~a\n" (expr->string simplified))))

;; (a + b)(c + d)
(let* ([expr (product (sum (var 'a) (var 'b))
                      (sum (var 'c) (var 'd)))]
       [expanded (expand expr)])
      (display (format "(a+b)(c+d) = ~a\n" (expr->string expanded))))

(newline)
(display "=== Examples Complete ===\n")
