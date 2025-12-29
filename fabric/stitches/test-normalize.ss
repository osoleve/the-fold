;;; Test harness for normalize.ss and expand.ss

(load "fabric/stitches/normalize.ss")
(load "fabric/stitches/expand.ss")

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (display "✓")
      (begin
       (display "✗
    expected: ")
       (display expected)
       (display "
    got: ")
       (display actual)))
  (newline))

;;; Test 1: Identity function
(display "Test 1: Identity function
")
(test "normalize (fn (x) x)"
      '(fn (dv 0))
      (normalize '(fn (x) x)))

(test "normalize (fn (y) y)"
      '(fn (dv 0))
      (normalize '(fn (y) y)))

(test "same normalized form"
      (normalize '(fn (x) x))
      (normalize '(fn (y) y)))

;;; Test 2: Nested binders
(display "
Test 2: Nested binders
")
(test "outer variable"
      '(fn (fn (dv 1)))
      (normalize '(fn (x) (fn (y) x))))

(test "inner variable"
      '(fn (fn (dv 0)))
      (normalize '(fn (x) (fn (y) y))))

;;; Test 3: Let bindings
(display "
Test 3: Let bindings
")
(test "simple let"
      '(let (42) (dv 0))
      (normalize '(let ((x 42)) x)))

;;; Test 4: Application
(display "
Test 4: Application
")
(test "apply inner to outer"
      '(fn (fn ((dv 1) (dv 0))))
      (normalize '(fn (f) (fn (x) (f x)))))

;;; Test 5: Free variables preserved
(display "
Test 5: Free variables preserved
")
(test "free var"
      '(fn (+ (dv 0) 1))
      (normalize '(fn (x) (+ x 1))))

;;; Test 6: Expansion
(display "
Test 6: Expansion
")
(test "expand identity"
      '(fn (a) a)
      (expand '(fn (dv 0)) '(a)))

(test "expand nested"
      '(fn (a) (fn (b) a))
      (expand '(fn (fn (dv 1))) '(a b)))

;;; Test 7: Round-trip
(display "
Test 7: Round-trip
")
(define orig '(fn (f) (fn (x) (f (f x)))))
(define normalized (normalize orig))
(define expanded (expand normalized '(g y)))
(define renormalized (normalize expanded))
(test "normalize = renormalize"
      normalized
      renormalized)

;;; Test 8: Complex expression
(display "
Test 8: Complex expression
")
(define complex-expr
  '(let ((compose (fn (f) (fn (g) (fn (x) (f (g x)))))))
    (compose inc inc)))
(define norm-complex (normalize complex-expr))
(display "  normalized: ") (display norm-complex) (newline)

(display "
✓ All tests complete.
")
