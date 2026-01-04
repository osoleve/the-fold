;;; Test harness for normalize.ss and expand.ss

(load "core/blocks/normalize.ss")
(load "core/blocks/expand.ss")

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

;;; Test 9: Multi-argument functions
(display "
Test 9: Multi-argument functions
")
(test "two-arg function"
      '(fn (+ (dv 1) (dv 0)))
      (normalize '(fn (x y) (+ x y))))

(test "three-arg function"
      '(fn (* (dv 2) (+ (dv 1) (dv 0))))
      (normalize '(fn (x y z) (* x (+ y z)))))

(test "nested multi-arg"
      '(fn (fn ((dv 2) (dv 0))))
      (normalize '(fn (f x) (fn (y) (f y)))))

;;; Test 10: Multi-binding let
(display "
Test 10: Multi-binding let
")
(test "two-binding let"
      '(let (1) (let (2) (+ (dv 1) (dv 0))))
      (normalize '(let ((x 1) (y 2)) (+ x y))))

(test "three-binding let"
      '(let (1) (let (2) (let (3) (+ (dv 2) (+ (dv 1) (dv 0))))))
      (normalize '(let ((x 1) (y 2) (z 3)) (+ x (+ y z)))))

(test "let with parallel binding (x free in y's value)"
      '(let (a) (let ((+ x 1)) (+ (dv 1) (dv 0))))
      (normalize '(let ((x a) (y (+ x 1))) (+ x y))))

(display "
✓ All tests complete.
")
