;;; fabric/stitches/test-vec.ss — Tests for Vector Operations

(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")

;;; ============================================================
;;; Test Framework
;;; ============================================================

(define *test-count* 0)
(define *pass-count* 0)
(define *fail-count* 0)

(define (vec-equiv? a b)
  (cond
   [(and (vector? a) (vector? b)) (vec-equal? a b)]
   [else (equal? a b)]))

(define (test name expected actual)
  (set! *test-count* (+ *test-count* 1))
  (if (vec-equiv? expected actual)
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "  ")
       (display name)
       (display ": ")
       (write actual)
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "  FAIL ")
       (display name)
       (newline)
       (display "    Expected: ")
       (write expected)
       (newline)
       (display "    Actual:   ")
       (write actual)
       (newline))))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

(define (test-error name actual)
  (test name #t (and (pair? actual) (eq? (car actual) 'error))))

(define (run-tests)
  (display "
=== Vector Operations Test Suite ===

")
  
  ;; Construction
  (display "--- Vector Construction ---
")
  
  (test "make-vec creates vector"
        (vector 0 0 0 0 0)
        (make-vec 5 0))
  
  (test "make-vec with init"
        (vector 7 7 7)
        (make-vec 3 7))
  
  (test "vec-from-list"
        (vector 1 2 3 4 5)
        (vec-from-list '(1 2 3 4 5)))
  
  (test "vec variadic"
        (vector 1 2 3)
        (vec 1 2 3))
  
  (test "vec->list"
        '(1 2 3)
        (vec->list (vec 1 2 3)))
  
  ;; Accessors
  (display "
--- Vector Accessors ---
")
  
  (test "vec-ref"
        3
        (vec-ref (vec 1 2 3 4 5) 2))
  
  (test "vec-length"
        5
        (vec-length (vec 1 2 3 4 5)))
  
  (test-true "vec-empty? true"
             (vec-empty? (vec)))
  
  (test-false "vec-empty? false"
              (vec-empty? (vec 1)))
  
  (test "vec-first"
        1
        (vec-first (vec 1 2 3)))
  
  (test "vec-last"
        3
        (vec-last (vec 1 2 3)))
  
  (test-error "vec-ref out of bounds"
              (vec-ref (vec 1 2 3) 10))
  
  (test-error "vec-first empty"
              (vec-first (vec)))
  
  ;; Transformations
  (display "
--- Vector Transformations ---
")
  
  (test "vec-map"
        (vector 2 4 6)
        (vec-map (lambda (x) (* x 2)) (vec 1 2 3)))
  
  (test "vec-fold sum"
        15
        (vec-fold + 0 (vec 1 2 3 4 5)))
  
  (test "vec-fold-right"
        '(1 2 3)
        (vec-fold-right cons '() (vec 1 2 3)))
  
  (test "vec-zip-with"
        (vector 5 7 9)
        (vec-zip-with + (vec 1 2 3) (vec 4 5 6)))
  
  (test-error "vec-zip-with dimension mismatch"
              (vec-zip-with + (vec 1 2) (vec 1 2 3)))
  
  (test "vec-append"
        (vector 1 2 3 4 5 6)
        (vec-append (vec 1 2 3) (vec 4 5 6)))
  
  (test "vec-take"
        (vector 1 2)
        (vec-take (vec 1 2 3 4 5) 2))
  
  (test "vec-drop"
        (vector 3 4 5)
        (vec-drop (vec 1 2 3 4 5) 2))
  
  (test "vec-slice"
        (vector 2 3 4)
        (vec-slice (vec 1 2 3 4 5) 1 4))
  
  (test "vec-reverse"
        (vector 5 4 3 2 1)
        (vec-reverse (vec 1 2 3 4 5)))
  
  ;; Arithmetic
  (display "
--- Vector Arithmetic ---
")
  
  (test "vec-add"
        (vector 5 7 9)
        (vec-add (vec 1 2 3) (vec 4 5 6)))
  
  (test "vec-sub"
        (vector -3 -3 -3)
        (vec-sub (vec 1 2 3) (vec 4 5 6)))
  
  (test "vec-mul (Hadamard)"
        (vector 4 10 18)
        (vec-mul (vec 1 2 3) (vec 4 5 6)))
  
  (test "vec-scale"
        (vector 2 4 6)
        (vec-scale 2 (vec 1 2 3)))
  
  (test "vec-negate"
        (vector -1 -2 -3)
        (vec-negate (vec 1 2 3)))
  
  ;; Products and Norms
  (display "
--- Products and Norms ---
")
  
  (test "vec-dot"
        32
        (vec-dot (vec 1 2 3) (vec 4 5 6)))
  
  (test "vec-dot same vector"
        14
        (vec-dot (vec 1 2 3) (vec 1 2 3)))
  
  (test "vec-norm-squared"
        14
        (vec-norm-squared (vec 1 2 3)))
  
  (test "vec-norm unit"
        1
        (vec-norm (vec 1 0 0)))
  
  (test "vec-norm-l1"
        6
        (vec-norm-l1 (vec 1 -2 3)))
  
  (test "vec-norm-linf"
        5
        (vec-norm-linf (vec 1 -5 3)))
  
  (test-error "vec-norm-linf empty"
              (vec-norm-linf (vec)))
  
  (let ([normalized (vec-normalize (vec 3 4 0))])
       (test "vec-normalize preserves direction"
             #t
             (vec-approx-equal? normalized (vec 0.6 0.8 0.0))))
  
  (test-error "vec-normalize zero vector"
              (vec-normalize (vec 0 0 0)))
  
  (test "vec-distance"
        5
        (vec-distance (vec 0 0) (vec 3 4)))
  
  ;; Comparisons
  (display "
--- Vector Comparisons ---
")
  
  (test-true "vec-equal? same"
             (vec-equal? (vec 1 2 3) (vec 1 2 3)))
  
  (test-false "vec-equal? different"
              (vec-equal? (vec 1 2 3) (vec 1 2 4)))
  
  (test-false "vec-equal? different length"
              (vec-equal? (vec 1 2 3) (vec 1 2)))
  
  (test-true "vec-approx-equal?"
             (vec-approx-equal? (vec 1.0 2.0) (vec 1.00000000001 2.0)))
  
  (test-false "vec-approx-equal? fails"
              (vec-approx-equal? (vec 1.0 2.0) (vec 1.1 2.0)))
  
  ;; Utilities
  (display "
--- Vector Utilities ---
")
  
  (test "vec-sum"
        15
        (vec-sum (vec 1 2 3 4 5)))
  
  (test "vec-product"
        120
        (vec-product (vec 1 2 3 4 5)))
  
  (test "vec-mean"
        3
        (vec-mean (vec 1 2 3 4 5)))
  
  (test "vec-min"
        1
        (vec-min (vec 3 1 4 1 5)))
  
  (test "vec-max"
        5
        (vec-max (vec 3 1 4 1 5)))
  
  (test "vec-argmin"
        1
        (vec-argmin (vec 3 1 4 1 5)))
  
  (test "vec-argmax"
        4
        (vec-argmax (vec 3 1 4 1 5)))
  
  ;; Special Vectors
  (display "
--- Special Vectors ---
")
  
  (test "vec-zeros"
        (vector 0 0 0)
        (vec-zeros 3))
  
  (test "vec-ones"
        (vector 1 1 1)
        (vec-ones 3))
  
  (test "vec-range"
        (vector 0 1 2 3 4)
        (vec-range 5))
  
  (test "vec-linspace"
        (vector 0 1 2 3 4)
        (vec-linspace 0 4 5))
  
  (let ([ls (vec-linspace 0 1 5)])
       (test-true "vec-linspace equally spaced"
                  (vec-approx-equal? ls (vec 0 0.25 0.5 0.75 1.0))))
  
  (test "vec-unit"
        (vector 0 0 1 0)
        (vec-unit 4 2))
  
  ;; Summary
  (display "
=== Test Summary ===
")
  (display "Total: ")
  (display *test-count*)
  (display ", Passed: ")
  (display *pass-count*)
  (display ", Failed: ")
  (display *fail-count*)
  (newline)
  
  (if (= *fail-count* 0)
      (display "All tests passed!
")
      (display "Some tests failed.
"))
  
  (= *fail-count* 0))

;; Run tests
(run-tests)
