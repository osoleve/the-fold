;;; core/linalg/test-iteration.ss --- Tests for Iteration Macros
;;;
;;; Verifies that iteration macros expand to equivalent code as hand-written loops.

(load "core/base/prelude.ss")
(load "lattice/linalg/iteration.ss")

;;; ====
;;; Test Framework
;;; ====

(define *test-count* 0)
(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expected actual)
  (set! *test-count* (+ *test-count* 1))
  (if (equal? expected actual)
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "  PASS ")
       (display name)
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

(define (test-vec name expected actual)
  (test name expected actual))

(define (run-tests)
  (display "
=== Iteration Macros Test Suite ===

")
  
  ;; vec-map-idx tests
  (display "--- vec-map-idx ---\n")
  
  (let ([v (vector 1 2 3 4 5)])
       (test-vec "vec-map-idx: add 1 to each"
                 (vector 2 3 4 5 6)
                 (vec-map-idx i v (+ (vector-ref v i) 1))))
  
  (let ([v (vector 0 1 2 3 4)])
       (test-vec "vec-map-idx: multiply by index"
                 (vector 0 1 4 9 16)
                 (vec-map-idx i v (* (vector-ref v i) i))))
  
  ;; vec-fold-idx tests
  (display "\n--- vec-fold-idx ---\n")
  
  (let ([v (vector 1 2 3 4 5)])
       (test "vec-fold-idx: sum elements"
             15
             (vec-fold-idx sum 0 i v (+ sum (vector-ref v i)))))
  
  (let ([v (vector 1 2 3 4 5)])
       (test "vec-fold-idx: weighted sum"
             40  ; 1*0 + 2*1 + 3*2 + 4*3 + 5*4 = 0 + 2 + 6 + 12 + 20 = 40
             (vec-fold-idx sum 0 i v (+ sum (* (vector-ref v i) i)))))
  
  ;; vec-fold tests
  (display "\n--- vec-fold ---\n")
  
  (let ([v (vector 1 2 3 4 5)])
       (test "vec-fold: sum"
             15
             (vec-fold sum 0 x v (+ sum x))))
  
  (let ([v (vector 1 2 3 4 5)])
       (test "vec-fold: product"
             120
             (vec-fold prod 1 x v (* prod x))))
  
  ;; vec-zip-map-idx tests
  (display "\n--- vec-zip-map-idx ---\n")
  
  (let ([v1 (vector 1 2 3)]
        [v2 (vector 10 20 30)])
       (test-vec "vec-zip-map-idx: add"
                 (vector 11 22 33)
                 (vec-zip-map-idx i v1 v2 (+ (vector-ref v1 i) (vector-ref v2 i)))))
  
  (let ([v1 (vector 1 2 3)]
        [v2 (vector 4 5 6)])
       (test-vec "vec-zip-map-idx: multiply"
                 (vector 4 10 18)
                 (vec-zip-map-idx i v1 v2 (* (vector-ref v1 i) (vector-ref v2 i)))))
  
  ;; vec-any? tests
  (display "\n--- vec-any? ---\n")
  
  (let ([v (vector 1 2 3 4 5)])
       (test "vec-any?: has element > 3"
             #t
             (vec-any? x v (> x 3))))
  
  (let ([v (vector 1 2 3)])
       (test "vec-any?: has element > 10"
             #f
             (vec-any? x v (> x 10))))
  
  ;; vec-all? tests
  (display "\n--- vec-all? ---\n")
  
  (let ([v (vector 1 2 3 4 5)])
       (test "vec-all?: all > 0"
             #t
             (vec-all? x v (> x 0))))
  
  (let ([v (vector 1 2 3 4 5)])
       (test "vec-all?: all > 2"
             #f
             (vec-all? x v (> x 2))))
  
  ;; range-fold tests
  (display "\n--- range-fold ---\n")
  
  (test "range-fold: sum 1..10"
        55
        (range-fold sum 0 i 1 11 (+ sum i)))
  
  (test "range-fold: factorial 5"
        120
        (range-fold prod 1 i 1 6 (* prod i)))
  
  ;; vec-fold-reverse tests
  (display "\n--- vec-fold-reverse ---\n")
  
  (let ([v (vector 1 2 3)])
       (test "vec-fold-reverse: build list"
             '(1 2 3)
             (vec-fold-reverse lst '() i v (cons (vector-ref v i) lst))))
  
  ;; matrix-do! tests
  (display "\n--- matrix-do! ---\n")
  
  (let ([data (make-vector 6 0)]
        [rows 2]
        [cols 3])
       (matrix-do! i j rows cols
                   (vector-set! data (+ (* i cols) j) (+ (* i 10) j)))
       (test-vec "matrix-do!: fill with i*10+j"
                 (vector 0 1 2 10 11 12)
                 data))
  
  ;; dot-product-loop tests
  (display "\n--- dot-product-loop ---\n")
  
  (let ([a (vector 1 2 3)]
        [b (vector 4 5 6)])
       (test "dot-product-loop: basic"
             32  ; 1*4 + 2*5 + 3*6 = 4 + 10 + 18 = 32
             (dot-product-loop k 3 (vector-ref a k) (vector-ref b k))))
  
  ;; Summary
  (display "
=== Summary ===
")
  (display "Total: ")
  (display *test-count*)
  (display ", Passed: ")
  (display *pass-count*)
  (display ", Failed: ")
  (display *fail-count*)
  (newline)
  
  (if (= *fail-count* 0)
      (display "All tests passed!\n")
      (display "SOME TESTS FAILED!\n")))

(run-tests)
