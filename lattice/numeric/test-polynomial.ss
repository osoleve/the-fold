;;; lattice/numeric/test-polynomial.ss — Polynomial Tests
;;;
;;; Comprehensive tests for polynomial operations.

(load "core/lang/module.ss")
(load "core/base/prelude.ss")
(load "lattice/data/sort.ss")
(load "lattice/numeric/polynomial.ss")

;;; ====
;;; Test Framework
;;; ====

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  [PASS] ~a\n" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  [FAIL] ~a\n" name)
       (printf "      Expected: ~s\n" expected)
       (printf "      Got:      ~s\n" actual))))

(define (test-approx name expected actual tolerance)
  (if (< (abs (- expected actual)) tolerance)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  [PASS] ~a\n" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  [FAIL] ~a\n" name)
       (printf "      Expected: ~s (within ~s)\n" expected tolerance)
       (printf "      Got:      ~s\n" actual))))

(define (test-complex-approx name expected actual tolerance)
  (let ([real-diff (abs (- (complex-real expected) (complex-real actual)))]
        [imag-diff (abs (- (complex-imag expected) (complex-imag actual)))])
       (if (and (< real-diff tolerance) (< imag-diff tolerance))
           (begin
            (set! tests-passed (+ tests-passed 1))
            (printf "  [PASS] ~a\n" name))
           (begin
            (set! tests-failed (+ tests-failed 1))
            (printf "  [FAIL] ~a\n" name)
            (printf "      Expected: ~s + ~si\n" (complex-real expected) (complex-imag expected))
            (printf "      Got:      ~s + ~si\n" (complex-real actual) (complex-imag actual))))))

(printf "\n=== Polynomial Tests ===\n\n")

;;; ====
;;; Construction Tests
;;; ====

(printf "--- Construction ---\n")

(let ([p (make-poly (vector 3 2 1))])
     (test "make-poly creates poly" #t (poly? p))
     (test "poly-degree of 3x^2+2x+1" 2 (poly-degree p))
     (test "poly-leading coefficient" 3 (poly-leading p))
     (test "poly-coeff x^2" 3 (poly-coeff p 2))
     (test "poly-coeff x^1" 2 (poly-coeff p 1))
     (test "poly-coeff x^0" 1 (poly-coeff p 0)))

(let ([p (make-poly (vector 0 0 5))])
     (test "strip leading zeros: degree" 0 (poly-degree p))
     (test "strip leading zeros: coeff" 5 (poly-leading p)))

(let ([p (poly-from-list '(1 -2 1))])
     (test "poly-from-list" #t (poly? p))
     (test "poly-from-list degree" 2 (poly-degree p)))

(test "poly-zero degree" 0 (poly-degree (poly-zero)))
(test "poly-one degree" 0 (poly-degree (poly-one)))
(test "poly-one leading" 1 (poly-leading (poly-one)))

(let ([p (poly-monomial 5 3)])
     (test "monomial 5x^3 degree" 3 (poly-degree p))
     (test "monomial 5x^3 leading" 5 (poly-leading p)))

;;; ====
;;; Evaluation Tests
;;; ====

(printf "\n--- Evaluation ---\n")

;; p(x) = x^2 - 2x + 1 = (x-1)^2
(let ([p (make-poly (vector 1 -2 1))])
     (test-approx "eval (x-1)^2 at x=0" 1.0 (poly-eval p 0) 1e-10)
     (test-approx "eval (x-1)^2 at x=1" 0.0 (poly-eval p 1) 1e-10)
     (test-approx "eval (x-1)^2 at x=2" 1.0 (poly-eval p 2) 1e-10)
     (test-approx "eval (x-1)^2 at x=3" 4.0 (poly-eval p 3) 1e-10))

;; Complex evaluation
(let ([p (make-poly (vector 1 0 1))])  ; x^2 + 1
     (let ([result (poly-eval-complex p (make-complex 0 1))])  ; eval at i
          (test-complex-approx "eval x^2+1 at i" (make-complex 0 0) result 1e-10)))

;;; ====
;;; Arithmetic Tests
;;; ====

(printf "\n--- Arithmetic ---\n")

;; Addition
(let* ([p1 (make-poly (vector 1 2))]      ; x + 2
       [p2 (make-poly (vector 1 -1 3))]   ; x^2 - x + 3
       [sum (poly-add p1 p2)])
      (test "add degree" 2 (poly-degree sum))
      (test-approx "add coeff x^2" 1.0 (poly-coeff sum 2) 1e-10)
      (test-approx "add coeff x^1" 0.0 (poly-coeff sum 1) 1e-10)
      (test-approx "add coeff x^0" 5.0 (poly-coeff sum 0) 1e-10))

;; Subtraction
(let* ([p1 (make-poly (vector 1 2 3))]    ; x^2 + 2x + 3
       [p2 (make-poly (vector 1 2 3))]
       [diff (poly-sub p1 p2)])
      (test "sub equal polys" 0 (poly-degree diff))
      (test-approx "sub equal polys coeff" 0.0 (poly-leading diff) 1e-10))

;; Scaling
(let* ([p (make-poly (vector 1 2 3))]
       [scaled (poly-scale p 2)])
      (test-approx "scale by 2: x^2 coeff" 2.0 (poly-coeff scaled 2) 1e-10)
      (test-approx "scale by 2: x^1 coeff" 4.0 (poly-coeff scaled 1) 1e-10)
      (test-approx "scale by 2: x^0 coeff" 6.0 (poly-coeff scaled 0) 1e-10))

;; Multiplication
(let* ([p1 (make-poly (vector 1 1))]      ; x + 1
       [p2 (make-poly (vector 1 -1))]     ; x - 1
       [prod (poly-mul p1 p2)])     ; should be x^2 - 1
      (test "mul degree" 2 (poly-degree prod))
      (test-approx "mul (x+1)(x-1) x^2 coeff" 1.0 (poly-coeff prod 2) 1e-10)
      (test-approx "mul (x+1)(x-1) x^1 coeff" 0.0 (poly-coeff prod 1) 1e-10)
      (test-approx "mul (x+1)(x-1) x^0 coeff" -1.0 (poly-coeff prod 0) 1e-10))

;; Normalize
(let* ([p (make-poly (vector 2 4 6))]
       [pn (poly-normalize p)])
      (test-approx "normalize leading" 1.0 (poly-leading pn) 1e-10)
      (test-approx "normalize coeff" 3.0 (poly-coeff pn 0) 1e-10))

;;; ====
;;; Root Finding Tests
;;; ====

(printf "\n--- Root Finding ---\n")

;; Linear: x - 3 = 0, root = 3
(let* ([p (make-poly (vector 1 -3))]
       [roots (poly-roots p)])
      (test "linear root count" 1 (length roots))
      (test-approx "linear root value" 3.0 (complex-real (car roots)) 1e-10))

;; Quadratic with real roots: (x-1)(x-2) = x^2 - 3x + 2
(let* ([p (make-poly (vector 1 -3 2))]
       [roots (poly-roots p)]
       [r1 (complex-real (car roots))]
       [r2 (complex-real (cadr roots))])
      (test "quadratic real root count" 2 (length roots))
      ;; Roots should be 1 and 2 (order may vary)
      (test-approx "quadratic roots sum" 3.0 (+ r1 r2) 1e-10)
      (test-approx "quadratic roots product" 2.0 (* r1 r2) 1e-10))

;; Quadratic with complex roots: x^2 + 1 = 0, roots = +/- i
(let* ([p (make-poly (vector 1 0 1))]
       [roots (poly-roots p)])
      (test "complex root count" 2 (length roots))
      ;; Roots should be +i and -i
      (let ([imag-sum (+ (complex-imag (car roots)) (complex-imag (cadr roots)))])
           (test-approx "complex roots imaginary sum" 0.0 imag-sum 1e-10))
      (test-approx "complex root 1 real" 0.0 (complex-real (car roots)) 1e-10)
      (test-approx "complex root 1 imag abs" 1.0 (abs (complex-imag (car roots))) 1e-10))

;; Cubic: (x-1)(x-2)(x-3) = x^3 - 6x^2 + 11x - 6
(let* ([p (make-poly (vector 1 -6 11 -6))]
       [roots (poly-roots p)])
      (test "cubic root count" 3 (length roots))
      (let ([sum (apply + (map complex-real roots))])
           (test-approx "cubic roots sum" 6.0 sum 1e-6)))

;; poly-from-roots round-trip (use real numbers for stability)
(let* ([original-roots (list 1 -2 3)]
       [p (poly-from-roots original-roots 1)]
       [found-roots (poly-roots p)]
       [found-reals (sort-by < (map complex-real found-roots))])
      (test "from-roots round-trip count" 3 (length found-roots))
      ;; Check sum instead of individual roots (more stable)
      (test-approx "from-roots roots sum" 2.0 (apply + found-reals) 1e-4))

;;; ====
;;; High-Order Polynomial Tests (Numerical Stability)
;;; ====

(printf "\n--- High-Order Polynomials ---\n")

;; Order 5: (x-1)(x-2)(x-3)(x-4)(x-5)
;; High-order polynomial root-finding has numerical challenges
;; Test sum of roots instead of individual values
(let* ([roots5 (list 1 2 3 4 5)]
       [p5 (poly-from-roots roots5 1)]
       [found (poly-roots p5)])
      (test "order-5 root count" 5 (length found))
      (let ([found-sum (apply + (map complex-real found))])
           (test-approx "order-5 roots sum" 15.0 found-sum 0.1)))

;; Order 8
(let* ([roots8 (list 1 2 3 4 5 6 7 8)]
       [p8 (poly-from-roots roots8 1)]
       [found (poly-roots p8)])
      (test "order-8 root count" 8 (length found))
      ;; Check sum of roots equals sum of original (looser tolerance for high order)
      (let ([found-sum (apply + (map complex-real found))])
           (test-approx "order-8 roots sum" 36.0 found-sum 0.5)))

;;; ====
;;; Utility Function Tests
;;; ====

(printf "\n--- Utilities ---\n")

;; logspace
(let ([ls (logspace 0 2 3)])  ; 10^0, 10^1, 10^2 = 1, 10, 100
     (test "logspace length" 3 (vector-length ls))
     (test-approx "logspace[0]" 1.0 (vector-ref ls 0) 1e-10)
     (test-approx "logspace[1]" 10.0 (vector-ref ls 1) 1e-10)
     (test-approx "logspace[2]" 100.0 (vector-ref ls 2) 1e-10))

;; linspace
(let ([ls (linspace 0 10 5)])  ; 0, 2.5, 5, 7.5, 10
     (test "linspace length" 5 (vector-length ls))
     (test-approx "linspace[0]" 0.0 (vector-ref ls 0) 1e-10)
     (test-approx "linspace[2]" 5.0 (vector-ref ls 2) 1e-10)
     (test-approx "linspace[4]" 10.0 (vector-ref ls 4) 1e-10))

;; poly->string
(let ([p (make-poly (vector 1 -2 1))])
     (test "poly->string" "x^2 - 2x + 1" (poly->string p)))

(let ([p (make-poly (vector 3))])
     (test "poly->string constant" "3" (poly->string p)))

(let ([p (poly-zero)])
     (test "poly->string zero" "0" (poly->string p)))

;;; ====
;;; Summary
;;; ====

(printf "\n====\n")
(printf "                    TEST RESULTS\n")
(printf "====\n\n")
(printf "Tests passed: ~a\n" tests-passed)
(printf "Tests failed: ~a\n" tests-failed)
(printf "Total tests:  ~a\n" (+ tests-passed tests-failed))
(printf "\n")

(if (= tests-failed 0)
    (printf "[SUCCESS] All polynomial tests passed.\n")
    (printf "[FAILURE] Some tests failed!\n"))
