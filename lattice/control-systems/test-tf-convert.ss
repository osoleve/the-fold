;;; lattice/control-systems/test-tf-convert.ss — SS↔TF Conversion Tests

(load "core/lang/module.ss")
(load "core/base/prelude.ss")
(load "lattice/control-systems/tf-convert.ss")

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

(printf "\n=== SS↔TF Conversion Tests ===\n\n")

;;; ====
;;; TF -> SS Tests
;;; ====

(printf "--- TF to SS ---\n")

;; First-order: H(s) = 1/(s+1)
;; Controllable form: A=[-1], B=[1], C=[1], D=[0]
(let* ([tf (tf-from-lists '(1) '(1 1))]
       [sys (tf->ss tf)])
      (test "tf->ss: order" 1 (ss-order sys))
      (test-approx "tf->ss: A[0,0]" -1.0 (matrix-ref (ss-A sys) 0 0) 1e-10)
      (test-approx "tf->ss: B[0,0]" 1.0 (matrix-ref (ss-B sys) 0 0) 1e-10)
      (test-approx "tf->ss: C[0,0]" 1.0 (matrix-ref (ss-C sys) 0 0) 1e-10)
      (test-approx "tf->ss: D[0,0]" 0.0 (matrix-ref (ss-D sys) 0 0) 1e-10))

;; Second-order: H(s) = 1/(s^2 + 3s + 2)
;; = 1/((s+1)(s+2)), poles at -1, -2
(let* ([tf (tf-from-lists '(1) '(1 3 2))]
       [sys (tf->ss tf)])
      (test "tf->ss 2nd order: order" 2 (ss-order sys))
      ;; A matrix: [[0,1], [-2,-3]]
      (test-approx "tf->ss 2nd: A[0,0]" 0.0 (matrix-ref (ss-A sys) 0 0) 1e-10)
      (test-approx "tf->ss 2nd: A[0,1]" 1.0 (matrix-ref (ss-A sys) 0 1) 1e-10)
      (test-approx "tf->ss 2nd: A[1,0]" -2.0 (matrix-ref (ss-A sys) 1 0) 1e-10)
      (test-approx "tf->ss 2nd: A[1,1]" -3.0 (matrix-ref (ss-A sys) 1 1) 1e-10))

;; Proper but not strictly proper: H(s) = (s+2)/(s+1)
;; D should be nonzero (D = 1)
(let* ([tf (tf-from-lists '(1 2) '(1 1))]
       [sys (tf->ss tf)])
      (test "tf->ss proper: order" 1 (ss-order sys))
      (test-approx "tf->ss proper: D" 1.0 (matrix-ref (ss-D sys) 0 0) 1e-10))

;;; ====
;;; SS -> TF Tests
;;; ====

(printf "\n--- SS to TF ---\n")

;; Integrator: A=[0], B=[1], C=[1], D=[0] => H(s) = 1/s
(let* ([A (make-matrix 1 1 0)]
       [B (make-matrix 1 1 1)]
       [C (make-matrix 1 1 1)]
       [D (make-matrix 1 1 0)]
       [sys (make-ss A B C D)]
       [tf (ss->tf sys)])
      (test "ss->tf integrator: order" 1 (tf-order tf))
      (test "ss->tf integrator: DC gain" 'infinite (tf-dc-gain tf)))

;; First-order lowpass: A=[-1], B=[1], C=[1], D=[0] => H(s) = 1/(s+1)
(let* ([A (make-matrix 1 1 -1)]
       [B (make-matrix 1 1 1)]
       [C (make-matrix 1 1 1)]
       [D (make-matrix 1 1 0)]
       [sys (make-ss A B C D)]
       [tf (ss->tf sys)])
      (test "ss->tf 1st order: order" 1 (tf-order tf))
      (test-approx "ss->tf 1st order: DC gain" 1.0 (tf-dc-gain tf) 1e-10)
      ;; Pole should be at -1
      (let ([poles (tf-poles tf)])
           (test "ss->tf 1st order: pole count" 1 (length poles))
           (test-approx "ss->tf 1st order: pole" -1.0 (complex-real (car poles)) 1e-6)))

;; Second-order with complex poles
;; A = [[0, 1], [-5, -2]], B = [[0], [1]], C = [[1, 0]], D = [[0]]
;; Char poly: s^2 + 2s + 5, poles at -1 +/- 2j
(let* ([A (matrix-from-lists '((0 1) (-5 -2)))]
       [B (matrix-from-lists '((0) (1)))]
       [C (matrix-from-lists '((1 0)))]
       [D (matrix-from-lists '((0)))]
       [sys (make-ss A B C D)]
       [tf (ss->tf sys)]
       [poles (tf-poles tf)])
      (test "ss->tf 2nd order: order" 2 (tf-order tf))
      (test "ss->tf 2nd order: pole count" 2 (length poles))
      ;; Sum of real parts should be -2
      (test-approx "ss->tf 2nd order: poles real sum"
                   -2.0
                   (apply + (map complex-real poles))
                   1e-4))

;;; ====
;;; Round-Trip Tests
;;; ====

(printf "\n--- Round-Trip ---\n")

;; TF -> SS -> TF: frequency response should match
(let* ([tf1 (tf-from-lists '(2 3) '(1 4 5))]
       [sys (tf->ss tf1)]
       [tf2 (ss->tf sys)]
       ;; Compare frequency responses at several points
       [freqs (vector 0.1 1.0 10.0)])
      (test "round-trip: order preserved" (tf-order tf1) (tf-order tf2))
      (let ([mag1 (tf-magnitude tf1 freqs)]
            [mag2 (tf-magnitude tf2 freqs)])
           (test-approx "round-trip: mag at 0.1" (vector-ref mag1 0) (vector-ref mag2 0) 1e-4)
           (test-approx "round-trip: mag at 1.0" (vector-ref mag1 1) (vector-ref mag2 1) 1e-4)
           (test-approx "round-trip: mag at 10" (vector-ref mag1 2) (vector-ref mag2 2) 1e-4)))

;; SS -> TF -> SS: eigenvalues should match
(let* ([A (matrix-from-lists '((0 1) (-2 -3)))]
       [B (matrix-from-lists '((0) (1)))]
       [C (matrix-from-lists '((1 0)))]
       [D (matrix-from-lists '((0)))]
       [sys1 (make-ss A B C D)]
       [tf (ss->tf sys1)]
       [sys2 (tf->ss tf)])
      (test "ss round-trip: order" 2 (ss-order sys2))
      ;; Eigenvalues of A should be preserved (characteristic polynomial)
      ;; Original poles: roots of s^2 + 3s + 2 = (s+1)(s+2), so -1, -2
      (let ([poles (tf-poles tf)])
           (test-approx "ss round-trip: poles sum" -3.0
                        (apply + (map complex-real poles)) 1e-4)
           (test-approx "ss round-trip: poles product" 2.0
                        (apply * (map complex-real poles)) 1e-4)))

;;; ====
;;; Edge Cases
;;; ====

(printf "\n--- Edge Cases ---\n")

;; Static gain: H(s) = 5
(let* ([tf (tf-from-lists '(5) '(1))]
       [sys (tf->ss tf)])
      (test "static gain: order" 0 (ss-order sys))
      (test-approx "static gain: D" 5.0 (matrix-ref (ss-D sys) 0 0) 1e-10))

;; Numerator same degree as denominator
(let* ([tf (tf-from-lists '(1 0) '(1 1))]  ; s/(s+1)
       [sys (tf->ss tf)])
      (test "same degree: order" 1 (ss-order sys))
      (test-approx "same degree: D" 1.0 (matrix-ref (ss-D sys) 0 0) 1e-10))

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
    (printf "[SUCCESS] All conversion tests passed.\n")
    (printf "[FAILURE] Some tests failed!\n"))
