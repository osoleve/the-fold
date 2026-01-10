;;; Test harness for core/info-theory/rate-distortion.ss
;;;
;;; Tests rate-distortion theory functions.

(load "core/base/prelude.ss")
(load "core/info-theory/rate-distortion.ss")

(define *tests-passed* 0)
(define *tests-failed* 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! *tests-passed* (+ *tests-passed* 1))
       (display "  + ")
       (display name)
       (newline))
      (begin
       (set! *tests-failed* (+ *tests-failed* 1))
       (display "  x ")
       (display name)
       (display "\n    expected: ")
       (display expected)
       (display "\n    got: ")
       (display actual)
       (newline))))

(define (test-approx name expected actual tolerance)
  (if (< (abs (- expected actual)) tolerance)
      (begin
       (set! *tests-passed* (+ *tests-passed* 1))
       (display "  + ")
       (display name)
       (newline))
      (begin
       (set! *tests-failed* (+ *tests-failed* 1))
       (display "  x ")
       (display name)
       (display "\n    expected: ")
       (display expected)
       (display " (+/- ")
       (display tolerance)
       (display ")\n    got: ")
       (display actual)
       (newline))))

(define (test-true name expr)
  (test name #t (if expr #t #f)))

(define (test-section name)
  (newline)
  (display name)
  (newline))

;;; ============================================================
;;; Distortion Measures Tests
;;; ============================================================
(test-section "Distortion Measures")

;; Squared error
(test-approx "squared error" 4.0 (squared-error 1 3) 0.001)
(test-approx "squared error zero" 0.0 (squared-error 5 5) 0.001)

;; Absolute error
(test-approx "absolute error" 2.0 (absolute-error 1 3) 0.001)
(test-approx "absolute error neg" 3.0 (absolute-error -1 2) 0.001)

;; Hamming distortion
(test "hamming same" 0 (hamming-distortion 'a 'a))
(test "hamming diff" 1 (hamming-distortion 'a 'b))

;; MSE
(define xs '(1 2 3 4 5))
(define ys '(1 2 3 4 5))
(test-approx "mse zero" 0.0 (mse xs ys) 0.001)

(define ys2 '(2 3 4 5 6))
(test-approx "mse one" 1.0 (mse xs ys2) 0.001)

;; MAE
(test-approx "mae zero" 0.0 (mae xs ys) 0.001)
(test-approx "mae one" 1.0 (mae xs ys2) 0.001)

;; RMSE
(test-approx "rmse" 1.0 (rmse xs ys2) 0.001)

;; SNR
(define signal '(0 1 2 3 4 5 6 7 8 9))
(define noisy '(0.1 1.1 2.1 2.9 4.1 5.0 5.9 7.1 8.0 9.1))
(test-true "snr positive" (> (snr signal noisy) 0))
(test-true "snr-db positive" (> (snr-db signal noisy) 0))

;; PSNR
(test-true "psnr positive" (> (psnr signal noisy 10) 0))

;; Normalized MSE
(define norm-mse (normalized-mse signal noisy))
(test-true "normalized mse < 1" (< norm-mse 1))
(test-true "normalized mse > 0" (> norm-mse 0))

;;; ============================================================
;;; Statistical Helpers Tests
;;; ============================================================
(test-section "Statistical Helpers")

(test-approx "mean" 3.0 (mean '(1 2 3 4 5)) 0.001)
(test-approx "mean single" 5.0 (mean '(5)) 0.001)
(test "mean empty" 0 (mean '()))

(test-approx "variance" 2.0 (variance '(1 2 3 4 5)) 0.001)
(test-approx "variance uniform" 0.0 (variance '(5 5 5)) 0.001)

;;; ============================================================
;;; Rate-Distortion Function Tests
;;; ============================================================
(test-section "Rate-Distortion Functions")

;; Gaussian source
(define sigma2 4.0)  ; variance = 4

;; At D = sigma^2, R = 0
(test-approx "gaussian R(D=sigma2)" 0.0 (gaussian-rate-distortion sigma2 sigma2) 0.001)

;; At D = sigma^2/4, R = 1 bit
(test-approx "gaussian R(D=sigma2/4)" 1.0 (gaussian-rate-distortion sigma2 1.0) 0.01)

;; Distortion-rate inverse
(test-approx "gaussian D(R=0)" sigma2 (gaussian-distortion-rate sigma2 0) 0.001)
(test-approx "gaussian D(R=1)" 1.0 (gaussian-distortion-rate sigma2 1) 0.01)

;; High rate gives high distortion reduction
(test-true "gaussian D(R=10) small" (< (gaussian-distortion-rate sigma2 10) 0.01))

;; Binary source
(test-approx "binary R(D=0)" 1.0 (binary-rate-distortion 0) 0.001)
(test-approx "binary R(D=0.5)" 0.0 (binary-rate-distortion 0.5) 0.001)
(test-true "binary R(D=0.1) < 1" (< (binary-rate-distortion 0.1) 1))

;; Binary distortion-rate
(test-approx "binary D(R=1)" 0.0 (binary-distortion-rate 1) 0.01)
(test-approx "binary D(R=0)" 0.5 (binary-distortion-rate 0) 0.01)

;;; ============================================================
;;; Uniform Quantization Tests
;;; ============================================================
(test-section "Uniform Quantization")

;; 4-level quantizer on [0, 1]
(test-approx "uniform q level 0" 0.125 (uniform-quantize 0.1 1.0 4) 0.001)
(test-approx "uniform q level 1" 0.375 (uniform-quantize 0.3 1.0 4) 0.001)
(test-approx "uniform q level 2" 0.625 (uniform-quantize 0.6 1.0 4) 0.001)
(test-approx "uniform q level 3" 0.875 (uniform-quantize 0.9 1.0 4) 0.001)

;; Quantize list
(define uq-list (uniform-quantize-list '(0.1 0.5 0.9) 1.0 4))
(test "uniform q list length" 3 (length uq-list))

;; Theoretical distortion
(define uq-dist (uniform-quantization-distortion 1.0 4))
(test-true "uniform q distortion > 0" (> uq-dist 0))
(test-true "uniform q distortion < 0.1" (< uq-dist 0.1))

;; Quantization bits
(test-approx "q bits 4 levels" 2.0 (quantization-bits 4) 0.001)
(test-approx "q bits 8 levels" 3.0 (quantization-bits 8) 0.001)
(test-approx "q bits 1 level" 0.0 (quantization-bits 1) 0.001)

;;; ============================================================
;;; Lloyd-Max Quantization Tests
;;; ============================================================
(test-section "Lloyd-Max Quantization")

;; Simple test data
(define lm-samples '(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9))

;; Train 2-level quantizer
(define lm2 (lloyd-max-quantizer lm-samples 2 100))
(test "lloyd-max 2 levels" 2 (length lm2))

;; Levels should be in data range
(test-true "lm2 level 0 > 0" (> (car lm2) 0))
(test-true "lm2 level 1 < 1" (< (cadr lm2) 1))

;; Train 4-level quantizer
(define lm4 (lloyd-max-quantizer lm-samples 4 100))
(test "lloyd-max 4 levels" 4 (length lm4))

;; Levels should be sorted
(define lm4-sorted (list-sort < lm4))
(test "lloyd-max sorted" lm4-sorted lm4)

;; Quantize using trained levels
(define lm-q (lloyd-max-quantize 0.3 lm2))
(test-true "lm quantize in range" (and (> lm-q 0) (< lm-q 1)))

;; Empty/edge cases
(test "lloyd-max empty" '() (lloyd-max-quantizer '() 4 100))
(test "lloyd-max 0 levels" '() (lloyd-max-quantizer lm-samples 0 100))

;;; ============================================================
;;; Vector Quantization Tests
;;; ============================================================
(test-section "Vector Quantization")

(define cb '((0 0) (1 0) (0 1) (1 1)))  ; 4-vector codebook

(test-approx "vq distance same" 0.0 (vq-codebook-distance '(1 2) '(1 2)) 0.001)
(test-approx "vq distance unit" 1.0 (vq-codebook-distance '(0 0) '(1 0)) 0.001)
(test-approx "vq distance diagonal" 1.414 (vq-codebook-distance '(0 0) '(1 1)) 0.01)

(test "vq find nearest" 0 (vq-find-nearest '(0.1 0.1) cb))
(test "vq find nearest 3" 3 (vq-find-nearest '(0.9 0.9) cb))

(test "vq quantize" '(0 0) (vq-quantize '(0.1 0.1) cb))
(test "vq quantize 2" '(1 1) (vq-quantize '(0.9 0.9) cb))

;;; ============================================================
;;; Rate-Distortion Analysis Tests
;;; ============================================================
(test-section "Rate-Distortion Analysis")

(define orig '(1 2 3 4 5 6 7 8))
(define quant '(1 2 2 4 4 6 6 8))

(define op-rd (operational-rate-distortion orig quant))
(test-true "op rd is pair" (pair? op-rd))
(test-true "op rate > 0" (> (car op-rd) 0))
(test-true "op distortion > 0" (> (cdr op-rd) 0))

;; Unique values
(test "unique 1" '(c b a) (unique-values '(a b c)))
(test "unique dup" '(b a) (unique-values '(a b a b)))

;; RD gap (should be positive for operational point above bound)
(define gap (rd-gap 2.0 0.5 4.0))
(test-true "rd gap computed" (number? gap))

;;; ============================================================
;;; Entropy-Coded Quantization Tests
;;; ============================================================
(test-section "Entropy-Coded Quantization")

(define quant-out '(0 0 0 1 1 2))
(define q-entropy (quantizer-entropy quant-out))
(test-true "quantizer entropy > 0" (> q-entropy 0))
(test-true "quantizer entropy < log2(3)" (< q-entropy (log2 3)))

;; Count values
(define counts (count-values '(a a b c c c)))
(test "count a" 2 (cdr (assoc-helper 'a counts)))
(test "count c" 3 (cdr (assoc-helper 'c counts)))

;; Entropy coded rate
(define ec-rate (entropy-coded-rate quant-out))
(test-true "ec rate > 0" (> ec-rate 0))

;;; ============================================================
;;; Dithered Quantization Tests
;;; ============================================================
(test-section "Dithered Quantization")

(define dq (dither-quantize 0.5 1.0 4 0.0))
(test-true "dither q in range" (and (> dq 0) (< dq 1)))

;; With dither
(define dq2 (dither-quantize 0.5 1.0 4 0.05))
(test-true "dithered q in range" (and (> dq2 0) (< dq2 1)))

;;; ============================================================
;;; High-Rate Approximations Tests
;;; ============================================================
(test-section "High-Rate Approximations")

;; High-rate should match Gaussian R-D
(test-approx "high rate bits"
             (gaussian-rate-distortion 4.0 1.0)
             (high-rate-bits-per-sample 4.0 1.0)
             0.001)

(test-approx "high rate distortion"
             (gaussian-distortion-rate 4.0 2.0)
             (high-rate-distortion 4.0 2.0)
             0.001)

;;; ============================================================
;;; Summary Tests
;;; ============================================================
(test-section "Summary")

(define summary (rd-summary orig quant 10))
(test-true "summary is string" (string? summary))

;;; ============================================================
;;; Edge Cases
;;; ============================================================
(test-section "Edge Cases")

;; Empty lists
(test "mse empty" 0 (mse '() '()))
(test "mae empty" 0 (mae '() '()))

;; Zero variance
(test-approx "variance zero" 0.0 (variance '(5 5 5)) 0.001)

;; Gaussian R-D edge cases
(test "gaussian R(D) zero var" 0 (gaussian-rate-distortion 0 0.5))
(test "gaussian R(D) zero dist" +inf.0 (gaussian-rate-distortion 4.0 0))
(test "gaussian R(D) large dist" 0 (gaussian-rate-distortion 4.0 10))

;; Binary R-D edge cases
(test-approx "binary R(D<0)" 1.0 (binary-rate-distortion -0.1) 0.001)
(test-approx "binary R(D>0.5)" 0.0 (binary-rate-distortion 0.6) 0.001)

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(display "=======================================")
(newline)
(display "  Tests passed: ")
(display *tests-passed*)
(newline)
(display "  Tests failed: ")
(display *tests-failed*)
(newline)
(display "=======================================")
(newline)

(if (= *tests-failed* 0)
    (display "+ All rate-distortion tests passed!\n")
    (begin
     (display "x Some tests failed!\n")
     (exit 1)))
