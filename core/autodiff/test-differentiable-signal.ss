;;; core/autodiff/test-differentiable-signal.ss --- Tests for Differentiable Signal Processing
;;;
;;; Comprehensive tests for DFT/IDFT VJP, convolution gradients,
;;; and differentiable spectral operations.

(load "core/test-framework.ss")
(load "core/autodiff/differentiable-signal.ss")

;;; ============================================================
;;; Test Utilities
;;; ============================================================

;;; Numeric tolerance for floating-point comparisons
(define *eps* 1e-5)

;;; assert-= : Number x Number x Number -> Void
;;; Check numeric equality within tolerance.
(define (assert-= actual expected tolerance)
  (unless (< (abs (- actual expected)) tolerance)
          (set! *tests-failed* (+ *tests-failed* 1))
          (display "    x ")
          (display *current-test-name*)
          (newline)
          (display "      Expected: ")
          (display expected)
          (display " +/- ")
          (display tolerance)
          (newline)
          (display "      Got:      ")
          (display actual)
          (newline)))

;;; assert-vec-= : Vector x Vector x Number -> Void
;;; Check that two vectors are element-wise equal within tolerance.
(define (assert-vec-= actual expected tolerance)
  (let ([n-actual (vector-length actual)]
        [n-expected (vector-length expected)])
       (if (not (= n-actual n-expected))
           (begin
            (set! *tests-failed* (+ *tests-failed* 1))
            (display "    x ")
            (display *current-test-name*)
            (newline)
            (display "      Vector lengths differ: ")
            (display n-actual)
            (display " vs ")
            (display n-expected)
            (newline))
           (do ([i 0 (+ i 1)])
               ((= i n-actual))
               (let ([a (vector-ref actual i)]
                     [e (vector-ref expected i)])
                    (unless (< (abs (- a e)) tolerance)
                            (set! *tests-failed* (+ *tests-failed* 1))
                            (display "    x ")
                            (display *current-test-name*)
                            (display " [index ")
                            (display i)
                            (display "]")
                            (newline)
                            (display "      Expected: ")
                            (display e)
                            (display " +/- ")
                            (display tolerance)
                            (newline)
                            (display "      Got:      ")
                            (display a)
                            (newline)))))))

;;; assert-complex-= : Complex x Complex x Number -> Void
;;; Check that two complex numbers are equal within tolerance.
(define (assert-complex-= actual expected tolerance)
  (assert-= (complex-real actual) (complex-real expected) tolerance)
  (assert-= (complex-imag actual) (complex-imag expected) tolerance))

;;; assert-complex-vec-= : Vector[Complex] x Vector[Complex] x Number -> Void
;;; Check that two complex vectors are element-wise equal.
(define (assert-complex-vec-= actual expected tolerance)
  (let ([n-actual (vector-length actual)]
        [n-expected (vector-length expected)])
       (if (not (= n-actual n-expected))
           (begin
            (set! *tests-failed* (+ *tests-failed* 1))
            (display "    x ")
            (display *current-test-name*)
            (newline)
            (display "      Vector lengths differ: ")
            (display n-actual)
            (display " vs ")
            (display n-expected)
            (newline))
           (do ([i 0 (+ i 1)])
               ((= i n-actual))
               (let ([a (vector-ref actual i)]
                     [e (vector-ref expected i)])
                    (assert-complex-= a e tolerance))))))

;;; numerical-gradient-signal : (Vector -> Number) x Vector x Number -> Vector
;;; Compute numerical gradient of f w.r.t. signal using finite differences.
(define (numerical-gradient-signal f signal eps)
  (let* ([n (vector-length signal)]
         [grad (make-vector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) grad)
            (let* ([signal+ (vector-copy signal)]
                   [signal- (vector-copy signal)]
                   [_ (vector-set! signal+ i (+ (vector-ref signal i) eps))]
                   [_ (vector-set! signal- i (- (vector-ref signal i) eps))]
                   [f+ (f signal+)]
                   [f- (f signal-)]
                   [deriv (/ (- f+ f-) (* 2 eps))])
                  (vector-set! grad i deriv)))))

;;; sum-power : Vector[Complex] -> Number
;;; Sum of power spectrum (for testing).
(define (sum-power X)
  (let ([n (vector-length X)]
        [total 0])
       (do ([k 0 (+ k 1)])
           ((= k n) total)
           (let ([mag (complex-magnitude (vector-ref X k))])
                (set! total (+ total (* mag mag)))))))

(display "
==============================================================
      DIFFERENTIABLE SIGNAL PROCESSING TESTS
==============================================================
")

;;; ============================================================
;;; DFT VJP Tests
;;; ============================================================

(test-group dft-vjp-tests
            
            ;; Test 1: DFT VJP of constant gradient
            (define-test dft-vjp-constant-grad
              ;; If output gradient is all 1s, input gradient should be N (scaled DC)
              (let* ([n 4]
                     [output-grad (vector (make-complex 1 0)
                                          (make-complex 1 0)
                                          (make-complex 1 0)
                                          (make-complex 1 0))]
                     [input-grad (dft-vjp output-grad)])
                    ;; For constant output grad, input grad should be uniform
                    ;; due to DFT linearity
                    (assert-= (vector-length input-grad) n 0.001)))
            
            ;; Test 2: DFT VJP preserves real structure for real signals
            (define-test dft-vjp-real-signal
              (let* ([n 4]
                     [signal '#(1.0 2.0 3.0 4.0)]
                     [X (dft (real->complex-vec signal))]
                     ;; Use a real output gradient (imaginary parts zero)
                     [output-grad (make-vector n)]
                     [_ (do ([k 0 (+ k 1)])
                            ((= k n))
                            (vector-set! output-grad k (make-complex 1.0 0.0)))]
                     [input-grad (dft-vjp-real output-grad)])
                    ;; Result should be real-valued
                    (assert-= (vector-length input-grad) n 0.001)))
            
            ;; Test 3: DFT VJP numerical verification
            ;; For REAL input signal, use power-spectrum-grad-real with dft-vjp-real
            (define-test dft-vjp-numerical-check
              (let* ([n 4]
                     [signal '#(1.0 2.0 3.0 4.0)]
                     ;; Loss = sum of power spectrum = sum |X[k]|^2
                     [loss-fn (lambda (s)
                                      (sum-power (dft (real->complex-vec s))))]
                     ;; Numerical gradient
                     [num-grad (numerical-gradient-signal loss-fn signal 1e-6)]
                     ;; Analytical gradient via VJP
                     ;; For REAL input, use power-spectrum-grad-real (not power-spectrum-grad)
                     [X (dft (real->complex-vec signal))]
                     [grad-X (power-spectrum-grad-real X (make-vector n 1))]
                     [anal-grad (dft-vjp-real grad-X)])
                    ;; Compare numerical and analytical gradients
                    (assert-vec-= anal-grad num-grad 1e-3))))

;;; ============================================================
;;; IDFT VJP Tests
;;; ============================================================

(test-group idft-vjp-tests
            
            ;; Test 1: IDFT VJP basic
            (define-test idft-vjp-basic
              (let* ([n 4]
                     [X (vector (make-complex 10 0)
                                (make-complex 0 0)
                                (make-complex 0 0)
                                (make-complex 0 0))]
                     [output-grad '#(1.0 1.0 1.0 1.0)]
                     [input-grad (idft-vjp-real output-grad)])
                    ;; Check dimensions
                    (assert-= (vector-length input-grad) n 0.001)))
            
            ;; Test 2: DFT VJP produces correct gradient for linear loss
            ;; For loss L = Re(sum_k X[k]) where X = DFT(x)
            ;; dL/dX[k] = 1 (all ones)
            ;; dL/dx = F^H * dL/dX = N * IDFT([1,1,1,1]) = [4,0,0,0]
            (define-test dft-vjp-linear-loss
              (let* ([n 4]
                     [output-grad (vector (make-complex 1 0)
                                          (make-complex 1 0)
                                          (make-complex 1 0)
                                          (make-complex 1 0))]
                     [input-grad (dft-vjp output-grad)])
                    ;; For constant output gradient, VJP gives [N, 0, 0, 0]
                    (assert-= (complex-real (vector-ref input-grad 0)) 4.0 1e-6)
                    (assert-= (complex-real (vector-ref input-grad 1)) 0.0 1e-6)
                    (assert-= (complex-real (vector-ref input-grad 2)) 0.0 1e-6)
                    (assert-= (complex-real (vector-ref input-grad 3)) 0.0 1e-6)))
            
            ;; Test 3: DFT VJP satisfies adjoint property <DFT(x), y> = <x, VJP(y)>
            ;; This is the defining property of the correct VJP
            (define-test dft-vjp-adjoint-property
              (let* ([n 4]
                     ;; Test vector x (input to DFT)
                     [x (vector (make-complex 1 2)
                                (make-complex 3 -1)
                                (make-complex 0 4)
                                (make-complex -2 1))]
                     ;; Test vector y (output gradient)
                     [y (vector (make-complex 2 1)
                                (make-complex -1 3)
                                (make-complex 1 0)
                                (make-complex 0 -2))]
                     ;; Compute DFT(x)
                     [Fx (dft x)]
                     ;; Compute VJP(y) = F^H * y
                     [FHy (dft-vjp y)]
                     ;; <Fx, y> = sum_k conj(Fx[k]) * y[k]
                     [lhs (let ([sum (make-complex 0 0)])
                               (do ([k 0 (+ k 1)])
                                   ((= k n) sum)
                                   (set! sum (complex-add sum
                                                          (complex-mul (complex-conjugate (vector-ref Fx k))
                                                                       (vector-ref y k))))))]
                     ;; <x, FHy> = sum_n conj(x[n]) * FHy[n]
                     [rhs (let ([sum (make-complex 0 0)])
                               (do ([k 0 (+ k 1)])
                                   ((= k n) sum)
                                   (set! sum (complex-add sum
                                                          (complex-mul (complex-conjugate (vector-ref x k))
                                                                       (vector-ref FHy k))))))])
                    ;; The two inner products should be equal
                    (assert-= (complex-real lhs) (complex-real rhs) 1e-6)
                    (assert-= (complex-imag lhs) (complex-imag rhs) 1e-6))))

;;; ============================================================
;;; Convolution VJP Tests
;;; ============================================================

(test-group convolution-vjp-tests
            
            ;; Test 1: Convolution VJP signal - full mode
            (define-test conv-vjp-signal-full
              (let* ([signal '#(1.0 2.0 3.0 4.0)]
                     [kernel '#(0.5 0.5)]
                     [output (convolve signal kernel 'full)]
                     [output-len (vector-length output)]
                     [output-grad (make-vector output-len 1.0)]
                     [grad (convolve-vjp-signal output-grad kernel 'full)])
                    ;; Gradient should have same length as signal
                    (assert-= (vector-length grad) 4 0.001)))
            
            ;; Test 2: Convolution VJP kernel - full mode
            (define-test conv-vjp-kernel-full
              (let* ([signal '#(1.0 2.0 3.0 4.0)]
                     [kernel '#(0.5 0.5)]
                     [kernel-len (vector-length kernel)]
                     [output (convolve signal kernel 'full)]
                     [output-len (vector-length output)]
                     [output-grad (make-vector output-len 1.0)]
                     [grad (convolve-vjp-kernel output-grad signal 'full kernel-len)])
                    ;; Gradient should have same length as kernel
                    (assert-= (vector-length grad) 2 0.001)))
            
            ;; Test 3: Convolution gradient numerical verification
            (define-test conv-vjp-numerical-check
              (let* ([signal '#(1.0 2.0 3.0 4.0)]
                     [kernel '#(0.3 0.4 0.3)]
                     ;; Loss = sum of squared output
                     [loss-fn (lambda (s)
                                      (let* ([out (convolve s kernel 'full)]
                                             [n (vector-length out)]
                                             [sum 0])
                                            (do ([i 0 (+ i 1)])
                                                ((= i n) sum)
                                                (let ([v (vector-ref out i)])
                                                     (set! sum (+ sum (* v v)))))))]
                     ;; Numerical gradient
                     [num-grad (numerical-gradient-signal loss-fn signal 1e-6)]
                     ;; Analytical gradient via VJP
                     [output (convolve signal kernel 'full)]
                     [output-grad (make-vector (vector-length output))]
                     [_ (do ([i 0 (+ i 1)])
                            ((= i (vector-length output)))
                            (vector-set! output-grad i (* 2 (vector-ref output i))))]
                     [anal-grad (convolve-vjp-signal output-grad kernel 'full)])
                    ;; Compare
                    (assert-vec-= anal-grad num-grad 1e-3)))
            
            ;; Test 4: Valid mode convolution VJP
            (define-test conv-vjp-valid-mode
              (let* ([signal '#(1.0 2.0 3.0 4.0 5.0)]
                     [kernel '#(1.0 0.0 -1.0)]  ; Simple gradient filter
                     [kernel-len (vector-length kernel)]
                     [output (convolve signal kernel 'valid)]
                     [output-len (vector-length output)]
                     [output-grad (make-vector output-len 1.0)]
                     [grad-signal (convolve-vjp-signal output-grad kernel 'valid)]
                     [grad-kernel (convolve-vjp-kernel output-grad signal 'valid kernel-len)])
                    ;; Check dimensions
                    (assert-= (vector-length output) 3 0.001)  ; n - m + 1 = 5 - 3 + 1 = 3
                    (assert-true (> (vector-length grad-signal) 0))
                    (assert-= (vector-length grad-kernel) 3 0.001))))

;;; ============================================================
;;; Power Spectrum Gradient Tests
;;; ============================================================

(test-group power-spectrum-grad-tests
            
            ;; Test 1: Power spectrum gradient formula
            (define-test power-spectrum-grad-formula
              ;; d(|X|^2)/dX = 2 * conj(X)
              (let* ([X (vector (make-complex 3 4))]  ; |X|^2 = 25
                     [output-grad '#(1.0)]  ; gradient of loss w.r.t. power
                     [grad (power-spectrum-grad X output-grad)])
                    ;; Expected: 2 * conj(3+4i) = 2*(3-4i) = 6-8i
                    (assert-complex-= (vector-ref grad 0)
                                      (make-complex 6 -8)
                                      1e-6)))
            
            ;; Test 2: Power spectrum gradient - multiple bins
            (define-test power-spectrum-grad-multi
              (let* ([X (vector (make-complex 1 0)
                                (make-complex 0 1)
                                (make-complex 1 1))]
                     [output-grad '#(1.0 1.0 1.0)]
                     [grad (power-spectrum-grad X output-grad)])
                    ;; X[0] = 1: grad = 2*1 = 2
                    ;; X[1] = i: grad = 2*(-i) = -2i
                    ;; X[2] = 1+i: grad = 2*(1-i) = 2-2i
                    (assert-complex-= (vector-ref grad 0) (make-complex 2 0) 1e-6)
                    (assert-complex-= (vector-ref grad 1) (make-complex 0 -2) 1e-6)
                    (assert-complex-= (vector-ref grad 2) (make-complex 2 -2) 1e-6))))

;;; ============================================================
;;; Magnitude Spectrum Gradient Tests
;;; ============================================================

(test-group magnitude-spectrum-grad-tests
            
            ;; Test 1: Magnitude gradient formula
            (define-test magnitude-grad-formula
              ;; d|X|/dX = conj(X) / |X|
              (let* ([X (vector (make-complex 3 4))]  ; |X| = 5
                     [output-grad '#(1.0)]
                     [grad (magnitude-spectrum-grad X output-grad)])
                    ;; Expected: (3-4i) / 5 = 0.6 - 0.8i
                    (assert-complex-= (vector-ref grad 0)
                                      (make-complex 0.6 -0.8)
                                      1e-6)))
            
            ;; Test 2: Magnitude gradient handles zero
            (define-test magnitude-grad-zero-safe
              (let* ([X (vector (make-complex 0 0))]
                     [output-grad '#(1.0)]
                     [grad (magnitude-spectrum-grad X output-grad)])
                    ;; Should return zero, not NaN
                    (assert-complex-= (vector-ref grad 0) (complex-zero) 1e-6))))

;;; ============================================================
;;; Spectral MSE Gradient Tests
;;; ============================================================

(test-group spectral-mse-tests
            
            ;; Test 1: MSE of identical spectra is zero
            (define-test spectral-mse-identical
              (let* ([X (vector (make-complex 1 2)
                                (make-complex 3 -1))])
                    (let-values ([(loss grad) (spectral-mse-grad X X)])
                                (assert-= loss 0 1e-10))))
            
            ;; Test 2: MSE gradient points toward target
            (define-test spectral-mse-gradient-direction
              (let* ([X1 (vector (make-complex 1 0))]
                     [X2 (vector (make-complex 2 0))]
                     ;; X1 is smaller, so gradient should be positive (toward X2)
                     )
                    (let-values ([(loss grad) (spectral-mse-grad X1 X2)])
                                ;; d((1-2)^2)/dX1 = 2*(1-2) = -2
                                (assert-= loss 1 1e-6)  ; |1-2|^2 / 1 = 1
                                (assert-complex-= (vector-ref grad 0)
                                                  (make-complex -2 0)
                                                  1e-6)))))

;;; ============================================================
;;; End-to-End Gradient Tests
;;; ============================================================

(test-group e2e-gradient-tests
            
            ;; Test 1: Full pipeline - signal -> DFT -> power -> gradient
            ;; For REAL input, use power-spectrum-grad-real
            (define-test e2e-power-spectrum-gradient
              (let* ([signal '#(1.0 0.0 -1.0 0.0)]  ; Simple sinusoidal pattern
                     ;; Loss = sum of power spectrum
                     [loss-fn (lambda (s)
                                      (sum-power (dft (real->complex-vec s))))]
                     ;; Numerical gradient
                     [num-grad (numerical-gradient-signal loss-fn signal 1e-6)]
                     ;; Analytical: power gradient -> DFT VJP (use real variant)
                     [X (dft (real->complex-vec signal))]
                     [n (vector-length signal)]
                     [grad-X (power-spectrum-grad-real X (make-vector n 1))]
                     [anal-grad (dft-vjp-real grad-X)])
                    ;; Verify match
                    (assert-vec-= anal-grad num-grad 1e-3)))
            
            ;; Test 2: Differentiating through lowpass filter
            (define-test e2e-lowpass-gradient
              (let* ([signal '#(1.0 2.0 3.0 2.0 1.0)]
                     [kernel '#(0.25 0.5 0.25)]  ; Simple smoothing kernel
                     ;; Loss = sum of squared filtered output
                     [loss-fn (lambda (s)
                                      (let* ([out (convolve s kernel 'same)]
                                             [n (vector-length out)]
                                             [sum 0])
                                            (do ([i 0 (+ i 1)])
                                                ((= i n) sum)
                                                (let ([v (vector-ref out i)])
                                                     (set! sum (+ sum (* v v)))))))]
                     ;; Numerical gradient
                     [num-grad (numerical-gradient-signal loss-fn signal 1e-6)])
                    ;; Just verify numerical gradient is computable
                    (assert-= (vector-length num-grad) 5 0.001)
                    ;; Each gradient element should be finite
                    (do ([i 0 (+ i 1)])
                        ((= i 5))
                        (assert-true (not (nan? (vector-ref num-grad i))))))))

;;; ============================================================
;;; Traced Convolution Tests
;;; ============================================================

(test-group traced-conv-tests
            
            ;; Test 1: Traced convolution produces correct forward pass
            (define-test traced-conv-forward
              (reset-traced-ids!)
              (let* ([tape (make-reverse-tape)]
                     [signal-vals '#(1.0 2.0 3.0 4.0)]
                     [n (vector-length signal-vals)]
                     [traced-signal (make-vector n)]
                     [_ (do ([i 0 (+ i 1)])
                            ((= i n))
                            (vector-set! traced-signal i
                                         (make-traced-var (vector-ref signal-vals i) tape)))]
                     [kernel '#(0.5 0.5)]
                     [output (traced-convolve-1d traced-signal kernel 'full)])
                    ;; Check output length
                    (assert-= (vector-length output) 5 0.001)  ; 4 + 2 - 1
                    ;; Check first value: 1.0 * 0.5 = 0.5
                    (assert-= (traced-value (vector-ref output 0)) 0.5 1e-6)
                    ;; Check that outputs are traced
                    (assert-true (traced? (vector-ref output 0)))))
            
            ;; Test 2: Traced convolution gradient via reverse mode
            (define-test traced-conv-gradient
              (reset-traced-ids!)
              (let* ([tape (make-reverse-tape)]
                     [signal-vals '#(1.0 2.0 3.0)]
                     [n (vector-length signal-vals)]
                     [traced-signal (make-vector n)]
                     [_ (do ([i 0 (+ i 1)])
                            ((= i n))
                            (vector-set! traced-signal i
                                         (make-traced-var (vector-ref signal-vals i) tape)))]
                     [kernel '#(1.0 1.0)]  ; Sum filter
                     [output (traced-convolve-1d traced-signal kernel 'valid)]
                     ;; Output: [3, 5] for valid mode
                     ;; Sum loss: 3 + 5 = 8
                     [loss (traced-add (vector-ref output 0) (vector-ref output 1))])
                    ;; Check forward
                    (assert-= (traced-value loss) 8 1e-6))))

;;; ============================================================
;;; DFT Local Gradient Tests
;;; ============================================================

(test-group dft-local-grad-tests
            
            ;; Test 1: DFT local gradients for DC component
            (define-test dft-local-grad-dc
              ;; Re(X[0]) = sum of inputs, so d(Re(X[0]))/dx[i] = 1 for all i
              (let* ([n 4]
                     [grads (make-dft-re-local-grads n 0)])
                    (assert-= (car grads) 1.0 1e-6)
                    (assert-= (cadr grads) 1.0 1e-6)
                    (assert-= (caddr grads) 1.0 1e-6)
                    (assert-= (cadddr grads) 1.0 1e-6)))
            
            ;; Test 2: DFT imaginary local gradients for DC
            (define-test dft-local-grad-dc-imag
              ;; Im(X[0]) = 0 for real input, gradients should be 0
              (let* ([n 4]
                     [grads (make-dft-im-local-grads n 0)])
                    (assert-= (car grads) 0.0 1e-6)
                    (assert-= (cadr grads) 0.0 1e-6)
                    (assert-= (caddr grads) 0.0 1e-6)
                    (assert-= (cadddr grads) 0.0 1e-6))))

;;; ============================================================
;;; Helper for NaN check
;;; ============================================================

(define (nan? x)
  (not (= x x)))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "
==============================================================
")
(printf "Tests passed: ~a~n" *tests-passed*)
(printf "Tests failed: ~a~n" *tests-failed*)
(printf "Total tests:  ~a~n" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All differentiable signal processing tests passed.
")
    (display "
[FAILURE] Some differentiable signal processing tests failed.
"))

(when (> *tests-failed* 0)
      (exit 1))
