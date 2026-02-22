;;; @module differentiable-signal
;;; @requires prelude complex dft convolution reverse-diff

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'complex)
(require 'dft)
(require 'convolution)
(require 'reverse-diff)

(doc 'module 'differentiable-signal)
(doc 'bridges '(autodiff numeric))
(doc 'description "Differentiable Signal Processing - VJP (Vector-Jacobian Product) wrappers for signal processing operations, enabling gradients to flow through DFT, IDFT, and convolution")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'note "Mathematical Background:

DFT/IDFT Adjoints:
  The DFT is a linear operation represented by the DFT matrix F:
    X = F * x   (DFT)
    x = F^H * X / N   (IDFT, where F^H is conjugate transpose)

  For gradient computation (VJP):
    If loss L = g(X) where X = DFT(x), then:
      dL/dx = conj(DFT(conj(dL/dX)))   (adjoint relation)
            = IDFT(dL/dX) * N          (for real signals)

    Similarly for IDFT:
      dL/dX = DFT(dL/dx) / N

  For real signals, the imaginary parts of gradients are zero,
  so we can work with real vectors throughout.

Convolution Gradient:
  y = conv(x, k) in the 'full' mode has length len(x) + len(k) - 1

  For gradient w.r.t. signal x:
    dL/dx = correlate(dL/dy, k, 'full') trimmed to length of x

  For gradient w.r.t. kernel k:
    dL/dk = correlate(x, dL/dy, 'valid')

  These follow from viewing convolution as matrix multiplication
  where the Toeplitz structure determines the adjoint.")

(doc 'section 'vector-utilities)

(define (traced-vec? v)
  (doc 'type '(-> (Vector alpha) Boolean))
  (and (vector? v)
       (> (vector-length v) 0)
       (traced? (vector-ref v 0))))

(define (vec-extract-values v)
  (doc 'type '(-> (Vector (Either Traced Number)) (Vector Number)))
  (let* ([n (vector-length v)]
         [result (make-vector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (vector-set! result i (traced-value (vector-ref v i))))))

(define (vec-extract-tape v)
  (doc 'type '(-> (Vector Traced) (Option Tape)))
  (if (and (vector? v) (> (vector-length v) 0) (traced? (vector-ref v 0)))
      (traced-tape (vector-ref v 0))
      #f))

(doc 'section 'dft-vjp)

(define (dft-vjp output-grad)
  (doc 'type '(-> (Vector Complex) (Vector Complex)))
  (doc 'description "DFT VJP (Vector-Jacobian Product)")
  (let* ([n (vector-length output-grad)]
         [idft-result (idft output-grad)]
         [result (make-vector n)])
        ;; Scale by N to get F^H * output-grad
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (vector-set! result i (complex-scale n (vector-ref idft-result i))))))

(define (dft-vjp-real output-grad)
  (doc 'type '(-> (Vector Number) (Vector Number)))
  (let* ([grad (dft-vjp output-grad)]
         [n (vector-length grad)]
         [result (make-vector n)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (vector-set! result i (complex-real (vector-ref grad i))))))

(doc 'section 'idft-vjp)

(define (idft-vjp output-grad)
  (doc 'type '(-> (Vector Complex) (Vector Complex)))
  (doc 'description "IDFT VJP - computes dL/dX = DFT(dL/dx) / N")
  (let* ([n (vector-length output-grad)]
         ;; VJP: dL/dX = DFT(dL/dx) / N
         [input-complex (if (and (> n 0) (complex? (vector-ref output-grad 0)))
                            output-grad
                            (real->complex-vec output-grad))]
         [grad-complex (dft input-complex)]
         [result (make-vector n)])
        ;; Scale by 1/N
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (let ([g (vector-ref grad-complex i)])
                 (vector-set! result i (complex-scale (/ 1.0 n) g))))))

(define (idft-vjp-real output-grad)
  (doc 'type '(-> (Vector Number) (Vector Complex)))
  (idft-vjp (real->complex-vec output-grad)))

(doc 'section 'convolution-vjp)

(define (convolve-vjp-signal output-grad kernel mode)
  (doc 'type '(-> (Vector Number) (Vector Number) Symbol (Vector Number)))
  (doc 'description "VJP for convolution with respect to signal")
  (let* ([m (vector-length kernel)]
         [grad-len (vector-length output-grad)]
         ;; The adjoint of convolution w.r.t. signal is correlation with the kernel
         ;; (or equivalently, convolution with the reversed kernel)
         [kernel-rev (vector-reverse kernel)]
         ;; Compute full correlation/convolution
         [full-grad (convolve output-grad kernel-rev 'full)])
        (case mode
              [(full)
               ;; For full mode, output has length n + m - 1
               ;; Signal gradient has same length as original signal
               ;; Extract the central portion
               (let* ([full-len (vector-length full-grad)]
                      [signal-len (+ grad-len 1 (- m))]
                      [offset (- m 1)]
                      [result (make-vector signal-len)])
                     (do ([i 0 (+ i 1)])
                         ((= i signal-len) result)
                         (vector-set! result i (vector-ref full-grad (+ i offset)))))]
              [(same)
               ;; For same mode, output has same length as signal
               ;; Gradient also has same length as signal
               (let* ([signal-len grad-len]
                      [full-len (vector-length full-grad)]
                      [offset (quotient m 2)]
                      [result (make-vector signal-len)])
                     (do ([i 0 (+ i 1)])
                         ((= i signal-len) result)
                         (let ([idx (+ i offset)])
                              (if (and (>= idx 0) (< idx full-len))
                                  (vector-set! result i (vector-ref full-grad idx))
                                  (vector-set! result i 0)))))]
              [(valid)
               ;; For valid mode, output has length n - m + 1
               ;; Gradient for signal is padded appropriately
               (let* ([signal-len (+ grad-len m -1)]
                      [full-len (vector-length full-grad)]
                      [result (make-vector signal-len 0)])
                     ;; The full correlation with reversed kernel handles this
                     (do ([i 0 (+ i 1)])
                         ((= i (min signal-len full-len)) result)
                         (vector-set! result i (vector-ref full-grad i))))]
              [else (error 'convolve-vjp-signal "invalid mode" mode)])))

;;; convolve-vjp-kernel : (Vector Number) × (Vector Number) × Symbol × Int → (Vector Number)
(define (convolve-vjp-kernel output-grad signal mode kernel-len)
  (let* ([n (vector-length signal)]
         [m kernel-len]
         [grad-len (vector-length output-grad)])
        (case mode
              [(full)
               ;; For full mode, output has length n + m - 1
               ;; dL/dk[j] = sum_i x[i] * dL/dy[i+j] for valid positions
               ;; This is correlate(signal, output-grad) keeping length m
               (let ([result (make-vector m 0)])
                    (do ([j 0 (+ j 1)])
                        ((= j m) result)
                        (let ([sum 0])
                             (do ([i 0 (+ i 1)])
                                 ((= i n))
                                 (let ([out-idx (+ i j)])
                                      (when (and (>= out-idx 0) (< out-idx grad-len))
                                            (set! sum (+ sum (* (vector-ref signal i)
                                                                (vector-ref output-grad out-idx)))))))
                             (vector-set! result j sum))))]
              [(same)
               ;; For same mode
               (let ([result (make-vector m 0)]
                     [offset (quotient m 2)])
                    (do ([j 0 (+ j 1)])
                        ((= j m) result)
                        (let ([sum 0])
                             (do ([i 0 (+ i 1)])
                                 ((= i n))
                                 (let ([out-idx (+ i (- offset) j)])
                                      (when (and (>= out-idx 0) (< out-idx grad-len))
                                            (set! sum (+ sum (* (vector-ref signal i)
                                                                (vector-ref output-grad out-idx)))))))
                             (vector-set! result j sum))))]
              [(valid)
               ;; For valid mode, output has length n - m + 1
               ;; dL/dk[j] = sum_i x[i+j] * dL/dy[i]
               (let ([result (make-vector m 0)])
                    (do ([j 0 (+ j 1)])
                        ((= j m) result)
                        (let ([sum 0])
                             (do ([i 0 (+ i 1)])
                                 ((= i grad-len))
                                 (let ([sig-idx (+ i j)])
                                      (when (< sig-idx n)
                                            (set! sum (+ sum (* (vector-ref signal sig-idx)
                                                                (vector-ref output-grad i)))))))
                             (vector-set! result j sum))))]
              [else (error 'convolve-vjp-kernel "invalid mode" mode)])))

(doc 'section 'traced-dft)

(define (traced-dft-real x)
  (doc 'type '(-> (Vector (Either Traced Number)) (Vector Traced)))
  (doc 'description "Traced DFT operation for real-valued signals")
  (let* ([tape (vec-extract-tape x)]
         [n (vector-length x)]
         [x-vals (vec-extract-values x)]
         ;; Forward: compute DFT
         [x-complex (real->complex-vec x-vals)]
         [result-complex (dft x-complex)]
         [result-mag (magnitude-spectrum result-complex)])
        (if (not tape)
            ;; No traced inputs - just return the complex result
            result-complex
            ;; Create traced outputs with gradient tracking
            (let ([result (make-vector (* 2 n))]  ; Interleave real/imag
                  [input-ids (let ([ids (make-vector n)])
                                  (do ([i 0 (+ i 1)])
                                      ((= i n) ids)
                                      (vector-set! ids i (traced-id (vector-ref x i)))))])
                 ;; Record the DFT operation
                 ;; Store: (result-ids 'dft-real input-ids forward-info)
                 ;; For now, return traced values with VJP callback
                 (do ([k 0 (+ k 1)])
                     ((= k n) result)
                     (let* ([c (vector-ref result-complex k)]
                            [re (complex-real c)]
                            [im (complex-imag c)]
                            [re-id (fresh-id)]
                            [im-id (fresh-id)])
                           ;; Record as custom operation with stored backward pass info
                           (reverse-tape-push! tape
                                               (list re-id 'dft-real-re (vector->list input-ids)
                                                     (make-dft-re-local-grads n k)))
                           (reverse-tape-push! tape
                                               (list im-id 'dft-real-im (vector->list input-ids)
                                                     (make-dft-im-local-grads n k)))
                           (vector-set! result (* 2 k) (traced re re-id tape))
                           (vector-set! result (+ (* 2 k) 1) (traced im im-id tape))))))))

(define (make-dft-re-local-grads n k)
  (doc 'type '(-> Int Int (List Number)))
  (let ([result (make-vector n)]
        [two-pi (* 2 (pi-value))])
       (do ([m 0 (+ m 1)])
           ((= m n) (vector->list result))
           (vector-set! result m (cos (/ (* -1 two-pi k m) n))))))

(define (make-dft-im-local-grads n k)
  (doc 'type '(-> Int Int (List Number)))
  (let ([result (make-vector n)]
        [two-pi (* 2 (pi-value))])
       (do ([m 0 (+ m 1)])
           ((= m n) (vector->list result))
           (vector-set! result m (sin (/ (* -1 two-pi k m) n))))))

(doc 'section 'high-level-operations)

(define (diff-dft signal loss-fn)
  (doc 'type '(-> (Vector Number) (-> (Vector Complex) Number) (Vector Number)))
  (doc 'description "Compute gradient of loss function w.r.t. signal through DFT")
  (let* ([n (vector-length signal)]
         ;; Compute DFT forward
         [X (dft (real->complex-vec signal))]
         ;; Compute loss and its gradient w.r.t. X
         ;; For now, use numerical differentiation for the loss
         [eps 1e-7]
         [grad-X (make-vector n)]
         [loss-0 (loss-fn X)])
        ;; Compute gradient of loss w.r.t. each DFT coefficient
        (do ([k 0 (+ k 1)])
            ((= k n))
            (let* ([X-k (vector-ref X k)]
                   ;; Perturb real part
                   [X+ (vector-copy X)]
                   [_ (vector-set! X+ k (make-complex (+ (complex-real X-k) eps)
                                                      (complex-imag X-k)))]
                   [loss-re+ (loss-fn X+)]
                   [grad-re (/ (- loss-re+ loss-0) eps)]
                   ;; Perturb imaginary part
                   [X++ (vector-copy X)]
                   [_ (vector-set! X++ k (make-complex (complex-real X-k)
                                                       (+ (complex-imag X-k) eps)))]
                   [loss-im+ (loss-fn X++)]
                   [grad-im (/ (- loss-im+ loss-0) eps)])
                  (vector-set! grad-X k (make-complex grad-re grad-im))))
        ;; Now backprop through DFT using VJP
        (dft-vjp-real grad-X)))

(define (diff-convolution signal kernel mode loss-fn)
  (doc 'type '(-> (Vector Number) (Vector Number) Symbol (-> (Vector Number) Number) (Values (Vector Number) (Vector Number))))
  (doc 'description "Compute gradients w.r.t. both signal and kernel through convolution")
  (let* ([output (convolve signal kernel mode)]
         [m (vector-length output)]
         [kernel-len (vector-length kernel)]
         [eps 1e-7]
         [grad-output (make-vector m)]
         [loss-0 (loss-fn output)])
        ;; Compute gradient of loss w.r.t. convolution output
        (do ([i 0 (+ i 1)])
            ((= i m))
            (let* ([output+ (vector-copy output)]
                   [_ (vector-set! output+ i (+ (vector-ref output i) eps))]
                   [loss+ (loss-fn output+)]
                   [grad-i (/ (- loss+ loss-0) eps)])
                  (vector-set! grad-output i grad-i)))
        ;; Backprop through convolution
        (values (convolve-vjp-signal grad-output kernel mode)
                (convolve-vjp-kernel grad-output signal mode kernel-len))))

(doc 'section 'traced-convolution)

(define (traced-convolve-1d signal kernel mode)
  (doc 'type '(-> (Vector Traced) (Vector Number) Symbol (Vector Traced)))
  (doc 'description "Traced convolution integrated with autodiff")
  (let* ([tape (vec-extract-tape signal)]
         [n (vector-length signal)]
         [m (vector-length kernel)]
         [signal-vals (vec-extract-values signal)]
         ;; Forward pass
         [output (convolve signal-vals kernel mode)]
         [output-len (vector-length output)])
        (if (not tape)
            output
            ;; Create traced outputs
            (let ([result (make-vector output-len)]
                  [input-ids (let ([ids (make-vector n)])
                                  (do ([i 0 (+ i 1)])
                                      ((= i n) ids)
                                      (vector-set! ids i (traced-id (vector-ref signal i)))))])
                 (do ([i 0 (+ i 1)])
                     ((= i output-len) result)
                     (let* ([out-val (vector-ref output i)]
                            [out-id (fresh-id)]
                            ;; Local gradients: d(output[i])/d(signal[j])
                            ;; For convolution, this is kernel[i-j] if in range
                            [local-grads (make-conv-local-grads i n m kernel mode)])
                           (reverse-tape-push! tape
                                               (list out-id 'conv1d (vector->list input-ids)
                                                     local-grads))
                           (vector-set! result i (traced out-val out-id tape))))))))

(define (make-conv-local-grads i n m kernel mode)
  (doc 'type '(-> Int Int Int (Vector Number) Symbol (List Number)))
  (let ([grads (make-vector n 0)])
       (case mode
             [(full)
              ;; For full mode: output[i] = sum_{j} signal[j] * kernel[i-j]
              ;; where 0 <= i-j < m and 0 <= j < n
              ;; So d(output[i])/d(signal[j]) = kernel[i-j] if 0 <= i-j < m
              (do ([j 0 (+ j 1)])
                  ((= j n) (vector->list grads))
                  (let ([k-idx (- i j)])
                       (when (and (>= k-idx 0) (< k-idx m))
                             (vector-set! grads j (vector-ref kernel k-idx)))))]
             [(same)
              (let ([offset (quotient m 2)])
                   (do ([j 0 (+ j 1)])
                       ((= j n) (vector->list grads))
                       (let ([k-idx (- (+ i offset) j)])
                            (when (and (>= k-idx 0) (< k-idx m))
                                  (vector-set! grads j (vector-ref kernel k-idx))))))]
             [(valid)
              (do ([j 0 (+ j 1)])
                  ((= j n) (vector->list grads))
                  (let ([k-idx (- (+ i (- m 1)) j)])
                       (when (and (>= k-idx 0) (< k-idx m) (>= (+ i j) 0) (< (+ i j) n))
                             (vector-set! grads j (vector-ref kernel k-idx)))))]
             [else (vector->list grads)])))

(doc 'section 'power-spectrum)

(define (power-spectrum-grad X output-grad)
  (doc 'type '(-> (Vector Complex) (Vector Number) (Vector Complex)))
  (doc 'description "Gradient of power spectrum: d(|X[k]|^2)/dX[k] = 2*conj(X[k])")
  (let* ([n (vector-length X)]
         [result (make-vector n)])
        (do ([k 0 (+ k 1)])
            ((= k n) result)
            (let* ([x-k (vector-ref X k)]
                   [g-k (if (< k (vector-length output-grad))
                            (vector-ref output-grad k)
                            0)]
                   ;; d(|X[k]|^2)/dX[k] = 2*conj(X[k])
                   [grad (complex-scale (* 2 g-k) (complex-conjugate x-k))])
                  (vector-set! result k grad)))))

(define (power-spectrum-grad-real X output-grad)
  (doc 'type '(-> (Vector Complex) (Vector Number) (Vector Complex)))
  (let* ([n (vector-length X)]
         [result (make-vector n)])
        (do ([k 0 (+ k 1)])
            ((= k n) result)
            (let* ([x-k (vector-ref X k)]
                   [g-k (if (< k (vector-length output-grad))
                            (vector-ref output-grad k)
                            0)]
                   ;; For real DFT: use X instead of conj(X)
                   [grad (complex-scale (* 2 g-k) x-k)])
                  (vector-set! result k grad)))))

(doc 'section 'magnitude-spectrum)

(define (magnitude-spectrum-grad X output-grad)
  (doc 'type '(-> (Vector Complex) (Vector Number) (Vector Complex)))
  (doc 'description "Gradient of magnitude spectrum")
  (let* ([n (vector-length X)]
         [result (make-vector n)])
        (do ([k 0 (+ k 1)])
            ((= k n) result)
            (let* ([x-k (vector-ref X k)]
                   [mag (complex-magnitude x-k)]
                   [g-k (if (< k (vector-length output-grad))
                            (vector-ref output-grad k)
                            0)])
                  (if (< mag 1e-12)
                      (vector-set! result k (complex-zero))
                      (vector-set! result k
                                   (complex-scale (/ g-k mag)
                                                  (complex-conjugate x-k))))))))

(doc 'section 'spectral-loss)

(define (spectral-mse-grad X1 X2)
  (doc 'type '(-> (Vector Complex) (Vector Complex) (Values Number (Vector Complex))))
  (doc 'description "Mean squared error gradient in spectral domain")
  (let* ([n (vector-length X1)]
         [total 0]
         [grad (make-vector n)])
        (do ([k 0 (+ k 1)])
            ((= k n) (values (/ total n) grad))
            (let* ([d (complex-sub (vector-ref X1 k) (vector-ref X2 k))]
                   [d-mag-sq (+ (* (complex-real d) (complex-real d))
                                (* (complex-imag d) (complex-imag d)))])
                  (set! total (+ total d-mag-sq))
                  (vector-set! grad k (complex-scale (/ 2.0 n) d))))))

(define (spectral-mse-signal-grad signal target-spectrum)
  (doc 'type '(-> (Vector Number) (Vector Complex) (Vector Number)))
  (let* ([X (dft (real->complex-vec signal))]
         [_ (spectral-mse-grad X target-spectrum)]
         [grad-X (call-with-values (lambda () (spectral-mse-grad X target-spectrum))
                                   (lambda (loss grad) grad))])
        (dft-vjp-real grad-X)))
