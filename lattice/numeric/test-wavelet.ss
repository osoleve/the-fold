;;; core/numeric/test-wavelet.ss — Tests for Wavelet Transforms

(load "core/lang/module.ss")
(load "lattice/numeric/wavelet.ss")

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
  (let ([diff (abs (- expected actual))])
       (if (< diff tolerance)
           (begin
            (set! tests-passed (+ tests-passed 1))
            (printf "  [PASS] ~a\n" name))
           (begin
            (set! tests-failed (+ tests-failed 1))
            (printf "  [FAIL] ~a\n" name)
            (printf "      Expected: ~s (±~a)\n" expected tolerance)
            (printf "      Got:      ~s\n" actual)
            (printf "      Diff:     ~a\n" diff)))))

(define (test-vec-approx name expected actual tolerance)
  (let* ([n (vector-length expected)]
         [m (vector-length actual)])
        (if (not (= n m))
            (begin
             (set! tests-failed (+ tests-failed 1))
             (printf "  [FAIL] ~a (length mismatch)\n" name)
             (printf "      Expected length: ~a\n" n)
             (printf "      Got length:      ~a\n" m))
            (let loop ([i 0] [max-diff 0])
                 (if (= i n)
                     (if (< max-diff tolerance)
                         (begin
                          (set! tests-passed (+ tests-passed 1))
                          (printf "  [PASS] ~a\n" name))
                         (begin
                          (set! tests-failed (+ tests-failed 1))
                          (printf "  [FAIL] ~a\n" name)
                          (printf "      Max diff: ~a (tolerance: ~a)\n" max-diff tolerance)))
                     (let ([diff (abs (- (vector-ref expected i) (vector-ref actual i)))])
                          (loop (+ i 1) (max max-diff diff))))))))

(define (test-predicate name predicate)
  (if predicate
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  [PASS] ~a\n" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  [FAIL] ~a\n" name))))

(printf "\n=== Wavelet Transform Tests ===\n\n")

;;; ====
;;; Wavelet Filter Coefficients
;;; ====

(printf "Wavelet Filter Coefficients:\n")

(let ([h (haar-scaling-filter)])
     (test "haar scaling filter - length" 2 (vector-length h))
     (test-approx "haar scaling filter - sum" (sqrt 2)
                  (+ (vector-ref h 0) (vector-ref h 1)) 1e-10))

(let ([g (haar-wavelet-filter)])
     (test "haar wavelet filter - length" 2 (vector-length g))
     (test-approx "haar wavelet filter - orthogonal to scaling"
                  0.0
                  (+ (* (vector-ref (haar-scaling-filter) 0) (vector-ref g 0))
                     (* (vector-ref (haar-scaling-filter) 1) (vector-ref g 1)))
                  1e-10))

(let ([h (daubechies-4-scaling-filter)])
     (test "db4 scaling filter - length" 4 (vector-length h)))

(let ([h (daubechies-6-scaling-filter)])
     (test "db6 scaling filter - length" 6 (vector-length h)))

;;; ====
;;; DWT/IDWT Perfect Reconstruction
;;; ====

(printf "\nDWT/IDWT Perfect Reconstruction:\n")

;; Test with constant signal
(let* ([signal (vector 1 1 1 1 1 1 1 1)]
       [coeffs (dwt-family signal 1 'haar)]
       [reconstructed (idwt-family coeffs 8 'haar)])
      (test-vec-approx "haar dwt-idwt: constant signal" signal reconstructed 1e-10))

;; Test with impulse
(let* ([signal (vector 1 0 0 0 0 0 0 0)]
       [coeffs (dwt-family signal 1 'haar)]
       [reconstructed (idwt-family coeffs 8 'haar)])
      (test-vec-approx "haar dwt-idwt: impulse" signal reconstructed 1e-10))

;; Test with alternating signal
(let* ([signal (vector 1 -1 1 -1 1 -1 1 -1)]
       [coeffs (dwt-family signal 1 'haar)]
       [reconstructed (idwt-family coeffs 8 'haar)])
      (test-vec-approx "haar dwt-idwt: alternating" signal reconstructed 1e-10))

;; Test multi-level reconstruction with Haar
(let* ([signal (vector 1 2 3 4 5 6 7 8)]
       [coeffs (dwt-family signal 2 'haar)]
       [reconstructed (idwt-family coeffs 8 'haar)])
      (test-vec-approx "haar dwt-idwt: 2 levels" signal reconstructed 1e-8))

;; Test with Daubechies-4
(let* ([signal (vector 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)]
       [coeffs (dwt-family signal 2 'db4)]
       [reconstructed (idwt-family coeffs 16 'db4)])
      (test-vec-approx "db4 dwt-idwt: 2 levels" signal reconstructed 0.5))

;; Test with Daubechies-6
(let* ([signal (vector 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)]
       [coeffs (dwt-family signal 2 'db6)]
       [reconstructed (idwt-family coeffs 16 'db6)])
      (test-vec-approx "db6 dwt-idwt: 2 levels" signal reconstructed 0.5))

;;; ====
;;; DWT Structure
;;; ====

(printf "\nDWT Structure:\n")

(let* ([signal (vector 1 2 3 4 5 6 7 8)]
       [coeffs (dwt-family signal 2 'haar)])
      (test "dwt returns correct number of components" 3 (length coeffs))
      (test-predicate "dwt approximation is vector" (vector? (car coeffs)))
      (test-predicate "dwt detail 1 is vector" (vector? (cadr coeffs)))
      (test-predicate "dwt detail 2 is vector" (vector? (caddr coeffs))))

(let* ([signal (vector 1 2 3 4 5 6 7 8)]
       [coeffs (dwt-family signal 2 'haar)]
       [approx (get-approximation coeffs)]
       [details (get-details coeffs)])
      (test "get-approximation extracts correctly" approx (car coeffs))
      (test "get-details extracts correctly" (length details) 2))

;; Test that approximation gets smaller at each level
(let* ([signal (make-vector 128 1)]
       [coeffs (dwt-family signal 3 'haar)]
       [approx (get-approximation coeffs)])
      (test-predicate "approximation shrinks by half each level"
                      (and (= (vector-length approx) (/ 128 8))
                           (= (vector-length (get-detail-level coeffs 1)) (/ 128 2))
                           (= (vector-length (get-detail-level coeffs 2)) (/ 128 4))
                           (= (vector-length (get-detail-level coeffs 3)) (/ 128 8)))))

;;; ====
;;; Multi-Resolution Analysis
;;; ====

(printf "\nMulti-Resolution Analysis:\n")

(let* ([signal (vector 1 2 3 4 5 6 7 8)]
       [decomp (wavelet-decompose signal 2 'haar)]
       [recon (wavelet-reconstruct decomp 8 'haar)])
      (test-vec-approx "wavelet decompose/reconstruct" signal recon 1e-8))

(let* ([signal (make-vector 64 1)]
       [coeffs (wavelet-decompose signal 3 'haar)]
       [energies (wavelet-energy-distribution coeffs)])
      (test "energy distribution - correct number of levels" 4 (length energies))
      (test-predicate "energy distribution - all positive"
                      (let check ([lst energies])
                           (cond
                            [(null? lst) #t]
                            [(< (car lst) 0) #f]
                            [else (check (cdr lst))]))))

;;; ====
;;; Wavelet Thresholding
;;; ====

(printf "\nWavelet Thresholding:\n")

(let* ([coeffs (vector 0.5 1.5 -0.3 2.0 -1.0)]
       [threshold 1.0]
       [hard (threshold-coefficients coeffs threshold 'hard)])
      (test-approx "hard threshold - zeros small values 1" 0.0 (vector-ref hard 0) 1e-10)
      (test-approx "hard threshold - preserves large values" 1.5 (vector-ref hard 1) 1e-10)
      (test-approx "hard threshold - zeros small values 2" 0.0 (vector-ref hard 2) 1e-10))

(let* ([coeffs (vector 0.5 1.5 -0.3 2.0 -1.5)]
       [threshold 1.0]
       [soft (threshold-coefficients coeffs threshold 'soft)])
      (test-approx "soft threshold - zeros small values" 0.0 (vector-ref soft 0) 1e-10)
      (test-approx "soft threshold - shrinks large values" 0.5 (vector-ref soft 1) 1e-10)
      (test-approx "soft threshold - shrinks negative values" -0.5 (vector-ref soft 4) 1e-10))

;; Test wavelet denoising doesn't break signal structure
(let* ([signal (vector 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)]
       [denoised (wavelet-denoise signal 2 'haar 0.5 'soft)])
      (test "denoising preserves signal length" 16 (vector-length denoised)))

;;; ====
;;; Energy Conservation
;;; ====

(printf "\nEnergy Conservation:\n")

(let* ([signal (vector 1 2 3 4 5 6 7 8)]
       [signal-energy (wavelet-energy signal)]
       [coeffs (dwt-family signal 2 'haar)]
       [coeff-energies (wavelet-energy-distribution coeffs)]
       [total-coeff-energy (apply + coeff-energies)])
      ;; Energy should be approximately preserved (up to numerical precision)
      (test-approx "wavelet transform preserves energy"
                   signal-energy total-coeff-energy 1e-6))

;;; ====
;;; Wavelet Family API
;;; ====

(printf "\nWavelet Family API:\n")

(test-predicate "get-wavelet-filters - haar"
                (let ([filters (get-wavelet-filters 'haar)])
                     (and (pair? filters)
                          (vector? (car filters))
                          (vector? (cdr filters)))))

(test-predicate "get-wavelet-filters - db4"
                (let ([filters (get-wavelet-filters 'db4)])
                     (and (pair? filters)
                          (= (vector-length (car filters)) 4))))

(test-predicate "get-wavelet-filters - db6"
                (let ([filters (get-wavelet-filters 'db6)])
                     (and (pair? filters)
                          (= (vector-length (car filters)) 6))))

;;; ====
;;; Edge Cases
;;; ====

(printf "\nEdge Cases:\n")

;; Small signal
(let* ([signal (vector 1 2)]
       [coeffs (dwt-family signal 1 'haar)]
       [reconstructed (idwt-family coeffs 2 'haar)])
      (test-vec-approx "small signal reconstruction" signal reconstructed 1e-10))

;; Power of 2 lengths
(let test-lengths ([len 4])
     (when (<= len 64)
           (let* ([signal (make-vector len 1)]
                  [max-levels (floor (log len 2))]
                  [levels (max 1 (- max-levels 1))]
                  [coeffs (dwt-family signal levels 'haar)]
                  [reconstructed (idwt-family coeffs len 'haar)])
                 (test-vec-approx (string-append "power-of-2 length "
                                                 (number->string len))
                                  signal reconstructed 1e-8)
                 (test-lengths (* len 2)))))

;;; ====
;;; Test Summary
;;; ====

(printf "\n=== Test Summary ===\n")
(printf "Passed: ~a\n" tests-passed)
(printf "Failed: ~a\n" tests-failed)
(printf "Total:  ~a\n" (+ tests-passed tests-failed))

(if (= tests-failed 0)
    (printf "\n✓ All tests passed!\n\n")
    (begin
     (printf "\n✗ Some tests failed\n\n")
     (exit 1)))
