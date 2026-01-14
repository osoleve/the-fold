;;; Test harness for core/info-theory/channel-capacity.ss
;;;
;;; Tests channel capacity calculations for various channel models.

(load "core/base/prelude.ss")
(load "lattice/info/channel-capacity.ss")

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

;;; ====
;;; Binary Symmetric Channel Tests
;;; ====
(test-section "Binary Symmetric Channel")

;; Perfect channel (p=0)
(test-approx "BSC p=0 capacity" 1.0 (bsc-capacity 0) 0.001)

;; All-flip channel (p=1) - still perfect, just inverted
(test-approx "BSC p=1 capacity" 1.0 (bsc-capacity 1) 0.001)

;; Random channel (p=0.5) - no information
(test-approx "BSC p=0.5 capacity" 0.0 (bsc-capacity 0.5) 0.001)

;; Typical noisy channel
(test-true "BSC p=0.1 capacity > 0" (> (bsc-capacity 0.1) 0))
(test-true "BSC p=0.1 capacity < 1" (< (bsc-capacity 0.1) 1))

;; Symmetry: C(p) = C(1-p)
(test-approx "BSC symmetry" (bsc-capacity 0.1) (bsc-capacity 0.9) 0.001)
(test-approx "BSC symmetry 2" (bsc-capacity 0.2) (bsc-capacity 0.8) 0.001)

;; Known value: C(0.11) approx 0.5 bits
(test-approx "BSC p=0.11 capacity" 0.5 (bsc-capacity 0.11) 0.02)

;; Transition matrix
(define bsc-tm (bsc-transition-matrix 0.1))
(test "BSC matrix rows" 2 (length bsc-tm))
(test-approx "BSC matrix (0,0)" 0.9 (car (car bsc-tm)) 0.001)
(test-approx "BSC matrix (0,1)" 0.1 (cadr (car bsc-tm)) 0.001)

;; Mutual information
(test-true "BSC MI with uniform input"
           (> (bsc-mutual-information 0.1 0.5) 0))

;;; ====
;;; Binary Erasure Channel Tests
;;; ====
(test-section "Binary Erasure Channel")

;; Perfect channel (epsilon=0)
(test-approx "BEC eps=0 capacity" 1.0 (bec-capacity 0) 0.001)

;; All-erasure channel (epsilon=1)
(test-approx "BEC eps=1 capacity" 0.0 (bec-capacity 1) 0.001)

;; Half-erasure channel
(test-approx "BEC eps=0.5 capacity" 0.5 (bec-capacity 0.5) 0.001)

;; Linear relationship
(test-approx "BEC eps=0.3 capacity" 0.7 (bec-capacity 0.3) 0.001)
(test-approx "BEC eps=0.7 capacity" 0.3 (bec-capacity 0.7) 0.001)

;;; ====
;;; Z-Channel Tests
;;; ====
(test-section "Z-Channel")

;; Perfect channel (p=0)
(test-approx "Z-channel p=0 capacity" 1.0 (z-channel-capacity 0) 0.001)

;; All 1->0 channel (p=1)
(test-approx "Z-channel p=1 capacity" 0.0 (z-channel-capacity 1) 0.001)

;; Intermediate values
(test-true "Z-channel p=0.5 capacity > 0" (> (z-channel-capacity 0.5) 0))
(test-true "Z-channel p=0.5 capacity < 1" (< (z-channel-capacity 0.5) 1))

;; Transition matrix
(define z-tm (z-channel-transition-matrix 0.2))
(test-approx "Z-channel matrix (0,0)" 1.0 (car (car z-tm)) 0.001)
(test-approx "Z-channel matrix (0,1)" 0.0 (cadr (car z-tm)) 0.001)
(test-approx "Z-channel matrix (1,0)" 0.2 (car (cadr z-tm)) 0.001)
(test-approx "Z-channel matrix (1,1)" 0.8 (cadr (cadr z-tm)) 0.001)

;;; ====
;;; AWGN Channel Tests
;;; ====
(test-section "AWGN Channel")

;; Zero SNR - no capacity
(test-approx "AWGN SNR=0 capacity" 0.0 (awgn-capacity 0) 0.001)

;; SNR=1 (0 dB)
(test-approx "AWGN SNR=1 capacity" 0.5 (awgn-capacity 1) 0.001)

;; High SNR
(test-true "AWGN high SNR > 3 bits" (> (awgn-capacity 100) 3))

;; SNR from dB
(test-approx "AWGN 0dB capacity" 0.5 (awgn-capacity-db 0) 0.001)
(test-true "AWGN 10dB > 1.7 bits" (> (awgn-capacity-db 10) 1.7))

;; Inverse: SNR for capacity
(test-approx "SNR for C=1" 3.0 (snr-for-capacity 1) 0.01)
(test-approx "SNR for C=0.5" 1.0 (snr-for-capacity 0.5) 0.01)

;; Shannon-Hartley with bandwidth
(test-true "AWGN bandwidth positive"
           (> (awgn-capacity-bandwidth 1000 1 0.001) 0))

;;; ====
;;; Discrete Memoryless Channel Tests
;;; ====
(test-section "Discrete Memoryless Channel")

;; Uniform distribution
(define uniform-2 (make-uniform-dist 2))
(test "uniform-2 length" 2 (length uniform-2))
(test-approx "uniform-2 sum" 1.0 (fold-left + 0 uniform-2) 0.001)

(define uniform-4 (make-uniform-dist 4))
(test "uniform-4 length" 4 (length uniform-4))
(test-approx "uniform-4 prob" 0.25 (car uniform-4) 0.001)

;; DMC output distribution
(define test-transition '((0.9 0.1) (0.2 0.8)))
(define test-input '(0.5 0.5))
(define out-dist (dmc-output-distribution test-transition test-input))
(test "output dist length" 2 (length out-dist))
(test-approx "output dist sum" 1.0 (fold-left + 0 out-dist) 0.001)

;; DMC joint distribution
(define joint (dmc-joint-distribution test-transition test-input))
(test "joint rows" 2 (length joint))
(test "joint cols" 2 (length (car joint)))

;; DMC mutual information
(define mi (dmc-mutual-information test-transition test-input))
(test-true "DMC MI non-negative" (>= mi 0))

;; Symmetric channel capacity
(define sym-channel (bsc-transition-matrix 0.1))
(define sym-cap (dmc-capacity-symmetric sym-channel))
(test-approx "symmetric DMC = BSC" (bsc-capacity 0.1) sym-cap 0.01)

;;; ====
;;; Blahut-Arimoto Algorithm Tests
;;; ====
(test-section "Blahut-Arimoto Algorithm")

;; BSC capacity via Blahut-Arimoto
(define bsc-ba (blahut-arimoto (bsc-transition-matrix 0.1) 100 0.0001))
(test-approx "BA BSC capacity" (bsc-capacity 0.1) (cdr bsc-ba) 0.01)

;; Optimal input should be near uniform for BSC
(define ba-dist (car bsc-ba))
(test-approx "BA optimal dist near uniform" 0.5 (car ba-dist) 0.05)

;; Z-channel - should find non-uniform optimal
(define z-ba (blahut-arimoto (z-channel-transition-matrix 0.3) 100 0.0001))
(test-true "BA Z-channel capacity > 0" (> (cdr z-ba) 0))

;;; ====
;;; Capacity Bounds Tests
;;; ====
(test-section "Capacity Bounds")

;; Upper bound
(define upper (dmc-capacity-upper-bound test-transition))
(test-approx "upper bound" 1.0 upper 0.001)

;; Lower bound <= actual <= upper
(define lower (dmc-capacity-lower-bound test-transition))
(test-true "lower <= actual" (<= lower mi))

;;; ====
;;; Special Channel Models Tests
;;; ====
(test-section "Special Channel Models")

;; Binary symmetric = q-ary symmetric with q=2
(test-approx "q=2 sym = BSC" (bsc-capacity 0.1) (qary-symmetric-capacity 2 0.1) 0.01)

;; q-ary with p=0 is perfect
(test-approx "q-ary p=0" (log2 4) (qary-symmetric-capacity 4 0) 0.001)

;; Parallel channels
(test-approx "parallel channels" 3.0 (parallel-channel-capacity '(1.0 1.0 1.0)) 0.001)
(test-approx "parallel unequal" 2.5 (parallel-channel-capacity '(1.0 0.5 1.0)) 0.001)

;; Cascaded channels
(test-approx "cascaded bound" 0.5 (cascaded-channel-capacity-bound 0.5 1.0) 0.001)
(test-approx "cascaded symmetric" 0.5 (cascaded-channel-capacity-bound 1.0 0.5) 0.001)

;;; ====
;;; Rate-Capacity Tests
;;; ====
(test-section "Rate-Capacity Relationships")

(test "achievable below capacity" #t (achievable-rate? 0.4 0.5))
(test "not achievable above capacity" #f (achievable-rate? 0.6 0.5))
(test "achievable at capacity" #t (achievable-rate? 0.5 0.5))

(test-approx "max reliable rate" 0.7 (max-reliable-rate 0.7) 0.001)

(test-approx "spectral efficiency" 2.0 (spectral-efficiency 2000 1000) 0.001)

;;; ====
;;; Error Exponent Tests
;;; ====
(test-section "Error Exponents")

;; At capacity, exponent is 0
(define cap (bsc-capacity 0.1))
(test-approx "BSC exponent at capacity" 0.0 (bsc-random-coding-exponent 0.1 cap) 0.01)

;; Below capacity, exponent is positive
(test-true "BSC exponent below capacity > 0"
           (> (bsc-random-coding-exponent 0.1 (- cap 0.2)) 0))

;; AWGN exponent
(test-approx "AWGN exponent at capacity" 0.0
             (awgn-sphere-packing-exponent 10 (awgn-capacity 10)) 0.01)
(test-true "AWGN exponent below capacity > 0"
           (> (awgn-sphere-packing-exponent 10 0.5) 0))

;;; ====
;;; Utility Function Tests
;;; ====
(test-section "Utility Functions")

;; dB conversions
(test-approx "0 dB = 1" 1.0 (db-to-linear 0) 0.001)
(test-approx "10 dB = 10" 10.0 (db-to-linear 10) 0.01)
(test-approx "3 dB = 2" 2.0 (db-to-linear 3) 0.1)

(test-approx "1 -> 0 dB" 0.0 (linear-to-db 1) 0.001)
(test-approx "10 -> 10 dB" 10.0 (linear-to-db 10) 0.01)

;; log10
(test-approx "log10(10)" 1.0 (log10 10) 0.001)
(test-approx "log10(100)" 2.0 (log10 100) 0.001)

;;; ====
;;; Channel Summary Tests
;;; ====
(test-section "Channel Summary")

(test-true "BSC summary is string"
           (string? (channel-summary 'bsc 0.1)))
(test-true "BEC summary is string"
           (string? (channel-summary 'bec 0.5)))
(test-true "AWGN summary is string"
           (string? (channel-summary 'awgn 10)))

;;; ====
;;; Edge Cases
;;; ====
(test-section "Edge Cases")

;; Empty distribution
(test "uniform-0" '() (make-uniform-dist 0))

;; Very small/large values
(test-approx "AWGN very small SNR" 0.0 (awgn-capacity 0.0001) 0.001)
(test-true "AWGN very large SNR" (> (awgn-capacity 1000000) 9.5))

;; Negative SNR
(test-approx "AWGN negative SNR" 0.0 (awgn-capacity -1) 0.001)

;;; ====
;;; Summary
;;; ====

(newline)
(display "====")
(newline)
(display "  Tests passed: ")
(display *tests-passed*)
(newline)
(display "  Tests failed: ")
(display *tests-failed*)
(newline)
(display "====")
(newline)

(if (= *tests-failed* 0)
    (display "+ All channel capacity tests passed!\n")
    (begin
     (display "x Some tests failed!\n")
     (exit 1)))
