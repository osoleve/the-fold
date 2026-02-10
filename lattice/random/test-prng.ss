;;; core/random/test-prng.ss --- Tests for Pseudorandom Number Generators
;;;
;;; Comprehensive tests for PRNG algorithms:
;;;   - Splitmix64 generator
;;;   - PCG (Permuted Congruential Generator)
;;;   - Xorshift128+ generator
;;;   - Seed reproducibility
;;;   - Distribution uniformity
;;;   - Generator splitting and serialization
;;;
;;; This is a test file for core/random/prng.ss

(load "core/lang/module.ss")
(load "core/test-framework.ss")
(load "lattice/random/prng.ss")

;;; ====
;;; Test Utilities
;;; ====

;;; mean : (List Number) -> Number
(define (mean lst)
  (if (null? lst)
      0
      (/ (fold-left + 0 lst) (length lst))))

;;; variance : (List Number) -> Number
(define (variance lst)
  (if (null? lst)
      0
      (let ([m (mean lst)]
            [n (length lst)])
           (/ (fold-left + 0 (map (lambda (x) (expt (- x m) 2)) lst))
              n))))

;;; within-tolerance : Number x Number x Number -> Boolean
(define (within-tolerance expected actual tolerance)
  (< (abs (- expected actual)) tolerance))

;;; chi-squared-uniform : (List Int) x Int -> Number
;;; Chi-squared test for uniformity across bins.
;;; Returns the chi-squared statistic.
(define (chi-squared-uniform samples num-bins)
  (let* ([n (length samples)]
         [expected (/ n num-bins)]
         [bins (make-vector num-bins 0)]
         [_ (for-each (lambda (s)
                              (let ([bin (modulo s num-bins)])
                                   (vector-set! bins bin (+ 1 (vector-ref bins bin)))))
                      samples)]
         [chi2 (let loop ([i 0] [sum 0])
                    (if (>= i num-bins)
                        sum
                        (let ([observed (vector-ref bins i)])
                             (loop (+ i 1)
                                   (+ sum (/ (expt (- observed expected) 2) expected))))))])
        chi2))

(display "\n")
(display "====\n")
(display "         PRNG Algorithm Tests\n")
(display "====\n")
(display "\n")

;;; ====
;;; Bit Manipulation Tests
;;; ====

(test-group bit-operations
            
            (define-test u32-truncation
              ;; u32 should truncate to 32 bits
              (assert-equal (u32 #xFFFFFFFF) #xFFFFFFFF)
              (assert-equal (u32 #x100000000) 0)
              (assert-equal (u32 #x1FFFFFFFF) #xFFFFFFFF)
              (assert-equal (u32 0) 0))
            
            (define-test u64-truncation
              ;; u64 should truncate to 64 bits
              (assert-equal (u64 #xFFFFFFFFFFFFFFFF) #xFFFFFFFFFFFFFFFF)
              (assert-equal (u64 #x10000000000000000) 0)
              (assert-equal (u64 0) 0))
            
            (define-test rotl32-operations
              ;; Left rotation of 32-bit integers
              (assert-equal (rotl32 1 1) 2)
              (assert-equal (rotl32 1 4) 16)
              (assert-equal (rotl32 1 31) #x80000000)
              (assert-equal (rotl32 #x80000000 1) 1)
              (assert-equal (rotl32 #xF0000000 4) #x0000000F))
            
            (define-test rotr32-operations
              ;; Right rotation of 32-bit integers
              (assert-equal (rotr32 2 1) 1)
              (assert-equal (rotr32 16 4) 1)
              (assert-equal (rotr32 1 1) #x80000000)
              (assert-equal (rotr32 #x0000000F 4) #xF0000000))
            
            (define-test rotl64-operations
              ;; Left rotation of 64-bit integers
              (assert-equal (rotl64 1 1) 2)
              (assert-equal (rotl64 1 63) #x8000000000000000)
              (assert-equal (rotl64 #x8000000000000000 1) 1))
            
            (define-test rotr64-operations
              ;; Right rotation of 64-bit integers
              (assert-equal (rotr64 2 1) 1)
              (assert-equal (rotr64 1 1) #x8000000000000000)))

;;; ====
;;; Splitmix64 Tests
;;; ====

(test-group splitmix64
            
            (define-test splitmix-constructor
              ;; Constructor should create valid splitmix state
              (let ([sm (make-splitmix 12345)])
                   (assert-true (splitmix? sm))
                   (assert-equal (splitmix-state sm) 12345)))
            
            (define-test splitmix-state-update
              ;; Each call should update state
              (let* ([sm0 (make-splitmix 42)]
                     [r1 (splitmix-next sm0)]
                     [sm1 (cdr r1)]
                     [r2 (splitmix-next sm1)]
                     [sm2 (cdr r2)])
                    (assert-true (not (equal? (splitmix-state sm0) (splitmix-state sm1))))
                    (assert-true (not (equal? (splitmix-state sm1) (splitmix-state sm2))))))
            
            (define-test splitmix-seed-reproducibility
              ;; Same seed must produce identical sequences
              (let* ([sm1 (make-splitmix 42)]
                     [sm2 (make-splitmix 42)]
                     [seq1 (let loop ([g sm1] [n 10] [acc '()])
                                (if (= n 0)
                                    (reverse acc)
                                    (let ([r (splitmix-next g)])
                                         (loop (cdr r) (- n 1) (cons (car r) acc)))))]
                     [seq2 (let loop ([g sm2] [n 10] [acc '()])
                                (if (= n 0)
                                    (reverse acc)
                                    (let ([r (splitmix-next g)])
                                         (loop (cdr r) (- n 1) (cons (car r) acc)))))])
                    (assert-equal seq1 seq2)))
            
            (define-test splitmix-different-seeds
              ;; Different seeds should produce different sequences
              (let* ([sm1 (make-splitmix 1)]
                     [sm2 (make-splitmix 2)]
                     [r1 (car (splitmix-next sm1))]
                     [r2 (car (splitmix-next sm2))])
                    (assert-true (not (= r1 r2)))))
            
            (define-test splitmix-state-monad
              ;; State monad interface should work
              (let* ([sm (make-splitmix 999)]
                     [result (run-state splitmix-random sm)])
                    (assert-true (integer? (car result)))
                    (assert-true (splitmix? (cdr result)))))
            
            (define-test splitmix-64bit-output
              ;; Output should be 64-bit integers
              (let* ([sm (make-splitmix 42)]
                     [result (car (splitmix-next sm))])
                    (assert-true (<= 0 result))
                    (assert-true (< result (expt 2 64))))))

;;; ====
;;; PCG Tests
;;; ====

(test-group pcg
            
            (define-test pcg-constructor
              ;; Constructor should create valid PCG state
              (let ([p (make-pcg 12345 1)])
                   (assert-true (pcg? p))))
            
            (define-test pcg-seed-reproducibility
              ;; Same seed and stream should produce identical sequences
              (let* ([p1 (make-pcg 42 1)]
                     [p2 (make-pcg 42 1)]
                     [seq1 (let loop ([g p1] [n 20] [acc '()])
                                (if (= n 0)
                                    (reverse acc)
                                    (let ([r (pcg-next g)])
                                         (loop (cdr r) (- n 1) (cons (car r) acc)))))]
                     [seq2 (let loop ([g p2] [n 20] [acc '()])
                                (if (= n 0)
                                    (reverse acc)
                                    (let ([r (pcg-next g)])
                                         (loop (cdr r) (- n 1) (cons (car r) acc)))))])
                    (assert-equal seq1 seq2)))
            
            (define-test pcg-different-streams
              ;; Different stream IDs should produce different sequences
              (let* ([p1 (make-pcg 42 1)]
                     [p2 (make-pcg 42 2)]
                     [r1 (car (pcg-next p1))]
                     [r2 (car (pcg-next p2))])
                    (assert-true (not (= r1 r2)))))
            
            (define-test pcg-32bit-output
              ;; PCG-XSH-RR produces 32-bit output
              (let* ([p (make-pcg 0 1)]
                     [result (car (pcg-next p))])
                    (assert-true (<= 0 result))
                    (assert-true (< result (expt 2 32)))))
            
            (define-test pcg-state-monad
              ;; State monad interface should work
              (let* ([p (make-pcg 123 1)]
                     [result (run-state pcg-random p)])
                    (assert-true (integer? (car result)))
                    (assert-true (pcg? (cdr result))))))

;;; ====
;;; Xorshift128+ Tests
;;; ====

(test-group xorshift128
            
            (define-test xorshift-constructor
              ;; Constructor should create valid xorshift state
              (let ([xs (make-xorshift128 12345)])
                   (assert-true (xorshift128? xs))))
            
            (define-test xorshift-seed-reproducibility
              ;; Same seed should produce identical sequences
              (let* ([xs1 (make-xorshift128 42)]
                     [xs2 (make-xorshift128 42)]
                     [seq1 (let loop ([g xs1] [n 15] [acc '()])
                                (if (= n 0)
                                    (reverse acc)
                                    (let ([r (xorshift128-next g)])
                                         (loop (cdr r) (- n 1) (cons (car r) acc)))))]
                     [seq2 (let loop ([g xs2] [n 15] [acc '()])
                                (if (= n 0)
                                    (reverse acc)
                                    (let ([r (xorshift128-next g)])
                                         (loop (cdr r) (- n 1) (cons (car r) acc)))))])
                    (assert-equal seq1 seq2)))
            
            (define-test xorshift-non-zero-state
              ;; Xorshift requires non-zero state
              (let* ([xs (make-xorshift128 0)]
                     [s0 (xorshift128-s0 xs)]
                     [s1 (xorshift128-s1 xs)])
                    ;; At least one should be non-zero
                    (assert-true (or (not (= s0 0)) (not (= s1 0))))))
            
            (define-test xorshift-64bit-output
              ;; Xorshift128+ produces 64-bit output
              (let* ([xs (make-xorshift128 999)]
                     [result (car (xorshift128-next xs))])
                    (assert-true (<= 0 result))
                    (assert-true (< result (expt 2 64)))))
            
            (define-test xorshift-state-monad
              ;; State monad interface should work
              (let* ([xs (make-xorshift128 555)]
                     [result (run-state xorshift128-random xs)])
                    (assert-true (integer? (car result)))
                    (assert-true (xorshift128? (cdr result))))))

;;; ====
;;; Random Float Tests
;;; ====

(test-group random-float
            
            (define-test float-range-pcg
              ;; Floats should be in [0, 1)
              (let* ([samples (with-random 42 (random-list 1000 random-float))]
                     [min-val (apply min samples)]
                     [max-val (apply max samples)])
                    (assert-true (>= min-val 0))
                    (assert-true (< max-val 1))))
            
            (define-test float-mean-uniform
              ;; Mean should be approximately 0.5
              (let* ([samples (with-random 12345 (random-list 10000 random-float))]
                     [m (mean samples)])
                    (assert-true (within-tolerance 0.5 m 0.02))))
            
            (define-test float-variance-uniform
              ;; Variance of U[0,1) is 1/12 ~ 0.0833
              (let* ([samples (with-random 42 (random-list 10000 random-float))]
                     [v (variance samples)])
                    (assert-true (within-tolerance (/ 1 12) v 0.01))))
            
            (define-test float-range-custom
              ;; random-float-range should respect bounds
              (let* ([samples (with-random 42 (random-list 1000 (random-float-range 5 10)))]
                     [min-val (apply min samples)]
                     [max-val (apply max samples)])
                    (assert-true (>= min-val 5))
                    (assert-true (< max-val 10)))))

;;; ====
;;; Random Integer Range Tests
;;; ====

(test-group random-int-range
            
            (define-test int-range-bounds
              ;; Integers should be in [lo, hi] inclusive
              (let* ([samples (with-random 42 (random-list 1000 (random-int-range 1 6)))]
                     [min-val (apply min samples)]
                     [max-val (apply max samples)])
                    (assert-true (>= min-val 1))
                    (assert-true (<= max-val 6))))
            
            (define-test int-range-mean
              ;; Mean of 1-6 should be 3.5
              (let* ([samples (with-random 42 (random-list 5000 (random-int-range 1 6)))]
                     [m (mean samples)])
                    (assert-true (within-tolerance 3.5 m 0.15))))
            
            (define-test int-range-uniformity
              ;; Chi-squared test for uniformity
              (let* ([samples (with-random 42 (random-list 6000 (random-int-range 0 5)))]
                     [chi2 (chi-squared-uniform samples 6)])
                    ;; Chi-squared should be small for uniform distribution
                    ;; Critical value for df=5 at p=0.05 is 11.07
                    (assert-true (< chi2 20))))
            
            (define-test int-range-single-value
              ;; When lo = hi, should always return that value
              (let* ([samples (with-random 42 (random-list 100 (random-int-range 42 42)))])
                    (assert-true (andmap (lambda (x) (= x 42)) samples)))))

;;; ====
;;; Random Bool Tests
;;; ====

(test-group random-bool
            
            (define-test bool-approximate-half
              ;; Should be approximately 50% true
              (let* ([samples (with-random 42 (random-list 10000 random-bool))]
                     [true-count (length (filter identity samples))])
                    (assert-true (within-tolerance 5000 true-count 300))))
            
            (define-test bool-both-values
              ;; Should produce both true and false
              (let* ([samples (with-random 42 (random-list 1000 random-bool))]
                     [has-true (if (memq #t samples) #t #f)]
                     [has-false (if (memq #f samples) #t #f)])
                    (assert-true has-true)
                    (assert-true has-false))))

;;; ====
;;; Sampling Utilities Tests
;;; ====

(test-group sampling
            
            (define-test random-element-membership
              ;; Sampled elements should come from the list
              (let* ([lst '(a b c d e)]
                     [samples (with-random 42 (random-list 100 (random-element lst)))])
                    (assert-true (andmap (lambda (x) (memq x lst)) samples))))
            
            (define-test shuffle-preserves-elements
              ;; Shuffle should preserve all elements
              (let* ([lst '(1 2 3 4 5)]
                     [shuffled (with-random 42 (shuffle lst))])
                    (assert-equal (sort-by < shuffled) (sort-by < lst))))
            
            (define-test shuffle-different-seeds
              ;; Different seeds should give different orderings (usually)
              (let* ([lst '(1 2 3 4 5 6 7 8 9 10)]
                     [s1 (with-random 1 (shuffle lst))]
                     [s2 (with-random 2 (shuffle lst))])
                    (assert-true (not (equal? s1 s2)))))
            
            (define-test sample-correct-size
              ;; Sample should return k elements
              (let* ([lst '(1 2 3 4 5 6 7 8 9 10)]
                     [sampled (with-random 42 (sample 3 lst))])
                    (assert-equal (length sampled) 3)))
            
            (define-test sample-from-list
              ;; Sampled elements should be from the original list
              (let* ([lst '(1 2 3 4 5 6 7 8 9 10)]
                     [sampled (with-random 42 (sample 5 lst))])
                    (assert-true (andmap (lambda (x) (member x lst)) sampled))))
            
            (define-test weighted-choice-respects-weights
              ;; Heavy weight should dominate
              (let* ([weighted '((a . 100) (b . 1) (c . 1))]
                     [samples (with-random 42 (random-list 1000 (weighted-choice weighted)))]
                     [a-count (length (filter (lambda (x) (eq? x 'a)) samples))])
                    ;; 'a has 100/102 ~ 98% probability
                    (assert-true (> a-count 900)))))

;;; ====
;;; Generator Advance Tests
;;; ====

(test-group gen-advance
            
            (define-test advance-zero-steps
              ;; Advancing 0 steps should return same generator
              (let* ([p (make-pcg 42 1)]
                     [p2 (gen-advance 0 p)])
                    (assert-equal p p2)))
            
            (define-test advance-deterministic
              ;; Advancing should be equivalent to multiple nexts
              (let* ([p (make-pcg 42 1)]
                     [p-advanced (gen-advance 5 p)]
                     [p-manual (let loop ([g p] [n 5])
                                    (if (= n 0) g (loop (cdr (pcg-next g)) (- n 1))))])
                    (assert-equal (car (pcg-next p-advanced))
                                  (car (pcg-next p-manual))))))

;;; ====
;;; Generator Splitting Tests
;;; ====

(test-group gen-split
            
            (define-test split-produces-pair
              ;; Split should produce two generators
              (let* ([p (make-pcg 42 1)]
                     [split (gen-split p)]
                     [g1 (car split)]
                     [g2 (cdr split)])
                    (assert-true (pcg? g1))
                    (assert-true (pcg? g2))))
            
            (define-test split-children-different
              ;; Split children should produce different sequences
              (let* ([p (make-pcg 42 1)]
                     [split (gen-split p)]
                     [seq1 (with-random-stream (pcg-state (car split)) 1
                                               (random-list 10 random-float))]
                     [seq2 (with-random-stream (pcg-state (cdr split)) 1
                                               (random-list 10 random-float))])
                    (assert-true (not (equal? seq1 seq2)))))
            
            (define-test split-deterministic
              ;; Same input should produce same split
              (let* ([p1 (make-pcg 42 1)]
                     [p2 (make-pcg 42 1)]
                     [split1 (gen-split p1)]
                     [split2 (gen-split p2)])
                    (assert-equal (pcg-state (car split1)) (pcg-state (car split2)))
                    (assert-equal (pcg-state (cdr split1)) (pcg-state (cdr split2))))))

;;; ====
;;; Generator Serialization Tests
;;; ====

(test-group gen-serialization
            
            (define-test serialize-pcg
              ;; PCG should serialize to expected format
              (let* ([p (make-pcg 42 1)]
                     [s (gen-serialize p)])
                    (assert-equal (car s) 'pcg)
                    (assert-equal (length s) 3)))
            
            (define-test serialize-splitmix
              ;; Splitmix should serialize to expected format
              (let* ([sm (make-splitmix 12345)]
                     [s (gen-serialize sm)])
                    (assert-equal (car s) 'splitmix)
                    (assert-equal (length s) 2)))
            
            (define-test serialize-xorshift
              ;; Xorshift should serialize to expected format
              (let* ([xs (make-xorshift128 999)]
                     [s (gen-serialize xs)])
                    (assert-equal (car s) 'xorshift128)
                    (assert-equal (length s) 3)))
            
            (define-test roundtrip-pcg
              ;; Serialize then deserialize should preserve state
              (let* ([p (make-pcg 42 1)]
                     [serialized (gen-serialize p)]
                     [restored (gen-deserialize serialized)]
                     [r1 (car (pcg-next p))]
                     [r2 (car (pcg-next restored))])
                    (assert-equal r1 r2)))
            
            (define-test roundtrip-splitmix
              ;; Serialize then deserialize should preserve state
              (let* ([sm (make-splitmix 12345)]
                     [serialized (gen-serialize sm)]
                     [restored (gen-deserialize serialized)]
                     [r1 (car (splitmix-next sm))]
                     [r2 (car (splitmix-next restored))])
                    (assert-equal r1 r2)))
            
            (define-test roundtrip-xorshift
              ;; Serialize then deserialize should preserve state
              (let* ([xs (make-xorshift128 999)]
                     [serialized (gen-serialize xs)]
                     [restored (gen-deserialize serialized)]
                     [r1 (car (xorshift128-next xs))]
                     [r2 (car (xorshift128-next restored))])
                    (assert-equal r1 r2))))

;;; ====
;;; Random Bytes Tests
;;; ====

(test-group random-bytes
            
            (define-test bytes-correct-length
              ;; Should produce bytevector of specified length
              (let* ([bv (with-random 42 (random-bytes 100))])
                    (assert-equal (bytevector-length bv) 100)))
            
            (define-test bytes-zero-length
              ;; Zero bytes should work
              (let* ([bv (with-random 42 (random-bytes 0))])
                    (assert-equal (bytevector-length bv) 0)))
            
            (define-test bytes-range
              ;; Each byte should be 0-255
              (let* ([bv (with-random 42 (random-bytes 1000))]
                     [bytes (let loop ([i 0] [acc '()])
                                 (if (>= i 1000)
                                     acc
                                     (loop (+ i 1) (cons (bytevector-u8-ref bv i) acc))))])
                    (assert-true (andmap (lambda (b) (and (>= b 0) (<= b 255))) bytes))))
            
            (define-test bytes-deterministic
              ;; Same seed should produce same bytes
              (let* ([bv1 (with-random 42 (random-bytes 50))]
                     [bv2 (with-random 42 (random-bytes 50))])
                    (assert-true (equal? bv1 bv2)))))

;;; ====
;;; Random List/Vector Tests
;;; ====

(test-group random-list-vector
            
            (define-test random-list-length
              ;; Should produce list of correct length
              (let* ([lst (with-random 42 (random-list 100 random-float))])
                    (assert-equal (length lst) 100)))
            
            (define-test random-list-zero
              ;; Zero length should produce empty list
              (let* ([lst (with-random 42 (random-list 0 random-float))])
                    (assert-equal lst '())))
            
            (define-test random-vector-length
              ;; Should produce vector of correct length
              (let* ([vec (with-random 42 (random-vector 100 random-float))])
                    (assert-equal (vector-length vec) 100)))
            
            (define-test random-vector-contents
              ;; Vector contents should match list
              (let* ([lst (with-random 42 (random-list 20 random-float))]
                     [vec (with-random 42 (random-vector 20 random-float))])
                    (assert-equal lst (vector->list vec)))))

;;; ====
;;; Cross-Generator Consistency Tests
;;; ====

(test-group cross-generator
            
            (define-test random-u32-all-generators
              ;; random-u32-from should work with all generator types
              (let* ([pcg-gen (make-pcg 42 1)]
                     [sm-gen (make-splitmix 42)]
                     [xs-gen (make-xorshift128 42)])
                    (assert-true (integer? (car (run-state (random-u32-from pcg-gen) pcg-gen))))
                    (assert-true (integer? (car (run-state (random-u32-from sm-gen) sm-gen))))
                    (assert-true (integer? (car (run-state (random-u32-from xs-gen) xs-gen))))))
            
            (define-test random-u64-all-generators
              ;; random-u64-from should work with all generator types
              (let* ([pcg-gen (make-pcg 42 1)]
                     [sm-gen (make-splitmix 42)]
                     [xs-gen (make-xorshift128 42)])
                    (assert-true (integer? (car (run-state (random-u64-from pcg-gen) pcg-gen))))
                    (assert-true (integer? (car (run-state (random-u64-from sm-gen) sm-gen))))
                    (assert-true (integer? (car (run-state (random-u64-from xs-gen) xs-gen)))))))

;;; ====
;;; Print Summary
;;; ====

(display "\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)
(newline)

(if (= *tests-failed* 0)
    (display "[SUCCESS] All PRNG tests passed.\n")
    (display "[FAILURE] Some PRNG tests failed.\n"))
