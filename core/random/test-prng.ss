;;; core/random/test-prng.ss — Tests for PRNG Module
;;;
;;; Comprehensive tests for pseudorandom number generators.

(load "core/random/prng.ss")

;;; ============================================================
;;; Test Framework
;;; ============================================================

(define *test-count* 0)
(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expected actual)
  (set! *test-count* (+ *test-count* 1))
  (if (equal? expected actual)
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "  ✓ ")
       (display name)
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "  ✗ ")
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

(define (test-approx name expected actual tolerance)
  (set! *test-count* (+ *test-count* 1))
  (let ([diff (abs (- expected actual))])
       (if (< diff tolerance)
           (begin
            (set! *pass-count* (+ *pass-count* 1))
            (display "  ✓ ")
            (display name)
            (newline))
           (begin
            (set! *fail-count* (+ *fail-count* 1))
            (display "  ✗ ")
            (display name)
            (newline)
            (display "    Expected: ")
            (write expected)
            (display " ± ")
            (write tolerance)
            (newline)
            (display "    Actual:   ")
            (write actual)
            (display " (diff: ")
            (write diff)
            (display ")")
            (newline)))))

(define (run-tests)
  (display "\n=== PRNG Test Suite ===\n")
  
  ;;; ============================================================
  ;;; Bit Manipulation Tests
  ;;; ============================================================
  
  (display "\n--- Bit Manipulation ---\n")
  
  ;; u32 truncation
  (test "u32: truncates to 32 bits" (- (expt 2 32) 1) (u32 (- (expt 2 32) 1)))
  (test "u32: wraps overflow" 0 (u32 (expt 2 32)))
  (test "u32: handles large numbers" 1 (u32 (+ (expt 2 32) 1)))
  (test "u32: zero unchanged" 0 (u32 0))
  
  ;; u64 truncation
  (test "u64: truncates to 64 bits" (- (expt 2 64) 1) (u64 (- (expt 2 64) 1)))
  (test "u64: wraps overflow" 0 (u64 (expt 2 64)))
  (test "u64: handles large numbers" 1 (u64 (+ (expt 2 64) 1)))
  
  ;; 32-bit rotations
  (test "rotl32: rotate left by 1" 2 (rotl32 1 1))
  (test "rotl32: rotate left wraps" 1 (rotl32 (expt 2 31) 1))
  (test "rotl32: rotate by 0 unchanged" 42 (rotl32 42 0))
  (test "rotl32: rotate by 32 unchanged" 42 (rotl32 42 32))
  (test "rotr32: rotate right by 1" (expt 2 31) (rotr32 1 1))
  (test "rotr32: rotate right wraps" 1 (rotr32 2 1))
  
  ;; 64-bit rotations
  (test "rotl64: rotate left by 1" 2 (rotl64 1 1))
  (test "rotl64: rotate left wraps" 1 (rotl64 (expt 2 63) 1))
  (test "rotr64: rotate right by 1" (expt 2 63) (rotr64 1 1))
  
  ;;; ============================================================
  ;;; Splitmix64 Tests
  ;;; ============================================================
  
  (display "\n--- Splitmix64 ---\n")
  
  ;; Constructor
  (test-true "make-splitmix: creates splitmix" (splitmix? (make-splitmix 0)))
  (test "make-splitmix: stores seed" 42 (splitmix-state (make-splitmix 42)))
  (test "make-splitmix: truncates large seeds"
        (u64 (+ (expt 2 64) 5))
        (splitmix-state (make-splitmix (+ (expt 2 64) 5))))
  
  ;; Determinism
  (let* ([sm1 (make-splitmix 12345)]
         [sm2 (make-splitmix 12345)]
         [r1 (splitmix-next sm1)]
         [r2 (splitmix-next sm2)])
        (test "splitmix: deterministic - same seed same output"
              (car r1) (car r2)))
  
  ;; Different seeds give different outputs
  (let* ([sm1 (make-splitmix 1)]
         [sm2 (make-splitmix 2)]
         [r1 (car (splitmix-next sm1))]
         [r2 (car (splitmix-next sm2))])
        (test-false "splitmix: different seeds give different output"
                    (= r1 r2)))
  
  ;; Output is 64-bit
  (let* ([sm (make-splitmix 42)]
         [val (car (splitmix-next sm))])
        (test-true "splitmix: output is non-negative" (>= val 0))
        (test-true "splitmix: output < 2^64" (< val (expt 2 64))))
  
  ;; State monad version
  (let* ([sm (make-splitmix 99)]
         [result (run-state splitmix-random sm)]
         [value (car result)]
         [new-state (cdr result)])
        (test-true "splitmix-random: returns value" (integer? value))
        (test-true "splitmix-random: returns new state" (splitmix? new-state)))
  
  ;;; ============================================================
  ;;; PCG Tests
  ;;; ============================================================
  
  (display "\n--- PCG ---\n")
  
  ;; Constructor
  (test-true "make-pcg: creates pcg" (pcg? (make-pcg 0 1)))
  (test-true "make-pcg: inc is odd" (odd? (pcg-inc (make-pcg 0 0))))
  (test-true "make-pcg: inc is odd (stream 5)" (odd? (pcg-inc (make-pcg 0 5))))
  
  ;; Determinism
  (let* ([p1 (make-pcg 12345 1)]
         [p2 (make-pcg 12345 1)]
         [r1 (pcg-next p1)]
         [r2 (pcg-next p2)])
        (test "pcg: deterministic - same seed/stream same output"
              (car r1) (car r2)))
  
  ;; Different streams give different outputs
  (let* ([p1 (make-pcg 42 1)]
         [p2 (make-pcg 42 2)]
         [r1 (car (pcg-next p1))]
         [r2 (car (pcg-next p2))])
        (test-false "pcg: different streams give different output"
                    (= r1 r2)))
  
  ;; Output is 32-bit
  (let* ([p (make-pcg 42 1)]
         [val (car (pcg-next p))])
        (test-true "pcg: output is non-negative" (>= val 0))
        (test-true "pcg: output < 2^32" (< val (expt 2 32))))
  
  ;; State monad version
  (let* ([p (make-pcg 99 1)]
         [result (run-state pcg-random p)]
         [value (car result)]
         [new-state (cdr result)])
        (test-true "pcg-random: returns value" (integer? value))
        (test-true "pcg-random: returns new state" (pcg? new-state)))
  
  ;;; ============================================================
  ;;; Xorshift128+ Tests
  ;;; ============================================================
  
  (display "\n--- Xorshift128+ ---\n")
  
  ;; Constructor
  (test-true "make-xorshift128: creates xorshift128"
             (xorshift128? (make-xorshift128 0)))
  
  ;; Non-zero state guarantee
  (let ([xs (make-xorshift128 0)])
       (test-true "make-xorshift128: s0 non-zero" (not (= 0 (xorshift128-s0 xs))))
       (test-true "make-xorshift128: s1 non-zero" (not (= 0 (xorshift128-s1 xs)))))
  
  ;; Determinism
  (let* ([xs1 (make-xorshift128 12345)]
         [xs2 (make-xorshift128 12345)]
         [r1 (xorshift128-next xs1)]
         [r2 (xorshift128-next xs2)])
        (test "xorshift128: deterministic - same seed same output"
              (car r1) (car r2)))
  
  ;; Output is 64-bit
  (let* ([xs (make-xorshift128 42)]
         [val (car (xorshift128-next xs))])
        (test-true "xorshift128: output is non-negative" (>= val 0))
        (test-true "xorshift128: output < 2^64" (< val (expt 2 64))))
  
  ;;; ============================================================
  ;;; Uniform Random Number Tests
  ;;; ============================================================
  
  (display "\n--- Uniform Random Numbers ---\n")
  
  ;; random-float in [0, 1)
  (let* ([gen (make-pcg 42 1)]
         [floats (let loop ([g gen] [n 100] [acc '()])
                      (if (= n 0)
                          acc
                          (let ([result (run-state random-float g)])
                               (loop (cdr result) (- n 1) (cons (car result) acc)))))])
        (test-true "random-float: all values >= 0"
                   (for-all (lambda (x) (>= x 0)) floats))
        (test-true "random-float: all values < 1"
                   (for-all (lambda (x) (< x 1)) floats)))
  
  ;; random-float-range
  (let* ([gen (make-pcg 42 1)]
         [floats (let loop ([g gen] [n 100] [acc '()])
                      (if (= n 0)
                          acc
                          (let ([result (run-state (random-float-range 5.0 10.0) g)])
                               (loop (cdr result) (- n 1) (cons (car result) acc)))))])
        (test-true "random-float-range: all values >= lo"
                   (for-all (lambda (x) (>= x 5.0)) floats))
        (test-true "random-float-range: all values < hi"
                   (for-all (lambda (x) (< x 10.0)) floats)))
  
  ;; random-int-range bounds
  (let* ([gen (make-pcg 42 1)]
         [ints (let loop ([g gen] [n 100] [acc '()])
                    (if (= n 0)
                        acc
                        (let ([result (run-state (random-int-range 1 6) g)])
                             (loop (cdr result) (- n 1) (cons (car result) acc)))))])
        (test-true "random-int-range: all values >= lo"
                   (for-all (lambda (x) (>= x 1)) ints))
        (test-true "random-int-range: all values <= hi"
                   (for-all (lambda (x) (<= x 6)) ints)))
  
  ;; random-bool produces both values
  (let* ([gen (make-pcg 42 1)]
         [bools (let loop ([g gen] [n 100] [acc '()])
                     (if (= n 0)
                         acc
                         (let ([result (run-state random-bool g)])
                              (loop (cdr result) (- n 1) (cons (car result) acc)))))])
        (test-true "random-bool: produces #t" (and (member #t bools) #t))
        (test-true "random-bool: produces #f" (and (member #f bools) #t)))
  
  ;;; ============================================================
  ;;; Sampling Utilities Tests
  ;;; ============================================================
  
  (display "\n--- Sampling Utilities ---\n")
  
  ;; random-element
  (let* ([lst '(a b c d e)]
         [gen (make-pcg 42 1)]
         [elements (let loop ([g gen] [n 50] [acc '()])
                        (if (= n 0)
                            acc
                            (let ([result (run-state (random-element lst) g)])
                                 (loop (cdr result) (- n 1) (cons (car result) acc)))))])
        (test-true "random-element: all values in list"
                   (for-all (lambda (x) (and (member x lst) #t)) elements)))
  
  ;; shuffle preserves elements
  (let* ([lst '(1 2 3 4 5)]
         [gen (make-pcg 42 1)]
         [shuffled (car (run-state (shuffle lst) gen))])
        (test "shuffle: same length" (length lst) (length shuffled))
        (test "shuffle: same elements (sorted)"
              (sort < lst)
              (sort < shuffled)))
  
  ;; sample k elements
  (let* ([lst '(a b c d e f g h i j)]
         [gen (make-pcg 42 1)]
         [sampled (car (run-state (sample 3 lst) gen))])
        (test "sample: correct count" 3 (length sampled))
        (test-true "sample: all elements from original"
                   (for-all (lambda (x) (and (member x lst) #t)) sampled)))
  
  ;; sample without replacement
  (let* ([lst '(1 2 3 4 5)]
         [gen (make-pcg 42 1)]
         [sampled (car (run-state (sample 3 lst) gen))])
        (test "sample: no duplicates"
              (length sampled)
              (length (remove-duplicates sampled))))
  
  ;; weighted-choice respects weights
  (let* ([weighted '((a . 0.9) (b . 0.05) (c . 0.05))]
         [gen (make-pcg 42 1)]
         [choices (let loop ([g gen] [n 100] [acc '()])
                       (if (= n 0)
                           acc
                           (let ([result (run-state (weighted-choice weighted) g)])
                                (loop (cdr result) (- n 1) (cons (car result) acc)))))]
         [a-count (length (filter (lambda (x) (eq? x 'a)) choices))])
        ;; 'a should appear most often (roughly 90% of time)
        (test-true "weighted-choice: high-weight item appears often"
                   (> a-count 60)))
  
  ;;; ============================================================
  ;;; Generator Utilities Tests
  ;;; ============================================================
  
  (display "\n--- Generator Utilities ---\n")
  
  ;; gen-advance
  (let* ([gen1 (make-pcg 42 1)]
         [gen2 (make-pcg 42 1)]
         [advanced (gen-advance 10 gen1)]
         [manual (let loop ([g gen2] [n 10])
                      (if (= n 0) g (loop (cdr (pcg-next g)) (- n 1))))])
        (test "gen-advance: advances correctly"
              (pcg-state advanced)
              (pcg-state manual)))
  
  ;; gen-split produces two generators
  (let* ([gen (make-pcg 42 1)]
         [split (gen-split gen)]
         [gen1 (car split)]
         [gen2 (cdr split)])
        (test-true "gen-split: first is pcg" (pcg? gen1))
        (test-true "gen-split: second is pcg" (pcg? gen2))
        ;; Split generators should produce different sequences
        (let ([r1 (car (pcg-next gen1))]
              [r2 (car (pcg-next gen2))])
             (test-false "gen-split: children produce different values"
                         (= r1 r2))))
  
  ;;; ============================================================
  ;;; Serialization Tests
  ;;; ============================================================
  
  (display "\n--- Serialization ---\n")
  
  ;; PCG round-trip
  (let* ([gen (make-pcg 12345 67)]
         [serialized (gen-serialize gen)]
         [restored (gen-deserialize serialized)])
        (test "pcg: round-trip state" (pcg-state gen) (pcg-state restored))
        (test "pcg: round-trip inc" (pcg-inc gen) (pcg-inc restored)))
  
  ;; Splitmix round-trip
  (let* ([gen (make-splitmix 99999)]
         [serialized (gen-serialize gen)]
         [restored (gen-deserialize serialized)])
        (test "splitmix: round-trip state"
              (splitmix-state gen)
              (splitmix-state restored)))
  
  ;; Xorshift128 round-trip
  (let* ([gen (make-xorshift128 77777)]
         [serialized (gen-serialize gen)]
         [restored (gen-deserialize serialized)])
        (test "xorshift128: round-trip s0"
              (xorshift128-s0 gen)
              (xorshift128-s0 restored))
        (test "xorshift128: round-trip s1"
              (xorshift128-s1 gen)
              (xorshift128-s1 restored)))
  
  ;; String serialization round-trip
  (let* ([gen (make-pcg 42 1)]
         [str (gen->string gen)]
         [restored (string->gen str)]
         [val-orig (car (pcg-next gen))]
         [val-restored (car (pcg-next restored))])
        (test "string round-trip: produces same values"
              val-orig val-restored))
  
  ;;; ============================================================
  ;;; Safe Parsing Tests
  ;;; ============================================================
  
  (display "\n--- Safe Parsing ---\n")
  
  ;; Valid inputs
  (test "safe-parse: pcg" '(pcg 123 456)
        (safe-parse-gen-state "(pcg 123 456)"))
  (test "safe-parse: splitmix" '(splitmix 789)
        (safe-parse-gen-state "(splitmix 789)"))
  (test "safe-parse: xorshift128" '(xorshift128 111 222)
        (safe-parse-gen-state "(xorshift128 111 222)"))
  
  ;; With whitespace
  (test "safe-parse: handles leading/trailing whitespace"
        '(pcg 1 2)
        (safe-parse-gen-state "  (pcg 1 2)  "))
  (test "safe-parse: handles internal whitespace"
        '(pcg 1 2)
        (safe-parse-gen-state "(pcg   1   2)"))
  
  ;;; ============================================================
  ;;; Convenience Functions Tests
  ;;; ============================================================
  
  (display "\n--- Convenience Functions ---\n")
  
  ;; with-random
  (test "with-random: deterministic"
        (with-random 42 random-float)
        (with-random 42 random-float))
  
  ;; with-random-stream
  (let ([v1 (with-random-stream 42 1 random-float)]
        [v2 (with-random-stream 42 2 random-float)])
       (test-false "with-random-stream: different streams differ"
                   (= v1 v2)))
  
  ;; random-list
  (let* ([gen (make-pcg 42 1)]
         [lst (car (run-state (random-list 5 random-float) gen))])
        (test "random-list: correct length" 5 (length lst))
        (test-true "random-list: all floats"
                   (for-all number? lst)))
  
  ;; random-vector
  (let* ([gen (make-pcg 42 1)]
         [vec (car (run-state (random-vector 5 random-float) gen))])
        (test "random-vector: correct length" 5 (vector-length vec))
        (test-true "random-vector: is vector" (vector? vec)))
  
  ;; random-bytes
  (let* ([gen (make-pcg 42 1)]
         [bv (car (run-state (random-bytes 16) gen))])
        (test "random-bytes: correct length" 16 (bytevector-length bv))
        (test-true "random-bytes: is bytevector" (bytevector? bv)))
  
  ;;; ============================================================
  ;;; Summary
  ;;; ============================================================
  
  (display "\n=== Test Summary ===\n")
  (display "Total:  ")
  (display *test-count*)
  (newline)
  (display "Passed: ")
  (display *pass-count*)
  (newline)
  (display "Failed: ")
  (display *fail-count*)
  (newline)
  
  (if (= *fail-count* 0)
      (display "\n✓ All tests passed!\n")
      (begin
       (display "\n✗ Some tests failed.\n")
       (exit 1))))

;;; Helper functions
(define (for-all pred lst)
  (cond
   [(null? lst) #t]
   [(pred (car lst)) (for-all pred (cdr lst))]
   [else #f]))

(define (remove-duplicates lst)
  (if (null? lst)
      '()
      (cons (car lst)
            (remove-duplicates (filter (lambda (x) (not (equal? x (car lst))))
                                       (cdr lst))))))

;; Run tests when loaded
(run-tests)
