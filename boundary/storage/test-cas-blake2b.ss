;;; test-cas-blake2b.ss — Tests for BLAKE2b CAS extension

(load "boundary/storage/cas-blake2b.ss")

(define *test-count* 0)
(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expected actual)
  (set! *test-count* (+ *test-count* 1))
  (if (equal? expected actual)
      (begin
        (set! *pass-count* (+ *pass-count* 1))
        (display "✓ ")
        (display name)
        (newline))
      (begin
        (set! *fail-count* (+ *fail-count* 1))
        (display "✗ ")
        (display name)
        (newline)
        (display "  Expected: ")
        (write expected)
        (newline)
        (display "  Actual:   ")
        (write actual)
        (newline))))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

(display "\n=== BLAKE2b CAS Extension Tests ===\n\n")

;;; ====
;;; Version Byte Tests
;;; ====

(display "--- Version Bytes ---\n")

(test "blake2b-alpha version" #x10 address-version-blake2b-alpha)
(test "blake2b-algebraic version" #x11 address-version-blake2b-algebraic)
(test "blake2b-v2 version" #x12 address-version-blake2b-v2)
(test "blake2b-v3 version" #x13 address-version-blake2b-v3)

;;; ====
;;; Hash Block Tests
;;; ====

(display "\n--- Hash Block ---\n")

(let* ([blk (make-block 'test (string->utf8 "hello world") empty-refs)]
       [sha-addr (hash-block blk)]
       [blake-addr (hash-block-blake2b blk)])
  (test "SHA-256 address length" address-size (bytevector-length sha-addr))
  (test "BLAKE2b address length" address-size (bytevector-length blake-addr))
  (test "SHA-256 version byte" #x00 (address-version-byte sha-addr))
  (test "BLAKE2b version byte" #x10 (address-version-byte blake-addr))
  (test-false "Different algorithms produce different hashes"
              (equal? sha-addr blake-addr)))

;;; ====
;;; Algorithm Detection
;;; ====

(display "\n--- Algorithm Detection ---\n")

(let* ([blk (make-block 'test (string->utf8 "test") empty-refs)]
       [sha-addr (hash-block blk)]
       [blake-addr (hash-block-blake2b blk)])
  (test-true "address-sha256? on SHA-256 address" (address-sha256? sha-addr))
  (test-false "address-sha256? on BLAKE2b address" (address-sha256? blake-addr))
  (test-false "address-blake2b? on SHA-256 address" (address-blake2b? sha-addr))
  (test-true "address-blake2b? on BLAKE2b address" (address-blake2b? blake-addr))
  (test "address-hash-algorithm SHA-256" 'sha256 (address-hash-algorithm sha-addr))
  (test "address-hash-algorithm BLAKE2b" 'blake2b (address-hash-algorithm blake-addr)))

;;; ====
;;; Normalization Level Detection
;;; ====

(display "\n--- Normalization Levels ---\n")

(let ([expr '(lambda (x) (+ x 1))])
  (test "SHA-256 alpha normalization level" 'alpha
        (address-normalization-level (hash-sexpr 'fn expr)))
  (test "SHA-256 algebraic normalization level" 'algebraic
        (address-normalization-level (hash-sexpr-algebraic 'fn expr)))
  (test "SHA-256 v2 normalization level" 'v2
        (address-normalization-level (hash-sexpr-v2 'fn expr)))
  (test "SHA-256 v3 normalization level" 'v3
        (address-normalization-level (hash-sexpr-v3 'fn expr)))
  (test "BLAKE2b alpha normalization level" 'alpha
        (address-normalization-level (hash-sexpr-blake2b 'fn expr)))
  (test "BLAKE2b algebraic normalization level" 'algebraic
        (address-normalization-level (hash-sexpr-blake2b-algebraic 'fn expr)))
  (test "BLAKE2b v2 normalization level" 'v2
        (address-normalization-level (hash-sexpr-blake2b-v2 'fn expr)))
  (test "BLAKE2b v3 normalization level" 'v3
        (address-normalization-level (hash-sexpr-blake2b-v3 'fn expr))))

;;; ====
;;; α-Equivalence Tests
;;; ====

(display "\n--- α-Equivalence ---\n")

;; Same semantic expression with different variable names should hash identically
;; Note: normalizer uses 'fn' not 'lambda' for function abstraction
(let* ([expr1 '(fn (x) (+ x 1))]
       [expr2 '(fn (y) (+ y 1))]
       [blake1 (hash-sexpr-blake2b 'fn expr1)]
       [blake2 (hash-sexpr-blake2b 'fn expr2)])
  (test "α-equivalent expressions produce same BLAKE2b hash"
        blake1 blake2))

;; Different expressions should produce different hashes
(let* ([expr1 '(fn (x) (+ x 1))]
       [expr2 '(fn (x) (+ x 2))]
       [blake1 (hash-sexpr-blake2b 'fn expr1)]
       [blake2 (hash-sexpr-blake2b 'fn expr2)])
  (test-false "Different expressions produce different BLAKE2b hashes"
              (equal? blake1 blake2)))

;;; ====
;;; Consistency Tests
;;; ====

(display "\n--- Consistency ---\n")

;; Same input should always produce same output (deterministic)
(let* ([expr '(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))]
       [hash1 (hash-sexpr-blake2b 'fn expr)]
       [hash2 (hash-sexpr-blake2b 'fn expr)])
  (test "BLAKE2b hash is deterministic" hash1 hash2))

;;; ====
;;; Summary
;;; ====

(newline)
(display "=== Test Summary ===\n")
(display (format "Total:  ~a\n" *test-count*))
(display (format "Passed: ~a\n" *pass-count*))
(display (format "Failed: ~a\n" *fail-count*))
(newline)
(if (= *fail-count* 0)
    (display "✓ All tests passed!\n")
    (display "✗ Some tests failed.\n"))
