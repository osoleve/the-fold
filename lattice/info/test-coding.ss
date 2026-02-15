;;; Test harness for core/info-theory/coding.ss
;;;
;;; Tests coding theory functions: Huffman, arithmetic, LZ78, channel coding.

(load "core/lang/module.ss")
(require 'coding)

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
;;; Huffman Coding Tests
;;; ====
(test-section "Huffman Coding")

;; Simple probability distribution
(define test-probs '((a . 0.5) (b . 0.25) (c . 0.125) (d . 0.125)))
(define test-tree (build-huffman-tree test-probs))
(define test-codes (huffman-codes test-tree))

(test-true "huffman tree built" (not (null? test-tree)))
(test "huffman codes count" 4 (length test-codes))

;; More probable symbols should have shorter codes
(define a-code (cdr (assq 'a test-codes)))
(define d-code (cdr (assq 'd test-codes)))
(test-true "a shorter than d" (<= (string-length a-code) (string-length d-code)))

;; Encode and decode round-trip
(define test-msg '(a b a c a d a))
(define encoded (huffman-encode test-msg test-tree))
(test-true "encoded is string" (string? encoded))
(test-true "encoded non-empty" (> (string-length encoded) 0))

(define decoded (huffman-decode encoded test-tree))
(test "huffman round-trip" test-msg decoded)

;; Average code length
(define avg-len (huffman-average-length test-codes test-probs))
(test-true "avg length > entropy minimum" (>= avg-len (entropy (map cdr test-probs))))
(test-true "avg length < entropy + 1" (< avg-len (+ (entropy (map cdr test-probs)) 1)))

;; Efficiency
(define eff (huffman-efficiency test-codes test-probs))
(test-true "huffman efficiency > 0.9" (> eff 0.9))
(test-true "huffman efficiency <= 1" (<= eff 1.0))

;; Single symbol
(define single-tree (build-huffman-tree '((x . 1.0))))
(test-true "single symbol tree" (huffman-leaf? single-tree))

;; Two symbols
(define two-probs '((a . 0.5) (b . 0.5)))
(define two-tree (build-huffman-tree two-probs))
(define two-codes (huffman-codes two-tree))
(test "two symbol codes" 2 (length two-codes))

;;; ====
;;; Arithmetic Coding Tests
;;; ====
(test-section "Arithmetic Coding")

;; Encode to interval
(define arith-probs '((a . 0.5) (b . 0.3) (c . 0.2)))
(define arith-msg '(a b a))
(define arith-interval (arithmetic-encode arith-msg arith-probs))

(test-true "interval is pair" (pair? arith-interval))
(test-true "interval low < high" (< (car arith-interval) (cdr arith-interval)))
(test-true "interval in [0,1)" (and (>= (car arith-interval) 0)
                                    (< (cdr arith-interval) 1)))

;; Decode from interval
(define arith-decoded (arithmetic-decode arith-interval arith-probs 3))
(test "arithmetic round-trip" arith-msg arith-decoded)

;; Code length
(define arith-len (arithmetic-code-length arith-interval))
(test-true "code length positive" (> arith-len 0))

;; Cumulative probabilities
(define cumul (build-cumulative-probs arith-probs))
(test "cumul entries" 3 (length cumul))
(test-approx "cumul first starts at 0" 0.0 (cadr (car cumul)) 0.001)

;; Empty message
(define empty-interval (arithmetic-encode '() arith-probs))
(test-approx "empty msg low" 0.0 (car empty-interval) 0.001)
(test-approx "empty msg high" 1.0 (cdr empty-interval) 0.001)

;;; ====
;;; Lempel-Ziv Tests
;;; ====
(test-section "Lempel-Ziv Compression")

;; Simple message
(define lz-msg '(a b a b a b c))
(define lz-encoded (lz78-encode lz-msg))
(test-true "lz78 encoded is list" (list? lz-encoded))
(test-true "lz78 encoded shorter" (<= (length lz-encoded) (length lz-msg)))

;; Decode
(define lz-decoded (lz78-decode lz-encoded))
(test "lz78 round-trip" lz-msg lz-decoded)

;; Repetitive message should compress well
(define rep-msg '(a a a a a a a a a a))
(define rep-encoded (lz78-encode rep-msg))
(test-true "repetitive compresses" (< (length rep-encoded) (length rep-msg)))

;; Prefix match
(test "prefix match full" 3 (prefix-match-length '(a b c) '(a b c d)))
(test "prefix match partial" 2 (prefix-match-length '(a b x) '(a b c)))
(test "prefix match none" 0 (prefix-match-length '(x) '(a)))

;; Empty message
(test "empty lz78" '() (lz78-encode '()))
(test "empty lz78 decode" '() (lz78-decode '()))

;;; ====
;;; Run-Length Encoding Tests
;;; ====
(test-section "Run-Length Encoding")

(define rle-msg '(a a a b b c c c c))
(define rle-encoded (rle-encode rle-msg))
(test "rle encoded" '((a . 3) (b . 2) (c . 4)) rle-encoded)

(define rle-decoded (rle-decode rle-encoded))
(test "rle round-trip" rle-msg rle-decoded)

;; Single symbol
(test "rle single" '((x . 1)) (rle-encode '(x)))

;; No runs
(test "rle no runs" '((a . 1) (b . 1) (c . 1)) (rle-encode '(a b c)))

;; Empty
(test "rle empty" '() (rle-encode '()))
(test "rle decode empty" '() (rle-decode '()))

;; Replicate helper
(test "replicate" '(x x x) (replicate 3 'x))
(test "replicate 0" '() (replicate 0 'x))

;;; ====
;;; Parity Check Tests
;;; ====
(test-section "Parity Check")

(define bits '(1 0 1 1))
(define parity-encoded (parity-encode bits))
(test "parity length" 5 (length parity-encoded))

;; Even parity: sum should be even
(test "parity check valid" #t (parity-check parity-encoded))

;; Flip a bit: should fail
(define corrupted (cons (bitwise-xor (car parity-encoded) 1) (cdr parity-encoded)))
(test "parity check invalid" #f (parity-check corrupted))

;; Decode
(test "parity decode" bits (parity-decode parity-encoded))

;; All zeros
(test "parity all zeros" '(0 0 0 0) (parity-encode '(0 0 0)))

;; All ones (odd count)
(define ones-enc (parity-encode '(1 1 1)))
(test "parity check ones" #t (parity-check ones-enc))

;;; ====
;;; Repetition Code Tests
;;; ====
(test-section "Repetition Code")

(define rep-bits '(1 0 1))
(define rep3-encoded (repetition-encode rep-bits 3))
(test "rep3 length" 9 (length rep3-encoded))
(test "rep3 encoded" '(1 1 1 0 0 0 1 1 1) rep3-encoded)

(define rep3-decoded (repetition-decode rep3-encoded 3))
(test "rep3 round-trip" rep-bits rep3-decoded)

;; Error correction
(define rep3-error '(1 0 1 0 0 0 0 1 1))  ; One error in each group
(test "rep3 correct errors" rep-bits (repetition-decode rep3-error 3))

;; Majority vote
(test "majority 1s" 1 (majority-vote '(1 1 0)))
(test "majority 0s" 0 (majority-vote '(0 0 1)))
(test "majority tie-break" 0 (majority-vote '(0 1)))  ; 0 wins when equal

;;; ====
;;; Hamming(7,4) Tests
;;; ====
(test-section "Hamming(7,4) Code")

(define data-bits '(1 0 1 1))
(define hamming-enc (hamming74-encode data-bits))
(test "hamming encode length" 7 (length hamming-enc))

;; No error: syndrome = 0
(test "hamming no error syndrome" 0 (hamming74-syndrome hamming-enc))

;; Decode without error
(define hamming-dec (hamming74-decode hamming-enc))
(test "hamming round-trip" data-bits hamming-dec)

;; Introduce single-bit error at each position and verify correction
(define (test-error-correction pos)
  (let* ([corrupted (flip-bit-at hamming-enc pos)]
         [decoded (hamming74-decode corrupted)])
        (equal? data-bits decoded)))

(test-true "hamming correct pos 0" (test-error-correction 0))
(test-true "hamming correct pos 1" (test-error-correction 1))
(test-true "hamming correct pos 2" (test-error-correction 2))
(test-true "hamming correct pos 3" (test-error-correction 3))
(test-true "hamming correct pos 4" (test-error-correction 4))
(test-true "hamming correct pos 5" (test-error-correction 5))
(test-true "hamming correct pos 6" (test-error-correction 6))

;; Different data
(define data2 '(0 0 0 0))
(define enc2 (hamming74-encode data2))
(test "hamming all zeros" '(0 0 0 0 0 0 0) enc2)
(test "hamming decode zeros" data2 (hamming74-decode enc2))

(define data3 '(1 1 1 1))
(define enc3 (hamming74-encode data3))
(test "hamming decode ones" data3 (hamming74-decode enc3))

;;; ====
;;; Code Properties Tests
;;; ====
(test-section "Code Properties")

;; Hamming distance
(test "hamming dist same" 0 (hamming-distance '(1 0 1) '(1 0 1)))
(test "hamming dist diff 1" 1 (hamming-distance '(1 0 1) '(1 0 0)))
(test "hamming dist diff 2" 2 (hamming-distance '(1 0 1) '(0 0 0)))
(test "hamming dist diff all" 3 (hamming-distance '(1 1 1) '(0 0 0)))

;; Minimum distance of Hamming(7,4) = 3
(define hamming-codewords
  (list (hamming74-encode '(0 0 0 0))
        (hamming74-encode '(0 0 0 1))
        (hamming74-encode '(0 0 1 0))
        (hamming74-encode '(0 0 1 1))))
(test-true "hamming(7,4) min dist >= 3" (>= (minimum-distance hamming-codewords) 3))

;; Code rate
(test-approx "hamming rate" (/ 4 7) (code-rate 4 7) 0.01)
(test-approx "rep3 rate" (/ 1 3) (code-rate 1 3) 0.01)

;; Error correcting capability
(test "1-error correcting" 1 (error-correcting-capability 3))
(test "2-error correcting" 2 (error-correcting-capability 5))

;; Error detecting capability
(test "2-error detecting" 2 (error-detecting-capability 3))

;;; ====
;;; Compression Metrics Tests
;;; ====
(test-section "Compression Metrics")

(test-approx "compression ratio 2:1" 2.0 (compression-ratio 100 50) 0.01)
(test-approx "compression ratio 1:1" 1.0 (compression-ratio 100 100) 0.01)

(test-approx "space savings 50%" 0.5 (space-savings 100 50) 0.01)
(test-approx "space savings 0%" 0.0 (space-savings 100 100) 0.01)

;;; ====
;;; Edge Cases
;;; ====
(test-section "Edge Cases")

;; Flip bit helper
(test "flip bit" '(0 1 0) (flip-bit-at '(1 1 0) 0))
(test "flip bit mid" '(1 0 0) (flip-bit-at '(1 1 0) 1))

;; Take/drop
(test "take-n" '(a b) (take-n 2 '(a b c d)))
(test "take-n more" '(a b) (take-n 5 '(a b)))
(test "drop-n" '(c d) (drop-n 2 '(a b c d)))
(test "drop-n all" '() (drop-n 5 '(a b)))

;; Bitwise xor
(test "xor 0 0" 0 (bitwise-xor 0 0))
(test "xor 0 1" 1 (bitwise-xor 0 1))
(test "xor 1 0" 1 (bitwise-xor 1 0))
(test "xor 1 1" 0 (bitwise-xor 1 1))

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
    (display "+ All coding theory tests passed!\n")
    (begin
     (display "x Some tests failed!\n")
     (exit 1)))
