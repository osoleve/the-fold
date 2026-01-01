;;; Test harness for cas.ss

(load "core/cas.ss")
(load "core/normalize.ss")

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (display "✓")
      (begin
       (display "✗
    expected: ")
       (display expected)
       (display "
    got: ")
       (display actual)))
  (newline))

(display "Content-Addressed Store Tests
")
(display "==============================

")

;;; Test 1: Basic store and fetch
(display "Test 1: Store and fetch
")
(define blk1 (make-block 'test (string->utf8 "hello") empty-refs))
(define hash1 (store! blk1))
(display "  stored hash: ") (display (hash->hex hash1)) (newline)
(test "address length" address-size (bytevector-length hash1))
(test "address version byte" address-version (bytevector-u8-ref hash1 0))
(define retrieved (fetch hash1))
(test "fetch returns block" #t (block? retrieved))
(test "tag matches" 'test (block-tag retrieved))
(test "payload matches" "hello" (utf8->string (block-payload retrieved)))

;;; Test 2: Same content = same hash
(display "
Test 2: Content identity
")
(define blk2 (make-block 'test (string->utf8 "hello") empty-refs))
(define hash2 (hash-block blk2))
(test "same content → same hash" (hash->hex hash1) (hash->hex hash2))

;;; Test 3: Different content = different hash
(display "
Test 3: Different content
")
(define blk3 (make-block 'test (string->utf8 "world") empty-refs))
(define hash3 (store! blk3))
(test "different content → different hash" #f (bytevector=? hash1 hash3))

;;; Test 4: Blocks with references
(display "
Test 4: Blocks with references
")
(define parent (make-block 'parent empty-payload (vector hash1 hash3)))
(define parent-hash (store! parent))
(display "  parent hash: ") (display (hash->hex parent-hash)) (newline)
(define retrieved-parent (fetch parent-hash))
(test "parent refs count" 2 (vector-length (block-refs retrieved-parent)))
(test "first ref correct" hash1 (vector-ref (block-refs retrieved-parent) 0))

;;; Test 5: S-expression storage
(display "
Test 5: S-expression storage
")
(define sexpr-hash (store-sexpr! 'code '(fn (x) (+ x 1))))
(define retrieved-sexpr (fetch-sexpr sexpr-hash))
(test "sexpr round-trip" '(fn (x) (+ x 1)) retrieved-sexpr)

;;; Test 6: Normalized S-expressions get same hash
(display "
Test 6: α-equivalent expressions
")
(define expr1 '(fn (x) x))
(define expr2 '(fn (y) y))
(define norm1 (normalize expr1))
(define norm2 (normalize expr2))
(define h1 (store-sexpr! 'lambda norm1))
(define h2 (store-sexpr! 'lambda norm2))
(test "normalized forms equal" norm1 norm2)
(test "same hash for α-equivalent" (hash->hex h1) (hash->hex h2))

;;; Test 7: Store statistics
(display "
Test 7: Store statistics
")
(test "store has blocks" #t (> (store-count) 0))
(display "  total blocks: ") (display (store-count)) (newline)

;;; Test 8: Hex conversion round-trip
(display "
Test 8: Hex conversion
")
(define hex-str (hash->hex hash1))
(define hash-back (hex->hash hex-str))
(test "hex round-trip" hash1 hash-back)

(display "
✓ All CAS tests complete.
")
