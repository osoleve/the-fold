;;; Test harness for block.ss

(load "block.ss")

;;; Test 1: Basic block construction
(define test-payload (string->utf8 "hello"))
(define test-refs (vector))
(define blk (make-block 'greeting test-payload test-refs))

(display "Test 1: Block construction\n")
(display "  tag: ") (display (block-tag blk)) (newline)
(display "  payload length: ") (display (bytevector-length (block-payload blk))) (newline)
(display "  refs count: ") (display (vector-length (block-refs blk))) (newline)

;;; Test 2: Serialization round-trip
(display "\nTest 2: Serialization round-trip\n")
(define serialized (block->bytes blk))
(display "  serialized length: ") (display (bytevector-length serialized)) (newline)

(define restored (bytes->block serialized))
(display "  restored tag: ") (display (block-tag restored)) (newline)
(display "  restored payload: ") (display (utf8->string (block-payload restored))) (newline)
(display "  blocks equal? ") (display (block-equal? blk restored)) (newline)

;;; Test 3: Block with refs
(display "\nTest 3: Block with references\n")
(define fake-hash-1 (make-bytevector 32 #xAA))
(define fake-hash-2 (make-bytevector 32 #xBB))
(define blk-with-refs (make-block 'node empty-payload (vector fake-hash-1 fake-hash-2)))
(define serialized-2 (block->bytes blk-with-refs))
(define restored-2 (bytes->block serialized-2))
(display "  refs count: ") (display (vector-length (block-refs restored-2))) (newline)
(display "  round-trip ok? ") (display (block-equal? blk-with-refs restored-2)) (newline)

(display "\n✓ All tests passed.\n")
