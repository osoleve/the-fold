;;; Test harness for block.ss
;;;
;;; Uses the unified test framework.

;;; Load test framework if not already loaded
(unless (top-level-bound? 'define-test)
        (load "fabric/stitches/test-framework.ss"))

(load "fabric/stitches/block.ss")

(display "Block System Tests
")
(display "===================

")

;;; ============================================================
;;; Block Construction Tests
;;; ============================================================

(test-group block-construction
            (define-test basic-block
              (let* ([test-payload (string->utf8 "hello")]
                     [test-refs (vector)]
                     [blk (make-block 'greeting test-payload test-refs)])
                    (assert-equal 'greeting (block-tag blk))
                    (assert-equal 5 (bytevector-length (block-payload blk)))
                    (assert-equal 0 (vector-length (block-refs blk))))))

;;; ============================================================
;;; Serialization Tests
;;; ============================================================

(test-group serialization
            (define-test round-trip-simple
              (let* ([test-payload (string->utf8 "hello")]
                     [test-refs (vector)]
                     [blk (make-block 'greeting test-payload test-refs)]
                     [serialized (block->bytes blk)]
                     [restored (bytes->block serialized)])
                    (assert-equal 'greeting (block-tag restored))
                    (assert-equal "hello" (utf8->string (block-payload restored)))
                    (assert-true (block-equal? blk restored))))
            
            (define-test round-trip-with-refs
              (let* ([fake-hash-1 (make-bytevector address-size #xAA)]
                     [fake-hash-2 (make-bytevector address-size #xBB)]
                     [blk (make-block 'node empty-payload (vector fake-hash-1 fake-hash-2))]
                     [serialized (block->bytes blk)]
                     [restored (bytes->block serialized)])
                    (assert-equal 2 (vector-length (block-refs restored)))
                    (assert-true (block-equal? blk restored)))))

;;; ============================================================
;;; Tests run immediately when defined via define-test
;;; ============================================================
