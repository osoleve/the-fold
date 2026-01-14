;;; Test harness for shell/tools/validate.ss
;;;
;;; Run from ccverse root: scheme --script shell/test-validate.ss

(load "shell/tools/validate.ss")

;;; Helper to test validations (now using Result type)
;;; Validators return (ok #t) for valid or (error message) for invalid
(define (test-valid name validator obj expected-valid)
  (display "  ")
  (display name)
  (display ": ")
  (let ([result (validator obj)])
       (cond
        [(and expected-valid (ok? result))
         (display "✓")]
        [(and (not expected-valid) (error? result))
         (display "✓ (correctly rejected)")]
        [else
         (display "✗\n    expected: ")
         (display (if expected-valid "(ok #t)" "(error ...)"))
         (display "\n    got: ")
         (display result)]))
  (newline))

;;; Helper to test predicates
(define (test-pred name pred obj expected)
  (display "  ")
  (display name)
  (display ": ")
  (if (eq? (pred obj) expected)
      (display "✓")
      (begin
       (display "✗\n    expected: ")
       (display expected)
       (display "\n    got: ")
       (display (pred obj))))
  (newline))

(display "Validation Tests (shell/tools/validate.ss)\n")
(display "====\n\n")

;;; ====
;;; Hash Validation Tests
;;; ====

(display "Test 1: Hash validation\n")
(define valid-hash (make-bytevector address-size #xAB))
(define short-hash (make-bytevector 16 #xAB))
(define long-hash (make-bytevector 64 #xAB))

(test-pred "valid hash predicate" hash? valid-hash #t)
(test-pred "short hash predicate" hash? short-hash #f)
(test-pred "non-bytevector predicate" hash? "not a hash" #f)

(test-valid "valid hash" validate-hash valid-hash #t)
(test-valid "short hash" validate-hash short-hash #f)
(test-valid "long hash" validate-hash long-hash #f)
(test-valid "non-bytevector" validate-hash 42 #f)
(test-valid "string instead of hash" validate-hash "abcd" #f)

;;; ====
;;; Hex String Validation Tests
;;; ====

(display "\nTest 2: Hex string validation\n")
(define valid-hex "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef00")
(define short-hex "0123456789abcdef")
(define invalid-hex "0123456789abcdefghij0123456789abcdef0123456789abcdef0123456789ab00")
(define uppercase-hex "0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF00")

(test-pred "valid hex string predicate" hex-string? valid-hex #t)
(test-pred "uppercase hex predicate" hex-string? uppercase-hex #t)
(test-pred "short hex predicate" hex-string? short-hex #f)
(test-pred "invalid hex chars predicate" hex-string? invalid-hex #f)

(test-valid "valid hex string" validate-hex-string valid-hex #t)
(test-valid "uppercase hex string" validate-hex-string uppercase-hex #t)
(test-valid "short hex string" validate-hex-string short-hex #f)
(test-valid "invalid hex chars" validate-hex-string invalid-hex #f)
(test-valid "non-string" validate-hex-string 123 #f)

;;; ====
;;; Reference Validation Tests
;;; ====

(display "\nTest 3: Reference validation\n")
(define hash1 (make-bytevector address-size #x11))
(define hash2 (make-bytevector address-size #x22))
(define bad-hash (make-bytevector 16 #x33))

(define valid-refs (vector hash1 hash2))
(define empty-refs-vec (vector))
(define refs-with-bad (vector hash1 bad-hash))
(define refs-not-vector (list hash1 hash2))

(test-pred "valid refs predicate" refs-all-valid? valid-refs #t)
(test-pred "empty refs predicate" refs-all-valid? empty-refs-vec #t)
(test-pred "refs with bad hash predicate" refs-all-valid? refs-with-bad #f)
(test-pred "refs not vector predicate" refs-all-valid? refs-not-vector #f)

(test-valid "valid refs" validate-refs valid-refs #t)
(test-valid "empty refs" validate-refs empty-refs-vec #t)
(test-valid "refs with bad hash" validate-refs refs-with-bad #f)
(test-valid "refs as list" validate-refs refs-not-vector #f)

;;; ====
;;; Tag Validation Tests
;;; ====

(display "\nTest 4: Tag validation\n")
(test-pred "valid tag predicate" tag-valid? 'test #t)
(test-pred "invalid tag predicate" tag-valid? "test" #f)

(test-valid "valid tag" validate-tag 'greeting #t)
(test-valid "tag as string" validate-tag "greeting" #f)
(test-valid "tag as number" validate-tag 42 #f)

;;; ====
;;; Payload Validation Tests
;;; ====

(display "\nTest 5: Payload validation\n")
(define valid-payload (string->utf8 "hello"))
(define invalid-payload "hello")

(test-pred "valid payload predicate" payload-valid? valid-payload #t)
(test-pred "invalid payload predicate" payload-valid? invalid-payload #f)

(test-valid "valid payload" validate-payload valid-payload #t)
(test-valid "payload as string" validate-payload invalid-payload #f)
(test-valid "payload as number" validate-payload 42 #f)

;;; ====
;;; Block Validation Tests
;;; ====

(display "\nTest 6: Block validation\n")
(define valid-block (make-block 'test
                                (string->utf8 "hello")
                                (vector hash1 hash2)))
(define block-empty-refs (make-block 'greeting
                                     empty-payload
                                     empty-refs))
(define block-bad-refs (make-block 'test
                                   (string->utf8 "hello")
                                   (vector hash1 bad-hash)))

(test-pred "valid block predicate" block-valid? valid-block #t)
(test-pred "block with empty refs predicate" block-valid? block-empty-refs #t)
(test-pred "block with bad refs predicate" block-valid? block-bad-refs #f)
(test-pred "non-block predicate" block-valid? "not a block" #f)

(test-valid "valid block" validate-block valid-block #t)
(test-valid "block with empty refs" validate-block block-empty-refs #t)
(test-valid "block with bad refs" validate-block block-bad-refs #f)
(test-valid "non-block object" validate-block 'not-a-block #f)
(test-valid "number instead of block" validate-block 42 #f)

;;; ====
;;; Serialization Validation Tests
;;; ====

(display "\nTest 7: Serialization validation\n")
(define serialized-valid (block->bytes valid-block))
(define too-short (make-bytevector 8))
(define truncated (let* ([full (block->bytes valid-block)]
                         [short (make-bytevector (- (bytevector-length full) 5))])
                        (bytevector-copy! full 0 short 0 (bytevector-length short))
                        short))

(test-valid "valid serialized block" validate-serialized serialized-valid #t)
(test-valid "too short" validate-serialized too-short #f)
(test-valid "truncated serialized" validate-serialized truncated #f)
(test-valid "non-bytevector" validate-serialized "not bytes" #f)

;;; Test round-trip: serialize, validate, deserialize
(display "\nTest 8: Round-trip validation\n")
(define original (make-block 'data (string->utf8 "test data") (vector hash1)))
(define serialized (block->bytes original))
(display "  serialize then validate: ")
(let ([valid-result (validate-serialized serialized)])
     (if (ok? valid-result)
         (display "✓\n")
         (begin
          (display "✗\n    ")
          (display valid-result)
          (newline))))

(define deserialized (bytes->block serialized))
(display "  deserialize then validate: ")
(let ([valid-result (validate-block deserialized)])
     (if (ok? valid-result)
         (display "✓\n")
         (begin
          (display "✗\n    ")
          (display valid-result)
          (newline))))

(display "  blocks equal after round-trip: ")
(if (block-equal? original deserialized)
    (display "✓\n")
    (display "✗\n"))

;;; ====
;;; Hex Character Tests
;;; ====

(display "\nTest 9: Hex character validation\n")
(test-pred "digit 0" hex-char? #\0 #t)
(test-pred "digit 9" hex-char? #\9 #t)
(test-pred "letter a" hex-char? #\a #t)
(test-pred "letter f" hex-char? #\f #t)
(test-pred "letter A" hex-char? #\A #t)
(test-pred "letter F" hex-char? #\F #t)
(test-pred "letter g" hex-char? #\g #f)
(test-pred "letter z" hex-char? #\z #f)
(test-pred "space" hex-char? #\space #f)

;;; ====
;;; Edge Cases
;;; ====

(display "\nTest 10: Edge cases\n")

;; Empty block
(define empty-block (make-block 'empty empty-payload empty-refs))
(test-valid "empty block" validate-block empty-block #t)

;; Block with many refs
(define many-refs (make-vector 100 hash1))
(define block-many-refs (make-block 'many empty-payload many-refs))
(test-valid "block with many refs" validate-block block-many-refs #t)

;; Very long tag
(define long-tag (string->symbol (make-string 1000 #\x)))
(define block-long-tag (make-block long-tag empty-payload empty-refs))
(test-valid "block with long tag" validate-block block-long-tag #t)

;; Large payload
(define large-payload (make-bytevector 10000 #xAA))
(define block-large (make-block 'large large-payload empty-refs))
(test-valid "block with large payload" validate-block block-large #t)

(display "\n✓ All shell/tools/validate.ss tests complete.\n")
