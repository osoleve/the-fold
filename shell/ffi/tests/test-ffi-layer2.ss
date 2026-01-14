;;; shell/ffi/tests/test-ffi-layer2.ss — Integration Tests for Layer 2 FFI
;;;
;;; Tests the Scheme FFI bindings for Layer 2 bytevector and string
;;; operations, ensuring the ABI between Scheme and Rust is correct.
;;;
;;; Run: scheme --script shell/ffi/tests/test-ffi-layer2.ss

(load "core/testing/test-framework.ss")
(load "shell/ffi/bytevector-ffi.ss")
(load "core/base/prelude.ss")

;;; ====
;;; Bytevector Operation Tests
;;; ====

(test-group "Layer 2 Bytevector FFI"

  (define-test "rust-bv-hash produces consistent hash"
    (let* ([bv (string->utf8 "hello")]
           [result1 (rust-bv-hash bv 1000)]
           [result2 (rust-bv-hash bv 1000)])
      (assert-equal 'ok (car result1))
      (assert-equal 'ok (car result2))
      ;; Same input produces same hash
      (assert-equal (cadr result1) (cadr result2))))

  (define-test "rust-bv-hash empty bytevector"
    (let* ([bv (make-bytevector 0)]
           [result (rust-bv-hash bv 1000)])
      (assert-equal 'ok (car result))
      ;; FNV-1a offset basis for empty input
      (assert-equal #xcbf29ce484222325 (cadr result))))

  (define-test "rust-bv-compare less than"
    (let* ([bv1 (string->utf8 "abc")]
           [bv2 (string->utf8 "abd")]
           [result (rust-bv-compare bv1 bv2 1000)])
      (assert-equal 'ok (car result))
      (assert-equal -1 (cadr result))))

  (define-test "rust-bv-compare equal"
    (let* ([bv1 (string->utf8 "hello")]
           [bv2 (string->utf8 "hello")]
           [result (rust-bv-compare bv1 bv2 1000)])
      (assert-equal 'ok (car result))
      (assert-equal 0 (cadr result))))

  (define-test "rust-bv-compare greater than"
    (let* ([bv1 (string->utf8 "b")]
           [bv2 (string->utf8 "a")]
           [result (rust-bv-compare bv1 bv2 1000)])
      (assert-equal 'ok (car result))
      (assert-equal 1 (cadr result))))

  (define-test "rust-bv-equal? same content"
    (let* ([bv1 (string->utf8 "test")]
           [bv2 (string->utf8 "test")]
           [result (rust-bv-equal? bv1 bv2 1000)])
      (assert-equal 'ok (car result))
      (assert-true (cadr result))))

  (define-test "rust-bv-equal? different content"
    (let* ([bv1 (string->utf8 "test")]
           [bv2 (string->utf8 "tset")]
           [result (rust-bv-equal? bv1 bv2 1000)])
      (assert-equal 'ok (car result))
      (assert-false (cadr result))))

  (define-test "rust-bv-equal? different lengths"
    (let* ([bv1 (string->utf8 "short")]
           [bv2 (string->utf8 "longer")]
           [result (rust-bv-equal? bv1 bv2 1000)])
      (assert-equal 'ok (car result))
      (assert-false (cadr result))))

  (define-test "rust-bv-copy! basic"
    (let* ([src (string->utf8 "hello")]
           [dst (make-bytevector 10 0)]
           [result (rust-bv-copy! src dst 5 1000)])
      (assert-equal 'ok (car result))
      (assert-equal 5 (cadr result))
      ;; Check dst contains "hello" + zeros
      (assert-equal (char->integer #\h) (bytevector-u8-ref dst 0))
      (assert-equal (char->integer #\o) (bytevector-u8-ref dst 4))
      (assert-equal 0 (bytevector-u8-ref dst 5))))

  (define-test "rust-bv-fill! basic"
    (let* ([bv (make-bytevector 5 0)]
           [result (rust-bv-fill! bv #xAB 1000)])
      (assert-equal 'ok (car result))
      (assert-equal 5 (cadr result))
      ;; Check all bytes are #xAB
      (assert-equal #xAB (bytevector-u8-ref bv 0))
      (assert-equal #xAB (bytevector-u8-ref bv 4))))

) ; end bytevector tests

;;; ====
;;; String Operation Tests
;;; ====

(test-group "Layer 2 String FFI"

  (define-test "rust-levenshtein same string"
    (let ([result (rust-levenshtein "hello" "hello" 10000)])
      (assert-equal 'ok (car result))
      (assert-equal 0 (cadr result))))

  (define-test "rust-levenshtein kitten->sitting"
    (let ([result (rust-levenshtein "kitten" "sitting" 10000)])
      (assert-equal 'ok (car result))
      (assert-equal 3 (cadr result))))

  (define-test "rust-levenshtein single substitution"
    (let ([result (rust-levenshtein "cat" "bat" 10000)])
      (assert-equal 'ok (car result))
      (assert-equal 1 (cadr result))))

  (define-test "rust-levenshtein empty to string"
    (let ([result (rust-levenshtein "" "hello" 10000)])
      (assert-equal 'ok (car result))
      (assert-equal 5 (cadr result))))

  (define-test "rust-levenshtein matches Scheme edit-distance"
    (let ([s1 "algorithm"]
          [s2 "altruistic"])
      (let ([rust-result (cadr (rust-levenshtein s1 s2 100000))]
            [scheme-result (edit-distance s1 s2)])
        (assert-equal scheme-result rust-result))))

  (define-test "rust-str-contains? found"
    (let ([result (rust-str-contains? "hello world" "world" 1000)])
      (assert-equal 'ok (car result))
      (assert-true (cadr result))))

  (define-test "rust-str-contains? not found"
    (let ([result (rust-str-contains? "hello world" "foo" 1000)])
      (assert-equal 'ok (car result))
      (assert-false (cadr result))))

  (define-test "rust-str-contains? empty needle"
    (let ([result (rust-str-contains? "hello" "" 1000)])
      (assert-equal 'ok (car result))
      (assert-true (cadr result))))

  (define-test "rust-str-index-of found"
    (let ([result (rust-str-index-of "hello world" "world" 1000)])
      (assert-equal 'ok (car result))
      (assert-equal 6 (cadr result))))

  (define-test "rust-str-index-of not found"
    (let ([result (rust-str-index-of "hello world" "foo" 1000)])
      (assert-equal 'ok (car result))
      (assert-equal -1 (cadr result))))

  (define-test "rust-str-last-index-of multiple matches"
    (let ([result (rust-str-last-index-of "hello hello" "hello" 1000)])
      (assert-equal 'ok (car result))
      (assert-equal 6 (cadr result))))

  (define-test "rust-str-starts-with? true"
    (let ([result (rust-str-starts-with? "hello world" "hello" 1000)])
      (assert-equal 'ok (car result))
      (assert-true (cadr result))))

  (define-test "rust-str-starts-with? false"
    (let ([result (rust-str-starts-with? "hello world" "world" 1000)])
      (assert-equal 'ok (car result))
      (assert-false (cadr result))))

  (define-test "rust-str-ends-with? true"
    (let ([result (rust-str-ends-with? "hello world" "world" 1000)])
      (assert-equal 'ok (car result))
      (assert-true (cadr result))))

  (define-test "rust-str-ends-with? false"
    (let ([result (rust-str-ends-with? "hello world" "hello" 1000)])
      (assert-equal 'ok (car result))
      (assert-false (cadr result))))

  (define-test "rust-str-upcase ASCII"
    (let ([result (rust-str-upcase "Hello World!" 1000)])
      (assert-equal 'ok (car result))
      (assert-equal "HELLO WORLD!" (cadr result))))

  (define-test "rust-str-downcase ASCII"
    (let ([result (rust-str-downcase "Hello World!" 1000)])
      (assert-equal 'ok (car result))
      (assert-equal "hello world!" (cadr result))))

  (define-test "rust-str-upcase preserves non-ASCII"
    ;; Non-ASCII chars should pass through unchanged
    (let ([result (rust-str-upcase "café" 1000)])
      (assert-equal 'ok (car result))
      ;; 'é' is preserved, 'caf' becomes 'CAF'
      (assert-equal "CAFé" (cadr result))))

) ; end string tests

;;; ====
;;; Fuel Tracking Tests
;;; ====

(test-group "Layer 2 Fuel Tracking"

  (define-test "rust-bv-hash tracks fuel"
    (let* ([bv (string->utf8 "test")]
           [result (rust-bv-hash bv 1000)])
      (assert-equal 'ok (car result))
      ;; Should have used some fuel (base + per-byte)
      (assert-true (< (caddr result) 1000))))

  (define-test "rust-levenshtein out of fuel"
    ;; Cost is O(m*n), so 5*5=25 + base ~35
    ;; With fuel=1, should fail
    (let ([result (rust-levenshtein "hello" "world" 1)])
      (assert-equal 'out-of-fuel (car result))))

  (define-test "rust-bv-hash out of fuel"
    ;; 5 bytes * 1 cost/byte + 10 base = 15 fuel needed
    (let* ([bv (string->utf8 "hello")]
           [result (rust-bv-hash bv 5)])  ; Only 5 fuel
      (assert-equal 'out-of-fuel (car result))))

) ; end fuel tests

;;; ====
;;; Run All Tests
;;; ====

(display "\n=== Layer 2 FFI Integration Tests ===\n\n")
(run-all-tests)
