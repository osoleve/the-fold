;;; lattice/crypto/test-crypto.ss — Tests for cryptographic hash functions
;;;
;;; Tests SHA-256, SHA-384, SHA-512, HMAC, and BLAKE2b with official test vectors.

(load "core/testing/test-framework.ss")
(load "core/base/sha256.ss")
(load "lattice/crypto/sha512.ss")
(load "lattice/crypto/hmac.ss")
(load "lattice/crypto/blake2b.ss")

;;; Helper to convert string to bytevector
(define (string->bv s)
  (string->utf8 s))

;;; ============================================================
;;; SHA-256 Tests (NIST test vectors)
;;; ============================================================

(test-group 'sha256

  (define-test "empty string"
    (assert-equal
      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
      (sha256-hex (string->bv ""))))

  (define-test "abc"
    (assert-equal
      "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
      (sha256-hex (string->bv "abc"))))

  (define-test "448-bit message"
    (assert-equal
      "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
      (sha256-hex (string->bv "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))))

  (define-test "896-bit message"
    (assert-equal
      "cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1"
      (sha256-hex (string->bv "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu")))))

;;; ============================================================
;;; SHA-512 Tests (NIST test vectors)
;;; ============================================================

(test-group 'sha512

  (define-test "empty string"
    (assert-equal
      "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"
      (sha512-hex (string->bv ""))))

  (define-test "abc"
    (assert-equal
      "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"
      (sha512-hex (string->bv "abc"))))

  (define-test "448-bit message"
    (assert-equal
      "204a8fc6dda82f0a0ced7beb8e08a41657c16ef468b228a8279be331a703c33596fd15c13b1b07f9aa1d3bea57789ca031ad85c7a71dd70354ec631238ca3445"
      (sha512-hex (string->bv "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")))))

;;; ============================================================
;;; SHA-384 Tests (NIST test vectors)
;;; ============================================================

(test-group 'sha384

  (define-test "empty string"
    (assert-equal
      "38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b"
      (sha384-hex (string->bv ""))))

  (define-test "abc"
    (assert-equal
      "cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed8086072ba1e7cc2358baeca134c825a7"
      (sha384-hex (string->bv "abc")))))

;;; ============================================================
;;; HMAC-SHA256 Tests (RFC 4231 test vectors)
;;; ============================================================

(test-group 'hmac-sha256

  (define-test "test case 1"
    ;; Key = 0x0b repeated 20 times
    (let ([key (make-bytevector 20 #x0b)]
          [data (string->bv "Hi There")])
      (assert-equal
        "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"
        (hmac-sha256-hex key data))))

  (define-test "test case 2"
    ;; Key = "Jefe"
    (let ([key (string->bv "Jefe")]
          [data (string->bv "what do ya want for nothing?")])
      (assert-equal
        "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"
        (hmac-sha256-hex key data))))

  (define-test "test case 3"
    ;; Key = 0xaa repeated 20 times
    (let ([key (make-bytevector 20 #xaa)]
          [data (make-bytevector 50 #xdd)])
      (assert-equal
        "773ea91e36800e46854db8ebd09181a72959098b3ef8c122d9635514ced565fe"
        (hmac-sha256-hex key data)))))

;;; ============================================================
;;; HMAC-SHA512 Tests (RFC 4231 test vectors)
;;; ============================================================

(test-group 'hmac-sha512

  (define-test "test case 1"
    (let ([key (make-bytevector 20 #x0b)]
          [data (string->bv "Hi There")])
      (assert-equal
        "87aa7cdea5ef619d4ff0b4241a1d6cb02379f4e2ce4ec2787ad0b30545e17cdedaa833b7d6b8a702038b274eaea3f4e4be9d914eeb61f1702e696c203a126854"
        (hmac-sha512-hex key data))))

  (define-test "test case 2"
    (let ([key (string->bv "Jefe")]
          [data (string->bv "what do ya want for nothing?")])
      (assert-equal
        "164b7a7bfcf819e2e395fbe73b56e0a387bd64222e831fd610270cd7ea2505549758bf75c05a994a6d034f65f8f0e6fdcaeab1a34d4a6b4b636e070a38bce737"
        (hmac-sha512-hex key data)))))

;;; ============================================================
;;; BLAKE2b Tests (RFC 7693 test vectors)
;;; ============================================================

(test-group 'blake2b

  (define-test "empty string (64 bytes)"
    (assert-equal
      "786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce"
      (blake2b-hex (string->bv ""))))

  (define-test "abc (64 bytes)"
    (assert-equal
      "ba80a53f981c4d0d6a2797b69f12f6e94c212f14685ac4b74b12bb6fdbffa2d17d87c5392aab792dc252d5de4533cc9518d38aa8dbf1925ab92386edd4009923"
      (blake2b-hex (string->bv "abc"))))

  (define-test "custom output length (32 bytes)"
    (assert-equal
      "bddd813c634239723171ef3fee98579b94964e3bb1cb3e427262c8c068d52319"
      (blake2b-hex (string->bv "abc") 32)))

  (define-test "keyed hash (MAC mode)"
    ;; Using a simple key for the test
    (let ([key (string->bv "secret")]
          [msg (string->bv "message")])
      ;; Just verify it produces valid output of correct length
      (assert-equal 64 (bytevector-length (blake2b-keyed key msg)))))

  (define-test "keyed hash determinism"
    ;; Same key + message should produce same hash
    (let ([key (string->bv "mykey")]
          [msg (string->bv "test data")])
      (assert-equal (blake2b-keyed key msg)
                    (blake2b-keyed key msg))))

  (define-test "different keys produce different MACs"
    (let ([key1 (string->bv "key1")]
          [key2 (string->bv "key2")]
          [msg (string->bv "same message")])
      (assert-false (equal? (blake2b-keyed key1 msg)
                            (blake2b-keyed key2 msg)))))

  (define-test "long message"
    ;; 64 bytes of 'a'
    (let ([msg (make-bytevector 64 (char->integer #\a))])
      ;; Just verify it works without error
      (assert-equal 64 (bytevector-length (blake2b msg))))))

;;; ============================================================
;;; Cross-hash consistency tests
;;; ============================================================

(test-group 'consistency

  (define-test "different inputs produce different hashes"
    (let ([h1 (sha256 (string->bv "hello"))]
          [h2 (sha256 (string->bv "world"))])
      (assert-false (equal? h1 h2))))

  (define-test "same input produces same hash"
    (let ([h1 (sha256 (string->bv "test"))]
          [h2 (sha256 (string->bv "test"))])
      (assert-true (equal? h1 h2))))

  (define-test "sha256 output is 32 bytes"
    (assert-equal 32 (bytevector-length (sha256 (string->bv "test")))))

  (define-test "sha384 output is 48 bytes"
    (assert-equal 48 (bytevector-length (sha384 (string->bv "test")))))

  (define-test "sha512 output is 64 bytes"
    (assert-equal 64 (bytevector-length (sha512 (string->bv "test")))))

  (define-test "hmac-sha256 with same key/msg is deterministic"
    (let* ([key (string->bv "key")]
           [msg (string->bv "message")]
           [h1 (hmac-sha256 key msg)]
           [h2 (hmac-sha256 key msg)])
      (assert-true (equal? h1 h2))))

  (define-test "hmac-sha256 with different keys produces different MACs"
    (let* ([key1 (string->bv "key1")]
           [key2 (string->bv "key2")]
           [msg (string->bv "message")]
           [h1 (hmac-sha256 key1 msg)]
           [h2 (hmac-sha256 key2 msg)])
      (assert-false (equal? h1 h2)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(display "\n=== Cryptographic Hash Function Tests ===\n\n")
(run-all-tests)
