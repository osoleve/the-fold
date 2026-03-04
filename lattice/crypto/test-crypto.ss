;;; lattice/crypto/test-crypto.ss — Tests for cryptographic hash functions
;;;
;;; Tests SHA-256, SHA-384, SHA-512, HMAC, and BLAKE2b with official test vectors.
;;; Sources: NIST FIPS 180-4, RFC 4231, RFC 7693, BLAKE2 reference KAT.

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(require 'sha256)
(require 'sha512)
(require 'hmac)
(require 'blake2b)

;;; Helper to convert string to bytevector
(define (string->bv s)
  (string->utf8 s))

;;; Helper to convert hex string to bytevector
(define (hex->bv hex)
  (let* ([len (string-length hex)]
         [bv (make-bytevector (quotient len 2))])
    (do ([i 0 (+ i 2)])
        ((>= i len) bv)
      (bytevector-u8-set! bv (quotient i 2)
        (+ (* 16 (hex-digit (string-ref hex i)))
           (hex-digit (string-ref hex (+ i 1))))))))

(define (hex-digit c)
  (cond
    [(char<=? #\0 c #\9) (- (char->integer c) (char->integer #\0))]
    [(char<=? #\a c #\f) (+ 10 (- (char->integer c) (char->integer #\a)))]
    [(char<=? #\A c #\F) (+ 10 (- (char->integer c) (char->integer #\A)))]
    [else (error 'hex-digit "invalid hex character" c)]))

;;; ============================================================
;;; SHA-256 Tests (NIST FIPS 180-4 test vectors)
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
;;; SHA-512 Tests (NIST FIPS 180-4 test vectors)
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
      (sha512-hex (string->bv "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))))

  (define-test "896-bit message"
    (assert-equal
      "8e959b75dae313da8cf4f72814fc143f8f7779c6eb9f7fa17299aeadb6889018501d289e4900f7e4331b99dec4b5433ac7d329eeb6dd26545e96e55b874be909"
      (sha512-hex (string->bv "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu")))))

;;; ============================================================
;;; SHA-384 Tests (NIST FIPS 180-4 test vectors)
;;; ============================================================

(test-group 'sha384

  (define-test "empty string"
    (assert-equal
      "38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b"
      (sha384-hex (string->bv ""))))

  (define-test "abc"
    (assert-equal
      "cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed8086072ba1e7cc2358baeca134c825a7"
      (sha384-hex (string->bv "abc"))))

  (define-test "448-bit message"
    (assert-equal
      "3391fdddfc8dc7393707a65b1b4709397cf8b1d162af05abfe8f450de5f36bc6b0455a8520bc4e6f5fe95b1fe3c8452b"
      (sha384-hex (string->bv "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))))

  (define-test "896-bit message"
    (assert-equal
      "09330c33f71147e83d192fc782cd1b4753111b173b3b05d22fa08086e3b0f712fcc7c71a557e2db966c3e9fa91746039"
      (sha384-hex (string->bv "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu")))))

;;; ============================================================
;;; HMAC-SHA256 Tests (RFC 4231 test vectors)
;;; ============================================================

(test-group 'hmac-sha256

  (define-test "test case 1: short key"
    ;; Key = 0x0b repeated 20 times
    (let ([key (make-bytevector 20 #x0b)]
          [data (string->bv "Hi There")])
      (assert-equal
        "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"
        (hmac-sha256-hex key data))))

  (define-test "test case 2: natural-length key"
    ;; Key = "Jefe"
    (let ([key (string->bv "Jefe")]
          [data (string->bv "what do ya want for nothing?")])
      (assert-equal
        "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"
        (hmac-sha256-hex key data))))

  (define-test "test case 3: key and data are 0xaa/0xdd"
    ;; Key = 0xaa repeated 20 times, Data = 0xdd repeated 50 times
    (let ([key (make-bytevector 20 #xaa)]
          [data (make-bytevector 50 #xdd)])
      (assert-equal
        "773ea91e36800e46854db8ebd09181a72959098b3ef8c122d9635514ced565fe"
        (hmac-sha256-hex key data))))

  (define-test "test case 4: incrementing key"
    ;; Key = 0x01..0x19 (25 bytes), Data = 0xcd repeated 50 times
    (let ([key (hex->bv "0102030405060708090a0b0c0d0e0f10111213141516171819")]
          [data (make-bytevector 50 #xcd)])
      (assert-equal
        "82558a389a443c0ea4cc819899f2083a85f0faa3e578f8077a2e3ff46729665b"
        (hmac-sha256-hex key data))))

  (define-test "test case 6: key larger than block size"
    ;; Key = 0xaa repeated 131 bytes (> 64-byte SHA-256 block size → key gets hashed)
    (let ([key (make-bytevector 131 #xaa)]
          [data (string->bv "Test Using Larger Than Block-Size Key - Hash Key First")])
      (assert-equal
        "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54"
        (hmac-sha256-hex key data))))

  (define-test "test case 7: key and data larger than block size"
    ;; Key = 0xaa repeated 131 bytes, long data string
    (let ([key (make-bytevector 131 #xaa)]
          [data (string->bv "This is a test using a larger than block-size key and a larger than block-size data. The key needs to be hashed before being used by the HMAC algorithm.")])
      (assert-equal
        "9b09ffa71b942fcb27635fbcd5b0e944bfdc63644f0713938a7f51535c3a35e2"
        (hmac-sha256-hex key data)))))

;;; ============================================================
;;; HMAC-SHA512 Tests (RFC 4231 test vectors)
;;; ============================================================

(test-group 'hmac-sha512

  (define-test "test case 1: short key"
    (let ([key (make-bytevector 20 #x0b)]
          [data (string->bv "Hi There")])
      (assert-equal
        "87aa7cdea5ef619d4ff0b4241a1d6cb02379f4e2ce4ec2787ad0b30545e17cdedaa833b7d6b8a702038b274eaea3f4e4be9d914eeb61f1702e696c203a126854"
        (hmac-sha512-hex key data))))

  (define-test "test case 2: natural-length key"
    (let ([key (string->bv "Jefe")]
          [data (string->bv "what do ya want for nothing?")])
      (assert-equal
        "164b7a7bfcf819e2e395fbe73b56e0a387bd64222e831fd610270cd7ea2505549758bf75c05a994a6d034f65f8f0e6fdcaeab1a34d4a6b4b636e070a38bce737"
        (hmac-sha512-hex key data))))

  (define-test "test case 3: key and data are 0xaa/0xdd"
    (let ([key (make-bytevector 20 #xaa)]
          [data (make-bytevector 50 #xdd)])
      (assert-equal
        "fa73b0089d56a284efb0f0756c890be9b1b5dbdd8ee81a3655f83e33b2279d39bf3e848279a722c806b485a47e67c807b946a337bee8942674278859e13292fb"
        (hmac-sha512-hex key data))))

  (define-test "test case 4: incrementing key"
    (let ([key (hex->bv "0102030405060708090a0b0c0d0e0f10111213141516171819")]
          [data (make-bytevector 50 #xcd)])
      (assert-equal
        "b0ba465637458c6990e5a8c5f61d4af7e576d97ff94b872de76f8050361ee3dba91ca5c11aa25eb4d679275cc5788063a5f19741120c4f2de2adebeb10a298dd"
        (hmac-sha512-hex key data))))

  (define-test "test case 6: key larger than block size"
    ;; Key = 0xaa repeated 131 bytes (> 128-byte SHA-512 block size)
    (let ([key (make-bytevector 131 #xaa)]
          [data (string->bv "Test Using Larger Than Block-Size Key - Hash Key First")])
      (assert-equal
        "80b24263c7c1a3ebb71493c1dd7be8b49b46d1f41b4aeec1121b013783f8f3526b56d037e05f2598bd0fd2215d6a1e5295e64f73f63f0aec8b915a985d786598"
        (hmac-sha512-hex key data))))

  (define-test "test case 7: key and data larger than block size"
    (let ([key (make-bytevector 131 #xaa)]
          [data (string->bv "This is a test using a larger than block-size key and a larger than block-size data. The key needs to be hashed before being used by the HMAC algorithm.")])
      (assert-equal
        "e37b6a775dc87dbaa4dfa9f96e5e3ffddebd71f8867289865df5a32d20cdc944b6022cac3c4982b10d5eeb55c3e4de15134676fb6de0446065c97440fa8c6a58"
        (hmac-sha512-hex key data)))))

;;; ============================================================
;;; HMAC-SHA384 Tests (RFC 4231 test vectors)
;;; ============================================================

(test-group 'hmac-sha384

  (define-test "test case 1: short key"
    (let ([key (make-bytevector 20 #x0b)]
          [data (string->bv "Hi There")])
      (assert-equal
        "afd03944d84895626b0825f4ab46907f15f9dadbe4101ec682aa034c7cebc59cfaea9ea9076ede7f4af152e8b2fa9cb6"
        (hmac-sha384-hex key data))))

  (define-test "test case 2: natural-length key"
    (let ([key (string->bv "Jefe")]
          [data (string->bv "what do ya want for nothing?")])
      (assert-equal
        "af45d2e376484031617f78d2b58a6b1b9c7ef464f5a01b47e42ec3736322445e8e2240ca5e69e2c78b3239ecfab21649"
        (hmac-sha384-hex key data))))

  (define-test "test case 3: key and data are 0xaa/0xdd"
    (let ([key (make-bytevector 20 #xaa)]
          [data (make-bytevector 50 #xdd)])
      (assert-equal
        "88062608d3e6ad8a0aa2ace014c8a86f0aa635d947ac9febe83ef4e55966144b2a5ab39dc13814b94e3ab6e101a34f27"
        (hmac-sha384-hex key data))))

  (define-test "test case 4: incrementing key"
    (let ([key (hex->bv "0102030405060708090a0b0c0d0e0f10111213141516171819")]
          [data (make-bytevector 50 #xcd)])
      (assert-equal
        "3e8a69b7783c25851933ab6290af6ca77a9981480850009cc5577c6e1f573b4e6801dd23c4a7d679ccf8a386c674cffb"
        (hmac-sha384-hex key data))))

  (define-test "test case 6: key larger than block size"
    ;; Key = 0xaa repeated 131 bytes (> 128-byte block size)
    (let ([key (make-bytevector 131 #xaa)]
          [data (string->bv "Test Using Larger Than Block-Size Key - Hash Key First")])
      (assert-equal
        "4ece084485813e9088d2c63a041bc5b44f9ef1012a2b588f3cd11f05033ac4c60c2ef6ab4030fe8296248df163f44952"
        (hmac-sha384-hex key data))))

  (define-test "test case 7: key and data larger than block size"
    (let ([key (make-bytevector 131 #xaa)]
          [data (string->bv "This is a test using a larger than block-size key and a larger than block-size data. The key needs to be hashed before being used by the HMAC algorithm.")])
      (assert-equal
        "6617178e941f020d351e2f254e8fd32c602420feb0b8fb9adccebb82461e99c5a678cc31e799176d3860e6110c46523e"
        (hmac-sha384-hex key data)))))

;;; ============================================================
;;; BLAKE2b Tests (RFC 7693 + reference implementation KAT)
;;; ============================================================

(test-group 'blake2b

  (define-test "unkeyed: empty string"
    (assert-equal
      "786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce"
      (blake2b-hex (string->bv ""))))

  (define-test "unkeyed: abc"
    (assert-equal
      "ba80a53f981c4d0d6a2797b69f12f6e94c212f14685ac4b74b12bb6fdbffa2d17d87c5392aab792dc252d5de4533cc9518d38aa8dbf1925ab92386edd4009923"
      (blake2b-hex (string->bv "abc"))))

  (define-test "unkeyed: custom output length (32 bytes)"
    (assert-equal
      "bddd813c634239723171ef3fee98579b94964e3bb1cb3e427262c8c068d52319"
      (blake2b-hex (string->bv "abc") 32)))

  (define-test "keyed KAT: empty input (reference impl)"
    ;; Key = 00 01 02 ... 3f (64 bytes), Input = empty
    (let ([key (let ([bv (make-bytevector 64)])
                 (do ([i 0 (+ i 1)]) ((= i 64) bv)
                   (bytevector-u8-set! bv i i)))])
      (assert-equal
        "10ebb67700b1868efb4417987acf4690ae9d972fb7a590c2f02871799aaa4786b5e996e8f0f4eb981fc214b005f42d2ff4233499391653df7aefcbc13fc51568"
        (blake2b-keyed-hex key #vu8()))))

  (define-test "keyed KAT: single byte 0x00 (reference impl)"
    ;; Key = 00 01 02 ... 3f (64 bytes), Input = 0x00
    (let ([key (let ([bv (make-bytevector 64)])
                 (do ([i 0 (+ i 1)]) ((= i 64) bv)
                   (bytevector-u8-set! bv i i)))])
      (assert-equal
        "961f6dd1e4dd30f63901690c512e78e4b45e4742ed197c3c5e45c549fd25f2e4187b0bc9fe30492b16b0d0bc4ef9b0f34c7003fac09a5ef1532e69430234cebd"
        (blake2b-keyed-hex key #vu8(0)))))

  (define-test "keyed KAT: two bytes 0x0001 (reference impl)"
    ;; Key = 00 01 02 ... 3f (64 bytes), Input = 0x00 0x01
    (let ([key (let ([bv (make-bytevector 64)])
                 (do ([i 0 (+ i 1)]) ((= i 64) bv)
                   (bytevector-u8-set! bv i i)))])
      (assert-equal
        "da2cfbe2d8409a0f38026113884f84b50156371ae304c4430173d08a99d9fb1b983164a3770706d537f49e0c916d9f32b95cc37a95b99d857436f0232c88a965"
        (blake2b-keyed-hex key #vu8(0 1)))))

  (define-test "keyed: determinism"
    (let ([key (string->bv "mykey")]
          [msg (string->bv "test data")])
      (assert-equal (blake2b-keyed key msg)
                    (blake2b-keyed key msg))))

  (define-test "keyed: different keys produce different MACs"
    (let ([key1 (string->bv "key1")]
          [key2 (string->bv "key2")]
          [msg (string->bv "same message")])
      (assert-false (equal? (blake2b-keyed key1 msg)
                            (blake2b-keyed key2 msg)))))

  (define-test "multi-block: 128+ bytes"
    ;; Force multi-block processing (BLAKE2b block = 128 bytes)
    (let ([msg (make-bytevector 200 (char->integer #\x))])
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

  (define-test "blake2b output is 64 bytes by default"
    (assert-equal 64 (bytevector-length (blake2b (string->bv "test")))))

  (define-test "hmac-sha256 determinism"
    (let* ([key (string->bv "key")]
           [msg (string->bv "message")]
           [h1 (hmac-sha256 key msg)]
           [h2 (hmac-sha256 key msg)])
      (assert-true (equal? h1 h2))))

  (define-test "hmac-sha256 key sensitivity"
    (let* ([key1 (string->bv "key1")]
           [key2 (string->bv "key2")]
           [msg (string->bv "message")])
      (assert-false (equal? (hmac-sha256 key1 msg)
                            (hmac-sha256 key2 msg)))))

  (define-test "hmac-sha384 output is 48 bytes"
    (assert-equal 48 (bytevector-length
                       (hmac-sha384 (string->bv "key") (string->bv "msg")))))

  (define-test "hmac-sha512 output is 64 bytes"
    (assert-equal 64 (bytevector-length
                       (hmac-sha512 (string->bv "key") (string->bv "msg")))))

  (define-test "all hash families differ on same input"
    ;; Verify no accidental collision between hash families
    (let* ([msg (string->bv "cross-hash test")]
           [s256 (sha256 msg)]
           [s384 (sha384 msg)]
           [s512 (sha512 msg)]
           [b2b  (blake2b msg)])
      (assert-false (equal? s256 s384))
      (assert-false (equal? s256 s512))
      (assert-false (equal? s256 b2b))
      (assert-false (equal? s384 s512))
      (assert-false (equal? s512 b2b)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(display "\n=== Cryptographic Hash Function Tests ===\n\n")
(run-all-tests)
