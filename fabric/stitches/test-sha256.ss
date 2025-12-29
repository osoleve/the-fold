;;; Test harness for sha256.ss using NIST test vectors

(load "sha256.ss")

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (display "✓")
      (begin
       (display "✗\n    expected: ")
       (display expected)
       (display "\n    got: ")
       (display actual)))
  (newline))

;;; NIST test vectors from FIPS 180-4

(display "SHA-256 Test Vectors (NIST FIPS 180-4)\n")
(display "======================================\n\n")

;;; Test 1: Empty message
(display "Test 1: Empty message\n")
(test "SHA-256(\"\")"
      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
      (sha256-hex (string->utf8 "")))

;;; Test 2: "abc"
(display "\nTest 2: \"abc\"\n")
(test "SHA-256(\"abc\")"
      "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
      (sha256-hex (string->utf8 "abc")))

;;; Test 3: Two-block message
(display "\nTest 3: 448-bit message (two blocks after padding)\n")
(define msg3 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
(test "SHA-256(448-bit)"
      "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
      (sha256-hex (string->utf8 msg3)))

;;; Test 4: "The quick brown fox..."
(display "\nTest 4: Common test string\n")
(test "SHA-256(fox)"
      "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592"
      (sha256-hex (string->utf8 "The quick brown fox jumps over the lazy dog")))

(display "\n✓ All SHA-256 tests complete.\n")
