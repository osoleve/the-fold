((name "crypto")
 (purpose "Cryptographic hash functions and message authentication")
 (description "Extends core SHA-256 with SHA-512 family, BLAKE2b, and HMAC.
All implementations follow official specifications and pass test vectors.")
 (modules
  ((sha512.ss "SHA-512 and SHA-384 (FIPS 180-4):
     - sha512: 64-byte hash using 64-bit operations
     - sha384: 48-byte truncated variant
     - sha512-hex, sha384-hex: hex string output")
   (blake2b.ss "BLAKE2b (RFC 7693):
     - blake2b: configurable output length (1-64 bytes)
     - blake2b-keyed: MAC mode with secret key
     - Faster than SHA-2 with equivalent security")
   (hmac.ss "HMAC (RFC 2104):
     - make-hmac: create HMAC for any hash function
     - hmac-sha256, hmac-sha512, hmac-sha384: pre-built HMACs
     - Keyed-hash message authentication codes")))
 (tests
  ((test-crypto.ss "26 tests with official NIST and RFC test vectors")))
 (dependencies (core/base/sha256)))
