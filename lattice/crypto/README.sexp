((name "crypto")
(purpose "Cryptographic hash functions and message authentication")
(description "Extends core SHA-256 with SHA-512 family, BLAKE2b, and HMAC.\nAll implementations follow official specifications and pass test vectors.")
(modules 
  ((sha512.ss "SHA-512 and SHA-384 (FIPS 180-4):\n     - sha512: 64-byte hash using 64-bit operations\n     - sha384: 48-byte truncated variant\n     - sha512-hex, sha384-hex: hex string output")
  (blake2b.ss "BLAKE2b (RFC 7693):\n     - blake2b: configurable output length (1-64 bytes)\n     - blake2b-keyed: MAC mode with secret key\n     - Faster than SHA-2 with equivalent security")
  (hmac.ss "HMAC (RFC 2104):\n     - make-hmac: create HMAC for any hash function\n     - hmac-sha256, hmac-sha512, hmac-sha384: pre-built HMACs\n     - Keyed-hash message authentication codes")))
(tests 
  ((test-crypto.ss "28 tests with official NIST and RFC test vectors")))
(dependencies (core/base/sha256)))
