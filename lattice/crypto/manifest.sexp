(skill crypto
  (version "0.1.0")
  (tier 1)
  (path "lattice/crypto")
  (purity total)
  (stability stable)
  (fuel-bound "O(n) for n-byte messages")
  (deps ())
  (description "Cryptographic hash functions and message authentication.
Implements SHA-2 family, BLAKE2b, and HMAC with official test vectors.")
  (keywords (hash sha256 sha384 sha512 blake2b hmac mac crypto security))
  (aliases (hash hashing))
  (exports
    (sha512
      sha512 sha512-hex sha384 sha384-hex)
    (blake2b
      blake2b blake2b-hex blake2b-keyed blake2b-keyed-hex)
    (hmac
      make-hmac hmac-sha256 hmac-sha512 hmac-sha384
      hmac-sha256-hex hmac-sha512-hex hmac-sha384-hex))
  (modules
    (sha512 "sha512.ss" "SHA-512 and SHA-384 hash functions (FIPS 180-4)")
    (blake2b "blake2b.ss" "BLAKE2b hash function (RFC 7693)")
    (hmac "hmac.ss" "HMAC keyed-hash message authentication (RFC 2104)")))
