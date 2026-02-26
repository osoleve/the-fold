(skill crypto
  (version "0.2.0")
  (path "lattice/crypto")
  (purity total)
  (stability stable)
  (fuel-bound "O(n) for n-byte messages, O(log k) for k-bit scalar multiplication")
  (deps (number-theory))
  (description "Cryptographic primitives: hash functions, HMAC, and elliptic curves.
Implements SHA-2 family, BLAKE2b, HMAC, and elliptic curve arithmetic over prime fields.
Includes standard curves secp256k1 (Bitcoin) and P-256 (NIST).")
  (keywords (hash sha256 sha384 sha512 blake2b hmac mac crypto security
             elliptic-curve ecc secp256k1 p256 ecdh ecdsa weierstrass))
  (aliases (hash hashing ec ecc))
  (concepts
    (concept cryptographic-primitives
      (description "Hash functions (SHA-256/384/512, BLAKE2b), HMAC, and elliptic curve arithmetic (secp256k1, P-256, ECDH).")
      (parent mathematics)
      (synonyms cryptography crypto hash hashing ec ecc sha256 sha512 sha384 blake2b hmac message-authentication elliptic-curve secp256k1 p256 ecdh ecdsa)))
  (exports
    (sha512
      sha512 sha512-hex sha384 sha384-hex)
    (blake2b
      blake2b blake2b-hex blake2b-keyed blake2b-keyed-hex)
    (hmac
      make-hmac hmac-sha256 hmac-sha512 hmac-sha384
      hmac-sha256-hex hmac-sha512-hex hmac-sha384-hex)
    (elliptic-curve
      make-ec-curve ec-curve? ec-p ec-a ec-b ec-n ec-G
      make-ec-point ec-point? ec-point-x ec-point-y
      ec-infinity ec-infinity?
      ec-on-curve? ec-valid-point?
      ec-negate ec-add ec-double ec-mul ec-mul-affine
      ec-to-jacobian ec-from-jacobian
      ec-jacobian-double ec-jacobian-add ec-jacobian-add-mixed
      ec-compress ec-decompress
      secp256k1 p256
      ec-pubkey ec-equal? ec-order-check
      ecdh-shared-secret ecdh-shared-x))
  (modules
    (sha512 "sha512.ss" "SHA-512 and SHA-384 hash functions (FIPS 180-4)")
    (blake2b "blake2b.ss" "BLAKE2b hash function (RFC 7693)")
    (hmac "hmac.ss" "HMAC keyed-hash message authentication (RFC 2104)")
    (elliptic-curve "elliptic-curve.ss" "Elliptic curve arithmetic: secp256k1, P-256, ECDH")))
