;;; skills/crypto.sexp — Skill curation file

(skill crypto
  (description
    "Cryptographic primitives: hash functions, HMAC, and elliptic curves.\nImplements SHA-2 family, BLAKE2b, HMAC, and elliptic curve arithmetic over prime fields.\nIncludes standard curves secp256k1 (Bitcoin) and P-256 (NIST).")

  (keywords (hash sha256 sha384 sha512 blake2b hmac mac crypto security elliptic-curve ecc secp256k1 p256 ecdh ecdsa weierstrass))

  (aliases (hash hashing ec ecc))

  (concepts (cryptographic-primitives))

  (modules
    sha512
    blake2b
    hmac
    elliptic-curve
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
