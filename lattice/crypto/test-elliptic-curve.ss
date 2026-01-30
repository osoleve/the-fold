(load "core/testing/test-framework.ss")
(load "lattice/crypto/elliptic-curve.ss")

;;; ============================================================================
;;; Elliptic Curve Tests
;;; ============================================================================

(test-group "elliptic-curve"

  ;; -------------------------------------------------------------------------
  ;; Small curve for testing (easy to verify by hand)
  ;; y² = x³ + 2x + 3 over F_97
  ;; -------------------------------------------------------------------------

  (define test-p 97)
  (define test-a 2)
  (define test-b 3)
  ;; Generator point (3, 6) is on the curve: 6² = 36, 3³ + 2*3 + 3 = 27 + 6 + 3 = 36 ✓
  (define test-G (make-ec-point 3 6))
  ;; Curve order: computed by counting points
  (define test-n 100)  ; #E(F_97) for y² = x³ + 2x + 3

  (define test-curve (make-ec-curve test-p test-a test-b test-n test-G))

  ;; -------------------------------------------------------------------------
  ;; Point validation
  ;; -------------------------------------------------------------------------

  (define-test "point on curve validation"
    (assert-true (ec-on-curve? test-curve test-G))
    (assert-true (ec-on-curve? test-curve ec-infinity))
    ;; Point not on curve
    (assert-false (ec-on-curve? test-curve (make-ec-point 3 7))))

  ;; -------------------------------------------------------------------------
  ;; Point operations on small curve
  ;; -------------------------------------------------------------------------

  (define-test "point negation"
    (let ([neg-G (ec-negate test-curve test-G)])
      (assert-equal 3 (ec-point-x neg-G))
      (assert-equal (- test-p 6) (ec-point-y neg-G))  ; 97 - 6 = 91
      (assert-true (ec-on-curve? test-curve neg-G))))

  (define-test "point doubling"
    (let ([2G (ec-double test-curve test-G)])
      (assert-true (ec-on-curve? test-curve 2G))
      ;; Verify 2G is not G and not infinity
      (assert-false (ec-equal? 2G test-G))
      (assert-false (ec-infinity? 2G))))

  (define-test "point addition"
    (let* ([2G (ec-double test-curve test-G)]
           [3G (ec-add test-curve test-G 2G)])
      (assert-true (ec-on-curve? test-curve 3G))
      ;; 3G should equal G + 2G
      (let ([3G-alt (ec-add test-curve 2G test-G)])
        (assert-true (ec-equal? 3G 3G-alt)))))

  (define-test "addition with infinity"
    (assert-true (ec-equal? test-G (ec-add test-curve test-G ec-infinity)))
    (assert-true (ec-equal? test-G (ec-add test-curve ec-infinity test-G))))

  (define-test "point plus negation equals infinity"
    (let ([neg-G (ec-negate test-curve test-G)])
      (assert-true (ec-infinity? (ec-add test-curve test-G neg-G)))))

  ;; -------------------------------------------------------------------------
  ;; Scalar multiplication
  ;; -------------------------------------------------------------------------

  (define-test "scalar multiplication basic"
    ;; 0 * G = infinity
    (assert-true (ec-infinity? (ec-mul test-curve 0 test-G)))
    ;; 1 * G = G
    (assert-true (ec-equal? test-G (ec-mul test-curve 1 test-G)))
    ;; 2 * G = G + G = 2G
    (let ([2G-mul (ec-mul test-curve 2 test-G)]
          [2G-add (ec-double test-curve test-G)])
      (assert-true (ec-equal? 2G-mul 2G-add))))

  (define-test "scalar multiplication associativity"
    ;; 5*G computed two ways
    (let* ([5G (ec-mul test-curve 5 test-G)]
           [2G (ec-mul test-curve 2 test-G)]
           [3G (ec-mul test-curve 3 test-G)]
           [5G-alt (ec-add test-curve 2G 3G)])
      (assert-true (ec-equal? 5G 5G-alt))))

  (define-test "scalar multiplication negative"
    ;; (-1)*G = -G
    (let ([neg-G (ec-negate test-curve test-G)]
          [neg1-G (ec-mul test-curve -1 test-G)])
      (assert-true (ec-equal? neg-G neg1-G))))

  (define-test "affine vs jacobian scalar multiplication"
    ;; Both methods should produce same result
    (let ([result-jac (ec-mul test-curve 17 test-G)]
          [result-aff (ec-mul-affine test-curve 17 test-G)])
      (assert-true (ec-equal? result-jac result-aff))))

  ;; -------------------------------------------------------------------------
  ;; Montgomery Ladder (constant-time scalar multiplication)
  ;; -------------------------------------------------------------------------

  (define-test "montgomery ladder basic"
    ;; 0 * G = infinity
    (assert-true (ec-infinity? (ec-mul-montgomery test-curve 0 test-G)))
    ;; 1 * G = G
    (assert-true (ec-equal? test-G (ec-mul-montgomery test-curve 1 test-G)))
    ;; 2 * G matches doubling
    (let ([2G-montgomery (ec-mul-montgomery test-curve 2 test-G)]
          [2G-double (ec-double test-curve test-G)])
      (assert-true (ec-equal? 2G-montgomery 2G-double))))

  (define-test "montgomery vs double-and-add"
    ;; Montgomery and double-and-add should produce identical results
    (let ([k1 17]
          [k2 99]
          [k3 12345])
      ;; Various scalar values
      (assert-true (ec-equal? (ec-mul test-curve k1 test-G)
                              (ec-mul-montgomery test-curve k1 test-G)))
      (assert-true (ec-equal? (ec-mul test-curve k2 test-G)
                              (ec-mul-montgomery test-curve k2 test-G)))
      (assert-true (ec-equal? (ec-mul test-curve k3 test-G)
                              (ec-mul-montgomery test-curve k3 test-G)))))

  (define-test "montgomery negative scalar"
    ;; (-k)*G = -(k*G)
    (let ([neg5-G (ec-mul-montgomery test-curve -5 test-G)]
          [5G-neg (ec-negate test-curve (ec-mul-montgomery test-curve 5 test-G))])
      (assert-true (ec-equal? neg5-G 5G-neg))))

  (define-test "montgomery with infinity"
    ;; k * O = O for any k
    (assert-true (ec-infinity? (ec-mul-montgomery test-curve 42 ec-infinity))))

  (define-test "ec-mul-secure alias"
    ;; ec-mul-secure should be an alias for ec-mul-montgomery
    (let ([result-montgomery (ec-mul-montgomery test-curve 73 test-G)]
          [result-secure (ec-mul-secure test-curve 73 test-G)])
      (assert-true (ec-equal? result-montgomery result-secure))))

  (define-test "montgomery on secp256k1"
    ;; Test Montgomery on a real curve
    (let* ([G (ec-G secp256k1)]
           [k 12345678901234567890]
           [result-daa (ec-mul secp256k1 k G)]
           [result-mont (ec-mul-montgomery secp256k1 k G)])
      (assert-true (ec-on-curve? secp256k1 result-mont))
      (assert-true (ec-equal? result-daa result-mont))))

  (define-test "montgomery pubkey consistency"
    ;; ec-pubkey now uses Montgomery internally - verify consistency
    (let* ([privkey 42]
           [pubkey (ec-pubkey test-curve privkey)]
           [pubkey-daa (ec-mul test-curve privkey (ec-G test-curve))])
      ;; Both should give same result
      (assert-true (ec-equal? pubkey pubkey-daa))))

  ;; -------------------------------------------------------------------------
  ;; Point compression
  ;; -------------------------------------------------------------------------

  (define-test "point compression roundtrip"
    (let* ([compressed (ec-compress test-curve test-G)]
           [decompressed (ec-decompress test-curve compressed)])
      (assert-true (ec-equal? test-G decompressed))))

  (define-test "compression preserves parity"
    (let* ([2G (ec-mul test-curve 2 test-G)]
           [compressed (ec-compress test-curve 2G)]
           [parity (car compressed)])
      ;; Parity should match actual y coordinate
      (assert-equal parity (if (even? (ec-point-y 2G)) 'even 'odd))))

  (define-test "infinity compression"
    (assert-equal 'infinity (ec-compress test-curve ec-infinity))
    (assert-true (ec-infinity? (ec-decompress test-curve 'infinity))))

  ;; -------------------------------------------------------------------------
  ;; secp256k1 tests (Bitcoin curve)
  ;; -------------------------------------------------------------------------

  (define-test "secp256k1 generator on curve"
    (assert-true (ec-on-curve? secp256k1 (ec-G secp256k1))))

  (define-test "secp256k1 scalar multiplication"
    ;; Compute 2*G and verify on curve
    (let ([2G (ec-mul secp256k1 2 (ec-G secp256k1))])
      (assert-true (ec-on-curve? secp256k1 2G))
      (assert-false (ec-infinity? 2G))))

  (define-test "secp256k1 known test vector"
    ;; Private key 1 → public key = G
    (let ([pubkey (ec-pubkey secp256k1 1)])
      (assert-true (ec-equal? pubkey (ec-G secp256k1)))))

  (define-test "secp256k1 private key 2"
    ;; Private key 2 → public key = 2G
    ;; Known value: 2*G for secp256k1
    (let ([pubkey (ec-pubkey secp256k1 2)])
      (assert-true (ec-on-curve? secp256k1 pubkey))
      ;; Verify x-coordinate matches known value
      (assert-equal
       #xC6047F9441ED7D6D3045406E95C07CD85C778E4B8CEF3CA7ABAC09B95C709EE5
       (ec-point-x pubkey))))

  ;; -------------------------------------------------------------------------
  ;; P-256 tests
  ;; -------------------------------------------------------------------------

  (define-test "P-256 generator on curve"
    (assert-true (ec-on-curve? p256 (ec-G p256))))

  (define-test "P-256 basic operations"
    (let* ([G (ec-G p256)]
           [2G (ec-mul p256 2 G)]
           [3G (ec-mul p256 3 G)]
           [sum (ec-add p256 G 2G)])
      (assert-true (ec-on-curve? p256 2G))
      (assert-true (ec-on-curve? p256 3G))
      (assert-true (ec-equal? 3G sum))))

  ;; -------------------------------------------------------------------------
  ;; ECDH key exchange
  ;; -------------------------------------------------------------------------

  (define-test "ECDH shared secret"
    ;; Alice and Bob generate keys
    (let* ([alice-priv 12345]
           [bob-priv 67890]
           [alice-pub (ec-pubkey test-curve alice-priv)]
           [bob-pub (ec-pubkey test-curve bob-priv)]
           ;; Compute shared secrets
           [alice-shared (ecdh-shared-secret test-curve alice-priv bob-pub)]
           [bob-shared (ecdh-shared-secret test-curve bob-priv alice-pub)])
      ;; Both should compute the same shared secret
      (assert-true (ec-equal? alice-shared bob-shared))
      ;; Shared secret should be on curve
      (assert-true (ec-on-curve? test-curve alice-shared))))

  (define-test "ECDH shared x-coordinate"
    (let* ([a 111]
           [b 222]
           [A (ec-pubkey test-curve a)]
           [B (ec-pubkey test-curve b)]
           [x-a (ecdh-shared-x test-curve a B)]
           [x-b (ecdh-shared-x test-curve b A)])
      (assert-equal x-a x-b)))

  ;; -------------------------------------------------------------------------
  ;; Jacobian coordinate tests
  ;; -------------------------------------------------------------------------

  (define-test "jacobian conversion roundtrip"
    (let* ([jac (ec-to-jacobian test-G)]
           [aff (ec-from-jacobian test-curve jac)])
      (assert-true (ec-equal? test-G aff))))

  (define-test "jacobian doubling matches affine"
    (let* ([aff-2G (ec-double test-curve test-G)]
           [jac-G (ec-to-jacobian test-G)]
           [jac-2G (ec-jacobian-double test-curve jac-G)]
           [aff-from-jac (ec-from-jacobian test-curve jac-2G)])
      (assert-true (ec-equal? aff-2G aff-from-jac))))

  (define-test "jacobian addition matches affine"
    (let* ([2G (ec-double test-curve test-G)]
           [aff-3G (ec-add test-curve test-G 2G)]
           [jac-G (ec-to-jacobian test-G)]
           [jac-2G (ec-to-jacobian 2G)]
           [jac-3G (ec-jacobian-add test-curve jac-G jac-2G)]
           [aff-from-jac (ec-from-jacobian test-curve jac-3G)])
      (assert-true (ec-equal? aff-3G aff-from-jac))))

  (define-test "mixed jacobian-affine addition"
    (let* ([jac-G (ec-to-jacobian test-G)]
           [2G (ec-double test-curve test-G)]
           [jac-3G (ec-jacobian-add-mixed test-curve (ec-jacobian-double test-curve jac-G) test-G)]
           [aff-3G (ec-add test-curve test-G 2G)])
      (assert-true (ec-equal? aff-3G (ec-from-jacobian test-curve jac-3G)))))

) ; end test-group

(run-all-tests)
