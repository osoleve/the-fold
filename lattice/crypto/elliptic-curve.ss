;;; @module elliptic-curve
;;; @requires prelude modular
;;; @description Elliptic curve arithmetic: secp256k1, P-256, ECDH
;;; @purity total
;;; @stability stable

(require 'prelude)
(require 'modular)

(doc 'module 'elliptic-curve)
(doc 'description "Elliptic curve arithmetic over prime fields for cryptography")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'note "Implements elliptic curves in short Weierstrass form: y² = x³ + ax + b (mod p)")
(doc 'note "Provides both affine and Jacobian projective coordinates")
(doc 'note "Includes standard curves: secp256k1 (Bitcoin), P-256 (NIST)")

;;; ============================================================================
;;; Section 1: Curve and Point Representation
;;; ============================================================================

(doc 'section 'curve-representation)

;;; A curve is: (ec-curve p a b n G)
;;; - p: prime field modulus
;;; - a, b: curve parameters (y² = x³ + ax + b)
;;; - n: curve order (number of points on curve)
;;; - G: generator point (base point)

(define (make-ec-curve p a b n G)
  (doc 'export #t)
  (doc 'type '(-> Int Int Int Int Point Curve))
  (doc 'description "Create an elliptic curve y² = x³ + ax + b over F_p")
  (list 'ec-curve p a b n G))

(define (ec-curve? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'ec-curve)))

(define (ec-p curve)
  (doc 'export #t)
  (list-ref curve 1))
(define (ec-a curve)
  (doc 'export #t)
  (list-ref curve 2))
(define (ec-b curve)
  (doc 'export #t)
  (list-ref curve 3))
(define (ec-n curve)
  (doc 'export #t)
  (list-ref curve 4))
(define (ec-G curve)
  (doc 'export #t)
  (list-ref curve 5))

;;; Points in affine coordinates: (ec-point x y) or (ec-infinity)

(define (make-ec-point x y)
  (doc 'export #t)
  (doc 'type '(-> Int Int Point))
  (doc 'description "Create an affine point (x, y)")
  (list 'ec-point x y))

(define (ec-point? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'ec-point)))

(define (ec-point-x pt) (list-ref pt 1))
(define (ec-point-y pt) (list-ref pt 2))

(doc ec-infinity 'export #t)
(doc ec-infinity 'description "The point at infinity (identity element)")
(define ec-infinity '(ec-infinity))

(define (ec-infinity? pt)
  (doc 'export #t)
  (and (pair? pt) (eq? (car pt) 'ec-infinity)))

;;; ============================================================================
;;; Section 2: Point Validation
;;; ============================================================================

(doc 'section 'point-validation)

;;; ec-on-curve? : Curve × Point → Boolean
;;; Verify that a point lies on the curve.
(define (ec-on-curve? curve pt)
  (doc 'export #t)
  (doc 'type '(-> Curve Point Boolean))
  (doc 'description "Check if point satisfies curve equation y² = x³ + ax + b (mod p)")
  (if (ec-infinity? pt)
      #t
      (let* ([p (ec-p curve)]
             [a (ec-a curve)]
             [b (ec-b curve)]
             [x (ec-point-x pt)]
             [y (ec-point-y pt)]
             ;; y² mod p
             [lhs (mod* y y p)]
             ;; x³ + ax + b mod p
             [x2 (mod* x x p)]
             [x3 (mod* x2 x p)]
             [ax (mod* a x p)]
             [rhs (mod+ x3 (mod+ ax b p) p)])
        (= lhs rhs))))

;;; ec-valid-point? : Curve × Point → Boolean
;;; Full validation: on curve and in correct subgroup.
(define (ec-valid-point? curve pt)
  (doc 'export #t)
  (doc 'type '(-> Curve Point Boolean))
  (doc 'description "Validate point: on curve and in prime-order subgroup")
  (doc 'note "For prime-order curves, just checks on-curve (no cofactor issues)")
  (and (ec-on-curve? curve pt)
       ;; For curves with cofactor 1, no additional check needed
       ;; For curves with cofactor h > 1, would check n*P = O
       #t))

;;; ============================================================================
;;; Section 3: Affine Coordinate Operations
;;; ============================================================================

(doc 'section 'affine-operations)

(doc 'note "Affine operations are simpler but require modular inversion per operation")
(doc 'note "For single operations or when simplicity matters, use affine")
(doc 'note "For scalar multiplication, Jacobian coordinates are faster")

;;; ec-negate : Curve × Point → Point
;;; Negate a point: -P = (x, -y)
(define (ec-negate curve pt)
  (doc 'export #t)
  (doc 'type '(-> Curve Point Point))
  (doc 'description "Point negation: -(x, y) = (x, -y)")
  (if (ec-infinity? pt)
      ec-infinity
      (let ([p (ec-p curve)])
        (make-ec-point (ec-point-x pt)
                       (modulo (- p (ec-point-y pt)) p)))))

;;; ec-add : Curve × Point × Point → Point
;;; Add two points in affine coordinates.
(define (ec-add curve P Q)
  (doc 'export #t)
  (doc 'type '(-> Curve Point Point Point))
  (doc 'description "Point addition P + Q in affine coordinates")
  (doc 'complexity "O(log p) for one modular inversion")
  (cond
    [(ec-infinity? P) Q]
    [(ec-infinity? Q) P]
    [else
     (let* ([p (ec-p curve)]
            [a (ec-a curve)]
            [x1 (ec-point-x P)]
            [y1 (ec-point-y P)]
            [x2 (ec-point-x Q)]
            [y2 (ec-point-y Q)])
       (cond
         ;; P = -Q → return infinity
         [(and (= x1 x2) (= (mod+ y1 y2 p) 0))
          ec-infinity]
         ;; P = Q → use doubling formula
         [(and (= x1 x2) (= y1 y2))
          (ec-double curve P)]
         ;; General case: P ≠ Q, P ≠ -Q
         [else
          (let* ([dx (mod- x2 x1 p)]
                 [dy (mod- y2 y1 p)]
                 [dx-inv (mod-inverse dx p)]
                 [lambda (mod* dy dx-inv p)]
                 ;; x3 = λ² - x1 - x2
                 [lambda2 (mod* lambda lambda p)]
                 [x3 (mod- (mod- lambda2 x1 p) x2 p)]
                 ;; y3 = λ(x1 - x3) - y1
                 [y3 (mod- (mod* lambda (mod- x1 x3 p) p) y1 p)])
            (make-ec-point x3 y3))]))]))

;;; ec-double : Curve × Point → Point
;;; Double a point in affine coordinates.
(define (ec-double curve P)
  (doc 'export #t)
  (doc 'type '(-> Curve Point Point))
  (doc 'description "Point doubling 2P in affine coordinates")
  (if (ec-infinity? P)
      ec-infinity
      (let* ([p (ec-p curve)]
             [a (ec-a curve)]
             [x1 (ec-point-x P)]
             [y1 (ec-point-y P)])
        ;; If y1 = 0, the tangent is vertical → 2P = O
        (if (= y1 0)
            ec-infinity
            (let* ([x1-sq (mod* x1 x1 p)]
                   ;; numerator = 3x₁² + a
                   [num (mod+ (mod* 3 x1-sq p) a p)]
                   ;; denominator = 2y₁
                   [denom (mod* 2 y1 p)]
                   [denom-inv (mod-inverse denom p)]
                   [lambda (mod* num denom-inv p)]
                   ;; x3 = λ² - 2x1
                   [lambda2 (mod* lambda lambda p)]
                   [x3 (mod- lambda2 (mod* 2 x1 p) p)]
                   ;; y3 = λ(x1 - x3) - y1
                   [y3 (mod- (mod* lambda (mod- x1 x3 p) p) y1 p)])
              (make-ec-point x3 y3))))))

;;; ============================================================================
;;; Section 4: Jacobian Projective Coordinates
;;; ============================================================================

(doc 'section 'jacobian-coordinates)

(doc 'note "Jacobian coordinates: (X, Y, Z) represents affine (X/Z², Y/Z³)")
(doc 'note "Avoids modular inversion during addition/doubling")
(doc 'note "Convert to affine only at the end of computation")
(doc 'note "Speedup: ~3-4x for scalar multiplication vs affine")

;;; Jacobian point: (ec-jacobian X Y Z)
;;; Represents affine point (X/Z², Y/Z³)
;;; Point at infinity: Z = 0 (conventionally (1, 1, 0))

(define (make-ec-jacobian X Y Z)
  (list 'ec-jacobian X Y Z))

(define (ec-jacobian? x)
  (and (pair? x) (eq? (car x) 'ec-jacobian)))

(define (ec-jacobian-X pt) (list-ref pt 1))
(define (ec-jacobian-Y pt) (list-ref pt 2))
(define (ec-jacobian-Z pt) (list-ref pt 3))

(define ec-jacobian-infinity (make-ec-jacobian 1 1 0))

(define (ec-jacobian-infinity? pt)
  (and (ec-jacobian? pt) (= (ec-jacobian-Z pt) 0)))

;;; ec-to-jacobian : Point → JacobianPoint
;;; Convert affine to Jacobian: (x, y) → (x, y, 1)
(define (ec-to-jacobian pt)
  (doc 'export #t)
  (doc 'type '(-> Point JacobianPoint))
  (if (ec-infinity? pt)
      ec-jacobian-infinity
      (make-ec-jacobian (ec-point-x pt) (ec-point-y pt) 1)))

;;; ec-from-jacobian : Curve × JacobianPoint → Point
;;; Convert Jacobian to affine: (X, Y, Z) → (X/Z², Y/Z³)
(define (ec-from-jacobian curve jpt)
  (doc 'export #t)
  (doc 'type '(-> Curve JacobianPoint Point))
  (if (ec-jacobian-infinity? jpt)
      ec-infinity
      (let* ([p (ec-p curve)]
             [X (ec-jacobian-X jpt)]
             [Y (ec-jacobian-Y jpt)]
             [Z (ec-jacobian-Z jpt)]
             [Z-inv (mod-inverse Z p)]
             [Z2-inv (mod* Z-inv Z-inv p)]
             [Z3-inv (mod* Z2-inv Z-inv p)]
             [x (mod* X Z2-inv p)]
             [y (mod* Y Z3-inv p)])
        (make-ec-point x y))))

;;; ec-jacobian-double : Curve × JacobianPoint → JacobianPoint
;;; Point doubling in Jacobian coordinates.
;;; Uses 1M + 5S + 1*a + 7A (M=mul, S=square, A=add/sub) when a = -3
;;; Uses 1M + 6S + 1*a + 7A for general a
(define (ec-jacobian-double curve jpt)
  (doc 'export #t)
  (doc 'type '(-> Curve JacobianPoint JacobianPoint))
  (doc 'description "Point doubling 2P in Jacobian coordinates")
  (if (ec-jacobian-infinity? jpt)
      ec-jacobian-infinity
      (let* ([p (ec-p curve)]
             [a (ec-a curve)]
             [X1 (modulo (ec-jacobian-X jpt) p)]
             [Y1 (modulo (ec-jacobian-Y jpt) p)]
             [Z1 (modulo (ec-jacobian-Z jpt) p)])
        ;; If Y = 0, result is infinity
        (if (= Y1 0)
            ec-jacobian-infinity
            (let* (;; S = 4 * X1 * Y1²
                   [Y1-sq (mod* Y1 Y1 p)]
                   [S (mod* 4 (mod* X1 Y1-sq p) p)]
                   ;; M = 3 * X1² + a * Z1⁴
                   [X1-sq (mod* X1 X1 p)]
                   [Z1-sq (mod* Z1 Z1 p)]
                   [Z1-4 (mod* Z1-sq Z1-sq p)]
                   [M (mod+ (mod* 3 X1-sq p) (mod* a Z1-4 p) p)]
                   ;; X3 = M² - 2S
                   [M-sq (mod* M M p)]
                   [X3 (mod- M-sq (mod* 2 S p) p)]
                   ;; Y3 = M(S - X3) - 8 * Y1⁴
                   [Y1-4 (mod* Y1-sq Y1-sq p)]
                   [Y3 (mod- (mod* M (mod- S X3 p) p) (mod* 8 Y1-4 p) p)]
                   ;; Z3 = 2 * Y1 * Z1
                   [Z3 (mod* 2 (mod* Y1 Z1 p) p)])
              (make-ec-jacobian X3 Y3 Z3))))))

;;; ec-jacobian-add : Curve × JacobianPoint × JacobianPoint → JacobianPoint
;;; Point addition in Jacobian coordinates (both points Jacobian).
(define (ec-jacobian-add curve P Q)
  (doc 'export #t)
  (doc 'type '(-> Curve JacobianPoint JacobianPoint JacobianPoint))
  (doc 'description "Point addition P + Q in Jacobian coordinates")
  (cond
    [(ec-jacobian-infinity? P) Q]
    [(ec-jacobian-infinity? Q) P]
    [else
     (let* ([p (ec-p curve)]
            [X1 (modulo (ec-jacobian-X P) p)]
            [Y1 (modulo (ec-jacobian-Y P) p)]
            [Z1 (modulo (ec-jacobian-Z P) p)]
            [X2 (modulo (ec-jacobian-X Q) p)]
            [Y2 (modulo (ec-jacobian-Y Q) p)]
            [Z2 (modulo (ec-jacobian-Z Q) p)]
            ;; U1 = X1 * Z2², U2 = X2 * Z1²
            [Z1-sq (mod* Z1 Z1 p)]
            [Z2-sq (mod* Z2 Z2 p)]
            [U1 (mod* X1 Z2-sq p)]
            [U2 (mod* X2 Z1-sq p)]
            ;; S1 = Y1 * Z2³, S2 = Y2 * Z1³
            [Z1-cu (mod* Z1-sq Z1 p)]
            [Z2-cu (mod* Z2-sq Z2 p)]
            [S1 (mod* Y1 Z2-cu p)]
            [S2 (mod* Y2 Z1-cu p)])
       (cond
         ;; U1 = U2 means same x-coordinate
         [(= U1 U2)
          (if (= S1 S2)
              ;; Same point → double
              (ec-jacobian-double curve P)
              ;; Opposite points → infinity
              ec-jacobian-infinity)]
         [else
          (let* (;; H = U2 - U1, R = S2 - S1
                 [H (mod- U2 U1 p)]
                 [R (mod- S2 S1 p)]
                 [H-sq (mod* H H p)]
                 [H-cu (mod* H-sq H p)]
                 ;; X3 = R² - H³ - 2*U1*H²
                 [X3 (mod- (mod- (mod* R R p) H-cu p)
                           (mod* 2 (mod* U1 H-sq p) p) p)]
                 ;; Y3 = R*(U1*H² - X3) - S1*H³
                 [Y3 (mod- (mod* R (mod- (mod* U1 H-sq p) X3 p) p)
                           (mod* S1 H-cu p) p)]
                 ;; Z3 = H * Z1 * Z2
                 [Z3 (mod* H (mod* Z1 Z2 p) p)])
            (make-ec-jacobian X3 Y3 Z3))]))]))

;;; ec-jacobian-add-mixed : Curve × JacobianPoint × Point → JacobianPoint
;;; Mixed addition: Jacobian + affine (Z2 = 1).
;;; Slightly faster than full Jacobian addition.
(define (ec-jacobian-add-mixed curve jP aQ)
  (doc 'export #t)
  (doc 'type '(-> Curve JacobianPoint Point JacobianPoint))
  (doc 'description "Mixed addition: Jacobian + affine point")
  (cond
    [(ec-jacobian-infinity? jP) (ec-to-jacobian aQ)]
    [(ec-infinity? aQ) jP]
    [else
     (let* ([p (ec-p curve)]
            [X1 (modulo (ec-jacobian-X jP) p)]
            [Y1 (modulo (ec-jacobian-Y jP) p)]
            [Z1 (modulo (ec-jacobian-Z jP) p)]
            [x2 (ec-point-x aQ)]
            [y2 (ec-point-y aQ)]
            ;; U1 = X1, S1 = Y1 (since Z2 = 1)
            ;; U2 = x2 * Z1², S2 = y2 * Z1³
            [Z1-sq (mod* Z1 Z1 p)]
            [Z1-cu (mod* Z1-sq Z1 p)]
            [U2 (mod* x2 Z1-sq p)]
            [S2 (mod* y2 Z1-cu p)])
       (cond
         [(= X1 U2)
          (if (= Y1 S2)
              (ec-jacobian-double curve jP)
              ec-jacobian-infinity)]
         [else
          (let* ([H (mod- U2 X1 p)]
                 [R (mod- S2 Y1 p)]
                 [H-sq (mod* H H p)]
                 [H-cu (mod* H-sq H p)]
                 [X3 (mod- (mod- (mod* R R p) H-cu p)
                           (mod* 2 (mod* X1 H-sq p) p) p)]
                 [Y3 (mod- (mod* R (mod- (mod* X1 H-sq p) X3 p) p)
                           (mod* Y1 H-cu p) p)]
                 [Z3 (mod* H Z1 p)])
            (make-ec-jacobian X3 Y3 Z3))]))]))

;;; ============================================================================
;;; Section 5: Scalar Multiplication
;;; ============================================================================

(doc 'section 'scalar-multiplication)

(doc 'note "Scalar multiplication: compute k*P for scalar k and point P")
(doc 'note "This is the core operation for ECDH key exchange and ECDSA signing")
(doc 'note "Uses double-and-add with Jacobian coordinates for efficiency")

;;; ec-mul : Curve × Int × Point → Point
;;; Scalar multiplication using double-and-add in Jacobian coordinates.
;;;
;;; WARNING: This implementation is NOT constant-time. The execution time
;;; depends on the bit pattern of scalar k, making it vulnerable to timing
;;; side-channel attacks. Do NOT use with secret scalars in production
;;; cryptographic applications. For secure implementations, use Montgomery
;;; Ladder or fixed-window methods with constant-time lookups.
;;; See BBS issue fold-zxup for constant-time implementation tracking.
(define (ec-mul curve k P)
  (doc 'export #t)
  (doc 'type '(-> Curve Int Point Point))
  (doc 'description "Scalar multiplication k*P using double-and-add")
  (doc 'complexity "O(log k) point operations")
  (doc 'note "WARNING: Not constant-time - vulnerable to timing attacks with secret scalars")
  (cond
    [(= k 0) ec-infinity]
    [(ec-infinity? P) ec-infinity]
    [(< k 0)
     ;; k < 0: compute |k| * (-P)
     (ec-mul curve (- k) (ec-negate curve P))]
    [else
     ;; Double-and-add in Jacobian coordinates
     (let ([jP (ec-to-jacobian P)])
       (ec-from-jacobian
        curve
        (let loop ([n k] [R ec-jacobian-infinity] [Q jP])
          (cond
            [(= n 0) R]
            [(odd? n)
             (loop (quotient n 2)
                   (ec-jacobian-add curve R Q)
                   (ec-jacobian-double curve Q))]
            [else
             (loop (quotient n 2)
                   R
                   (ec-jacobian-double curve Q))]))))]))

;;; ec-mul-affine : Curve × Int × Point → Point
;;; Scalar multiplication using affine coordinates (slower, simpler).
(define (ec-mul-affine curve k P)
  (doc 'export #t)
  (doc 'type '(-> Curve Int Point Point))
  (doc 'description "Scalar multiplication in affine coordinates (reference implementation)")
  (cond
    [(= k 0) ec-infinity]
    [(ec-infinity? P) ec-infinity]
    [(< k 0) (ec-mul-affine curve (- k) (ec-negate curve P))]
    [else
     (let loop ([n k] [R ec-infinity] [Q P])
       (cond
         [(= n 0) R]
         [(odd? n)
          (loop (quotient n 2)
                (ec-add curve R Q)
                (ec-double curve Q))]
         [else
          (loop (quotient n 2)
                R
                (ec-double curve Q))]))]))

;;; ----------------------------------------------------------------------------
;;; Constant-Time Scalar Multiplication (Montgomery Ladder)
;;; ----------------------------------------------------------------------------

(doc 'note "Montgomery Ladder: constant-time scalar multiplication")
(doc 'note "Always performs same operations regardless of scalar bit pattern")
(doc 'note "Resistant to timing side-channel attacks")
(doc 'note "Uses: swap → add → double → swap pattern for each bit")

;;; bit-length : Int → Int
;;; Count the number of bits needed to represent n.
(define (bit-length n)
  (if (<= n 0)
      0
      (let loop ([x n] [count 0])
        (if (= x 0)
            count
            (loop (quotient x 2) (+ count 1))))))

;;; bit-ref : Int × Int → Int
;;; Get bit i of n (0-indexed from LSB). Returns 0 or 1.
;;; Uses bitwise operations for O(1) performance vs O(log i) for expt.
(define (bit-ref n i)
  (bitwise-and (bitwise-arithmetic-shift-right n i) 1))

;;; ec-cswap : Int × JacobianPoint × JacobianPoint → (Values JacobianPoint JacobianPoint)
;;; Conditional swap: if bit=1, swap R0 and R1; else keep as-is.
;;;
;;; CAVEAT: This uses a branch (if), which is NOT constant-time at the
;;; interpreter/CPU level. True constant-time cswap requires:
;;;   mask = -bit  (all 1s if bit=1, all 0s if bit=0)
;;;   diff = R0 XOR R1
;;;   R0' = R0 XOR (diff AND mask)
;;;   R1' = R1 XOR (diff AND mask)
;;; This needs machine-word-level operations, typically in assembly or C.
;;; The current implementation is correct for the *algorithm*, not for
;;; side-channel resistance in hostile environments.
(define (ec-cswap bit R0 R1)
  (if (= bit 1)
      (values R1 R0)
      (values R0 R1)))

;;; ec-mul-montgomery : Curve × Int × Point → Point
;;; Constant-time scalar multiplication using Montgomery Ladder.
;;;
;;; The Montgomery Ladder performs exactly the same sequence of operations
;;; for every bit of the scalar, making execution time independent of the
;;; scalar's bit pattern. This prevents timing side-channel attacks.
;;;
;;; Algorithm for each bit (from MSB to LSB):
;;;   1. Conditional swap R0, R1 based on bit
;;;   2. R1 = R0 + R1
;;;   3. R0 = 2 * R0
;;;   4. Conditional swap R0, R1 based on bit
;;;
;;; Invariant: R1 - R0 = P throughout the computation.
;;;
;;; TIMING FIX: Loop always iterates bit-length(n) times where n is the
;;; curve order, not bit-length(k). This prevents MSB timing leaks that
;;; would reveal information about the scalar's bit length.
;;;
;;; Note on "constant-time": This implementation follows the constant-time
;;; *algorithm* (same operations for all scalar bit patterns), but Scheme
;;; interpreter-level timing may still vary. For production cryptography,
;;; assembly-level implementations are required for true constant-time
;;; guarantees. This implementation is suitable for:
;;; - Educational/reference use
;;; - Applications where timing attacks are not in the threat model
;;; - Prototype development before native implementation
(define (ec-mul-montgomery curve k P)
  (doc 'export #t)
  (doc 'type '(-> Curve Int Point Point))
  (doc 'description "Scalar multiplication using Montgomery Ladder (constant-time algorithm)")
  (doc 'note "Follows constant-time algorithm pattern; see source for caveats on interpreter-level timing")
  (doc 'complexity "O(log n) point operations where n is the curve order")
  (cond
    [(= k 0) ec-infinity]
    [(ec-infinity? P) ec-infinity]
    [(< k 0)
     ;; k < 0: compute |k| * (-P)
     (ec-mul-montgomery curve (- k) (ec-negate curve P))]
    [else
     ;; Reduce scalar modulo curve order to minimize iterations
     ;; k*P = (k mod n)*P for any k, where n is the curve order
     (let* ([n (ec-n curve)]
            [k-reduced (if (>= k n) (modulo k n) k)])
       (if (= k-reduced 0)
           ec-infinity  ; k was a multiple of n
           ;; Montgomery Ladder in Jacobian coordinates
           ;; CRITICAL: Always iterate bit-length(n) times, not bit-length(k)
           ;; This prevents timing leaks that reveal the scalar's MSB position
           (let ([jP (ec-to-jacobian P)]
                 [nbits (bit-length n)])  ; Fixed iteration count based on curve order
             (ec-from-jacobian
              curve
              (let loop ([i (- nbits 1)]   ; Start from MSB of curve order
                         [R0 ec-jacobian-infinity]
                         [R1 jP])
                (if (< i 0)
                    R0
                    (let* ([bi (bit-ref k-reduced i)])
                      ;; Conditional swap before operations
                      (let-values ([(R0* R1*) (ec-cswap bi R0 R1)])
                        ;; Always do both operations
                        (let ([R1** (ec-jacobian-add curve R0* R1*)]
                              [R0** (ec-jacobian-double curve R0*)])
                          ;; Conditional swap after operations
                          (let-values ([(R0*** R1***) (ec-cswap bi R0** R1**)])
                            (loop (- i 1) R0*** R1***)))))))))))]))

;;; ec-mul-secure : Curve × Int × Point → Point
;;; Alias for ec-mul-montgomery for explicit security-conscious code.
;;; "Secure" here means resistant to timing attacks at the algorithm level;
;;; see ec-mul-montgomery documentation for interpreter-level caveats.
(define (ec-mul-secure curve k P)
  (doc 'export #t)
  (doc 'type '(-> Curve Int Point Point))
  (doc 'description "Scalar multiplication using constant-time algorithm (see caveats)")
  (doc 'note "Algorithm-level timing resistance; see ec-mul-montgomery for full caveats")
  (ec-mul-montgomery curve k P))

;;; ============================================================================
;;; Section 6: Point Compression and Decompression
;;; ============================================================================

(doc 'section 'point-compression)

(doc 'note "Point compression: store only x-coordinate and parity of y")
(doc 'note "Reduces storage from 64 bytes to 33 bytes for 256-bit curves")
(doc 'note "Decompression requires computing y = ±sqrt(x³ + ax + b)")

;;; ec-compress : Curve × Point → (List Symbol Int)
;;; Compress point to (parity x) where parity is 'even or 'odd.
(define (ec-compress curve pt)
  (doc 'export #t)
  (doc 'type '(-> Curve Point (Union (List Symbol Int) Symbol)))
  (doc 'description "Compress point to x-coordinate and y-parity")
  (if (ec-infinity? pt)
      'infinity
      (let* ([y (ec-point-y pt)]
             [parity (if (even? y) 'even 'odd)])
        (list parity (ec-point-x pt)))))

;;; ec-decompress : Curve × (List Symbol Int) → Point
;;; Decompress point from (parity x).
(define (ec-decompress curve compressed)
  (doc 'export #t)
  (doc 'type '(-> Curve (Union (List Symbol Int) Symbol) Point))
  (doc 'description "Decompress point from x-coordinate and y-parity")
  (if (eq? compressed 'infinity)
      ec-infinity
      (let* ([parity (car compressed)]
             [x (cadr compressed)]
             [p (ec-p curve)]
             [a (ec-a curve)]
             [b (ec-b curve)]
             ;; y² = x³ + ax + b
             [x2 (mod* x x p)]
             [x3 (mod* x2 x p)]
             [y-sq (mod+ x3 (mod+ (mod* a x p) b p) p)]
             [y (mod-sqrt y-sq p)])
        (if (not y)
            (error 'ec-decompress "invalid compressed point: no square root")
            ;; Choose y with correct parity
            (let ([y-final (if (eq? (if (even? y) 'even 'odd) parity)
                               y
                               (- p y))])
              (make-ec-point x y-final))))))

;;; ============================================================================
;;; Section 7: Standard Curves
;;; ============================================================================

(doc 'section 'standard-curves)

(doc 'note "Standard elliptic curves with officially published parameters")
(doc 'note "secp256k1: used by Bitcoin, Ethereum (Koblitz curve)")
(doc 'note "P-256 (secp256r1): NIST recommended curve")

;;; secp256k1 curve parameters (Bitcoin/Ethereum)
;;; y² = x³ + 7 over F_p
(define secp256k1-p
  #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F)

(define secp256k1-a 0)
(define secp256k1-b 7)

(define secp256k1-n  ; Curve order
  #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141)

(define secp256k1-Gx  ; Generator x
  #x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798)

(define secp256k1-Gy  ; Generator y
  #x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8)

(doc secp256k1 'export #t)
(doc secp256k1 'description "secp256k1 curve (Bitcoin, Ethereum)")
(define secp256k1
  (make-ec-curve secp256k1-p
                 secp256k1-a
                 secp256k1-b
                 secp256k1-n
                 (make-ec-point secp256k1-Gx secp256k1-Gy)))

;;; P-256 (secp256r1 / prime256v1) curve parameters
;;; y² = x³ - 3x + b over F_p (a = -3 for efficiency)
(define p256-p
  #xFFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF)

(define p256-a
  #xFFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFC)  ; = -3 mod p

(define p256-b
  #x5AC635D8AA3A93E7B3EBBD55769886BC651D06B0CC53B0F63BCE3C3E27D2604B)

(define p256-n  ; Curve order
  #xFFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551)

(define p256-Gx  ; Generator x
  #x6B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C296)

(define p256-Gy  ; Generator y
  #x4FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5)

(doc p256 'export #t)
(doc p256 'description "P-256 curve (NIST, secp256r1)")
(define p256
  (make-ec-curve p256-p p256-a p256-b p256-n
                 (make-ec-point p256-Gx p256-Gy)))

;;; ============================================================================
;;; Section 8: Key Generation and Utilities
;;; ============================================================================

(doc 'section 'key-utilities)

;;; ec-pubkey : Curve × Int → Point
;;; Compute public key from private key: Q = d*G
;;; Uses constant-time multiplication to protect the private key.
(define (ec-pubkey curve privkey)
  (doc 'export #t)
  (doc 'type '(-> Curve Int Point))
  (doc 'description "Compute public key Q = d*G from private key d")
  (doc 'note "Uses constant-time Montgomery Ladder to protect private key")
  (ec-mul-montgomery curve privkey (ec-G curve)))

;;; ec-equal? : Point × Point → Boolean
;;; Test if two points are equal.
(define (ec-equal? P Q)
  (doc 'export #t)
  (doc 'type '(-> Point Point Boolean))
  (cond
    [(and (ec-infinity? P) (ec-infinity? Q)) #t]
    [(or (ec-infinity? P) (ec-infinity? Q)) #f]
    [else (and (= (ec-point-x P) (ec-point-x Q))
               (= (ec-point-y P) (ec-point-y Q)))]))

;;; ec-order-check : Curve × Point → Boolean
;;; Verify that n*P = O (point has correct order).
;;; For prime-order curves, this is automatic for valid points.
(define (ec-order-check curve pt)
  (doc 'export #t)
  (doc 'type '(-> Curve Point Boolean))
  (doc 'description "Verify n*P = O (point has correct order)")
  (ec-infinity? (ec-mul curve (ec-n curve) pt)))

;;; ============================================================================
;;; Section 9: ECDH Key Exchange
;;; ============================================================================

(doc 'section 'ecdh)

(doc 'note "ECDH (Elliptic Curve Diffie-Hellman) key agreement protocol")
(doc 'note "Alice: a (private), A = a*G (public)")
(doc 'note "Bob: b (private), B = b*G (public)")
(doc 'note "Shared secret: a*B = b*A = ab*G")

;;; ecdh-shared-secret : Curve × Int × Point → Point
;;; Compute ECDH shared secret from own private key and peer's public key.
;;; Uses constant-time multiplication to protect the private key.
(define (ecdh-shared-secret curve privkey peer-pubkey)
  (doc 'export #t)
  (doc 'type '(-> Curve Int Point Point))
  (doc 'description "Compute ECDH shared secret = privkey * peer_pubkey")
  (doc 'note "Uses constant-time Montgomery Ladder to protect private key")
  (ec-mul-montgomery curve privkey peer-pubkey))

;;; ecdh-shared-x : Curve × Int × Point → Int
;;; Return just the x-coordinate of the shared secret (common practice).
(define (ecdh-shared-x curve privkey peer-pubkey)
  (doc 'export #t)
  (doc 'type '(-> Curve Int Point Int))
  (doc 'description "ECDH shared secret x-coordinate (commonly used as key material)")
  (let ([shared (ecdh-shared-secret curve privkey peer-pubkey)])
    (if (ec-infinity? shared)
        (error 'ecdh-shared-x "invalid: shared secret is point at infinity")
        (ec-point-x shared))))
