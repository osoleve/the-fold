;;; core/number-theory/modular.ss — Modular Arithmetic
;;; @module modular
;;; @requires prelude
;;;
;;; Foundational modular arithmetic for number theory and cryptography.
;;;
;;; SECURITY CAVEAT: This implementation is NOT constant-time and is
;;; vulnerable to timing side-channel attacks. It is suitable for:
;;;   - Educational purposes
;;;   - Non-adversarial number theory computations
;;;   - Key generation and public operations
;;;
;;; DO NOT USE for private key operations (RSA decrypt, ECDSA sign, etc.)
;;; without implementing constant-time variants (Montgomery ladder, etc.).
;;;
;;; This is not dual-use cryptography - timing attacks are a known and
;;; accepted limitation of this educational implementation.
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies: core/base/prelude.ss

(load "core/base/prelude.ss")

;;; ============================================================
;;; Basic Modular Operations
;;; ============================================================

;;; mod+ : Int × Int × Int → Int
;;; Modular addition: (a + b) mod m
;;; Assumes m > 0.
(define (mod+ a b m)
  (modulo (+ a b) m))

;;; mod- : Int × Int × Int → Int
;;; Modular subtraction: (a - b) mod m
;;; Assumes m > 0.
(define (mod- a b m)
  (modulo (- a b) m))

;;; mod* : Int × Int × Int → Int
;;; Modular multiplication: (a * b) mod m
;;; Assumes m > 0.
(define (mod* a b m)
  (modulo (* a b) m))

;;; ============================================================
;;; Modular Exponentiation
;;; ============================================================

;;; mod-expt : Int × Nat × Int → Int
;;; Modular exponentiation using square-and-multiply algorithm.
;;; Computes (base^exp) mod m efficiently.
;;; Time complexity: O(log exp)
;;; Assumes m > 0, exp >= 0.
(define (mod-expt base exp m)
  (let loop ([b (modulo base m)]
             [e exp]
             [result 1])
       (cond
        [(= e 0) result]
        [(odd? e)
         (loop (modulo (* b b) m)
               (quotient e 2)
               (modulo (* result b) m))]
        [else
         (loop (modulo (* b b) m)
               (quotient e 2)
               result)])))

;;; ============================================================
;;; Extended Euclidean Algorithm
;;; ============================================================

;;; extended-gcd : Int × Int → (List Int Int Int)
;;; Extended Euclidean algorithm.
;;; Returns (gcd a b, x, y) where gcd = ax + by.
;;; Bézout's identity.
(define (extended-gcd a b)
  (if (= b 0)
      (list a 1 0)
      (let* ([q (quotient a b)]
             [r (- a (* q b))]
             [result (extended-gcd b r)]
             [gcd (car result)]
             [x1 (cadr result)]
             [y1 (caddr result)]
             [x y1]
             [y (- x1 (* q y1))])
            (list gcd x y))))

;;; gcd : Int × Int → Int
;;; Greatest common divisor using Euclidean algorithm.
(define (gcd a b)
  (if (= b 0)
      (abs a)
      (gcd b (modulo a b))))

;;; ============================================================
;;; Modular Inverse
;;; ============================================================

;;; mod-inverse : Int × Int → Int | #f
;;; Compute modular multiplicative inverse of a modulo m.
;;; Returns x such that (a * x) ≡ 1 (mod m), or #f if no inverse exists.
;;; Inverse exists iff gcd(a, m) = 1.
(define (mod-inverse a m)
  (let* ([result (extended-gcd a m)]
         [g (car result)]
         [x (cadr result)])
        (if (= g 1)
            (modulo x m)
            #f)))

;;; ============================================================
;;; Chinese Remainder Theorem
;;; ============================================================

;;; crt : (List Int) × (List Int) → Int | #f
;;; Chinese Remainder Theorem solver.
;;; Given remainders [a1, a2, ..., ak] and moduli [m1, m2, ..., mk],
;;; find x such that:
;;;   x ≡ a1 (mod m1)
;;;   x ≡ a2 (mod m2)
;;;   ...
;;;   x ≡ ak (mod mk)
;;;
;;; Assumes all moduli are pairwise coprime.
;;; Returns #f if moduli are not pairwise coprime.
(define (crt remainders moduli)
  (if (or (null? remainders) (null? moduli))
      0
      (let ([M (fold-left * 1 moduli)])
           (let loop ([rs remainders]
                      [ms moduli]
                      [result 0])
                (if (null? rs)
                    (modulo result M)
                    (let* ([ai (car rs)]
                           [mi (car ms)]
                           [Mi (quotient M mi)]
                           [yi (mod-inverse Mi mi)])
                          (if yi
                              (loop (cdr rs)
                                    (cdr ms)
                                    (+ result (* ai Mi yi)))
                              #f)))))))

;;; ============================================================
;;; Montgomery Multiplication
;;; ============================================================

;;; Montgomery multiplication is an optimization for modular multiplication
;;; when doing many multiplications with the same modulus.
;;; It works in "Montgomery space" where numbers are represented as aR mod m.

;;; montgomery-reduce : Int × Int × Int × Int × Int → Int
;;; Montgomery reduction: converts from Montgomery space back to normal.
;;; Given T, modulus m, R-mask (R-1), R-bits (log2 R), and m' (negative inverse of m mod R),
;;; computes (T * R^-1) mod m.
;;; This is an internal helper for Montgomery multiplication.
;;; Uses bitwise operations for efficiency since R is a power of 2.
(define (montgomery-reduce T m R-mask R-bits m-prime)
  (let* ([t (bitwise-and (* T m-prime) R-mask)]
         [u (bitwise-arithmetic-shift (+ T (* t m)) (- R-bits))])
        (if (>= u m)
            (- u m)
            u)))

;;; montgomery-setup : Int → (List Int Int Int Int)
;;; Setup for Montgomery multiplication.
;;; Given modulus m, finds R (smallest power of 2 > m) and computes parameters.
;;; Returns (R, R-mask, R-bits, m') where:
;;;   R = 2^k > m
;;;   R-mask = R - 1 (for bitwise AND to compute mod R)
;;;   R-bits = k (for arithmetic shift to compute quotient by R)
;;;   m' = -m^-1 mod R
(define (montgomery-setup m)
  (let* ([R (let loop ([r 1] [bits 0])
                 (if (> r m)
                     (cons r bits)
                     (loop (* r 2) (+ bits 1))))]
         [R-val (car R)]
         [R-bits (cdr R)]
         [R-mask (- R-val 1)]
         [result (extended-gcd m R-val)]
         [m-inv (cadr result)]
         [m-prime (modulo (- m-inv) R-val)])
        (list R-val R-mask R-bits m-prime)))

;;; montgomery-mult : Int × Int × Int × Int × Int × Int → Int
;;; Montgomery multiplication: computes (a * b * R^-1) mod m.
;;; Given a, b in Montgomery space (i.e., aR mod m, bR mod m),
;;; R-mask, R-bits, and m' = -m^-1 mod R,
;;; returns (a * b * R^-1) mod m, which is (ab)R mod m (still in Montgomery space).
(define (montgomery-mult a b m R-mask R-bits m-prime)
  (montgomery-reduce (* a b) m R-mask R-bits m-prime))

;;; to-montgomery : Int × Int × Int → Int
;;; Convert a to Montgomery space: (a * R) mod m.
(define (to-montgomery a m R)
  (modulo (* a R) m))

;;; from-montgomery : Int × Int × Int × Int × Int → Int
;;; Convert from Montgomery space back to normal: (a * R^-1) mod m.
(define (from-montgomery a m R-mask R-bits m-prime)
  (montgomery-reduce a m R-mask R-bits m-prime))

;;; montgomery-expt : Int × Nat × Int → Int
;;; Modular exponentiation using Montgomery multiplication.
;;; Computes (base^exp) mod m using Montgomery representation.
;;; More efficient than regular mod-expt for large exponents.
(define (montgomery-expt base exp m)
  (if (= m 1)
      0
      (let* ([setup (montgomery-setup m)]
             [R (car setup)]
             [R-mask (cadr setup)]
             [R-bits (caddr setup)]
             [m-prime (cadddr setup)]
             [base-mont (to-montgomery base m R)]
             [one-mont (to-montgomery 1 m R)])
            (let loop ([b base-mont]
                       [e exp]
                       [result one-mont])
                 (cond
                  [(= e 0)
                   (from-montgomery result m R-mask R-bits m-prime)]
                  [(odd? e)
                   (loop (montgomery-mult b b m R-mask R-bits m-prime)
                         (quotient e 2)
                         (montgomery-mult result b m R-mask R-bits m-prime))]
                  [else
                   (loop (montgomery-mult b b m R-mask R-bits m-prime)
                         (quotient e 2)
                         result)])))))
