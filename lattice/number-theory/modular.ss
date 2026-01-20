(load "core/base/prelude.ss")

(doc 'module 'modular)
(doc 'description "Foundational modular arithmetic for number theory and cryptography")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'basic-modular-ops)

(define (mod+ a b m)
  (doc 'type '(-> Int Int Int Int))
  (doc 'description "Modular addition: (a + b) mod m. Assumes m > 0")
  (modulo (+ a b) m))

(define (mod- a b m)
  (doc 'type '(-> Int Int Int Int))
  (doc 'description "Modular subtraction: (a - b) mod m. Assumes m > 0")
  (modulo (- a b) m))

(define (mod* a b m)
  (doc 'type '(-> Int Int Int Int))
  (doc 'description "Modular multiplication: (a * b) mod m. Assumes m > 0")
  (modulo (* a b) m))

(doc 'section 'modular-exponentiation)

(define (mod-expt base exp m)
  (doc 'type '(-> Int Nat Int Int))
  (doc 'description "Modular exponentiation using square-and-multiply algorithm. Computes (base^exp) mod m efficiently")
  (doc 'complexity "O(log exp)")
  (doc 'note "Assumes m > 0, exp >= 0")
  (let loop ([b (modulo base m)]
             [e exp]
             [result 1])
       (cond
        [(= e 0) (modulo result m)]  ; Handle m=1 case: n^0 mod 1 = 0
        [(odd? e)
         (loop (modulo (* b b) m)
               (quotient e 2)
               (modulo (* result b) m))]
        [else
         (loop (modulo (* b b) m)
               (quotient e 2)
               result)])))

(doc 'section 'extended-euclidean)

(define (extended-gcd a b)
  (doc 'type '(-> Int Int (List Int)))
  (doc 'description "Extended Euclidean algorithm. Returns (gcd a b, x, y) where gcd = ax + by (Bézout's identity)")
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

(doc 'section 'modular-inverse)

(define (mod-inverse a m)
  (doc 'type '(-> Int Int (Union Int Boolean)))
  (doc 'description "Compute modular multiplicative inverse of a modulo m. Returns x such that (a * x) ≡ 1 (mod m), or #f if no inverse exists. Inverse exists iff gcd(a, m) = 1")
  (let* ([result (extended-gcd a m)]
         [g (car result)]
         [x (cadr result)])
        (if (= g 1)
            (modulo x m)
            #f)))

(doc 'section 'chinese-remainder-theorem)

(define (crt remainders moduli)
  (doc 'type '(-> (List Int) (List Int) (Union Int Boolean)))
  (doc 'description "Chinese Remainder Theorem solver. Given remainders [a1, a2, ..., ak] and moduli [m1, m2, ..., mk], find x such that x ≡ ai (mod mi) for all i")
  (doc 'note "Assumes all moduli are pairwise coprime. Returns #f if moduli are not pairwise coprime")
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

(doc 'section 'montgomery-multiplication)

(doc 'note "Montgomery multiplication is an optimization for modular multiplication when doing many multiplications with the same modulus. It works in Montgomery space where numbers are represented as aR mod m")

(define (montgomery-reduce T m R m-prime)
  (doc 'type '(-> Int Int Int Int Int))
  (doc 'description "Montgomery reduction: converts from Montgomery space back to normal. Given T, modulus m, R (power of 2), and m' (negative inverse of m mod R), computes (T * R^-1) mod m. This is an internal helper for Montgomery multiplication")
  (let* ([t (modulo (* T m-prime) R)]
         [u (quotient (+ T (* t m)) R)])
        (if (>= u m)
            (- u m)
            u)))

;;; montgomery-setup : Int → (List Int)
;;; Setup for Montgomery multiplication.
;;; Given modulus m, finds R (smallest power of 2 > m) and R' (R^-1 mod m).
;;; Returns (R, m') where m' = -m^-1 mod R.
(define (montgomery-setup m)
  (let* ([R (let loop ([r 1])
                 (if (> r m)
                     r
                     (loop (* r 2))))]
         [result (extended-gcd m R)]
         [m-inv (cadr result)]
         [m-prime (modulo (- m-inv) R)])
        (list R m-prime)))

;;; montgomery-mult : Int × Int × Int × Int × Int → Int
;;; Montgomery multiplication: computes (a * b * R^-1) mod m.
;;; Given a, b in Montgomery space (i.e., aR mod m, bR mod m),
;;; R (power of 2 > m), and m' = -m^-1 mod R,
;;; returns (a * b * R^-1) mod m, which is (ab)R mod m (still in Montgomery space).
(define (montgomery-mult a b m R m-prime)
  (montgomery-reduce (* a b) m R m-prime))

;;; to-montgomery : Int × Int × Int → Int
;;; Convert a to Montgomery space: (a * R) mod m.
(define (to-montgomery a m R)
  (modulo (* a R) m))

;;; from-montgomery : Int × Int × Int × Int → Int
;;; Convert from Montgomery space back to normal: (a * R^-1) mod m.
(define (from-montgomery a m R m-prime)
  (montgomery-reduce a m R m-prime))

;;; montgomery-expt : Int × Nat × Int → Int
;;; Modular exponentiation using Montgomery multiplication.
;;; Computes (base^exp) mod m using Montgomery representation.
;;; More efficient than regular mod-expt for large exponents.
(define (montgomery-expt base exp m)
  (if (= m 1)
      0
      (let* ([setup (montgomery-setup m)]
             [R (car setup)]
             [m-prime (cadr setup)]
             [base-mont (to-montgomery base m R)]
             [one-mont (to-montgomery 1 m R)])
            (let loop ([b base-mont]
                       [e exp]
                       [result one-mont])
                 (cond
                  [(= e 0)
                   (from-montgomery result m R m-prime)]
                  [(odd? e)
                   (loop (montgomery-mult b b m R m-prime)
                         (quotient e 2)
                         (montgomery-mult result b m R m-prime))]
                  [else
                   (loop (montgomery-mult b b m R m-prime)
                         (quotient e 2)
                         result)])))))
