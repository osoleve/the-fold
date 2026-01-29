(load "core/base/prelude.ss")

(doc 'module 'modular)
(doc 'description "Foundational modular arithmetic for number theory and cryptography")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'basic-modular-ops)

(define (mod+ a b m)
  (doc 'export #t)
  (doc 'type '(-> Int Int Int Int))
  (doc 'description "Modular addition: (a + b) mod m. Assumes m > 0")
  (modulo (+ a b) m))

(define (mod- a b m)
  (doc 'export #t)
  (doc 'type '(-> Int Int Int Int))
  (doc 'description "Modular subtraction: (a - b) mod m. Assumes m > 0")
  (modulo (- a b) m))

(define (mod* a b m)
  (doc 'export #t)
  (doc 'type '(-> Int Int Int Int))
  (doc 'description "Modular multiplication: (a * b) mod m. Assumes m > 0")
  (modulo (* a b) m))

(doc 'section 'modular-exponentiation)

(define (mod-expt base exp m)
  (doc 'export #t)
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
  (doc 'export #t)
  (doc 'type '(-> Int Int (List Int)))
  (doc 'description "Extended Euclidean algorithm (iterative). Returns (gcd a b, x, y) where gcd = ax + by (Bézout's identity). Uses iteration to avoid stack growth for large inputs.")
  ;; Iterative version: maintain (old-r, r) and corresponding (old-s, s), (old-t, t)
  ;; such that old-r = a*old-s + b*old-t and r = a*s + b*t
  (let loop ([old-r a] [r b]
             [old-s 1] [s 0]
             [old-t 0] [t 1])
    (if (= r 0)
        (list old-r old-s old-t)
        (let* ([q (quotient old-r r)]
               [new-r (- old-r (* q r))]
               [new-s (- old-s (* q s))]
               [new-t (- old-t (* q t))])
          (loop r new-r s new-s t new-t)))))

;;; gcd : Int × Int → Int
;;; Greatest common divisor using Euclidean algorithm.
(define (gcd a b)
  (doc 'export #t)
  (if (= b 0)
      (abs a)
      (gcd b (modulo a b))))

(doc 'section 'modular-inverse)

(define (mod-inverse a m)
  (doc 'export #t)
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
  (doc 'export #t)
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
  (doc 'export #t)
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
  (doc 'export #t)
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
  (doc 'export #t)
  (montgomery-reduce (* a b) m R m-prime))

;;; to-montgomery : Int × Int × Int → Int
;;; Convert a to Montgomery space: (a * R) mod m.
(define (to-montgomery a m R)
  (doc 'export #t)
  (modulo (* a R) m))

;;; from-montgomery : Int × Int × Int × Int → Int
;;; Convert from Montgomery space back to normal: (a * R^-1) mod m.
(define (from-montgomery a m R m-prime)
  (doc 'export #t)
  (montgomery-reduce a m R m-prime))

;;; montgomery-expt : Int × Nat × Int → Int
;;; Modular exponentiation using Montgomery multiplication.
;;; Computes (base^exp) mod m using Montgomery representation.
;;; More efficient than regular mod-expt for large exponents.
(define (montgomery-expt base exp m)
  (doc 'export #t)
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

;;; ============================================================================
;;; Quadratic Residues and Modular Square Roots
;;; ============================================================================

(doc 'section 'quadratic-residues)

(doc 'note "Quadratic residue theory and Tonelli-Shanks algorithm for modular square roots.")
(doc 'note "Essential for elliptic curve cryptography where we compute y = sqrt(x³ + ax + b) mod p.")

;;; quadratic-residue? : Int × Int → Boolean
;;; Test if a is a quadratic residue mod p (i.e., has a square root).
;;; Uses Euler's criterion: a is a QR iff a^((p-1)/2) ≡ 1 (mod p).
;;; Assumes p is an odd prime and a is not divisible by p.
(define (quadratic-residue? a p)
  (doc 'export #t)
  (doc 'type '(-> Int Int Boolean))
  (doc 'description "Test if a has a square root mod p using Euler's criterion")
  (doc 'note "Returns #t if a ≡ 0 (mod p), as 0 is trivially a QR")
  (let ([a-mod (modulo a p)])
    (or (= a-mod 0)
        (= 1 (mod-expt a-mod (quotient (- p 1) 2) p)))))

;;; legendre-symbol : Int × Int → Int
;;; Compute the Legendre symbol (a/p) for odd prime p.
;;; Returns: 1 if a is a non-zero QR, -1 if a is a non-residue, 0 if p|a.
(define (legendre-symbol a p)
  (doc 'export #t)
  (doc 'type '(-> Int Int Int))
  (doc 'description "Legendre symbol: 1 if QR, -1 if non-residue, 0 if p divides a")
  (let ([a-mod (modulo a p)])
    (cond
      [(= a-mod 0) 0]
      [(= 1 (mod-expt a-mod (quotient (- p 1) 2) p)) 1]
      [else -1])))

;;; mod-sqrt : Int × Int → (Union Int #f)
;;; Compute modular square root: find x such that x² ≡ a (mod p).
;;; Returns one of the two roots (the other is p - x), or #f if no root exists.
;;; Assumes p is an odd prime.
(define (mod-sqrt a p)
  (doc 'export #t)
  (doc 'type '(-> Int Int (Union Int Boolean)))
  (doc 'description "Modular square root using Tonelli-Shanks algorithm")
  (doc 'note "Returns #f if a is not a quadratic residue mod p")
  (doc 'note "For the other root, compute (p - result)")
  (let ([a-mod (modulo a p)])
    (cond
      ;; Trivial case: sqrt(0) = 0
      [(= a-mod 0) 0]
      ;; Special case: p = 2 - in GF(2), sqrt(a) = a
      [(= p 2) a-mod]
      ;; Check if a is a quadratic residue
      [(not (quadratic-residue? a-mod p)) #f]
      ;; Special case: p ≡ 3 (mod 4) - simple formula
      [(= 3 (modulo p 4))
       (mod-expt a-mod (quotient (+ p 1) 4) p)]
      ;; General case: Tonelli-Shanks algorithm
      [else
       (tonelli-shanks a-mod p)])))

;;; tonelli-shanks : Int × Int → Int
;;; Tonelli-Shanks algorithm for modular square root.
;;; Assumes a is a non-zero quadratic residue mod p, and p ≡ 1 (mod 4).
(define (tonelli-shanks a p)
  (doc 'export #t)
  (doc 'type '(-> Int Int Int))
  (doc 'description "Tonelli-Shanks algorithm for p ≡ 1 (mod 4)")
  ;; Step 1: Factor out powers of 2 from p-1
  ;; Write p - 1 = Q * 2^S where Q is odd
  (let-values ([(Q S) (factor-out-2s (- p 1))])
    ;; Step 2: Find a quadratic non-residue z
    (let ([z (find-non-residue p)])
      ;; Step 3: Initialize
      (let loop ([M S]
                 [c (mod-expt z Q p)]           ; c = z^Q
                 [t (mod-expt a Q p)]           ; t = a^Q
                 [R (mod-expt a (quotient (+ Q 1) 2) p)])  ; R = a^((Q+1)/2)
        ;; Step 4: Loop until t = 1
        (cond
          [(= t 1) R]  ; Done! R² ≡ a (mod p)
          [else
           ;; Find least i, 0 < i < M, such that t^(2^i) ≡ 1 (mod p)
           (let ([i (find-least-i t M p)])
             ;; Update values
             (let* ([b (mod-expt c (expt 2 (- M i 1)) p)]  ; b = c^(2^(M-i-1))
                    [new-M i]
                    [new-c (mod* b b p)]                    ; c = b²
                    [new-t (mod* t new-c p)]                ; t = t * c
                    [new-R (mod* R b p)])                   ; R = R * b
               (loop new-M new-c new-t new-R)))])))))

;;; factor-out-2s : Int → (Values Int Int)
;;; Factor n = Q * 2^S where Q is odd.
;;; Returns (Q, S).
(define (factor-out-2s n)
  (let loop ([q n] [s 0])
    (if (odd? q)
        (values q s)
        (loop (quotient q 2) (+ s 1)))))

;;; find-non-residue : Int → Int
;;; Find the smallest quadratic non-residue mod p.
;;; For random p, expected to find one quickly (about half of 1..p-1 are non-residues).
;;; Requires p to be an odd prime (p > 2). For p = 2, there are no non-residues.
(define (find-non-residue p)
  (when (= p 2)
    (error 'find-non-residue "no non-residues exist for p=2 (GF(2))"))
  (let loop ([z 2])
    (if (= -1 (legendre-symbol z p))
        z
        (loop (+ z 1)))))

;;; find-least-i : Int × Int × Int → Int
;;; Find the least i, 0 < i < M, such that t^(2^i) ≡ 1 (mod p).
(define (find-least-i t M p)
  (let loop ([i 1] [power (mod* t t p)])  ; Start with t^2
    (cond
      [(= power 1) i]
      [(>= i M) (error 'find-least-i "should not happen if t is a QR")]
      [else (loop (+ i 1) (mod* power power p))])))

;;; mod-sqrt-both : Int × Int → (Union (List Int Int) #f)
;;; Return both square roots of a mod p, or #f if none exist.
;;; The roots are (r, p-r) where r is the "positive" root (r ≤ p/2).
(define (mod-sqrt-both a p)
  (doc 'export #t)
  (doc 'type '(-> Int Int (Union (List Int) Boolean)))
  (doc 'description "Return both square roots as a list (smaller first), or #f")
  (let ([r (mod-sqrt a p)])
    (if r
        (let ([r2 (modulo (- p r) p)])  ; Ensure r2 is in [0, p-1]
          (if (= r r2)
              (list r)        ; Single root (e.g., 0, or p=2)
              (if (<= r r2)
                  (list r r2)
                  (list r2 r))))
        #f)))
