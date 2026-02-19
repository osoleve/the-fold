;;; modular.sexp — Development tasks for lattice/number-theory/modular.ss
;;;
;;; Module: modular (tier 0, depends on prelude)
;;; 333 lines, ~19 exported functions
;;; Modular arithmetic: basic ops, exponentiation (square-and-multiply),
;;; extended GCD, modular inverse, CRT, Montgomery multiplication,
;;; quadratic residues, Tonelli-Shanks.
;;;
;;; Git history:
;;;   fcf2741b feat(module): migrate lattice/crypto and deps from load to require
;;;   939f791a perf(number-theory): Convert extended-gcd to iterative implementation
;;;   3c34da74 feat(number-theory): Add Tonelli-Shanks modular square root

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement mod-expt (square-and-multiply)
  ;; ──────────────────────────────────────────────
  ;; The classic modular exponentiation algorithm. Foundation for
  ;; everything else in the module.

  (task
    (id "number-theory/modular/mod-expt-impl")
    (module modular)
    (tier 0)
    (type implement)
    (description "Implement mod-expt: compute (base^exp) mod m using the square-and-multiply algorithm. This runs in O(log exp) time. Maintain three values in a loop: b (current base), e (remaining exponent), and result (accumulated product). At each step: if e is odd, multiply result by b (mod m). Then square b and halve e. When e reaches 0, return result. Handle the edge case m=1 (any n^0 mod 1 = 0). Use modulo, odd?, and quotient.")
    (context "Available: prelude (modulo, odd?, even?, quotient, *, +, =). Assumes m > 0, exp >= 0. All arithmetic uses Scheme's exact integers (arbitrary precision).")
    (before "(define (mod-expt base exp m)
  (doc 'type '(-> Int Nat Int Int))
  (doc 'description \"Modular exponentiation using square-and-multiply\")
  ;; TODO: implement square-and-multiply loop
  0)")
    (test-file "lattice/number-theory/test-modular.ss")
    (test-forms ("(assert-equal 1 (mod-expt 2 0 7))"
                 "(assert-equal 2 (mod-expt 2 1 7))"
                 "(assert-equal 1 (mod-expt 2 3 7))"
                 "(assert-equal 8 (mod-expt 5 3 13))"
                 "(assert-equal 976371285 (mod-expt 2 100 1000000007))"
                 ";; Fermat's Little Theorem: a^(p-1) = 1 (mod p)
(assert-equal 1 (mod-expt 2 6 7))"))
    (available-modules (prelude))
    (hints ("Initialize: b = base mod m, e = exp, result = 1"
            "Loop until e = 0"
            "If e is odd: result = (result * b) mod m"
            "Always: b = (b * b) mod m, e = e / 2 (integer division)"
            "At the end, return (modulo result m) for the m=1 edge case"))
    (reference-solution "(define (mod-expt base exp m)
  (let loop ([b (modulo base m)]
             [e exp]
             [result 1])
       (cond
        [(= e 0) (modulo result m)]
        [(odd? e)
         (loop (modulo (* b b) m)
               (quotient e 2)
               (modulo (* result b) m))]
        [else
         (loop (modulo (* b b) m)
               (quotient e 2)
               result)])))")
    (alternatives ())
    (source-commit "939f791a")
    (difficulty 4))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement extended-gcd (iterative)
  ;; ──────────────────────────────────────────────
  ;; The extended Euclidean algorithm: returns (gcd, x, y) where
  ;; gcd = a*x + b*y (Bézout's identity).

  (task
    (id "number-theory/modular/extended-gcd-impl")
    (module modular)
    (tier 0)
    (type implement)
    (description "Implement extended-gcd (iterative): given integers a and b, return (list gcd x y) where gcd = a*x + b*y (Bézout's identity). Maintain six values: old-r and r (remainders), old-s and s (x coefficients), old-t and t (y coefficients). At each step: compute q = old-r / r (integer division), then update all three pairs: new = old - q*current. When r reaches 0, return (list old-r old-s old-t). This iterative form avoids stack growth for large inputs.")
    (context "Available: prelude (quotient, -, *, =, list). Scheme has arbitrary precision integers. The invariant at each step is: old-r = a*old-s + b*old-t and r = a*s + b*t.")
    (before "(define (extended-gcd a b)
  (doc 'type '(-> Int Int (List Int)))
  (doc 'description \"Extended Euclidean algorithm: returns (gcd x y) where gcd = ax + by\")
  ;; TODO: iterative implementation
  (list a 1 0))")
    (test-file "lattice/number-theory/test-modular.ss")
    (test-forms (";; gcd(240, 46) = 2
(assert-equal 2 (car (extended-gcd 240 46)))"
                 ";; Bézout: gcd = 240*x + 46*y
(let* ([result (extended-gcd 240 46)]
       [g (car result)] [x (cadr result)] [y (caddr result)])
  (assert-equal g (+ (* 240 x) (* 46 y))))"
                 "(assert-equal 1 (car (extended-gcd 17 13)))"
                 ";; Bézout for (17, 13)
(let* ([result (extended-gcd 17 13)]
       [g (car result)] [x (cadr result)] [y (caddr result)])
  (assert-equal g (+ (* 17 x) (* 13 y))))"))
    (available-modules (prelude))
    (hints ("Initialize: old-r=a, r=b, old-s=1, s=0, old-t=0, t=1"
            "Loop while r != 0"
            "Compute q = quotient(old-r, r)"
            "Update: new-r = old-r - q*r, new-s = old-s - q*s, new-t = old-t - q*t"
            "Then shift: old becomes current, current becomes new"
            "Return (list old-r old-s old-t) when r = 0"))
    (reference-solution "(define (extended-gcd a b)
  (let loop ([old-r a] [r b]
             [old-s 1] [s 0]
             [old-t 0] [t 1])
    (if (= r 0)
        (list old-r old-s old-t)
        (let* ([q (quotient old-r r)]
               [new-r (- old-r (* q r))]
               [new-s (- old-s (* q s))]
               [new-t (- old-t (* q t))])
          (loop r new-r s new-s t new-t)))))")
    (alternatives ())
    (source-commit "939f791a")
    (difficulty 5))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement mod-inverse
  ;; ──────────────────────────────────────────────
  ;; Composition: uses extended-gcd to find multiplicative inverse.

  (task
    (id "number-theory/modular/mod-inverse-impl")
    (module modular)
    (tier 0)
    (type implement)
    (description "Implement mod-inverse: compute the modular multiplicative inverse of a modulo m. The inverse x satisfies a*x ≡ 1 (mod m). It exists if and only if gcd(a, m) = 1. Use extended-gcd to find (gcd, x, y) where gcd = a*x + m*y. If gcd = 1, return x mod m (to ensure result is in [0, m-1]). If gcd ≠ 1, return #f (no inverse exists).")
    (context "Available: prelude, extended-gcd (returns (list gcd x y) where gcd = a*x + b*y), modulo. Return #f when no inverse exists.")
    (before "(define (mod-inverse a m)
  (doc 'type '(-> Int Int (Union Int Boolean)))
  (doc 'description \"Modular multiplicative inverse, or #f if none exists\")
  ;; TODO: use extended-gcd, check gcd = 1
  #f)")
    (test-file "lattice/number-theory/test-modular.ss")
    (test-forms ("(assert-equal 5 (mod-inverse 3 7))"
                 "(assert-equal 8 (mod-inverse 5 13))"
                 ";; Verify: (a * a^-1) mod m = 1
(let ([inv (mod-inverse 3 7)])
  (assert-equal 1 (modulo (* 3 inv) 7)))"
                 ";; No inverse when gcd(a,m) != 1
(assert-false (mod-inverse 4 8))"
                 "(assert-false (mod-inverse 6 9))"))
    (available-modules (prelude))
    (hints ("Call (extended-gcd a m) to get (list gcd x y)"
            "Extract gcd with (car result), x with (cadr result)"
            "If gcd = 1, return (modulo x m) — the modulo handles negative x"
            "If gcd ≠ 1, return #f"))
    (reference-solution "(define (mod-inverse a m)
  (let* ([result (extended-gcd a m)]
         [g (car result)]
         [x (cadr result)])
        (if (= g 1)
            (modulo x m)
            #f)))")
    (alternatives ())
    (source-commit "939f791a")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement Chinese Remainder Theorem
  ;; ──────────────────────────────────────────────
  ;; Classic number theory algorithm using modular inverse.

  (task
    (id "number-theory/modular/crt-impl")
    (module modular)
    (tier 0)
    (type implement)
    (description "Implement crt (Chinese Remainder Theorem): given remainders [a1,...,ak] and moduli [m1,...,mk] (pairwise coprime), find x such that x ≡ ai (mod mi) for all i. Algorithm: compute M = product of all moduli. For each i: compute Mi = M/mi, find yi = mod-inverse(Mi, mi), accumulate ai*Mi*yi. Return result mod M. If any mod-inverse returns #f, the moduli are not coprime — return #f. Empty lists should return 0.")
    (context "Available: prelude (fold-left, car, cdr, null?, quotient, modulo, +, *), mod-inverse (returns integer or #f). Lists use standard Scheme pairs.")
    (before "(define (crt remainders moduli)
  (doc 'type '(-> (List Int) (List Int) (Union Int Boolean)))
  (doc 'description \"Chinese Remainder Theorem solver\")
  ;; TODO: compute M, iterate through remainders/moduli, accumulate
  0)")
    (test-file "lattice/number-theory/test-modular.ss")
    (test-forms (";; x ≡ 2 (mod 3), x ≡ 3 (mod 5), x ≡ 2 (mod 7) → 23
(assert-equal 23 (crt '(2 3 2) '(3 5 7)))"
                 ";; Verify: 23 mod 3 = 2, 23 mod 5 = 3, 23 mod 7 = 2
(let ([x (crt '(2 3 2) '(3 5 7))])
  (assert-equal 2 (modulo x 3))
  (assert-equal 3 (modulo x 5))
  (assert-equal 2 (modulo x 7)))"
                 "(assert-equal 23 (crt '(1 2 3) '(2 3 5)))"
                 "(assert-equal 0 (crt '(0 0 0) '(3 4 5)))"
                 "(assert-equal 0 (crt '() '()))"))
    (available-modules (prelude))
    (hints ("Compute M = product of all moduli using fold-left"
            "Handle empty case: return 0"
            "For each (ai, mi): Mi = M/mi, yi = mod-inverse(Mi, mi)"
            "If yi is #f, return #f (moduli not coprime)"
            "Accumulate result += ai * Mi * yi"
            "Return (modulo result M)"))
    (reference-solution "(define (crt remainders moduli)
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
                              #f)))))))")
    (alternatives ())
    (source-commit "939f791a")
    (difficulty 5))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement legendre-symbol
  ;; ──────────────────────────────────────────────
  ;; Uses Euler's criterion — elegant one-liner once you know it.

  (task
    (id "number-theory/modular/legendre-impl")
    (module modular)
    (tier 0)
    (type implement)
    (description "Implement legendre-symbol: compute the Legendre symbol (a/p) for odd prime p. Returns 1 if a is a non-zero quadratic residue mod p (i.e., some x² ≡ a mod p exists), -1 if a is a quadratic non-residue, and 0 if p divides a. Use Euler's criterion: compute a^((p-1)/2) mod p. If the result is 1, a is a QR. If it is p-1 (which is ≡ -1 mod p), a is a non-residue. If a ≡ 0 (mod p), return 0.")
    (context "Available: prelude, mod-expt (modular exponentiation), modulo, quotient. Assumes p is an odd prime.")
    (before "(define (legendre-symbol a p)
  (doc 'type '(-> Int Int Int))
  (doc 'description \"Legendre symbol: 1 if QR, -1 if NR, 0 if p|a\")
  ;; TODO: Euler's criterion
  0)")
    (test-file "lattice/number-theory/test-modular.ss")
    (test-forms ("(assert-equal 1 (legendre-symbol 1 7))"
                 "(assert-equal 1 (legendre-symbol 2 7))"
                 "(assert-equal -1 (legendre-symbol 3 7))"
                 "(assert-equal 1 (legendre-symbol 4 7))"
                 "(assert-equal 0 (legendre-symbol 0 7))"
                 "(assert-equal 0 (legendre-symbol 7 7))"))
    (available-modules (prelude))
    (hints ("First reduce: a-mod = (modulo a p)"
            "If a-mod = 0, return 0"
            "Compute e = (mod-expt a-mod (quotient (- p 1) 2) p)"
            "If e = 1, return 1. Otherwise return -1."))
    (reference-solution "(define (legendre-symbol a p)
  (let ([a-mod (modulo a p)])
    (cond
      [(= a-mod 0) 0]
      [(= 1 (mod-expt a-mod (quotient (- p 1) 2) p)) 1]
      [else -1])))")
    (alternatives ())
    (source-commit "3c34da74")
    (difficulty 3))

) ;; end tasks
