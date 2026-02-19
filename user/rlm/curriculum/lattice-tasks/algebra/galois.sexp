;;; galois.sexp — Development tasks for lattice/algebra/galois.ss
;;;
;;; Module: galois (tier 0, depends on prelude, field, algebra/polynomial, primality)
;;; 794 lines, ~25 exported functions
;;; Galois field (finite field) arithmetic: GF(p), GF(p^n), GF(2^n).
;;; Prime fields, extension fields via polynomial quotient rings,
;;; binary fields with bitwise optimization, irreducible polynomial testing,
;;; primitive elements, trace, and norm.
;;;
;;; Git history:
;;;   6051e47c feat(algebra): Add finite field (Galois field) arithmetic
;;;   24724eb7 fix(galois): Support large fields without element enumeration
;;;   2f0c966d feat(galois): Implement Rabin's fast irreducibility test
;;;   8c418dfe fix(galois): Fix P1 issues from Gemini QA

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement gf-ext-normalize
  ;; ──────────────────────────────────────────────
  ;; Normalize coefficient lists for extension field elements.

  (task
    (id "algebra/galois/gf-ext-normalize-impl")
    (module galois)
    (tier 0)
    (type implement)
    (description "Implement gf-ext-normalize: normalize a coefficient list for a GF(p^n) extension field element. The result should have degree < n with trailing zeros removed (but at least one element). Two steps: (1) Pad or truncate the list to exactly length n using the field's zero element. (2) Remove trailing zeros (from the high-degree end) but keep at least one coefficient. Padding uses make-list and append. Trailing-zero removal: reverse the list, skip elements equal to zero (via field-equal-fn), then reverse back — but always keep the last element.")
    (context "Available: length, <, append, make-list, -, take, reverse, cdr, car, null?, cond, field-zero, field-equal-fn, let*, let loop. Two phases: pad/truncate then strip trailing zeros.")
    (before "(define (gf-ext-normalize coeffs n base-field)
  ;; TODO: pad/truncate to n, remove trailing zeros, keep at least one
  coeffs)")
    (test-file "lattice/algebra/test-galois.ss")
    (test-forms (";; Truncate long list
(let ([r (gf-ext-normalize '(1 2 3 4 5) 3 (gf-prime 7))])
  (assert-equal '(1 2 3) r))"
                 ";; Pad short list and strip trailing zeros
(let ([r (gf-ext-normalize '(1 2) 4 (gf-prime 5))])
  (assert-equal '(1 2) r))"
                 ";; All zeros -> keep at least one
(let ([r (gf-ext-normalize '(0 0 0) 3 (gf-prime 5))])
  (assert-equal '(0) r))"
                 ";; No trailing zeros -> unchanged
(let ([r (gf-ext-normalize '(1 2 3) 3 (gf-prime 5))])
  (assert-equal '(1 2 3) r))"))
    (available-modules (prelude field algebra/polynomial primality))
    (hints ("Phase 1: pad/truncate — if (< (length coeffs) n), append zeros; else (take n coeffs)"
            "Phase 2: strip trailing zeros — (let loop ([cs (reverse padded)]) ...)"
            "Keep at least one: (null? (cdr cs)) -> (reverse cs) (base case)"
            "Strip: (eq-fn (car cs) zero) -> (loop (cdr cs))"
            "Else: (reverse cs)"))
    (reference-solution "(define (gf-ext-normalize coeffs n base-field)
  (let* ([zero (field-zero base-field)]
         [eq-fn (field-equal-fn base-field)]
         [padded (if (< (length coeffs) n)
                     (append coeffs (make-list (- n (length coeffs)) zero))
                     (take n coeffs))])
    (let loop ([cs (reverse padded)])
      (cond
        [(null? (cdr cs)) (reverse cs)]
        [(eq-fn (car cs) zero) (loop (cdr cs))]
        [else (reverse cs)]))))")
    (alternatives ())
    (source-commit "6051e47c")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement gf-ext-equal?
  ;; ──────────────────────────────────────────────
  ;; Coefficient-wise equality for extension field elements.

  (task
    (id "algebra/galois/gf-ext-equal-impl")
    (module galois)
    (tier 0)
    (type implement)
    (description "Implement gf-ext-equal?: check if two GF(p^n) extension field elements are equal by comparing their coefficient lists element-wise using the base field's equality function. First check that the coefficient lists have the same length. Then walk both lists in parallel: if we reach the end (null) of both, return #t. At each step, check if the current coefficients are equal via eq-fn; if not, return #f; if yes, recurse on the tails. Uses or/and for short-circuit: (or (null? as) (and (eq-fn ...) (loop ...))).")
    (context "Available: gf-ext-coeffs, length, =, null?, car, cdr, field-equal-fn, and, or, let*, let loop. Parallel list walk with short-circuit.")
    (before "(define (gf-ext-equal? a b base-field)
  ;; TODO: coefficient-wise equality check
  #f)")
    (test-file "lattice/algebra/test-galois.ss")
    (test-forms (";; Equal elements
(let ([F (gf-prime 5)])
  (assert-true (gf-ext-equal?
    (make-gf-ext-element '(1 2) 'dummy F)
    (make-gf-ext-element '(1 2) 'dummy F)
    F)))"
                 ";; Different elements
(let ([F (gf-prime 5)])
  (assert-false (gf-ext-equal?
    (make-gf-ext-element '(1 2) 'dummy F)
    (make-gf-ext-element '(1 3) 'dummy F)
    F)))"
                 ";; Different lengths
(let ([F (gf-prime 5)])
  (assert-false (gf-ext-equal?
    (make-gf-ext-element '(1 2 3) 'dummy F)
    (make-gf-ext-element '(1 2) 'dummy F)
    F)))"))
    (available-modules (prelude field algebra/polynomial primality))
    (hints ("Extract coefficients: (gf-ext-coeffs a) and (gf-ext-coeffs b)"
            "Length check: (= (length ca) (length cb))"
            "Parallel loop: (let loop ([as ca] [bs cb]) ...)"
            "Short-circuit: (or (null? as) (and (eq-fn (car as) (car bs)) (loop (cdr as) (cdr bs))))"))
    (reference-solution "(define (gf-ext-equal? a b base-field)
  (let* ([ca (gf-ext-coeffs a)]
         [cb (gf-ext-coeffs b)]
         [eq-fn (field-equal-fn base-field)])
    (and (= (length ca) (length cb))
         (let loop ([as ca] [bs cb])
           (or (null? as)
               (and (eq-fn (car as) (car bs))
                    (loop (cdr as) (cdr bs))))))))")
    (alternatives ())
    (source-commit "6051e47c")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement unique-prime-divisors
  ;; ──────────────────────────────────────────────
  ;; Deduplicate the prime factorization.

  (task
    (id "algebra/galois/unique-prime-divisors-impl")
    (module galois)
    (tier 0)
    (type implement)
    (description "Implement unique-prime-divisors: given a positive integer n, return the list of its unique prime divisors in order. Uses factorize (from primality module) to get the full prime factorization (with repetitions), then deduplicates. For n <= 1, return the empty list. Walk the factors list maintaining a 'seen' accumulator: if the current factor is already in seen (via member), skip it; otherwise cons it to seen. Return (reverse seen) at the end to preserve order. This function is used by Rabin's irreducibility test to find the prime divisors of the polynomial degree.")
    (context "Available: factorize (from primality), member, cons, car, cdr, null?, reverse, let loop, <=, if. Loop with seen-set accumulator.")
    (before "(define (unique-prime-divisors n)
  ;; TODO: unique prime factors of n
  '())")
    (test-file "lattice/algebra/test-galois.ss")
    (test-forms (";; 60 = 2^2 * 3 * 5
(assert-equal '(2 3 5) (unique-prime-divisors 60))"
                 ";; Prime number
(assert-equal '(7) (unique-prime-divisors 7))"
                 ";; 1 has no prime divisors
(assert-equal '() (unique-prime-divisors 1))"
                 ";; Power of 2
(assert-equal '(2) (unique-prime-divisors 16))"))
    (available-modules (prelude field algebra/polynomial primality))
    (hints ("Base case: (<= n 1) -> '()"
            "Get all factors: (factorize n) returns list with repetitions"
            "Loop with accumulator: (let loop ([factors ...] [seen '()]) ...)"
            "Skip duplicates: (if (member p seen) (loop (cdr factors) seen) ...)"
            "Add new: (loop (cdr factors) (cons p seen))"
            "Result: (reverse seen)"))
    (reference-solution "(define (unique-prime-divisors n)
  (if (<= n 1)
      '()
      (let loop ([factors (factorize n)] [seen '()])
        (if (null? factors)
            (reverse seen)
            (let ([p (car factors)])
              (if (member p seen)
                  (loop (cdr factors) seen)
                  (loop (cdr factors) (cons p seen))))))))")
    (alternatives ())
    (source-commit "2f0c966d")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement proper-divisors
  ;; ──────────────────────────────────────────────
  ;; Find all proper divisors for primitive element testing.

  (task
    (id "algebra/galois/proper-divisors-impl")
    (module galois)
    (tier 0)
    (type implement)
    (description "Implement proper-divisors: return all proper divisors of n, i.e., all integers d where 1 <= d < n and d divides n evenly. For n < 2, return the empty list (no proper divisors). Generate the candidates (1, 2, ..., n-1) using (cdr (iota n)) — iota produces (0 1 ... n-1), cdr drops the 0. Then filter to keep only those d where (= 0 (modulo n d)). This is used by gf-primitive? to check if an element generates the full multiplicative group: g is primitive iff g^d != 1 for every proper divisor d of q-1.")
    (context "Available: iota, cdr, filter, modulo, =, <, if. One filter over a range.")
    (before "(define (proper-divisors n)
  ;; TODO: all d where 1 <= d < n and d | n
  '())")
    (test-file "lattice/algebra/test-galois.ss")
    (test-forms (";; 12: divisors are 1,2,3,4,6
(assert-equal '(1 2 3 4 6) (proper-divisors 12))"
                 ";; Prime: only 1
(assert-equal '(1) (proper-divisors 7))"
                 ";; 1 has no proper divisors
(assert-equal '() (proper-divisors 1))"
                 ";; 0 has no proper divisors
(assert-equal '() (proper-divisors 0))"))
    (available-modules (prelude field algebra/polynomial primality))
    (hints ("Base: (< n 2) -> '()"
            "Candidates: (cdr (iota n)) gives (1 2 ... n-1)"
            "Filter: (filter (lambda (d) (= 0 (modulo n d))) candidates)"
            "This is a one-liner after the base case check"))
    (reference-solution "(define (proper-divisors n)
  (if (< n 2)
      '()
      (filter (lambda (d) (= 0 (modulo n d)))
              (cdr (iota n)))))")
    (alternatives ())
    (source-commit "6051e47c")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement gf2n-bit-length
  ;; ──────────────────────────────────────────────
  ;; Bit length for binary field polynomial degree.

  (task
    (id "algebra/galois/gf2n-bit-length-impl")
    (module galois)
    (tier 0)
    (type implement)
    (description "Implement gf2n-bit-length: compute the bit length of an integer, which equals degree+1 for a polynomial in GF(2^n) represented as an integer. For 0, return 0. Otherwise, recursively shift right by 1 and add 1. This gives the position of the highest set bit plus one. For example: 0->0, 1->1, 7->3, 8->4, 255->8. This is the fundamental degree computation for GF(2^n) binary field arithmetic, where polynomials are integers with bit i representing the coefficient of x^i.")
    (context "Available: =, +, bitwise-arithmetic-shift-right, if. Simple recursion on bit position.")
    (before "(define (gf2n-bit-length x)
  ;; TODO: number of bits (highest set bit + 1)
  0)")
    (test-file "lattice/algebra/test-galois.ss")
    (test-forms (";; Zero has 0 bits
(assert-equal 0 (gf2n-bit-length 0))"
                 ";; Powers of 2
(assert-equal 1 (gf2n-bit-length 1))
(assert-equal 4 (gf2n-bit-length 8))"
                 ";; All bits set
(assert-equal 3 (gf2n-bit-length 7))
(assert-equal 8 (gf2n-bit-length 255))"
                 ";; Arbitrary
(assert-equal 5 (gf2n-bit-length 16))
(assert-equal 5 (gf2n-bit-length 31))"))
    (available-modules (prelude field algebra/polynomial primality))
    (hints ("Base case: (= x 0) -> 0"
            "Recursive: shift right by 1 and add 1"
            "(+ 1 (gf2n-bit-length (bitwise-arithmetic-shift-right x 1)))"
            "This counts how many shifts until we reach 0"))
    (reference-solution "(define (gf2n-bit-length x)
  (if (= x 0)
      0
      (+ 1 (gf2n-bit-length (bitwise-arithmetic-shift-right x 1)))))")
    (alternatives ())
    (source-commit "6051e47c")
    (difficulty 1))

) ;; end tasks
