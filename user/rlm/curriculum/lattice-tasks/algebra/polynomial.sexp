;;; polynomial.sexp — Development tasks for lattice/algebra/polynomial.ss
;;;
;;; Module: algebra/polynomial (tier 2, depends on prelude, field)
;;; 648 lines, ~25 exported functions
;;; Provides: polynomial ring F[x] over any field, arithmetic, division with
;;;           remainder, GCD, extended GCD, Horner evaluation, derivative,
;;;           square-free factorization, Lagrange/Newton interpolation
;;;
;;; Test file: lattice/algebra/test-polynomial.ss (213 lines for univariate)
;;;
;;; Git history:
;;;   728445ef feat(algebra): Implement polynomial algebra
;;;   758457e0 fix(algebra): Fix Newton interpolation and add documentation
;;;   6e98e779 refactor(algebra): Introduce Field abstraction for polynomial division

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement poly-eval (Horner's method)
  ;; ──────────────────────────────────────────────
  ;; Horner's method evaluates a_0 + a_1*x + ... + a_n*x^n as
  ;; a_0 + x*(a_1 + x*(a_2 + ... x*a_n)). This is both more efficient
  ;; (O(n) multiplications vs O(n²)) and more numerically stable.

  (task
    (id "algebra/polynomial/eval-impl")
    (module algebra/polynomial)
    (tier 2)
    (type implement)
    (description "Implement poly-eval using Horner's method: evaluate polynomial p at point x. The polynomial's coefficients are stored in ascending order [a_0, a_1, ..., a_n]. Horner's method processes from highest degree down: start with acc = a_n, then acc = acc*x + a_{n-1}, etc. To process from highest degree, reverse the coefficient list first. Use the field's add and mul operations (from poly-field).")
    (context "Available: poly-field (get the coefficient field F), poly-coeffs (coefficient list in ascending order), field-add-op, field-mul-op, field-zero (from the field). The polynomial and x are over the same field.")
    (before "(define (poly-eval p x)
  ;; TODO: implement using Horner's method
  ;; Reverse coeffs to get descending, fold with acc = acc*x + c
  (field-zero (poly-field p)))")
    (test-file "lattice/algebra/test-polynomial.ss")
    (test-forms ("(assert-equal 14 (poly-eval (make-polynomial Q-field '(2 3)) 4))"
                 "(assert-equal 7 (poly-eval (make-polynomial Q-field '(1 1 1)) 2))"
                 "(assert-equal 0 (poly-eval (make-polynomial Q-field '(0)) 5))"
                 "(assert-equal 5 (poly-eval (make-polynomial Q-field '(5)) 99))"
                 "(assert-equal 27 (poly-eval (make-polynomial Q-field '(0 0 0 1)) 3))"))
    (available-modules (prelude ring field algebra/polynomial))
    (hints ("Reverse the coefficient list to get descending order"
            "Start with acc = first coefficient (highest degree)"
            "Each step: acc = add(mul(acc, x), next-coefficient)"
            "Empty polynomial evaluates to zero"))
    (reference-solution "(define (poly-eval p x)
  (let* ([F (poly-field p)]
         [coeffs (reverse (poly-coeffs p))]
         [add (field-add-op F)]
         [mul (field-mul-op F)])
    (if (null? coeffs)
        (field-zero F)
        (let loop ([cs (cdr coeffs)] [acc (car coeffs)])
          (if (null? cs)
              acc
              (loop (cdr cs) (add (mul acc x) (car cs))))))))")
    (alternatives ())
    (source-commit "728445ef")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement poly-derivative
  ;; ──────────────────────────────────────────────
  ;; Formal derivative: d/dx (sum a_k * x^k) = sum k * a_k * x^{k-1}.
  ;; The "multiply coefficient by integer k" must use field addition
  ;; (not Scheme's *) for generality over finite fields.

  (task
    (id "algebra/polynomial/derivative-impl")
    (module algebra/polynomial)
    (tier 2)
    (type implement)
    (description "Implement poly-derivative: compute the formal derivative of polynomial p. For p = a_0 + a_1*x + ... + a_n*x^n, the derivative is a_1 + 2*a_2*x + ... + n*a_n*x^{n-1}. Drop the constant term (a_0), then multiply each remaining coefficient a_k by k. To multiply a field element by integer k, use repeated addition (add c to itself k times). Return the result as a new polynomial via make-polynomial.")
    (context "Available: poly-field, poly-coeffs (ascending order), make-polynomial, poly-zero-over, field-add-op, field-zero. Coefficient list for result has indices shifted: the k-th coefficient of derivative is (k+1)*a_{k+1}.")
    (before "(define (poly-derivative p)
  ;; TODO: implement formal derivative
  (poly-zero-over (poly-field p)))")
    (test-file "lattice/algebra/test-polynomial.ss")
    (test-forms ("(let* ([p (make-polynomial Q-field '(0 1 2 1))] [dp (poly-derivative p)]) (assert-equal 2 (poly-degree dp)))"
                 "(let* ([p (make-polynomial Q-field '(0 1 2 1))] [dp (poly-derivative p)]) (assert-equal 1 (poly-coeff-at dp 0)))"
                 "(let* ([p (make-polynomial Q-field '(0 1 2 1))] [dp (poly-derivative p)]) (assert-equal 4 (poly-coeff-at dp 1)))"
                 "(let* ([p (make-polynomial Q-field '(0 1 2 1))] [dp (poly-derivative p)]) (assert-equal 3 (poly-coeff-at dp 2)))"
                 "(assert-true (poly-zero? (poly-derivative (poly-constant Q-field 5))))"))
    (available-modules (prelude ring field algebra/polynomial))
    (hints ("Skip the first coefficient (constant term drops out)"
            "For remaining coefficients at position k (1-indexed), multiply by k"
            "To multiply field element c by integer k: add c to itself k times"
            "If polynomial has degree 0 or less, derivative is zero"))
    (reference-solution "(define (poly-derivative p)
  (let* ([F (poly-field p)]
         [coeffs (poly-coeffs p)]
         [add (field-add-op F)])
    (if (<= (length coeffs) 1)
        (poly-zero-over F)
        (make-polynomial F
          (let loop ([cs (cdr coeffs)] [k 1] [result '()])
            (if (null? cs)
                (reverse result)
                (loop (cdr cs) (+ k 1)
                      (cons (let mul-loop ([i 1] [acc (car cs)])
                              (if (= i k) acc
                                  (mul-loop (+ i 1) (add acc (car cs)))))
                            result))))))))")
    (alternatives ())
    (source-commit "728445ef")
    (difficulty 4))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement poly-gcd
  ;; ──────────────────────────────────────────────
  ;; The Euclidean algorithm for polynomial GCD is the same structure
  ;; as for integers: gcd(a,b) = gcd(b, a mod b). The result is
  ;; made monic (leading coefficient = 1) for canonical form.

  (task
    (id "algebra/polynomial/gcd-impl")
    (module algebra/polynomial)
    (tier 2)
    (type implement)
    (description "Implement poly-gcd: compute the GCD of two polynomials using the Euclidean algorithm. gcd(p1, p2) = gcd(p2, p1 mod p2). Base case: gcd(p, 0) = monic(p). Use poly-mod for the remainder and poly-make-monic to normalize the result so the leading coefficient is 1. The poly-mod function is already available.")
    (context "Available: poly-mod (remainder of polynomial division), poly-zero? (check if polynomial is zero), poly-make-monic (scale so leading coeff = 1), poly-field. This is exactly the integer GCD algorithm adapted for polynomials.")
    (before "(define (poly-gcd p1 p2)
  ;; TODO: Euclidean algorithm for polynomial GCD
  ;; Base: gcd(p, 0) = monic(p)
  ;; Step: gcd(p1, p2) = gcd(p2, p1 mod p2)
  p1)")
    (test-file "lattice/algebra/test-polynomial.ss")
    (test-forms ("(let* ([p1 (make-polynomial Q-field '(1 1))] [p2 (make-polynomial Q-field '(-1 1))] [g (poly-gcd p1 p2)]) (assert-equal 0 (poly-degree g)))"
                 "(let* ([xp1 (make-polynomial Q-field '(1 1))] [xm1 (make-polynomial Q-field '(-1 1))] [p1 (poly-mul xp1 xp1)] [p2 (poly-mul xp1 xm1)] [g (poly-gcd p1 p2)]) (assert-equal 1 (poly-degree g)))"
                 "(let* ([xp1 (make-polynomial Q-field '(1 1))] [xm1 (make-polynomial Q-field '(-1 1))] [p1 (poly-mul xp1 xp1)] [p2 (poly-mul xp1 xm1)] [g (poly-gcd p1 p2)]) (assert-equal 1 (poly-leading-coeff g)))"))
    (available-modules (prelude ring field algebra/polynomial))
    (hints ("Base case: if p2 is zero, return poly-make-monic of p1"
            "Recursive case: call poly-gcd with p2 and (poly-mod p1 p2)"
            "This is exactly the integer Euclidean algorithm"))
    (reference-solution "(define (poly-gcd p1 p2)
  (let ([F (poly-field p1)])
    (if (poly-zero? p2)
        (poly-make-monic p1)
        (poly-gcd p2 (poly-mod p1 p2)))))")
    (alternatives ())
    (source-commit "728445ef")
    (difficulty 4))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Fix poly-mul (convolution)
  ;; ──────────────────────────────────────────────
  ;; Polynomial multiplication via convolution: the k-th coefficient
  ;; of the product is sum_{i+j=k} a_i * b_j. The bug is an off-by-one
  ;; in the output length.

  (task
    (id "algebra/polynomial/fix-poly-mul")
    (module algebra/polynomial)
    (tier 2)
    (type fix)
    (description "The poly-mul function has an off-by-one bug in the result length. The product of a degree-m polynomial and a degree-n polynomial has degree m+n, so the coefficient list has length m+n+1 = len(c1) + len(c2) - 1 terms. The buggy version uses len(c1) + len(c2) (one too many), which leaves a trailing zero coefficient. Fix the length calculation. The convolution itself is correct.")
    (context "Available: poly-field, poly-coeffs, poly-zero?, poly-zero-over, make-polynomial, field-add-op, field-mul-op, field-zero, length. The convolution formula: coeff_k = sum_{i=0}^{k} c1[i]*c2[k-i] (when both indices are in range).")
    (before "(define (poly-mul p1 p2)
  (let* ([F (poly-field p1)]
         [c1 (poly-coeffs p1)]
         [c2 (poly-coeffs p2)]
         [zero (field-zero F)]
         [add (field-add-op F)]
         [mul (field-mul-op F)]
         [n1 (length c1)]
         [n2 (length c2)]
         [n (+ n1 n2)])  ;; BUG: should be (+ n1 n2 -1)
    (if (or (poly-zero? p1) (poly-zero? p2))
        (poly-zero-over F)
        (make-polynomial F
          (let loop ([k 0] [result '()])
            (if (= k n)
                (reverse result)
                (loop (+ k 1)
                      (cons (let inner ([i 0] [sum zero])
                              (if (> i k)
                                  sum
                                  (let ([j (- k i)])
                                    (if (and (< i n1) (< j n2))
                                        (inner (+ i 1) (add sum (mul (list-ref c1 i) (list-ref c2 j))))
                                        (inner (+ i 1) sum)))))
                            result))))))))")
    (test-file "lattice/algebra/test-polynomial.ss")
    (test-forms ("(let* ([p1 (make-polynomial Q-field '(1 1))] [p2 (make-polynomial Q-field '(1 -1))] [prod (poly-mul p1 p2)]) (assert-equal 2 (poly-degree prod)))"
                 "(let* ([p1 (make-polynomial Q-field '(1 1))] [p2 (make-polynomial Q-field '(1 -1))] [prod (poly-mul p1 p2)]) (assert-equal 1 (poly-coeff-at prod 0)))"
                 "(let* ([p1 (make-polynomial Q-field '(1 1))] [p2 (make-polynomial Q-field '(1 -1))] [prod (poly-mul p1 p2)]) (assert-equal -1 (poly-coeff-at prod 2)))"
                 "(let* ([p (make-polynomial Q-field '(1 1))] [sq (poly-mul p p)]) (assert-equal '(1 2 1) (poly-coeffs sq)))"))
    (available-modules (prelude ring field algebra/polynomial))
    (hints ("The product of polynomials with lengths n1 and n2 has length n1+n2-1"
            "The bug is in the line [n (+ n1 n2)] — it should be (+ n1 n2 -1)"
            "This is because deg(p*q) = deg(p) + deg(q), and length = degree + 1"))
    (reference-solution "(define (poly-mul p1 p2)
  (let* ([F (poly-field p1)]
         [c1 (poly-coeffs p1)]
         [c2 (poly-coeffs p2)]
         [zero (field-zero F)]
         [add (field-add-op F)]
         [mul (field-mul-op F)]
         [n1 (length c1)]
         [n2 (length c2)]
         [n (+ n1 n2 -1)])
    (if (or (poly-zero? p1) (poly-zero? p2))
        (poly-zero-over F)
        (make-polynomial F
          (let loop ([k 0] [result '()])
            (if (= k n)
                (reverse result)
                (loop (+ k 1)
                      (cons (let inner ([i 0] [sum zero])
                              (if (> i k)
                                  sum
                                  (let ([j (- k i)])
                                    (if (and (< i n1) (< j n2))
                                        (inner (+ i 1) (add sum (mul (list-ref c1 i) (list-ref c2 j))))
                                        (inner (+ i 1) sum)))))
                            result))))))))")
    (alternatives ())
    (source-commit "728445ef")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Extend with poly-compose
  ;; ──────────────────────────────────────────────
  ;; Polynomial composition p(q(x)): substitute q for x in p.
  ;; Natural extension not currently in the module.

  (task
    (id "algebra/polynomial/compose-extend")
    (module algebra/polynomial)
    (tier 2)
    (type extend)
    (description "Add a function poly-compose that computes the composition p(q(x)) — substitute polynomial q for x in polynomial p. If p = a_0 + a_1*x + ... + a_n*x^n, then p(q) = a_0 + a_1*q + a_2*q^2 + ... + a_n*q^n. Use Horner's method for efficiency: start with acc = a_n, then acc = acc*q + a_{n-1}, etc. This avoids computing individual powers of q. Use poly-mul, poly-add, and poly-constant for the arithmetic.")
    (context "Available: poly-coeffs (ascending order), poly-field, poly-mul, poly-add, poly-constant, make-polynomial. The input polynomials must be over the same field.")
    (before ";; poly-compose does not yet exist in the module")
    (test-file "lattice/algebra/test-polynomial.ss")
    (test-forms ("(let* ([p (make-polynomial Q-field '(1 0 1))] [q (make-polynomial Q-field '(1 1))] [r (poly-compose p q)]) (assert-equal 2 (poly-eval r 0)))"
                 "(let* ([p (make-polynomial Q-field '(1 0 1))] [q (make-polynomial Q-field '(1 1))] [r (poly-compose p q)]) (assert-equal 5 (poly-eval r 1)))"
                 "(let* ([p (make-polynomial Q-field '(0 1))] [q (make-polynomial Q-field '(3 2))] [r (poly-compose p q)]) (assert-equal '(3 2) (poly-coeffs r)))"
                 "(let* ([p (make-polynomial Q-field '(5))] [q (make-polynomial Q-field '(0 0 1))] [r (poly-compose p q)]) (assert-equal 0 (poly-degree r)))"))
    (available-modules (prelude ring field algebra/polynomial))
    (hints ("Process coefficients from highest to lowest (Horner)"
            "Reverse the coefficient list, start with acc = poly-constant of last coeff"
            "Each step: acc = poly-add(poly-mul(acc, q), poly-constant(next-coeff))"
            "This computes p(q) without ever explicitly computing q^k"))
    (reference-solution "(define (poly-compose p q)
  (let* ([F (poly-field p)]
         [coeffs (reverse (poly-coeffs p))])
    (if (null? coeffs)
        (poly-zero-over F)
        (let loop ([cs (cdr coeffs)]
                   [acc (poly-constant F (car coeffs))])
          (if (null? cs)
              acc
              (loop (cdr cs)
                    (poly-add (poly-mul acc q)
                              (poly-constant F (car cs)))))))))")
    (alternatives ())
    (source-commit "728445ef")
    (difficulty 4))

) ;; end tasks
