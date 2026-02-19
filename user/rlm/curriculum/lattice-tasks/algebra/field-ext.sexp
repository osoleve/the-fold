;;; field-ext.sexp — Development tasks for lattice/algebra/field-ext.ss
;;;
;;; Module: field-ext (tier 1, depends on prelude, field, algebra/polynomial)
;;; 474 lines, ~20 exported functions
;;; Algebraic field extensions over infinite fields (Q, R): simple algebraic
;;; extensions F(alpha), quadratic extensions Q(sqrt(d)), minimal polynomials,
;;; field automorphisms, Galois groups, norm and trace.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/algebra/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement trim-trailing-zeros
  ;; ──────────────────────────────────────────────
  ;; Remove trailing zero coefficients.

  (task
    (id "algebra/field-ext/trim-trailing-zeros-impl")
    (module field-ext)
    (tier 0)
    (type implement)
    (description "Implement trim-trailing-zeros: remove trailing zero coefficients from a list, preserving at least one element. The field F provides the equality test and zero element. Reverse the list, drop leading zeros from the reversed list using a named let loop, then reverse back. If all elements are zero, return a singleton list containing just the zero. This is used to normalize algebraic extension elements before padding to the correct degree.")
    (context "Available: field-equal-fn, field-zero, reverse, null?, car, cdr, list, let. Named let on reversed list.")
    (before "(define (trim-trailing-zeros coeffs F)
  ;; TODO: remove trailing zeros, keep at least one element
  coeffs)")
    (test-file "lattice/algebra/test-field-ext.ss")
    (test-forms (";; Trailing zeros removed
(assert-equal '(1 2 3) (trim-trailing-zeros '(1 2 3 0 0) Q-field))"
                 ";; All zeros becomes singleton zero
(assert-equal '(0) (trim-trailing-zeros '(0 0 0) Q-field))"
                 ";; No trailing zeros: unchanged
(assert-equal '(5) (trim-trailing-zeros '(5) Q-field))"
                 ";; Internal zeros preserved
(assert-equal '(1 0 3) (trim-trailing-zeros '(1 0 3 0) Q-field))"))
    (available-modules (prelude field algebra/polynomial))
    (hints ("Get zero and equality: (let ([eq-fn (field-equal-fn F)] [zero (field-zero F)]) ...)"
            "Reverse, then drop matching zeros: (let loop ([cs (reverse coeffs)]) ...)"
            "Base: (null? cs) -> (list zero)"
            "If (eq-fn (car cs) zero) -> (loop (cdr cs)), else (reverse cs)"))
    (reference-solution "(define (trim-trailing-zeros coeffs F)
  (let ([eq-fn (field-equal-fn F)]
        [zero (field-zero F)])
    (let loop ([cs (reverse coeffs)])
      (cond
        [(null? cs) (list zero)]
        [(eq-fn (car cs) zero) (loop (cdr cs))]
        [else (reverse cs)]))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement alg-ext-add
  ;; ──────────────────────────────────────────────
  ;; Addition in algebraic extension field.

  (task
    (id "algebra/field-ext/alg-ext-add-impl")
    (module field-ext)
    (tier 0)
    (type implement)
    (description "Implement alg-ext-add: add two elements of an algebraic extension F(alpha). Elements are represented by coefficient lists. Addition is coefficient-wise: (a0+b0, a1+b1, ..., an-1+bn-1) using the base field's addition. Extract the coefficient lists from both elements using alg-ext-coeffs, get the base field's addition via field-add-op, then map addition over the paired coefficient lists. Construct the result with make-alg-ext-element.")
    (context "Available: alg-ext-coeffs, field-add-op, map, make-alg-ext-element. Extract + map + construct.")
    (before "(define (alg-ext-add a b F n min-poly sym)
  ;; TODO: coefficient-wise addition in F(alpha)
  a)")
    (test-file "lattice/algebra/test-field-ext.ss")
    (test-forms (";; (1+2sqrt2) + (3+4sqrt2) = (4+6sqrt2)
(let* ([ext (make-Q-sqrt 2)]
       [a (Q-sqrt-element ext 1 2)]
       [b (Q-sqrt-element ext 3 4)]
       [sum ((field-add-op ext) a b)])
  (assert-equal '(4 6) (alg-ext-coeffs sum)))"
                 ";; Adding zero: identity
(let* ([ext (make-Q-sqrt 2)]
       [a (Q-sqrt-element ext 5 3)]
       [z (field-zero ext)]
       [sum ((field-add-op ext) a z)])
  (assert-equal '(5 3) (alg-ext-coeffs sum)))"))
    (available-modules (prelude field algebra/polynomial))
    (hints ("Extract coefficients: (let* ([ca (alg-ext-coeffs a)] [cb (alg-ext-coeffs b)]) ...)"
            "Get add op: (let ([add (field-add-op F)]) ...)"
            "Map over paired coefficients: (map add ca cb)"
            "Construct result: (make-alg-ext-element result min-poly F sym)"))
    (reference-solution "(define (alg-ext-add a b F n min-poly sym)
  (let* ([ca (alg-ext-coeffs a)]
         [cb (alg-ext-coeffs b)]
         [add (field-add-op F)]
         [result (map add ca cb)])
    (make-alg-ext-element result min-poly F sym)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement alg-ext-neg
  ;; ──────────────────────────────────────────────
  ;; Negation in algebraic extension field.

  (task
    (id "algebra/field-ext/alg-ext-neg-impl")
    (module field-ext)
    (tier 0)
    (type implement)
    (description "Implement alg-ext-neg: negate an element of an algebraic extension F(alpha). Negation is coefficient-wise: negate each coefficient using the base field's negation function. Extract the coefficient list with alg-ext-coeffs, get the negation function via field-neg-fn, map it over all coefficients, and construct the result with make-alg-ext-element.")
    (context "Available: alg-ext-coeffs, field-neg-fn, map, make-alg-ext-element. Extract + map + construct.")
    (before "(define (alg-ext-neg a F n min-poly sym)
  ;; TODO: negate all coefficients
  a)")
    (test-file "lattice/algebra/test-field-ext.ss")
    (test-forms (";; Negate (3+2sqrt2) = (-3-2sqrt2)
(let* ([ext (make-Q-sqrt 2)]
       [a (Q-sqrt-element ext 3 2)]
       [neg-a ((field-neg-fn ext) a)])
  (assert-equal '(-3 -2) (alg-ext-coeffs neg-a)))"
                 ";; Negate zero is zero
(let* ([ext (make-Q-sqrt 2)]
       [z (field-zero ext)]
       [neg-z ((field-neg-fn ext) z)])
  (assert-equal '(0 0) (alg-ext-coeffs neg-z)))"))
    (available-modules (prelude field algebra/polynomial))
    (hints ("Extract coefficients: (let* ([ca (alg-ext-coeffs a)]) ...)"
            "Get negation: (let ([neg (field-neg-fn F)]) ...)"
            "Map negation: (map neg ca)"
            "Construct: (make-alg-ext-element result min-poly F sym)"))
    (reference-solution "(define (alg-ext-neg a F n min-poly sym)
  (let* ([ca (alg-ext-coeffs a)]
         [neg (field-neg-fn F)]
         [result (map neg ca)])
    (make-alg-ext-element result min-poly F sym)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement alg-ext-equal?
  ;; ──────────────────────────────────────────────
  ;; Equality in algebraic extension field.

  (task
    (id "algebra/field-ext/alg-ext-equal-impl")
    (module field-ext)
    (tier 0)
    (type implement)
    (description "Implement alg-ext-equal?: check if two algebraic extension elements are equal. Two elements are equal iff their coefficient lists have the same length and all corresponding coefficients are equal under the base field's equality test. Extract both coefficient lists, check lengths match, then walk the lists in parallel comparing each pair using a named let loop. Use short-circuit evaluation with and/or.")
    (context "Available: alg-ext-coeffs, field-equal-fn, length, =, null?, car, cdr, and, or, let. Length check + parallel walk.")
    (before "(define (alg-ext-equal? a b F)
  ;; TODO: coefficient-wise equality
  #f)")
    (test-file "lattice/algebra/test-field-ext.ss")
    (test-forms (";; Equal elements
(let* ([ext (make-Q-sqrt 2)]
       [a (Q-sqrt-element ext 1 2)]
       [b (Q-sqrt-element ext 1 2)])
  (assert-true (alg-ext-equal? a b Q-field)))"
                 ";; Different elements
(let* ([ext (make-Q-sqrt 2)]
       [a (Q-sqrt-element ext 1 2)]
       [c (Q-sqrt-element ext 1 3)])
  (assert-false (alg-ext-equal? a c Q-field)))"
                 ";; Zero equals zero
(let* ([ext (make-Q-sqrt 2)]
       [z1 (field-zero ext)]
       [z2 (Q-sqrt-element ext 0 0)])
  (assert-true (alg-ext-equal? z1 z2 Q-field)))"))
    (available-modules (prelude field algebra/polynomial))
    (hints ("Extract coeffs: (let* ([ca (alg-ext-coeffs a)] [cb (alg-ext-coeffs b)] [eq-fn (field-equal-fn F)]) ...)"
            "Check lengths: (and (= (length ca) (length cb)) ...)"
            "Parallel walk: (let loop ([as ca] [bs cb]) (or (null? as) (and (eq-fn (car as) (car bs)) (loop (cdr as) (cdr bs)))))"
            "Short-circuit: or handles empty case, and handles mismatch"))
    (reference-solution "(define (alg-ext-equal? a b F)
  (let* ([ca (alg-ext-coeffs a)]
         [cb (alg-ext-coeffs b)]
         [eq-fn (field-equal-fn F)])
    (and (= (length ca) (length cb))
         (let loop ([as ca] [bs cb])
           (or (null? as)
               (and (eq-fn (car as) (car bs))
                    (loop (cdr as) (cdr bs))))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement Q-sqrt-norm
  ;; ──────────────────────────────────────────────
  ;; Field norm for quadratic extensions.

  (task
    (id "algebra/field-ext/Q-sqrt-norm-impl")
    (module field-ext)
    (tier 0)
    (type implement)
    (description "Implement Q-sqrt-norm: compute the field norm of an element in Q(sqrt(d)). For alpha = a + b*sqrt(d), the norm is N(alpha) = alpha * conjugate(alpha) = a^2 - d*b^2. The norm maps extension elements to the base field (rationals). It is multiplicative: N(alpha*beta) = N(alpha)*N(beta). Extract the rational part a with Q-sqrt-real-part and the sqrt coefficient b with Q-sqrt-sqrt-part, then compute a^2 - d*b^2.")
    (context "Available: Q-sqrt-real-part, Q-sqrt-sqrt-part, *, -, let. Simple arithmetic formula.")
    (before "(define (Q-sqrt-norm elem d)
  ;; TODO: a^2 - d*b^2
  0)")
    (test-file "lattice/algebra/test-field-ext.ss")
    (test-forms (";; N(3+2sqrt2) = 9-8 = 1
(let* ([ext (make-Q-sqrt 2)]
       [elem (Q-sqrt-element ext 3 2)])
  (assert-equal 1 (Q-sqrt-norm elem 2)))"
                 ";; N(1+sqrt2) = 1-2 = -1
(let* ([ext (make-Q-sqrt 2)]
       [elem (Q-sqrt-element ext 1 1)])
  (assert-equal -1 (Q-sqrt-norm elem 2)))"
                 ";; N(5+0*sqrt3) = 25-0 = 25
(let* ([ext (make-Q-sqrt 3)]
       [elem (Q-sqrt-element ext 5 0)])
  (assert-equal 25 (Q-sqrt-norm elem 3)))"
                 ";; N(0+0*sqrt(d)) = 0
(let* ([ext (make-Q-sqrt 2)]
       [elem (Q-sqrt-element ext 0 0)])
  (assert-equal 0 (Q-sqrt-norm elem 2)))"))
    (available-modules (prelude field algebra/polynomial))
    (hints ("Extract parts: (let ([a (Q-sqrt-real-part elem)] [b (Q-sqrt-sqrt-part elem)]) ...)"
            "Formula: (- (* a a) (* d b b))"))
    (reference-solution "(define (Q-sqrt-norm elem d)
  (let ([a (Q-sqrt-real-part elem)]
        [b (Q-sqrt-sqrt-part elem)])
    (- (* a a) (* d b b))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

) ;; end tasks
