;;; units.sexp — Development tasks for lattice/fp/measure/units.ss
;;;
;;; Module: units (tier 0, depends on prelude)
;;; 679 lines, ~40 exported functions
;;; Type-safe physical units using dimension types. SI system: 7 base
;;; dimensions as integer exponent tuples. Quantities are (value . dimension)
;;; pairs with dimension-checked arithmetic.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/fp/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement dim*
  ;; ──────────────────────────────────────────────
  ;; Multiply dimensions (add exponents).

  (task
    (id "fp/units/dim-multiply-impl")
    (module units)
    (tier 0)
    (type implement)
    (description "Implement dim*: multiply two dimensions by adding their exponent tuples element-wise. Dimensions are 7-element lists of integer exponents (L T M I Θ N J). When you multiply physical quantities, their dimensions multiply — and dimension multiplication means adding exponents: m¹ × m² = m³, or L¹·T⁻¹ × T¹ = L¹ (velocity × time = length). Use map with + to add corresponding exponents.")
    (context "Available: map, +. Dimensions are 7-element lists of integers. Single expression using map.")
    (before "(define (dim* d1 d2)
  ;; TODO: add exponents element-wise
  d1)")
    (test-file "lattice/fp/measure/test-units.ss")
    (test-forms (";; Length × Acceleration = Force dimension
(assert-equal '(1 -2 1 0 0 0 0) (dim* '(1 0 0 0 0 0 0) '(0 -2 1 0 0 0 0)))"
                 ";; Velocity × Time = Length
(assert-equal '(1 0 0 0 0 0 0) (dim* '(1 -1 0 0 0 0 0) '(0 1 0 0 0 0 0)))"
                 ";; Dimensionless × anything = identity
(assert-equal '(2 -2 1 0 0 0 0) (dim* '(0 0 0 0 0 0 0) '(2 -2 1 0 0 0 0)))"))
    (available-modules (prelude))
    (hints ("Dimension multiplication = exponent addition"
            "Use map to add element-wise: (map + d1 d2)"))
    (reference-solution "(define (dim* d1 d2)
  (map + d1 d2))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement dim-pow
  ;; ──────────────────────────────────────────────
  ;; Raise dimension to integer power.

  (task
    (id "fp/units/dim-pow-impl")
    (module units)
    (tier 0)
    (type implement)
    (description "Implement dim-pow: raise a dimension to an integer power n by multiplying each exponent by n. For example, Length² means the length exponent goes from 1 to 2. Volume is Length³ = (3 0 0 0 0 0 0). This is used for operations like squaring a quantity or computing area/volume dimensions. Use map with a lambda that multiplies each exponent by n.")
    (context "Available: map, *, lambda. Single expression using map.")
    (before "(define (dim-pow d n)
  ;; TODO: multiply each exponent by n
  d)")
    (test-file "lattice/fp/measure/test-units.ss")
    (test-forms (";; Length^3 = Volume
(assert-equal '(3 0 0 0 0 0 0) (dim-pow '(1 0 0 0 0 0 0) 3))"
                 ";; Velocity^2 = L²/T²
(assert-equal '(2 -2 0 0 0 0 0) (dim-pow '(1 -1 0 0 0 0 0) 2))"
                 ";; Any^0 = dimensionless
(assert-equal '(0 0 0 0 0 0 0) (dim-pow '(1 -2 1 0 0 0 0) 0))"
                 ";; Negative power: inverse
(assert-equal '(-1 1 0 0 0 0 0) (dim-pow '(1 -1 0 0 0 0 0) -1))"))
    (available-modules (prelude))
    (hints ("Each exponent multiplied by n"
            "Use map: (map (lambda (e) (* e n)) d)"))
    (reference-solution "(define (dim-pow d n)
  (map (lambda (e) (* e n)) d))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement dim-sqrt
  ;; ──────────────────────────────────────────────
  ;; Square root with even-exponent check.

  (task
    (id "fp/units/dim-sqrt-impl")
    (module units)
    (tier 0)
    (type implement)
    (description "Implement dim-sqrt: compute the square root of a dimension if all exponents are even. Square root means halving each exponent: sqrt(L²·T⁻²) = L¹·T⁻¹ (sqrt of velocity²). But sqrt(L¹) is undefined for integer dimensions — you can't have half-integer exponents. First check that all exponents are even using andmap and even?. If so, halve each with map and quotient. If any exponent is odd, return #f.")
    (context "Available: andmap, even?, map, quotient, if. Guard + map pattern.")
    (before "(define (dim-sqrt d)
  ;; TODO: halve exponents if all even, else #f
  #f)")
    (test-file "lattice/fp/measure/test-units.ss")
    (test-forms (";; sqrt(L²/T²) = L/T (velocity)
(assert-equal '(1 -1 0 0 0 0 0) (dim-sqrt '(2 -2 0 0 0 0 0)))"
                 ";; sqrt(dimensionless) = dimensionless
(assert-equal '(0 0 0 0 0 0 0) (dim-sqrt '(0 0 0 0 0 0 0)))"
                 ";; sqrt(L¹) = #f (odd exponent)
(assert-false (dim-sqrt '(1 0 0 0 0 0 0)))"
                 ";; sqrt(L²·T⁻²·M²) = L·T⁻¹·M (all even)
(assert-equal '(1 -1 1 0 0 0 0) (dim-sqrt '(2 -2 2 0 0 0 0)))"))
    (available-modules (prelude))
    (hints ("Guard: (andmap even? d) — all exponents must be even"
            "Halve: (map (lambda (e) (quotient e 2)) d)"
            "Return #f if any exponent is odd"))
    (reference-solution "(define (dim-sqrt d)
  (if (andmap even? d)
      (map (lambda (e) (quotient e 2)) d)
      #f))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement qty*
  ;; ──────────────────────────────────────────────
  ;; Multiply quantities (always valid).

  (task
    (id "fp/units/qty-multiply-impl")
    (module units)
    (tier 0)
    (type implement)
    (description "Implement qty*: multiply two quantities. Unlike addition (which requires matching dimensions), multiplication always works — you can multiply meters by seconds to get meter-seconds. Multiply the numeric values and multiply the dimensions (using dim*). A quantity is a pair (value . dimension) constructed via make-qty, accessed via qty-value and qty-dim.")
    (context "Available: make-qty, qty-value, qty-dim, dim*, *. Compose value multiplication with dimension multiplication.")
    (before "(define (qty* q1 q2)
  ;; TODO: multiply values, multiply dimensions
  (make-qty 0 '(0 0 0 0 0 0 0)))")
    (test-file "lattice/fp/measure/test-units.ss")
    (test-forms (";; 5 meters × 3 seconds = 15 meter-seconds
(let ([q (qty* (meter 5) (second 3))])
  (assert-equal 15 (qty-value q))
  (assert-equal '(1 1 0 0 0 0 0) (qty-dim q)))"
                 ";; Force = mass × acceleration: 10 kg × 2 m/s² = 20 N
(let ([mass (kilogram 10)]
      [accel (make-qty 2 '(1 -2 0 0 0 0 0))])
  (let ([force (qty* mass accel)])
    (assert-equal 20 (qty-value force))
    (assert-equal '(1 -2 1 0 0 0 0) (qty-dim force))))"
                 ";; Scalar × quantity
(let ([q (qty* (scalar 3) (meter 7))])
  (assert-equal 21 (qty-value q))
  (assert-equal '(1 0 0 0 0 0 0) (qty-dim q)))"))
    (available-modules (prelude))
    (hints ("New value: (* (qty-value q1) (qty-value q2))"
            "New dim: (dim* (qty-dim q1) (qty-dim q2))"
            "Combine: (make-qty new-value new-dim)"))
    (reference-solution "(define (qty* q1 q2)
  (make-qty (* (qty-value q1) (qty-value q2))
            (dim* (qty-dim q1) (qty-dim q2))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement dim->string
  ;; ──────────────────────────────────────────────
  ;; Display dimension in standard notation.

  (task
    (id "fp/units/dim-to-string-impl")
    (module units)
    (tier 0)
    (type implement)
    (description "Implement dim->string: convert a dimension to human-readable notation. The 7 SI base unit symbols are (\"m\" \"s\" \"kg\" \"A\" \"K\" \"mol\" \"cd\"). For each dimension: if exponent is 0, skip it. If exponent is 1, emit just the name (e.g. \"m\"). If exponent is anything else, emit name^exp (e.g. \"s^-2\"). Join non-zero parts with \".\". If all exponents are 0, return \"1\" (dimensionless). Use a named let loop over indices 0-6, accumulating parts in reverse, then reverse and join.")
    (context "Available: list-ref, =, number->string, string-append, string-join, reverse, cons, let, cond. Named let loop with index and accumulator.")
    (before "(define (dim->string d)
  ;; TODO: format as \"m.s^-2.kg\" etc.
  \"?\")")
    (test-file "lattice/fp/measure/test-units.ss")
    (test-forms (";; Dimensionless
(assert-equal \"1\" (dim->string '(0 0 0 0 0 0 0)))"
                 ";; Simple: meters
(assert-equal \"m\" (dim->string '(1 0 0 0 0 0 0)))"
                 ";; Volume: m^3
(assert-equal \"m^3\" (dim->string '(3 0 0 0 0 0 0)))"
                 ";; Force: m.s^-2.kg
(assert-equal \"m.s^-2.kg\" (dim->string '(1 -2 1 0 0 0 0)))"))
    (available-modules (prelude))
    (hints ("Unit names: '(\"m\" \"s\" \"kg\" \"A\" \"K\" \"mol\" \"cd\")"
            "Loop: (let loop ([i 0] [parts '()]) ...)"
            "Cases: exp=0 → skip, exp=1 → name, else → name^exp"
            "Base: i=7 → (if (null? parts) \"1\" (string-join (reverse parts) \".\"))"))
    (reference-solution "(define (dim->string d)
  (let ([names '(\"m\" \"s\" \"kg\" \"A\" \"K\" \"mol\" \"cd\")]
        [exps d])
       (let loop ([i 0] [parts '()])
            (if (= i 7)
                (if (null? parts)
                    \"1\"
                    (string-join (reverse parts) \".\"))
                (let ([exp (list-ref exps i)]
                      [name (list-ref names i)])
                     (loop (+ i 1)
                           (cond
                            [(= exp 0) parts]
                            [(= exp 1) (cons name parts)]
                            [else (cons (string-append name \"^\" (number->string exp))
                                        parts)])))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

) ;; end tasks
