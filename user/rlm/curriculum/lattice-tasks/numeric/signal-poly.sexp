;;; signal-poly.sexp — Development tasks for lattice/numeric/signal-poly.ss
;;;
;;; Module: signal-poly (tier 0, depends on prelude, field, algebra/polynomial, digital-filters)
;;; 477 lines, ~25 exported functions
;;; Polynomial algebra for signal processing: exact filter analysis via polynomial
;;; arithmetic over Q-field, stability analysis via Jury criterion, deconvolution
;;; via polynomial division, cascade/parallel filter combination.
;;;
;;; Git history:
;;;   0042852d feat(lattice): Integrate polynomial algebra with signal processing
;;;   710fc341 feat(module): migrate lattice/numeric/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement sig-poly-degree
  ;; ──────────────────────────────────────────────
  ;; Degree of a signal polynomial.

  (task
    (id "numeric/signal-poly/sig-poly-degree-impl")
    (module signal-poly)
    (tier 0)
    (type implement)
    (description "Implement sig-poly-degree: return the degree of a signal polynomial. A signal polynomial stores coefficients in ascending order as a list, so the degree is the length of the coefficient list minus 1. Use max with 0 to handle the zero polynomial (which has a single coefficient of 0 but degree 0). Extract coefficients with sig-poly-coeffs.")
    (context "Available: sig-poly-coeffs, length, -, max. One extraction + one arithmetic operation.")
    (before "(define (sig-poly-degree p)
  ;; TODO: degree of polynomial
  0)")
    (test-file "lattice/numeric/test-signal-poly.ss")
    (test-forms (";; Quadratic: 1 + 2x + 3x^2 has degree 2
(let ([p (sig-make-polynomial Q-field-sig '(1 2 3))])
  (assert-equal 2 (sig-poly-degree p)))"
                 ";; Constant polynomial has degree 0
(let ([p (sig-make-polynomial Q-field-sig '(5))])
  (assert-equal 0 (sig-poly-degree p)))"
                 ";; Zero polynomial has degree 0
(let ([p (sig-make-polynomial Q-field-sig '(0))])
  (assert-equal 0 (sig-poly-degree p)))"))
    (available-modules (prelude field algebra/polynomial digital-filters))
    (hints ("Get coefficients: (let ([coeffs (sig-poly-coeffs p)]) ...)"
            "Degree = length - 1, but at least 0"
            "(max 0 (- (length coeffs) 1))"))
    (reference-solution "(define (sig-poly-degree p)
  (let ([coeffs (sig-poly-coeffs p)])
    (max 0 (- (length coeffs) 1))))")
    (alternatives ())
    (source-commit "0042852d")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement sig-poly-zero?
  ;; ──────────────────────────────────────────────
  ;; Check if a polynomial is the zero polynomial.

  (task
    (id "numeric/signal-poly/sig-poly-zero-impl")
    (module signal-poly)
    (tier 0)
    (type implement)
    (description "Implement sig-poly-zero?: check if a signal polynomial is the zero polynomial. After normalization, the zero polynomial is represented as the single-element list (0). So check two conditions with and: the coefficient list has exactly one element (length = 1), and that element equals 0. Extract coefficients with sig-poly-coeffs.")
    (context "Available: sig-poly-coeffs, length, =, car, and. Two comparisons combined with and.")
    (before "(define (sig-poly-zero? p)
  ;; TODO: is this the zero polynomial?
  #f)")
    (test-file "lattice/numeric/test-signal-poly.ss")
    (test-forms (";; Zero polynomial
(assert-true (sig-poly-zero? (sig-make-polynomial Q-field-sig '(0))))"
                 ";; Non-zero constant
(assert-false (sig-poly-zero? (sig-make-polynomial Q-field-sig '(5))))"
                 ";; Non-zero polynomial
(assert-false (sig-poly-zero? (sig-make-polynomial Q-field-sig '(1 2 3))))"
                 ";; Trailing zeros are normalized away: '(1 0 0) has degree > 0
(assert-false (sig-poly-zero? (sig-make-polynomial Q-field-sig '(1 0 0))))"))
    (available-modules (prelude field algebra/polynomial digital-filters))
    (hints ("Get coefficients: (let ([coeffs (sig-poly-coeffs p)]) ...)"
            "Check length: (= (length coeffs) 1)"
            "Check value: (= (car coeffs) 0)"
            "Both must hold: (and length-check value-check)"))
    (reference-solution "(define (sig-poly-zero? p)
  (let ([coeffs (sig-poly-coeffs p)])
    (and (= (length coeffs) 1)
         (= (car coeffs) 0))))")
    (alternatives ())
    (source-commit "0042852d")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement sig-poly-leading
  ;; ──────────────────────────────────────────────
  ;; Get the leading coefficient.

  (task
    (id "numeric/signal-poly/sig-poly-leading-impl")
    (module signal-poly)
    (tier 0)
    (type implement)
    (description "Implement sig-poly-leading: return the leading coefficient (highest-degree term) of a signal polynomial. Coefficients are stored in ascending order, so the leading coefficient is the last element of the list. Use list-ref with index (length - 1) to access it.")
    (context "Available: sig-poly-coeffs, list-ref, length, -, let. One extraction + one indexed access.")
    (before "(define (sig-poly-leading p)
  ;; TODO: leading coefficient
  0)")
    (test-file "lattice/numeric/test-signal-poly.ss")
    (test-forms (";; 1 + 2x + 3x^2: leading is 3
(let ([p (sig-make-polynomial Q-field-sig '(1 2 3))])
  (assert-equal 3 (sig-poly-leading p)))"
                 ";; Constant 5: leading is 5
(let ([p (sig-make-polynomial Q-field-sig '(5))])
  (assert-equal 5 (sig-poly-leading p)))"
                 ";; Zero polynomial: leading is 0
(let ([p (sig-make-polynomial Q-field-sig '(0))])
  (assert-equal 0 (sig-poly-leading p)))"))
    (available-modules (prelude field algebra/polynomial digital-filters))
    (hints ("Get coefficients: (let ([coeffs (sig-poly-coeffs p)]) ...)"
            "Last element index: (- (length coeffs) 1)"
            "(list-ref coeffs (- (length coeffs) 1))"))
    (reference-solution "(define (sig-poly-leading p)
  (let ([coeffs (sig-poly-coeffs p)])
    (list-ref coeffs (- (length coeffs) 1))))")
    (alternatives ())
    (source-commit "0042852d")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement poly-eval-at-1
  ;; ──────────────────────────────────────────────
  ;; Evaluate polynomial at z = 1.

  (task
    (id "numeric/signal-poly/poly-eval-at-1-impl")
    (module signal-poly)
    (tier 0)
    (type implement)
    (description "Implement poly-eval-at-1: evaluate a polynomial at z = 1. Since any power of 1 is 1, evaluating at z=1 simply sums all coefficients. This is used in the Jury stability test to check P(1) > 0, a necessary condition for all poles inside the unit circle. Takes a list of coefficients (not a polynomial struct) and returns their sum using apply +.")
    (context "Available: apply, +. Single expression.")
    (before "(define (poly-eval-at-1 coeffs)
  ;; TODO: evaluate polynomial at z = 1
  0)")
    (test-file "lattice/numeric/test-signal-poly.ss")
    (test-forms (";; 1 + 2 + 3 = 6
(assert-equal 6 (poly-eval-at-1 '(1 2 3)))"
                 ";; Alternating signs: 1 - 1 + 1 - 1 = 0
(assert-equal 0 (poly-eval-at-1 '(1 -1 1 -1)))"
                 ";; Single coefficient
(assert-equal 7 (poly-eval-at-1 '(7)))"
                 ";; All zeros
(assert-equal 0 (poly-eval-at-1 '(0 0 0)))"))
    (available-modules (prelude field algebra/polynomial digital-filters))
    (hints ("At z=1, every z^k = 1"
            "So P(1) = a0 + a1 + a2 + ..."
            "Just sum: (apply + coeffs)"))
    (reference-solution "(define (poly-eval-at-1 coeffs)
  (apply + coeffs))")
    (alternatives ())
    (source-commit "0042852d")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement vector-all-zero?
  ;; ──────────────────────────────────────────────
  ;; Check if all vector elements are approximately zero.

  (task
    (id "numeric/signal-poly/vector-all-zero-impl")
    (module signal-poly)
    (tier 0)
    (type implement)
    (description "Implement vector-all-zero?: check if all elements of a vector are approximately zero (absolute value < 1e-10). Used to verify exact deconvolution (zero remainder). Use a named let loop with index i from 0 to vector-length. At each step, check if we've reached the end (return #t) or if the current element's absolute value is too large (return #f via short-circuit). Combine the base case and element check with or and and.")
    (context "Available: vector-length, vector-ref, abs, <, >=, or, and, let. Named let with index.")
    (before "(define (vector-all-zero? v)
  ;; TODO: all elements approximately zero?
  #f)")
    (test-file "lattice/numeric/test-signal-poly.ss")
    (test-forms (";; All zeros
(assert-true (vector-all-zero? '#(0 0 0)))"
                 ";; Near-zero values
(assert-true (vector-all-zero? '#(1e-15 -1e-15 0)))"
                 ";; Non-zero element
(assert-false (vector-all-zero? '#(0 0.5 0)))"
                 ";; Empty vector
(assert-true (vector-all-zero? '#()))"))
    (available-modules (prelude field algebra/polynomial digital-filters))
    (hints ("Named let: (let loop ([i 0]) ...)"
            "Base case: (>= i n) means all checked -> #t"
            "Check element: (< (abs (vector-ref v i)) 1e-10)"
            "Combine with or/and: (or (>= i n) (and (< (abs ...) 1e-10) (loop (+ i 1))))"))
    (reference-solution "(define (vector-all-zero? v)
  (let ([n (vector-length v)])
    (let loop ([i 0])
      (or (>= i n)
          (and (< (abs (vector-ref v i)) 1e-10)
               (loop (+ i 1)))))))")
    (alternatives ())
    (source-commit "0042852d")
    (difficulty 1))

) ;; end tasks
