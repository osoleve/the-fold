;;; complex-bridge.sexp — Development tasks for lattice/numeric/complex-bridge.ss
;;;
;;; Module: complex-bridge (tier 0, depends on prelude, complex)
;;; 228 lines, ~20 exported functions
;;; Bridge between native Chez complex (fast computation) and custom
;;; serializable complex (CAS storage). Demonstrates the convert→compute→
;;; convert-back pattern essential for content-addressed computation.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/numeric/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement complex-compute
  ;; ──────────────────────────────────────────────
  ;; The polymorphic unwrapper — prepare any numeric value for calculation.

  (task
    (id "numeric/complex-bridge/complex-compute-impl")
    (module complex-bridge)
    (tier 0)
    (type implement)
    (description "Implement complex-compute: convert any numeric value to native Chez complex for efficient computation. Three cases via cond: (1) Already native complex (number? and not real?) → return as-is. (2) Custom complex (custom-complex?) → extract real and imaginary parts via complex-real and complex-imag, construct native via make-rectangular. (3) Real number → make-rectangular with 0.0 imaginary, converting exact to inexact if needed. (4) Otherwise → error. This is the 'unwrap for calculation' function in the bridge pattern.")
    (context "Available: number?, real?, exact?, custom-complex?, complex-real, complex-imag, make-rectangular, exact->inexact, error, cond, and, not, if. Four-case type dispatch.")
    (before "(define (complex-compute x)
  ;; TODO: convert any numeric to native complex
  x)")
    (test-file "lattice/numeric/test-complex-bridge.ss")
    (test-forms (";; Custom complex → native
(let ([result (complex-compute (make-complex 3 4))])
  (assert-true (number? result))
  (assert-equal 3 (real-part result))
  (assert-equal 4 (imag-part result)))"
                 ";; Real → native complex with 0 imaginary
(let ([result (complex-compute 5)])
  (assert-true (number? result)))"
                 ";; Native complex passes through
(let ([result (complex-compute (make-rectangular 1 2))])
  (assert-equal 1 (real-part result))
  (assert-equal 2 (imag-part result)))"))
    (available-modules (prelude complex))
    (hints ("Case 1: (and (number? x) (not (real? x))) → x"
            "Case 2: (custom-complex? x) → (make-rectangular (complex-real x) (complex-imag x))"
            "Case 3: (real? x) → (make-rectangular (if (exact? x) (exact->inexact x) x) 0.0)"
            "Case 4: else → error"))
    (reference-solution "(define (complex-compute x)
  (cond
    [(and (number? x) (not (real? x))) x]
    [(custom-complex? x)
     (make-rectangular (complex-real x) (complex-imag x))]
    [(real? x)
     (make-rectangular (if (exact? x) (exact->inexact x) x) 0.0)]
    [else (error 'complex-compute \"not a number\" x)]))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement complex-store
  ;; ──────────────────────────────────────────────
  ;; The polymorphic wrapper — prepare results for CAS storage.

  (task
    (id "numeric/complex-bridge/complex-store-impl")
    (module complex-bridge)
    (tier 0)
    (type implement)
    (description "Implement complex-store: convert a computation result to custom complex for CAS serialization. Three cases: (1) Native complex (number? and not real?) → extract parts via real-part and imag-part, construct custom via make-complex. (2) Real number → make-complex with 0 imaginary. (3) Already custom complex → return as-is. (4) Otherwise → error. This is the inverse of complex-compute: where compute unwraps for calculation, store wraps for persistence.")
    (context "Available: number?, real?, custom-complex?, real-part, imag-part, make-complex, error, cond, and, not. Four-case dispatch, inverse of complex-compute.")
    (before "(define (complex-store z)
  ;; TODO: convert any numeric to custom complex for storage
  z)")
    (test-file "lattice/numeric/test-complex-bridge.ss")
    (test-forms (";; Native complex → custom
(let ([result (complex-store (make-rectangular 3 4))])
  (assert-true (custom-complex? result))
  (assert-equal 3 (complex-real result))
  (assert-equal 4 (complex-imag result)))"
                 ";; Real → custom with 0 imaginary
(let ([result (complex-store 5)])
  (assert-true (custom-complex? result))
  (assert-equal 5 (complex-real result))
  (assert-equal 0 (complex-imag result)))"
                 ";; Round-trip: compute then store = identity
(let* ([c (make-complex 3 4)]
       [result (complex-store (complex-compute c))])
  (assert-equal 3 (complex-real result))
  (assert-equal 4 (complex-imag result)))"))
    (available-modules (prelude complex))
    (hints ("Case 1: (and (number? z) (not (real? z))) → (make-complex (real-part z) (imag-part z))"
            "Case 2: (real? z) → (make-complex z 0)"
            "Case 3: (custom-complex? z) → z"
            "Case 4: else → error"))
    (reference-solution "(define (complex-store z)
  (cond
    [(and (number? z) (not (real? z)))
     (make-complex (real-part z) (imag-part z))]
    [(real? z)
     (make-complex z 0)]
    [(custom-complex? z) z]
    [else (error 'complex-store \"not a number\" z)]))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement complex-bridge-binary
  ;; ──────────────────────────────────────────────
  ;; The higher-order bridge — lift any native binary op.

  (task
    (id "numeric/complex-bridge/bridge-binary-impl")
    (module complex-bridge)
    (tier 0)
    (type implement)
    (description "Implement complex-bridge-binary: apply a native binary operation to custom complex numbers, returning a custom complex result. This is the core bridge pattern as a higher-order function: (1) Convert both inputs to native via complex-compute. (2) Apply the native operation. (3) Convert the result back to custom via complex-store. All bridged arithmetic (c+, c-, c*, c/) are defined as partial applications of this function. The bridge separates concerns: native Chez handles computation efficiently, custom complex handles serialization.")
    (context "Available: complex-store, complex-compute. Three function calls composed.")
    (before "(define (complex-bridge-binary op c1 c2)
  ;; TODO: store(op(compute(c1), compute(c2)))
  c1)")
    (test-file "lattice/numeric/test-complex-bridge.ss")
    (test-forms (";; Addition via bridge
(let ([result (complex-bridge-binary + (make-complex 1 2) (make-complex 3 4))])
  (assert-equal 4 (complex-real result))
  (assert-equal 6 (complex-imag result)))"
                 ";; Multiplication via bridge
(let ([result (complex-bridge-binary * (make-complex 1 1) (make-complex 1 -1))])
  ;; (1+i)(1-i) = 1 - i^2 = 2
  (assert-equal 2 (complex-real result))
  (assert-equal 0 (complex-imag result)))"
                 ";; Works with real inputs too
(let ([result (complex-bridge-binary + (make-complex 1 0) (make-complex 0 1))])
  (assert-equal 1 (complex-real result))
  (assert-equal 1 (complex-imag result)))"))
    (available-modules (prelude complex))
    (hints ("Convert: (complex-compute c1) and (complex-compute c2)"
            "Apply: (op native1 native2)"
            "Wrap: (complex-store result)"))
    (reference-solution "(define (complex-bridge-binary op c1 c2)
  (complex-store (op (complex-compute c1) (complex-compute c2))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement c-magnitude
  ;; ──────────────────────────────────────────────
  ;; Complex magnitude through the bridge.

  (task
    (id "numeric/complex-bridge/c-magnitude-impl")
    (module complex-bridge)
    (tier 0)
    (type implement)
    (description "Implement c-magnitude: compute the magnitude (absolute value) of a custom complex number. |a+bi| = sqrt(a^2 + b^2). Convert to native via complex-compute, then use Chez's built-in magnitude function which handles the computation efficiently (with overflow protection). Returns a real number, not a complex — this is one of the few bridge functions that doesn't return a custom complex.")
    (context "Available: magnitude, complex-compute. Two function calls.")
    (before "(define (c-magnitude c)
  ;; TODO: |c| via native magnitude
  0)")
    (test-file "lattice/numeric/test-complex-bridge.ss")
    (test-forms (";; 3-4-5 triangle: |3+4i| = 5
(assert-equal 5 (c-magnitude (make-complex 3 4)))"
                 ";; Pure real: |5+0i| = 5
(assert-equal 5.0 (c-magnitude (make-complex 5 0)))"
                 ";; Pure imaginary: |0+3i| = 3
(assert-equal 3.0 (c-magnitude (make-complex 0 3)))"
                 ";; Zero: |0+0i| = 0
(assert-equal 0.0 (c-magnitude (make-complex 0 0)))"))
    (available-modules (prelude complex))
    (hints ("Convert to native: (complex-compute c)"
            "Return: (magnitude native)"))
    (reference-solution "(define (c-magnitude c)
  (magnitude (complex-compute c)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement with-native-complex
  ;; ──────────────────────────────────────────────
  ;; Batch bridge for efficient bulk operations.

  (task
    (id "numeric/complex-bridge/with-native-impl")
    (module complex-bridge)
    (tier 0)
    (type implement)
    (description "Implement with-native-complex: apply a batch computation in native complex space, returning custom complex results. Takes a list of custom complex inputs and a computation function that operates on a list of native complex numbers and returns a list of native complex results. Steps: (1) Convert all inputs to native via complex-compute-all (which maps complex-compute). (2) Apply the computation function to the native list. (3) Convert all results back to custom via complex-store-all (which maps complex-store). This is the batch version of the bridge pattern — it amortizes the conversion overhead for vector operations like DFT.")
    (context "Available: complex-compute-all, complex-store-all. Three function calls composed with a lambda.")
    (before "(define (with-native-complex inputs computation)
  ;; TODO: convert all → compute → convert all back
  inputs)")
    (test-file "lattice/numeric/test-complex-bridge.ss")
    (test-forms (";; Square each element
(let ([result (with-native-complex
                (list (make-complex 1 0) (make-complex 0 1))
                (lambda (ns) (map (lambda (z) (* z z)) ns)))])
  ;; 1^2 = 1, i^2 = -1
  (assert-equal 2 (length result))
  (assert-true (< (abs (- (complex-real (car result)) 1)) 0.001))
  (assert-true (< (abs (- (complex-real (cadr result)) -1)) 0.001)))"
                 ";; Identity computation
(let ([result (with-native-complex
                (list (make-complex 3 4))
                (lambda (ns) ns))])
  (assert-equal 3 (complex-real (car result)))
  (assert-equal 4 (complex-imag (car result))))"))
    (available-modules (prelude complex))
    (hints ("Convert all: (complex-compute-all inputs)"
            "Apply: (computation natives)"
            "Wrap all: (complex-store-all results)"))
    (reference-solution "(define (with-native-complex inputs computation)
  (complex-store-all
   (computation
    (complex-compute-all inputs))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

) ;; end tasks
