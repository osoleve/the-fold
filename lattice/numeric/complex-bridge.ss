;;; lattice/numeric/complex-bridge.ss — Complex Number Bridge
;;; @module complex-bridge
;;; @requires prelude complex

(require 'prelude)
(require 'complex)

(doc 'module 'complex-bridge)
(doc 'description "Bridge between native Chez complex and custom serializable complex numbers")
(doc 'note "The Fold uses two complex representations: Native (Chez 1+2i, efficient) and Custom ((complex 1 2), serializable)")
(doc 'note "Use native complex for CALCULATION (fast, full operator support). Use custom complex for STORAGE (CAS hashing, block serialization).")
(doc 'pattern "(complex-store (some-native-complex-calculation (complex-compute c1) (complex-compute c2)))")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'custom-to-native)

(define (complex->native c)
  (doc 'type '(-> CustomComplex NativeComplex))
  (doc 'description "Convert lattice (complex r i) to Chez native complex")
  (if (custom-complex? c)
      (make-rectangular (complex-real c) (complex-imag c))
      (error 'complex->native "not a custom complex" c)))

;;; complex-compute : CustomComplex | NativeComplex | Real → NativeComplex
;;; Prepare any numeric value for native complex arithmetic.
;;; This is the "unwrap for calculation" function.
(define (complex-compute x)
  (cond
    ;; Already native complex
    [(and (number? x) (not (real? x))) x]
    ;; Custom complex → native
    [(custom-complex? x)
     (make-rectangular (complex-real x) (complex-imag x))]
    ;; Real → complex with zero imaginary
    [(real? x)
     (make-rectangular (if (exact? x) (exact->inexact x) x) 0.0)]
    ;; Error
    [else (error 'complex-compute "not a number" x)]))

;;; ============================================================================
;;; Conversion: Native → Custom
;;; ============================================================================

;;; native->complex : NativeComplex → CustomComplex
;;; Convert Chez native complex to lattice (complex r i).
(define (native->complex z)
  (if (and (number? z) (not (real? z)))
      (make-complex (real-part z) (imag-part z))
      (error 'native->complex "not a native complex" z)))

;;; complex-store : NativeComplex | Real → CustomComplex
;;; Prepare a calculation result for storage/serialization.
;;; This is the "wrap for storage" function.
(define (complex-store z)
  (cond
    ;; Native complex → custom
    [(and (number? z) (not (real? z)))
     (make-complex (real-part z) (imag-part z))]
    ;; Real → custom with zero imaginary
    [(real? z)
     (make-complex z 0)]
    ;; Already custom complex
    [(custom-complex? z) z]
    ;; Error
    [else (error 'complex-store "not a number" z)]))

;;; ============================================================================
;;; Bridged Operations
;;; ============================================================================

;;; These operations take custom complex, compute with native, return custom.
;;; Use these for lattice code that needs CAS-serializable results.

;;; complex-bridge-unary : (NativeComplex → NativeComplex) × CustomComplex → CustomComplex
;;; Apply a native unary operation, bridging through native complex.
(define (complex-bridge-unary op c)
  (complex-store (op (complex-compute c))))

;;; complex-bridge-binary : (NC × NC → NC) × CC × CC → CC
;;; Apply a native binary operation, bridging through native complex.
(define (complex-bridge-binary op c1 c2)
  (complex-store (op (complex-compute c1) (complex-compute c2))))

;;; ============================================================================
;;; Bridged Arithmetic
;;; ============================================================================

;;; c+ : CustomComplex × CustomComplex → CustomComplex
;;; Addition via native bridge.
(define (c+ c1 c2)
  (complex-bridge-binary + c1 c2))

;;; c- : CustomComplex × CustomComplex → CustomComplex
;;; Subtraction via native bridge.
(define (c- c1 c2)
  (complex-bridge-binary - c1 c2))

;;; c* : CustomComplex × CustomComplex → CustomComplex
;;; Multiplication via native bridge.
(define (c* c1 c2)
  (complex-bridge-binary * c1 c2))

;;; c/ : CustomComplex × CustomComplex → CustomComplex
;;; Division via native bridge.
(define (c/ c1 c2)
  (complex-bridge-binary / c1 c2))

;;; ============================================================================
;;; Bridged Transcendentals
;;; ============================================================================

;;; c-exp : CustomComplex → CustomComplex
(define (c-exp c)
  (complex-bridge-unary exp c))

;;; c-log : CustomComplex → CustomComplex
(define (c-log c)
  (complex-bridge-unary log c))

;;; c-sqrt : CustomComplex → CustomComplex
(define (c-sqrt c)
  (complex-bridge-unary sqrt c))

;;; c-sin : CustomComplex → CustomComplex
(define (c-sin c)
  (complex-bridge-unary sin c))

;;; c-cos : CustomComplex → CustomComplex
(define (c-cos c)
  (complex-bridge-unary cos c))

;;; c-tan : CustomComplex → CustomComplex
(define (c-tan c)
  (complex-bridge-unary tan c))

;;; c-pow : CustomComplex × CustomComplex → CustomComplex
;;; Exponentiation via native bridge.
(define (c-pow base exponent)
  (complex-bridge-binary expt base exponent))

;;; ============================================================================
;;; Comparison (Magnitude-Based)
;;; ============================================================================

;;; c-magnitude : CustomComplex → Real
;;; Magnitude (absolute value) of custom complex.
(define (c-magnitude c)
  (magnitude (complex-compute c)))

;;; c-angle : CustomComplex → Real
;;; Phase angle of custom complex.
(define (c-angle c)
  (angle (complex-compute c)))

;;; c=? : CustomComplex × CustomComplex → Boolean
;;; Equality test for custom complex.
(define (c=? c1 c2)
  (and (= (complex-real c1) (complex-real c2))
       (= (complex-imag c1) (complex-imag c2))))

;;; ============================================================================
;;; Utilities
;;; ============================================================================

;;; custom-complex? : Value → Boolean
;;; Check if value is a lattice custom complex.
(define (custom-complex? x)
  (and (pair? x) (eq? (car x) 'complex)))

;;; ensure-complex : Value → CustomComplex
;;; Ensure value is a custom complex (convert if necessary).
(define (ensure-complex x)
  (cond
    [(custom-complex? x) x]
    [(and (number? x) (not (real? x)))
     (native->complex x)]
    [(real? x)
     (make-complex x 0)]
    [else (error 'ensure-complex "not a number" x)]))

;;; ============================================================================
;;; Batch Operations (for efficiency)
;;; ============================================================================

;;; complex-compute-all : (List CustomComplex) → (List NativeComplex)
;;; Convert a list of custom complex to native for batch calculation.
(define (complex-compute-all cs)
  (map complex-compute cs))

;;; complex-store-all : (List NativeComplex) → (List CustomComplex)
;;; Convert calculation results back to custom for storage.
(define (complex-store-all zs)
  (map complex-store zs))

;;; with-native-complex : (List CustomComplex) × ((List NC) → (List NC)) → (List CC)
;;; Apply a batch operation in native complex space, returning custom.
;;; Use for efficient bulk calculations.
(define (with-native-complex inputs computation)
  (complex-store-all
   (computation
    (complex-compute-all inputs))))

;;; ============================================================================
;;; Summary
;;; ============================================================================

;;; Conversions:
;;;   complex->native, complex-compute   ; Custom → Native
;;;   native->complex, complex-store     ; Native → Custom
;;;   ensure-complex                     ; Any → Custom
;;;
;;; Bridged Arithmetic:
;;;   c+, c-, c*, c/
;;;
;;; Bridged Transcendentals:
;;;   c-exp, c-log, c-sqrt, c-sin, c-cos, c-tan, c-pow
;;;
;;; Comparison:
;;;   c-magnitude, c-angle, c=?
;;;
;;; Batch:
;;;   complex-compute-all, complex-store-all, with-native-complex
;;;
;;; Predicates:
;;;   custom-complex?

(display "  Use complex-compute for calculation, complex-store for serialization.\n")
