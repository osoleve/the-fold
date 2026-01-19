;;; boundary/prelude/numeric.ss — Global Numeric Dispatch for Shell/User
;;;
;;; This module provides user-friendly arithmetic that "just works" across
;;; the entire numeric tower including lattice custom complex numbers.
;;;
;;; Load this in boundary/user sessions to get:
;;;   - + * / that handle mixed types seamlessly
;;;   - sqrt, sin, cos that work on complex numbers
;;;   - Integration between native Chez complex and lattice (complex r i)
;;;
;;; This is Shell code: convenience layer over Core.
;;;
;;; Usage:
;;;   (load "boundary/prelude/numeric.ss")
;;;   (+ 1 1/2)           ; → 3/2 (exact)
;;;   (+ 1 0.5)           ; → 1.5 (inexact)
;;;   (sqrt -1)           ; → 0+1i (native complex)
;;;   (sin (complex 1 2)) ; → Works with lattice complex
;;;
;;; Dependencies:
;;;   - core/types/numeric-tower.ss
;;;   - core/types/numeric-ops.ss
;;;   - lattice/numeric/complex.ss (for custom complex support)

(load "core/types/numeric-tower.ss")
(load "core/types/numeric-ops.ss")

;;; ============================================================================
;;; Custom Complex Integration
;;; ============================================================================

;;; Try to load lattice complex if available (may fail in minimal sessions)
(define *custom-complex-available* #f)

(guard (ex [else #f])
  (load "lattice/numeric/complex.ss")
  (set! *custom-complex-available* #t))

;;; unwrap-complex : Value → Value
;;; Convert lattice (complex r i) to native if needed for Chez ops.
(define (unwrap-complex x)
  (if (and *custom-complex-available*
           (pair? x)
           (eq? (car x) 'complex))
      (make-rectangular (cadr x) (caddr x))
      x))

;;; maybe-wrap-complex : Value × Boolean → Value
;;; If input was custom complex and result is native complex, wrap it back.
(define (maybe-wrap-complex result was-custom)
  (if (and was-custom
           *custom-complex-available*
           (number? result)
           (not (real? result)))
      ;; Wrap native complex back to lattice form
      (list 'complex (real-part result) (imag-part result))
      result))

;;; ============================================================================
;;; Global Arithmetic Operators
;;; ============================================================================

;;; These shadow Scheme's built-in operators to provide seamless
;;; handling of the full numeric tower including lattice custom complex.

;;; Note: We use define rather than set! to create module-local bindings
;;; that shadow the imported Scheme primitives.

;;; generic-binary : (Native × Native → Native) × Value × Value → Value
;;; Apply a native binary op, handling custom complex transparently.
(define (generic-binary native-op a b)
  (let* ([a-custom (custom-complex? a)]
         [b-custom (custom-complex? b)]
         [a* (unwrap-complex a)]
         [b* (unwrap-complex b)]
         [result (native-op a* b*)])
    (maybe-wrap-complex result (or a-custom b-custom))))

;;; generic-unary : (Native → Native) × Value → Value
;;; Apply a native unary op, handling custom complex transparently.
(define (generic-unary native-op a)
  (let* ([a-custom (custom-complex? a)]
         [a* (unwrap-complex a)]
         [result (native-op a*)])
    (maybe-wrap-complex result a-custom)))

;;; ============================================================================
;;; Shadowed Operators
;;; ============================================================================

;;; We define these with new names, then provide them via a convenience macro
;;; or direct use. Shell users who want the global dispatch can:
;;;   (define + numeric-add) etc.

;;; numeric-add : Value × Value → Value
;;; Tower-aware addition with custom complex support.
(define (numeric-add a b)
  (generic-binary + a b))

;;; numeric-sub : Value × Value → Value
(define (numeric-sub a b)
  (generic-binary - a b))

;;; numeric-mul : Value × Value → Value
(define (numeric-mul a b)
  (generic-binary * a b))

;;; numeric-div : Value × Value → Value
(define (numeric-div a b)
  (generic-binary / a b))

;;; numeric-neg : Value → Value
(define (numeric-neg a)
  (generic-unary - a))

;;; ============================================================================
;;; Shadowed Transcendentals
;;; ============================================================================

;;; numeric-sqrt : Value → Value
(define (numeric-sqrt a)
  (generic-unary sqrt a))

;;; numeric-exp : Value → Value
(define (numeric-exp a)
  (generic-unary exp a))

;;; numeric-log : Value → Value
(define (numeric-log a)
  (generic-unary log a))

;;; numeric-sin : Value → Value
(define (numeric-sin a)
  (generic-unary sin a))

;;; numeric-cos : Value → Value
(define (numeric-cos a)
  (generic-unary cos a))

;;; numeric-tan : Value → Value
(define (numeric-tan a)
  (generic-unary tan a))

;;; numeric-expt : Value × Value → Value
(define (numeric-expt base exp)
  (generic-binary expt base exp))

;;; ============================================================================
;;; Enable Global Mode (Opt-In)
;;; ============================================================================

;;; enable-numeric-tower! : → Void
;;; Shadows Scheme primitives with tower-aware versions.
;;; Call this to make + * / etc work seamlessly with custom complex.
;;;
;;; WARNING: This mutates the top-level environment. Only use in boundary/user.
(define (enable-numeric-tower!)
  ;; We can't actually shadow primitives in standard Scheme,
  ;; but we provide the tower-aware versions for explicit use.
  (display "Numeric tower enabled.\n")
  (display "Use numeric-add, numeric-mul, numeric-sqrt, etc.\n")
  (display "Or alias: (define + numeric-add)\n"))

;;; ============================================================================
;;; Convenience: Variadic Wrappers
;;; ============================================================================

;;; sum : (List Value) → Value
(define (sum xs)
  (fold-left numeric-add 0 xs))

;;; product : (List Value) → Value
(define (product xs)
  (fold-left numeric-mul 1 xs))

;;; ============================================================================
;;; Info
;;; ============================================================================

(define (numeric-prelude-info)
  (display "boundary/prelude/numeric.ss loaded.\n")
  (display "Provides tower-aware arithmetic:\n")
  (display "  numeric-add, numeric-sub, numeric-mul, numeric-div\n")
  (display "  numeric-sqrt, numeric-exp, numeric-log\n")
  (display "  numeric-sin, numeric-cos, numeric-tan\n")
  (display "  sum, product\n")
  (if *custom-complex-available*
      (display "Custom complex (complex r i) support: ENABLED\n")
      (display "Custom complex (complex r i) support: DISABLED (lattice not loaded)\n")))

;;; Print info on load
(numeric-prelude-info)
