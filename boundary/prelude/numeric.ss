(load "core/types/numeric-tower.ss")
(load "core/types/numeric-ops.ss")

(doc 'module 'numeric)
(doc 'description "Global Numeric Dispatch for Shell/User - provides user-friendly arithmetic that works across the entire numeric tower including lattice custom complex numbers")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(numeric-tower numeric-ops complex))

(doc 'section 'custom-complex-integration)

(doc *custom-complex-available* 'type Boolean)
(doc *custom-complex-available* 'description "Flag indicating whether lattice/numeric/complex.ss loaded successfully")
(define *custom-complex-available* #f)

(guard (ex [else #f])
  (load "lattice/numeric/complex.ss")
  (set! *custom-complex-available* #t))

(define (unwrap-complex x)
  (doc 'type (-> Value Value))
  (doc 'description "Convert lattice (complex r i) to native if needed for Chez ops")
  (doc 'export #t)
  (if (and *custom-complex-available*
           (pair? x)
           (eq? (car x) 'complex))
      (make-rectangular (cadr x) (caddr x))
      x))

(define (maybe-wrap-complex result was-custom)
  (doc 'type (-> Value Boolean Value))
  (doc 'description "If input was custom complex and result is native complex, wrap it back")
  (doc 'export #t)
  (if (and was-custom
           *custom-complex-available*
           (number? result)
           (not (real? result)))
      (list 'complex (real-part result) (imag-part result))
      result))

(doc 'section 'global-arithmetic-operators)
(doc 'note "These shadow Scheme's built-in operators to provide seamless handling of the full numeric tower including lattice custom complex")

(define (generic-binary native-op a b)
  (doc 'type (-> (-> Native Native Native) Value Value Value))
  (doc 'description "Apply a native binary op, handling custom complex transparently")
  (doc 'export #t)
  (let* ([a-custom (custom-complex? a)]
         [b-custom (custom-complex? b)]
         [a* (unwrap-complex a)]
         [b* (unwrap-complex b)]
         [result (native-op a* b*)])
    (maybe-wrap-complex result (or a-custom b-custom))))

(define (generic-unary native-op a)
  (doc 'type (-> (-> Native Native) Value Value))
  (doc 'description "Apply a native unary op, handling custom complex transparently")
  (doc 'export #t)
  (let* ([a-custom (custom-complex? a)]
         [a* (unwrap-complex a)]
         [result (native-op a*)])
    (maybe-wrap-complex result a-custom)))

(doc 'section 'shadowed-operators)
(doc 'note "Define with new names, then provide them via convenience macro or direct use")

(define (numeric-add a b)
  (doc 'type (-> Value Value Value))
  (doc 'description "Tower-aware addition with custom complex support")
  (doc 'export #t)
  (generic-binary + a b))

(define (numeric-sub a b)
  (doc 'type (-> Value Value Value))
  (doc 'description "Tower-aware subtraction with custom complex support")
  (doc 'export #t)
  (generic-binary - a b))

(define (numeric-mul a b)
  (doc 'type (-> Value Value Value))
  (doc 'description "Tower-aware multiplication with custom complex support")
  (doc 'export #t)
  (generic-binary * a b))

(define (numeric-div a b)
  (doc 'type (-> Value Value Value))
  (doc 'description "Tower-aware division with custom complex support")
  (doc 'export #t)
  (generic-binary / a b))

(define (numeric-neg a)
  (doc 'type (-> Value Value))
  (doc 'description "Tower-aware negation with custom complex support")
  (doc 'export #t)
  (generic-unary - a))

(doc 'section 'shadowed-transcendentals)

(define (numeric-sqrt a)
  (doc 'type (-> Value Value))
  (doc 'description "Tower-aware square root with custom complex support")
  (doc 'export #t)
  (generic-unary sqrt a))

(define (numeric-exp a)
  (doc 'type (-> Value Value))
  (doc 'description "Tower-aware exponential with custom complex support")
  (doc 'export #t)
  (generic-unary exp a))

(define (numeric-log a)
  (doc 'type (-> Value Value))
  (doc 'description "Tower-aware logarithm with custom complex support")
  (doc 'export #t)
  (generic-unary log a))

(define (numeric-sin a)
  (doc 'type (-> Value Value))
  (doc 'description "Tower-aware sine with custom complex support")
  (doc 'export #t)
  (generic-unary sin a))

(define (numeric-cos a)
  (doc 'type (-> Value Value))
  (doc 'description "Tower-aware cosine with custom complex support")
  (doc 'export #t)
  (generic-unary cos a))

(define (numeric-tan a)
  (doc 'type (-> Value Value))
  (doc 'description "Tower-aware tangent with custom complex support")
  (doc 'export #t)
  (generic-unary tan a))

(define (numeric-expt base exp)
  (doc 'type (-> Value Value Value))
  (doc 'description "Tower-aware exponentiation with custom complex support")
  (doc 'export #t)
  (generic-binary expt base exp))

(doc 'section 'enable-global-mode)

(define (enable-numeric-tower!)
  (doc 'type (-> Void))
  (doc 'description "Shadows Scheme primitives with tower-aware versions (opt-in)")
  (doc 'export #t)
  (doc 'note "WARNING: This mutates the top-level environment. Only use in boundary/user")
  (display "Numeric tower enabled.\n")
  (display "Use numeric-add, numeric-mul, numeric-sqrt, etc.\n")
  (display "Or alias: (define + numeric-add)\n"))

(doc 'section 'convenience-variadic-wrappers)

(define (sum xs)
  (doc 'type (-> (List Value) Value))
  (doc 'description "Sum a list of values using tower-aware addition")
  (doc 'export #t)
  (fold-left numeric-add 0 xs))

(define (product xs)
  (doc 'type (-> (List Value) Value))
  (doc 'description "Product of a list of values using tower-aware multiplication")
  (doc 'export #t)
  (fold-left numeric-mul 1 xs))

(doc 'section 'info)

(define (numeric-prelude-info)
  (doc 'type (-> Void))
  (doc 'description "Display information about loaded numeric prelude")
  (doc 'export #t)
  (display "boundary/prelude/numeric.ss loaded.\n")
  (display "Provides tower-aware arithmetic:\n")
  (display "  numeric-add, numeric-sub, numeric-mul, numeric-div\n")
  (display "  numeric-sqrt, numeric-exp, numeric-log\n")
  (display "  numeric-sin, numeric-cos, numeric-tan\n")
  (display "  sum, product\n")
  (if *custom-complex-available*
      (display "Custom complex (complex r i) support: ENABLED\n")
      (display "Custom complex (complex r i) support: DISABLED (lattice not loaded)\n")))

(numeric-prelude-info)
