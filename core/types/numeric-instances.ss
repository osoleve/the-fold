;;; core/types/numeric-instances.ss — Type Class Instance Dictionaries
;;;
;;; Instance dictionaries connecting Chez Scheme's native operations
;;; to The Fold's type class system (defined in kinds.ss).
;;;
;;; Type Class Hierarchy:
;;;   Num       : +, -, *, negate, abs, signum, fromInteger
;;;   Integral  : quot, rem, div, mod, toInteger  (requires Num)
;;;   Fractional: /, recip, fromRational           (requires Num)
;;;   Floating  : pi, exp, log, sqrt, **, trig...  (requires Fractional)
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - numeric-tower.ss

(load "core/base/prelude.ss")
(load "core/types/numeric-tower.ss")

;;; ============================================================================
;;; Dictionary Representation
;;; ============================================================================

;;; Dictionaries are association lists: ((method-name . implementation) ...)
;;; This matches the method lists in kinds.ss type class definitions.
;;;
;;; To invoke a method:
;;;   (dict-invoke dict 'method-name args...)
;;;
;;; Dictionaries can be composed for superclass constraints.

;;; dict-invoke : Dict × Symbol × Args... → Result
;;; Look up and invoke a method from a dictionary.
(define (dict-invoke dict method . args)
  (let ([entry (assq method dict)])
    (if entry
        (apply (cdr entry) args)
        (error 'dict-invoke "method not found" method))))

;;; dict-lookup : Dict × Symbol → Procedure | #f
;;; Look up a method without invoking.
(define (dict-lookup dict method)
  (let ([entry (assq method dict)])
    (if entry (cdr entry) #f)))

;;; dict-merge : Dict × Dict → Dict
;;; Merge two dictionaries (second overrides first).
(define (dict-merge d1 d2)
  (append d2 d1))

;;; ============================================================================
;;; Num Instances
;;; ============================================================================

;;; Num provides: +, -, *, negate, abs, signum, fromInteger

;;; Helper: signum for any real
(define (signum-real x)
  (cond [(< x 0) -1]
        [(> x 0) 1]
        [else 0]))

;;; Num instance for exact integers (Int, Fixnum, Bignum)
(define Num-Integer
  `((+           . ,+)
    (-           . ,-)
    (*           . ,*)
    (negate      . ,(lambda (x) (- x)))
    (abs         . ,abs)
    (signum      . ,signum-real)
    (fromInteger . ,(lambda (x) x))))

;;; Num instance for exact rationals
(define Num-Rational
  `((+           . ,+)
    (-           . ,-)
    (*           . ,*)
    (negate      . ,(lambda (x) (- x)))
    (abs         . ,abs)
    (signum      . ,signum-real)
    (fromInteger . ,(lambda (x) (/ x 1)))))  ; Integer → Rational

;;; Num instance for inexact reals (Float/Double)
(define Num-Real
  `((+           . ,+)
    (-           . ,-)
    (*           . ,*)
    (negate      . ,(lambda (x) (- x)))
    (abs         . ,abs)
    (signum      . ,signum-real)
    (fromInteger . ,(lambda (x) (exact->inexact x)))))

;;; Num instance for native complex
(define Num-Complex
  `((+           . ,+)
    (-           . ,-)
    (*           . ,*)
    (negate      . ,(lambda (x) (- x)))
    (abs         . ,magnitude)
    (signum      . ,(lambda (z)
                      (if (= z 0)
                          0
                          (/ z (magnitude z)))))
    (fromInteger . ,(lambda (x) (make-rectangular (exact->inexact x) 0)))))

;;; ============================================================================
;;; Integral Instances
;;; ============================================================================

;;; Integral provides: quot, rem, div, mod, toInteger
;;; Integral implies Num.

;;; Integral instance for exact integers
(define Integral-Integer
  (dict-merge
   Num-Integer
   `((quot      . ,quotient)
     (rem       . ,remainder)
     (div       . ,(lambda (a b) (floor (/ a b))))
     (mod       . ,modulo)
     (toInteger . ,(lambda (x) x)))))

;;; ============================================================================
;;; Fractional Instances
;;; ============================================================================

;;; Fractional provides: /, recip, fromRational
;;; Fractional implies Num.

;;; Fractional instance for exact rationals
(define Fractional-Rational
  (dict-merge
   Num-Rational
   `((/            . ,/)
     (recip        . ,(lambda (x) (/ 1 x)))
     (fromRational . ,(lambda (x) x)))))

;;; Fractional instance for inexact reals
(define Fractional-Real
  (dict-merge
   Num-Real
   `((/            . ,/)
     (recip        . ,(lambda (x) (/ 1.0 x)))
     (fromRational . ,(lambda (x) (exact->inexact x))))))

;;; Fractional instance for native complex
(define Fractional-Complex
  (dict-merge
   Num-Complex
   `((/            . ,/)
     (recip        . ,(lambda (z) (/ 1 z)))
     (fromRational . ,(lambda (x) (make-rectangular (exact->inexact x) 0))))))

;;; ============================================================================
;;; Floating Instances
;;; ============================================================================

;;; Floating provides: pi, exp, log, sqrt, **, sin, cos, tan, etc.
;;; Floating implies Fractional.

;;; Floating instance for inexact reals
(define Floating-Real
  (dict-merge
   Fractional-Real
   `((pi    . 3.141592653589793)
     (exp   . ,exp)
     (log   . ,log)
     (sqrt  . ,sqrt)
     (**    . ,expt)
     (sin   . ,sin)
     (cos   . ,cos)
     (tan   . ,tan)
     (asin  . ,asin)
     (acos  . ,acos)
     (atan  . ,atan)
     (sinh  . ,(lambda (x) (/ (- (exp x) (exp (- x))) 2)))
     (cosh  . ,(lambda (x) (/ (+ (exp x) (exp (- x))) 2)))
     (tanh  . ,(lambda (x)
                 (let ([ex (exp x)] [e-x (exp (- x))])
                   (/ (- ex e-x) (+ ex e-x)))))
     (asinh . ,(lambda (x) (log (+ x (sqrt (+ (* x x) 1))))))
     (acosh . ,(lambda (x) (log (+ x (sqrt (- (* x x) 1))))))
     (atanh . ,(lambda (x) (/ (log (/ (+ 1 x) (- 1 x))) 2))))))

;;; Floating instance for native complex
(define Floating-Complex
  (dict-merge
   Fractional-Complex
   `((pi    . ,(make-rectangular 3.141592653589793 0))
     (exp   . ,exp)
     (log   . ,log)
     (sqrt  . ,sqrt)
     (**    . ,expt)
     (sin   . ,sin)
     (cos   . ,cos)
     (tan   . ,tan)
     (asin  . ,asin)
     (acos  . ,acos)
     (atan  . ,atan)
     (sinh  . ,(lambda (z) (/ (- (exp z) (exp (- z))) 2)))
     (cosh  . ,(lambda (z) (/ (+ (exp z) (exp (- z))) 2)))
     (tanh  . ,(lambda (z)
                 (let ([ez (exp z)] [e-z (exp (- z))])
                   (/ (- ez e-z) (+ ez e-z)))))
     (asinh . ,(lambda (z) (log (+ z (sqrt (+ (* z z) 1))))))
     (acosh . ,(lambda (z) (log (+ z (sqrt (- (* z z) 1))))))
     (atanh . ,(lambda (z) (/ (log (/ (+ 1 z) (- 1 z))) 2))))))

;;; ============================================================================
;;; Instance Selection
;;; ============================================================================

;;; select-num-dict : Value → Dict
;;; Select appropriate Num dictionary based on value type.
(define (select-num-dict x)
  (let ([t (numeric-type x)])
    (case t
      [(Fixnum Bignum Integer) Num-Integer]
      [(Rational) Num-Rational]
      [(Real) Num-Real]
      [(Complex) Num-Complex]
      [else (error 'select-num-dict "not a number" x)])))

;;; select-integral-dict : Value → Dict
;;; Select Integral dictionary (only for integral types).
(define (select-integral-dict x)
  (let ([t (numeric-type x)])
    (if (integral-type? t)
        Integral-Integer
        (error 'select-integral-dict "not an integral type" x t))))

;;; select-fractional-dict : Value → Dict
;;; Select Fractional dictionary.
(define (select-fractional-dict x)
  (let ([t (numeric-type x)])
    (case t
      [(Rational) Fractional-Rational]
      [(Real) Fractional-Real]
      [(Complex) Fractional-Complex]
      ;; Integers can use Rational fractional
      [(Fixnum Bignum Integer) Fractional-Rational]
      [else (error 'select-fractional-dict "not a fractional type" x)])))

;;; select-floating-dict : Value → Dict
;;; Select Floating dictionary.
(define (select-floating-dict x)
  (let ([t (numeric-type x)])
    (case t
      [(Real) Floating-Real]
      [(Complex) Floating-Complex]
      ;; Exact types need conversion to float first
      [(Fixnum Bignum Integer Rational)
       Floating-Real]  ; Will need to convert value
      [else (error 'select-floating-dict "not a floating type" x)])))

;;; ============================================================================
;;; Convenience: Type-Dispatched Operations
;;; ============================================================================

;;; These use dictionary dispatch internally but present a simple interface.

;;; num-add : Num a => a × a → a
(define (num-add a b)
  (let-values ([(a* b*) (promote-pair a b)])
    (dict-invoke (select-num-dict a*) '+ a* b*)))

;;; num-sub : Num a => a × a → a
(define (num-sub a b)
  (let-values ([(a* b*) (promote-pair a b)])
    (dict-invoke (select-num-dict a*) '- a* b*)))

;;; num-mul : Num a => a × a → a
(define (num-mul a b)
  (let-values ([(a* b*) (promote-pair a b)])
    (dict-invoke (select-num-dict a*) '* a* b*)))

;;; num-negate : Num a => a → a
(define (num-negate a)
  (dict-invoke (select-num-dict a) 'negate a))

;;; num-abs : Num a => a → a
(define (num-abs a)
  (dict-invoke (select-num-dict a) 'abs a))

;;; frac-div : Fractional a => a × a → a
(define (frac-div a b)
  (let-values ([(a* b*) (promote-pair a b)])
    (dict-invoke (select-fractional-dict a*) '/ a* b*)))

;;; frac-recip : Fractional a => a → a
(define (frac-recip a)
  (dict-invoke (select-fractional-dict a) 'recip a))

;;; float-sqrt : Floating a => a → a
(define (float-sqrt a)
  (let ([a* (if (exact? a) (exact->inexact a) a)])
    (dict-invoke (select-floating-dict a*) 'sqrt a*)))

;;; float-sin : Floating a => a → a
(define (float-sin a)
  (let ([a* (if (exact? a) (exact->inexact a) a)])
    (dict-invoke (select-floating-dict a*) 'sin a*)))

;;; float-cos : Floating a => a → a
(define (float-cos a)
  (let ([a* (if (exact? a) (exact->inexact a) a)])
    (dict-invoke (select-floating-dict a*) 'cos a*)))

;;; float-exp : Floating a => a → a
(define (float-exp a)
  (let ([a* (if (exact? a) (exact->inexact a) a)])
    (dict-invoke (select-floating-dict a*) 'exp a*)))

;;; float-log : Floating a => a → a
(define (float-log a)
  (let ([a* (if (exact? a) (exact->inexact a) a)])
    (dict-invoke (select-floating-dict a*) 'log a*)))

;;; ============================================================================
;;; Eq and Ord Instances
;;; ============================================================================

;;; These are simpler - Scheme's = and < work across the numeric tower.

;;; Eq instance for all numbers
(define Eq-Number
  `((== . ,=)
    (/= . ,(lambda (a b) (not (= a b))))))

;;; Ord instance for real numbers (complex is not orderable)
(define Ord-Real
  (dict-merge
   Eq-Number
   `((compare . ,(lambda (a b)
                   (cond [(< a b) 'LT]
                         [(> a b) 'GT]
                         [else 'EQ])))
     (<  . ,<)
     (<= . ,<=)
     (>  . ,>)
     (>= . ,>=))))

;;; ============================================================================
;;; Summary
;;; ============================================================================

;;; Exports:
;;;
;;; Dictionary operations:
;;;   dict-invoke, dict-lookup, dict-merge
;;;
;;; Instance dictionaries:
;;;   Num-Integer, Num-Rational, Num-Real, Num-Complex
;;;   Integral-Integer
;;;   Fractional-Rational, Fractional-Real, Fractional-Complex
;;;   Floating-Real, Floating-Complex
;;;   Eq-Number, Ord-Real
;;;
;;; Instance selection:
;;;   select-num-dict, select-integral-dict
;;;   select-fractional-dict, select-floating-dict
;;;
;;; Type-dispatched operations:
;;;   num-add, num-sub, num-mul, num-negate, num-abs
;;;   frac-div, frac-recip
;;;   float-sqrt, float-sin, float-cos, float-exp, float-log
