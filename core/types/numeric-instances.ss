(load "core/base/prelude.ss")
(load "core/types/numeric-tower.ss")

(doc 'module 'numeric-instances)
(doc 'description "Type class instance dictionaries connecting Chez Scheme's native operations to The Fold's type class system. Type Class Hierarchy: Num (+, -, *, negate, abs, signum, fromInteger), Integral (quot, rem, div, mod, toInteger, requires Num), Fractional (/, recip, fromRational, requires Num), Floating (pi, exp, log, sqrt, **, trig..., requires Fractional).")
(doc 'layer 'core)

(doc 'section 'dictionary-representation)
(doc 'note "Dictionaries are association lists: ((method-name . implementation) ...). This matches the method lists in kinds.ss type class definitions. To invoke a method: (dict-invoke dict 'method-name args...). Dictionaries can be composed for superclass constraints.")

(define (dict-invoke dict method . args)
  (doc 'type '(-> Dict Symbol Any))
  (doc 'description "Look up and invoke a method from a dictionary.")
  (doc 'export #t)
  (let ([entry (assq method dict)])
    (if entry
        (apply (cdr entry) args)
        (error 'dict-invoke "method not found" method))))

(define (dict-lookup dict method)
  (doc 'type '(-> Dict Symbol (Maybe Procedure)))
  (doc 'description "Look up a method without invoking.")
  (doc 'export #t)
  (let ([entry (assq method dict)])
    (if entry (cdr entry) #f)))

(define (dict-merge d1 d2)
  (doc 'type '(-> Dict Dict Dict))
  (doc 'description "Merge two dictionaries (second overrides first).")
  (doc 'export #t)
  (append d2 d1))

(doc 'section 'num-instances)
(doc 'note "Num provides: +, -, *, negate, abs, signum, fromInteger")

(doc signum-real 'type '(-> Real Integer))
(doc signum-real 'description "Helper: signum for any real")
(define (signum-real x)
  (cond [(< x 0) -1]
        [(> x 0) 1]
        [else 0]))

(doc Num-Integer 'type 'Dict)
(doc Num-Integer 'description "Num instance for exact integers (Int, Fixnum, Bignum)")
(doc Num-Integer 'export #t)
(define Num-Integer
  `((+           . ,+)
    (-           . ,-)
    (*           . ,*)
    (negate      . ,(lambda (x) (- x)))
    (abs         . ,abs)
    (signum      . ,signum-real)
    (fromInteger . ,(lambda (x) x))))

(doc Num-Rational 'type 'Dict)
(doc Num-Rational 'description "Num instance for exact rationals")
(doc Num-Rational 'export #t)
(define Num-Rational
  `((+           . ,+)
    (-           . ,-)
    (*           . ,*)
    (negate      . ,(lambda (x) (- x)))
    (abs         . ,abs)
    (signum      . ,signum-real)
    (fromInteger . ,(lambda (x) (/ x 1)))))

(doc Num-Real 'type 'Dict)
(doc Num-Real 'description "Num instance for inexact reals (Float/Double)")
(doc Num-Real 'export #t)
(define Num-Real
  `((+           . ,+)
    (-           . ,-)
    (*           . ,*)
    (negate      . ,(lambda (x) (- x)))
    (abs         . ,abs)
    (signum      . ,signum-real)
    (fromInteger . ,(lambda (x) (exact->inexact x)))))

(doc Num-Complex 'type 'Dict)
(doc Num-Complex 'description "Num instance for native complex")
(doc Num-Complex 'export #t)
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

(doc 'section 'integral-instances)
(doc 'note "Integral provides: quot, rem, div, mod, toInteger. Integral implies Num.")

(doc Integral-Integer 'type 'Dict)
(doc Integral-Integer 'description "Integral instance for exact integers")
(doc Integral-Integer 'export #t)
(define Integral-Integer
  (dict-merge
   Num-Integer
   `((quot      . ,quotient)
     (rem       . ,remainder)
     (div       . ,(lambda (a b) (floor (/ a b))))
     (mod       . ,modulo)
     (toInteger . ,(lambda (x) x)))))

(doc 'section 'fractional-instances)
(doc 'note "Fractional provides: /, recip, fromRational. Fractional implies Num.")

(doc Fractional-Rational 'type 'Dict)
(doc Fractional-Rational 'description "Fractional instance for exact rationals")
(doc Fractional-Rational 'export #t)
(define Fractional-Rational
  (dict-merge
   Num-Rational
   `((/            . ,/)
     (recip        . ,(lambda (x) (/ 1 x)))
     (fromRational . ,(lambda (x) x)))))

(doc Fractional-Real 'type 'Dict)
(doc Fractional-Real 'description "Fractional instance for inexact reals")
(doc Fractional-Real 'export #t)
(define Fractional-Real
  (dict-merge
   Num-Real
   `((/            . ,/)
     (recip        . ,(lambda (x) (/ 1.0 x)))
     (fromRational . ,(lambda (x) (exact->inexact x))))))

(doc Fractional-Complex 'type 'Dict)
(doc Fractional-Complex 'description "Fractional instance for native complex")
(doc Fractional-Complex 'export #t)
(define Fractional-Complex
  (dict-merge
   Num-Complex
   `((/            . ,/)
     (recip        . ,(lambda (z) (/ 1 z)))
     (fromRational . ,(lambda (x) (make-rectangular (exact->inexact x) 0))))))

(doc 'section 'floating-instances)
(doc 'note "Floating provides: pi, exp, log, sqrt, **, sin, cos, tan, etc. Floating implies Fractional.")

(doc Floating-Real 'type 'Dict)
(doc Floating-Real 'description "Floating instance for inexact reals")
(doc Floating-Real 'export #t)
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

(doc Floating-Complex 'type 'Dict)
(doc Floating-Complex 'description "Floating instance for native complex")
(doc Floating-Complex 'export #t)
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

(doc 'section 'instance-selection)

(define (select-num-dict x)
  (doc 'type '(-> Value Dict))
  (doc 'description "Select appropriate Num dictionary based on value type.")
  (doc 'export #t)
  (let ([t (numeric-type x)])
    (case t
      [(Fixnum Bignum Integer) Num-Integer]
      [(Rational) Num-Rational]
      [(Real) Num-Real]
      [(Complex) Num-Complex]
      [else (error 'select-num-dict "not a number" x)])))

(define (select-integral-dict x)
  (doc 'type '(-> Value Dict))
  (doc 'description "Select Integral dictionary (only for integral types).")
  (doc 'export #t)
  (let ([t (numeric-type x)])
    (if (integral-type? t)
        Integral-Integer
        (error 'select-integral-dict "not an integral type" x t))))

(define (select-fractional-dict x)
  (doc 'type '(-> Value Dict))
  (doc 'description "Select Fractional dictionary.")
  (doc 'export #t)
  (let ([t (numeric-type x)])
    (case t
      [(Rational) Fractional-Rational]
      [(Real) Fractional-Real]
      [(Complex) Fractional-Complex]
      [(Fixnum Bignum Integer) Fractional-Rational]
      [else (error 'select-fractional-dict "not a fractional type" x)])))

(define (select-floating-dict x)
  (doc 'type '(-> Value Dict))
  (doc 'description "Select Floating dictionary.")
  (doc 'export #t)
  (let ([t (numeric-type x)])
    (case t
      [(Real) Floating-Real]
      [(Complex) Floating-Complex]
      [(Fixnum Bignum Integer Rational)
       Floating-Real]
      [else (error 'select-floating-dict "not a floating type" x)])))

(doc 'section 'convenience-type-dispatched-operations)
(doc 'note "These use dictionary dispatch internally but present a simple interface")

(define (num-add a b)
  (doc 'type '(-> Num a => a a a))
  (doc 'export #t)
  (let-values ([(a* b*) (promote-pair a b)])
    (dict-invoke (select-num-dict a*) '+ a* b*)))

(define (num-sub a b)
  (doc 'type '(-> Num a => a a a))
  (doc 'export #t)
  (let-values ([(a* b*) (promote-pair a b)])
    (dict-invoke (select-num-dict a*) '- a* b*)))

(define (num-mul a b)
  (doc 'type '(-> Num a => a a a))
  (doc 'export #t)
  (let-values ([(a* b*) (promote-pair a b)])
    (dict-invoke (select-num-dict a*) '* a* b*)))

(define (num-negate a)
  (doc 'type '(-> Num a => a a))
  (doc 'export #t)
  (dict-invoke (select-num-dict a) 'negate a))

(define (num-abs a)
  (doc 'type '(-> Num a => a a))
  (doc 'export #t)
  (dict-invoke (select-num-dict a) 'abs a))

(define (frac-div a b)
  (doc 'type '(-> Fractional a => a a a))
  (doc 'export #t)
  (let-values ([(a* b*) (promote-pair a b)])
    (dict-invoke (select-fractional-dict a*) '/ a* b*)))

(define (frac-recip a)
  (doc 'type '(-> Fractional a => a a))
  (doc 'export #t)
  (dict-invoke (select-fractional-dict a) 'recip a))

(define (float-sqrt a)
  (doc 'type '(-> Floating a => a a))
  (doc 'export #t)
  (let ([a* (if (exact? a) (exact->inexact a) a)])
    (dict-invoke (select-floating-dict a*) 'sqrt a*)))

(define (float-sin a)
  (doc 'type '(-> Floating a => a a))
  (doc 'export #t)
  (let ([a* (if (exact? a) (exact->inexact a) a)])
    (dict-invoke (select-floating-dict a*) 'sin a*)))

(define (float-cos a)
  (doc 'type '(-> Floating a => a a))
  (doc 'export #t)
  (let ([a* (if (exact? a) (exact->inexact a) a)])
    (dict-invoke (select-floating-dict a*) 'cos a*)))

(define (float-exp a)
  (doc 'type '(-> Floating a => a a))
  (doc 'export #t)
  (let ([a* (if (exact? a) (exact->inexact a) a)])
    (dict-invoke (select-floating-dict a*) 'exp a*)))

(define (float-log a)
  (doc 'type '(-> Floating a => a a))
  (doc 'export #t)
  (let ([a* (if (exact? a) (exact->inexact a) a)])
    (dict-invoke (select-floating-dict a*) 'log a*)))

(doc 'section 'eq-and-ord-instances)
(doc 'note "These are simpler - Scheme's = and < work across the numeric tower")

(doc Eq-Number 'type 'Dict)
(doc Eq-Number 'description "Eq instance for all numbers")
(doc Eq-Number 'export #t)
(define Eq-Number
  `((== . ,=)
    (/= . ,(lambda (a b) (not (= a b))))))

(doc Ord-Real 'type 'Dict)
(doc Ord-Real 'description "Ord instance for real numbers (complex is not orderable)")
(doc Ord-Real 'export #t)
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
