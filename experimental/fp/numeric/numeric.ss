;;; fabric/stitches/fp/numeric.ss -- Numeric Type Class Tower
;;;
;;; Implements the numeric type class hierarchy similar to Haskell:
;;;   Num -> Fractional -> Floating
;;;   Num -> Integral
;;;   Real -> RealFrac -> RealFloat
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Type Class Hierarchy:
;;;   Num           - Basic arithmetic (+, -, *, negate, abs, signum, fromInteger)
;;;   Integral      - Integer division (quotient, remainder, div, mod)
;;;   Fractional    - Real division (/, recip, fromRational)
;;;   Real          - Ordered reals (toRational)
;;;   RealFrac      - Rounding (floor, ceiling, truncate, round)
;;;   Floating      - Transcendental (sin, cos, exp, log, sqrt, pi, etc.)
;;;   RealFloat     - IEEE float operations
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss

(load "core/prelude.ss")
(load "core/fp/meta/combinators.ss")

;;; ============================================================
;;; Type Class Representation
;;; ============================================================
;;;
;;; We represent type classes as records containing the operations.
;;; This is a dictionary-passing style, similar to how GHC implements
;;; type classes under the hood.

;;; ============================================================
;;; Num Type Class
;;; ============================================================
;;;
;;; The foundation of the numeric tower.
;;; Num a where
;;;   (+)         : a -> a -> a
;;;   (-)         : a -> a -> a
;;;   (*)         : a -> a -> a
;;;   negate      : a -> a
;;;   abs         : a -> a
;;;   signum      : a -> a  (returns -1, 0, or 1)
;;;   fromInteger : Integer -> a

;;; make-num : (a a -> a) x ... -> Num a
(define (make-num plus minus times negate-fn abs-fn signum-fn from-integer)
  (vector 'num plus minus times negate-fn abs-fn signum-fn from-integer))

;;; num? : Any -> Boolean
(define (num? x)
  (and (vector? x) (= (vector-length x) 8) (eq? (vector-ref x 0) 'num)))

;;; Num accessors
(define (num-plus n) (vector-ref n 1))
(define (num-minus n) (vector-ref n 2))
(define (num-times n) (vector-ref n 3))
(define (num-negate n) (vector-ref n 4))
(define (num-abs n) (vector-ref n 5))
(define (num-signum n) (vector-ref n 6))
(define (num-from-integer n) (vector-ref n 7))

;;; Generic Num operations
(define (num+ inst a b) ((num-plus inst) a b))
(define (num- inst a b) ((num-minus inst) a b))
(define (num* inst a b) ((num-times inst) a b))
(define (num-neg inst a) ((num-negate inst) a))

;;; Derived operations
(define (num-sum inst xs)
  (fold-left (lambda (acc x) (num+ inst acc x))
             ((num-from-integer inst) 0)
             xs))

(define (num-product inst xs)
  (fold-left (lambda (acc x) (num* inst acc x))
             ((num-from-integer inst) 1)
             xs))

;;; ============================================================
;;; Integral Type Class
;;; ============================================================
;;;
;;; For types that support integer division.
;;; Integral a where (Num a)
;;;   quotient  : a -> a -> a
;;;   remainder : a -> a -> a
;;;   div       : a -> a -> a  (floor division)
;;;   mod       : a -> a -> a  (floor mod)
;;;   toInteger : a -> Integer

;;; make-integral : Num a x ... -> Integral a
(define (make-integral num-inst quot-fn rem-fn div-fn mod-fn to-integer)
  (vector 'integral num-inst quot-fn rem-fn div-fn mod-fn to-integer))

;;; integral? : Any -> Boolean
(define (integral? x)
  (and (vector? x) (= (vector-length x) 7) (eq? (vector-ref x 0) 'integral)))

;;; Integral accessors
(define (integral-num i) (vector-ref i 1))
(define (integral-quotient i) (vector-ref i 2))
(define (integral-remainder i) (vector-ref i 3))
(define (integral-div i) (vector-ref i 4))
(define (integral-mod i) (vector-ref i 5))
(define (integral-to-integer i) (vector-ref i 6))

;;; Generic Integral operations
(define (quot inst a b) ((integral-quotient inst) a b))
(define (rem inst a b) ((integral-remainder inst) a b))
(define (int-div inst a b) ((integral-div inst) a b))
(define (int-mod inst a b) ((integral-mod inst) a b))

;;; Derived: quotRem returns both quotient and remainder
(define (quot-rem inst a b)
  (cons (quot inst a b) (rem inst a b)))

;;; Derived: divMod returns both div and mod
(define (div-mod inst a b)
  (cons (int-div inst a b) (int-mod inst a b)))

;;; Derived: even and odd predicates
(define (int-even? inst a)
  (let ([num (integral-num inst)])
       (= ((integral-remainder inst) a ((num-from-integer num) 2))
          ((num-from-integer num) 0))))

(define (int-odd? inst a)
  (not (int-even? inst a)))

;;; Derived: gcd using Euclidean algorithm
(define (int-gcd inst a b)
  (let ([num (integral-num inst)]
        [zero ((num-from-integer (integral-num inst)) 0)])
       (let loop ([x ((num-abs num) a)]
                  [y ((num-abs num) b)])
            (if (= y zero)
                x
                (loop y (rem inst x y))))))

;;; Derived: lcm
(define (int-lcm inst a b)
  (let ([num (integral-num inst)]
        [zero ((num-from-integer (integral-num inst)) 0)])
       (if (or (= a zero) (= b zero))
           zero
           ((num-abs num)
            (quot inst (num* num a b) (int-gcd inst a b))))))

;;; ============================================================
;;; Fractional Type Class
;;; ============================================================
;;;
;;; For types that support real division.
;;; Fractional a where (Num a)
;;;   (/)          : a -> a -> a
;;;   recip        : a -> a
;;;   fromRational : Rational -> a

;;; make-fractional : Num a x ... -> Fractional a
(define (make-fractional num-inst div-fn recip-fn from-rational)
  (vector 'fractional num-inst div-fn recip-fn from-rational))

;;; fractional? : Any -> Boolean
(define (fractional? x)
  (and (vector? x) (= (vector-length x) 5) (eq? (vector-ref x 0) 'fractional)))

;;; Fractional accessors
(define (fractional-num f) (vector-ref f 1))
(define (fractional-div f) (vector-ref f 2))
(define (fractional-recip f) (vector-ref f 3))
(define (fractional-from-rational f) (vector-ref f 4))

;;; Generic Fractional operations
(define (frac/ inst a b) ((fractional-div inst) a b))
(define (frac-recip inst a) ((fractional-recip inst) a))

;;; ============================================================
;;; Real Type Class
;;; ============================================================
;;;
;;; For types that can be converted to rationals.
;;; Real a where (Num a, Ord a)
;;;   toRational : a -> Rational

;;; make-real : Num a x (a -> Rational) -> Real a
(define (make-real num-inst to-rational)
  (vector 'real num-inst to-rational))

;;; real? : Any -> Boolean
(define (real-typeclass? x)
  (and (vector? x) (= (vector-length x) 3) (eq? (vector-ref x 0) 'real)))

;;; Real accessors
(define (real-num r) (vector-ref r 1))
(define (real-to-rational r) (vector-ref r 2))

;;; ============================================================
;;; RealFrac Type Class
;;; ============================================================
;;;
;;; For types with rounding operations.
;;; RealFrac a where (Real a, Fractional a)
;;;   properFraction : a -> (Integer, a)  -- integral and fractional parts
;;;   truncate       : a -> Integer
;;;   round          : a -> Integer
;;;   ceiling        : a -> Integer
;;;   floor          : a -> Integer

;;; make-realfrac : Real a x Fractional a x ... -> RealFrac a
(define (make-realfrac real-inst frac-inst proper-fraction truncate-fn round-fn ceiling-fn floor-fn)
  (vector 'realfrac real-inst frac-inst proper-fraction truncate-fn round-fn ceiling-fn floor-fn))

;;; realfrac? : Any -> Boolean
(define (realfrac? x)
  (and (vector? x) (= (vector-length x) 8) (eq? (vector-ref x 0) 'realfrac)))

;;; RealFrac accessors
(define (realfrac-real rf) (vector-ref rf 1))
(define (realfrac-frac rf) (vector-ref rf 2))
(define (realfrac-proper-fraction rf) (vector-ref rf 3))
(define (realfrac-truncate rf) (vector-ref rf 4))
(define (realfrac-round rf) (vector-ref rf 5))
(define (realfrac-ceiling rf) (vector-ref rf 6))
(define (realfrac-floor rf) (vector-ref rf 7))

;;; Generic RealFrac operations
(define (frac-truncate inst a) ((realfrac-truncate inst) a))
(define (frac-round inst a) ((realfrac-round inst) a))
(define (frac-ceiling inst a) ((realfrac-ceiling inst) a))
(define (frac-floor inst a) ((realfrac-floor inst) a))

;;; ============================================================
;;; Floating Type Class
;;; ============================================================
;;;
;;; For types with transcendental operations.
;;; Floating a where (Fractional a)
;;;   pi      : a
;;;   exp     : a -> a
;;;   log     : a -> a
;;;   sqrt    : a -> a
;;;   (**)    : a -> a -> a  (power)
;;;   logBase : a -> a -> a
;;;   sin     : a -> a
;;;   cos     : a -> a
;;;   tan     : a -> a
;;;   asin    : a -> a
;;;   acos    : a -> a
;;;   atan    : a -> a
;;;   sinh    : a -> a
;;;   cosh    : a -> a
;;;   tanh    : a -> a
;;;   asinh   : a -> a
;;;   acosh   : a -> a
;;;   atanh   : a -> a

;;; make-floating : Fractional a x a x ... -> Floating a
(define (make-floating frac-inst pi-val exp-fn log-fn sqrt-fn power-fn
                       sin-fn cos-fn tan-fn asin-fn acos-fn atan-fn
                       sinh-fn cosh-fn tanh-fn asinh-fn acosh-fn atanh-fn)
  (vector 'floating frac-inst pi-val exp-fn log-fn sqrt-fn power-fn
          sin-fn cos-fn tan-fn asin-fn acos-fn atan-fn
          sinh-fn cosh-fn tanh-fn asinh-fn acosh-fn atanh-fn))

;;; floating? : Any -> Boolean
(define (floating? x)
  (and (vector? x) (= (vector-length x) 19) (eq? (vector-ref x 0) 'floating)))

;;; Floating accessors
(define (floating-frac f) (vector-ref f 1))
(define (floating-pi f) (vector-ref f 2))
(define (floating-exp f) (vector-ref f 3))
(define (floating-log f) (vector-ref f 4))
(define (floating-sqrt f) (vector-ref f 5))
(define (floating-power f) (vector-ref f 6))
(define (floating-sin f) (vector-ref f 7))
(define (floating-cos f) (vector-ref f 8))
(define (floating-tan f) (vector-ref f 9))
(define (floating-asin f) (vector-ref f 10))
(define (floating-acos f) (vector-ref f 11))
(define (floating-atan f) (vector-ref f 12))
(define (floating-sinh f) (vector-ref f 13))
(define (floating-cosh f) (vector-ref f 14))
(define (floating-tanh f) (vector-ref f 15))
(define (floating-asinh f) (vector-ref f 16))
(define (floating-acosh f) (vector-ref f 17))
(define (floating-atanh f) (vector-ref f 18))

;;; Generic Floating operations
(define (float-pi inst) (floating-pi inst))
(define (float-exp inst a) ((floating-exp inst) a))
(define (float-log inst a) ((floating-log inst) a))
(define (float-sqrt inst a) ((floating-sqrt inst) a))
(define (float-power inst a b) ((floating-power inst) a b))
(define (float-sin inst a) ((floating-sin inst) a))
(define (float-cos inst a) ((floating-cos inst) a))
(define (float-tan inst a) ((floating-tan inst) a))
(define (float-asin inst a) ((floating-asin inst) a))
(define (float-acos inst a) ((floating-acos inst) a))
(define (float-atan inst a) ((floating-atan inst) a))
(define (float-sinh inst a) ((floating-sinh inst) a))
(define (float-cosh inst a) ((floating-cosh inst) a))
(define (float-tanh inst a) ((floating-tanh inst) a))
(define (float-asinh inst a) ((floating-asinh inst) a))
(define (float-acosh inst a) ((floating-acosh inst) a))
(define (float-atanh inst a) ((floating-atanh inst) a))

;;; Derived: logBase using log
(define (float-log-base inst base x)
  (frac/ (floating-frac inst)
         (float-log inst x)
         (float-log inst base)))

;;; Derived: atan2
(define (float-atan2 inst y x)
  (let* ([frac (floating-frac inst)]
         [num (fractional-num frac)]
         [pi (float-pi inst)]
         [zero ((num-from-integer num) 0)])
        (cond
         [(> x zero) (float-atan inst (frac/ frac y x))]
         [(and (< x zero) (>= y zero))
          (num+ num (float-atan inst (frac/ frac y x)) pi)]
         [(and (< x zero) (< y zero))
          (num- num (float-atan inst (frac/ frac y x)) pi)]
         [(and (= x zero) (> y zero))
          (frac/ frac pi ((num-from-integer num) 2))]
         [(and (= x zero) (< y zero))
          (num-neg num (frac/ frac pi ((num-from-integer num) 2)))]
         [else zero])))  ; x = y = 0

;;; ============================================================
;;; Standard Instances: Integer (exact integers)
;;; ============================================================

(define integer-num
  (make-num + - * - abs
            (lambda (x) (cond [(> x 0) 1] [(< x 0) -1] [else 0]))
            (lambda (x) x)))  ; fromInteger is identity

(define integer-integral
  (make-integral integer-num
                 quotient remainder
                 (lambda (a b) (floor (/ a b)))
                 (lambda (a b) (- a (* b (floor (/ a b)))))
                 (lambda (x) x)))

;;; ============================================================
;;; Standard Instances: Flonum (inexact reals)
;;; ============================================================

(define flonum-num
  (make-num + - * - abs
            (lambda (x) (cond [(> x 0.0) 1.0] [(< x 0.0) -1.0] [else 0.0]))
            (lambda (x) (exact->inexact x))))

(define flonum-fractional
  (make-fractional flonum-num
                   /
                   (lambda (x) (/ 1.0 x))
                   (lambda (r) (exact->inexact r))))

(define flonum-real
  (make-real flonum-num inexact->exact))

(define flonum-realfrac
  (make-realfrac flonum-real flonum-fractional
                 (lambda (x) (cons (truncate x) (- x (truncate x))))
                 truncate round ceiling floor))

(define flonum-floating
  (make-floating flonum-fractional
                 3.141592653589793  ; pi
                 exp log sqrt expt
                 sin cos tan
                 asin acos atan
                 sinh cosh tanh
                 asinh acosh atanh))

;;; ============================================================
;;; Standard Instances: Scheme number (polymorphic)
;;; ============================================================
;;;
;;; This works with any Scheme number (exact or inexact).

(define scheme-num
  (make-num + - * -
            (lambda (x) (if (real? x) (abs x) (magnitude x)))
            (lambda (x)
                    (cond [(and (real? x) (> x 0)) 1]
                          [(and (real? x) (< x 0)) -1]
                          [(and (real? x) (= x 0)) 0]
                          [else 1]))  ; complex numbers
            (lambda (x) x)))

(define scheme-integral
  (make-integral scheme-num
                 quotient remainder
                 (lambda (a b) (floor (/ a b)))
                 modulo
                 (lambda (x) (inexact->exact (truncate x)))))

(define scheme-fractional
  (make-fractional scheme-num
                   /
                   (lambda (x) (/ 1 x))
                   (lambda (r) r)))

(define scheme-real
  (make-real scheme-num inexact->exact))

(define scheme-realfrac
  (make-realfrac scheme-real scheme-fractional
                 (lambda (x) (cons (truncate x) (- x (truncate x))))
                 truncate round ceiling floor))

(define scheme-floating
  (make-floating scheme-fractional
                 3.141592653589793  ; pi
                 exp log sqrt expt
                 sin cos tan
                 asin acos atan
                 sinh cosh tanh
                 asinh acosh atanh))

;;; ============================================================
;;; Complex Number Support
;;; ============================================================

;;; make-complex-num : Num a -> Num (Complex a)
;;; Lift a numeric instance to complex numbers.
(define (make-complex-num num-inst)
  (make-num
   ;; Addition
   (lambda (a b)
           (make-rectangular
            (num+ num-inst (real-part a) (real-part b))
            (num+ num-inst (imag-part a) (imag-part b))))
   ;; Subtraction
   (lambda (a b)
           (make-rectangular
            (num- num-inst (real-part a) (real-part b))
            (num- num-inst (imag-part a) (imag-part b))))
   ;; Multiplication: (a+bi)(c+di) = (ac-bd) + (ad+bc)i
   (lambda (a b)
           (let ([ar (real-part a)] [ai (imag-part a)]
                 [br (real-part b)] [bi (imag-part b)])
                (make-rectangular
                 (num- num-inst (num* num-inst ar br) (num* num-inst ai bi))
                 (num+ num-inst (num* num-inst ar bi) (num* num-inst ai br)))))
   ;; Negate
   (lambda (a)
           (make-rectangular
            ((num-negate num-inst) (real-part a))
            ((num-negate num-inst) (imag-part a))))
   ;; Abs (magnitude)
   magnitude
   ;; Signum: z/|z| or 0 if z=0
   (lambda (a)
           (let ([m (magnitude a)])
                (if (= m 0) 0 (/ a m))))
   ;; fromInteger
   (lambda (n)
           (make-rectangular ((num-from-integer num-inst) n) 0))))

(define complex-num (make-complex-num scheme-num))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

;;; Generic power function (for Integral exponents)
(define (num-power num-inst int-inst base exp)
  (let ([one ((num-from-integer num-inst) 1)]
        [zero ((num-from-integer num-inst) 0)])
       (cond
        [(< exp 0) (error 'num-power "Negative exponent requires Fractional")]
        [(= exp 0) one]
        [else
         (let loop ([n exp] [acc one])
              (cond
               [(= n 0) acc]
               [(int-odd? int-inst n)
                (loop (quot int-inst n 2)
                      (num* num-inst acc (num* num-inst base base)))]
               [else
                (loop (quot int-inst n 2)
                      (num* num-inst acc acc))]))])))

;;; Generic sum using Num
(define (generic-sum num-inst xs)
  (fold-left (lambda (acc x) (num+ num-inst acc x))
             ((num-from-integer num-inst) 0)
             xs))

;;; Generic product using Num
(define (generic-product num-inst xs)
  (fold-left (lambda (acc x) (num* num-inst acc x))
             ((num-from-integer num-inst) 1)
             xs))

;;; Mean (requires Fractional)
(define (generic-mean frac-inst xs)
  (let ([num (fractional-num frac-inst)]
        [n (length xs)])
       (if (= n 0)
           ((num-from-integer num) 0)
           (frac/ frac-inst
                  (generic-sum num xs)
                  ((num-from-integer num) n)))))

;;; Variance (requires Floating for sqrt, Fractional for division)
(define (generic-variance float-inst xs)
  (let* ([frac (floating-frac float-inst)]
         [num (fractional-num frac)]
         [n (length xs)]
         [mean (generic-mean frac xs)])
        (if (< n 2)
            ((num-from-integer num) 0)
            (frac/ frac
                   (generic-sum num
                                (map (lambda (x)
                                             (let ([diff (num- num x mean)])
                                                  (num* num diff diff)))
                                     xs))
                   ((num-from-integer num) (- n 1))))))

;;; Standard deviation
(define (generic-stddev float-inst xs)
  (float-sqrt float-inst (generic-variance float-inst xs)))

;;; ============================================================
;;; Numeric Literals Helper
;;; ============================================================

;;; from-int : Num a -> Integer -> a
(define (from-int num-inst n)
  ((num-from-integer num-inst) n))

;;; from-rat : Fractional a -> Rational -> a
(define (from-rat frac-inst r)
  ((fractional-from-rational frac-inst) r))

;;; zero : Num a -> a
(define (num-zero num-inst)
  ((num-from-integer num-inst) 0))

;;; one : Num a -> a
(define (num-one num-inst)
  ((num-from-integer num-inst) 1))

;;; ============================================================
;;; Conversion Functions
;;; ============================================================

;;; real->float : Real a => Floating b => a -> b
(define (real->float real-inst float-inst x)
  (from-rat (floating-frac float-inst)
            ((real-to-rational real-inst) x)))

;;; ============================================================
;;; Export Summary
;;; ============================================================
;;;
;;; Type Class Constructors:
;;;   make-num, make-integral, make-fractional, make-real,
;;;   make-realfrac, make-floating
;;;
;;; Type Class Predicates:
;;;   num?, integral?, fractional?, real-typeclass?, realfrac?, floating?
;;;
;;; Standard Instances:
;;;   integer-num, integer-integral
;;;   flonum-num, flonum-fractional, flonum-real, flonum-realfrac, flonum-floating
;;;   scheme-num, scheme-integral, scheme-fractional, scheme-real,
;;;   scheme-realfrac, scheme-floating
;;;   complex-num
;;;
;;; Generic Operations:
;;;   num+, num-, num*, num-neg, num-sum, num-product
;;;   quot, rem, int-div, int-mod, quot-rem, div-mod, int-even?, int-odd?,
;;;   int-gcd, int-lcm
;;;   frac/, frac-recip
;;;   frac-truncate, frac-round, frac-ceiling, frac-floor
;;;   float-pi, float-exp, float-log, float-sqrt, float-power
;;;   float-sin, float-cos, float-tan, float-asin, float-acos, float-atan
;;;   float-sinh, float-cosh, float-tanh, float-asinh, float-acosh, float-atanh
;;;   float-log-base, float-atan2
;;;
;;; Utility Functions:
;;;   num-power, generic-sum, generic-product, generic-mean,
;;;   generic-variance, generic-stddev
;;;   from-int, from-rat, num-zero, num-one, real->float
