;;; fabric/stitches/fp/bignum.ss -- Arbitrary Precision Numbers
;;;
;;; Implements arbitrary precision arithmetic for:
;;;   - BigInt: Arbitrary precision integers
;;;   - BigDecimal: Arbitrary precision fixed-point decimals
;;;   - BigRational: Exact rational numbers
;;;
;;; All types integrate with the numeric type class tower (numeric.ss).
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Design Philosophy:
;;;   - Efficiency: Use efficient representations (digit arrays, GMP-style)
;;;   - Integration: Full integration with numeric type classes
;;;   - Purity: All operations are pure, no mutation
;;;   - Gradual: Seamless conversion between fixed and arbitrary precision
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss
;;;   - fp/numeric.ss

(load "core/prelude.ss")
(load "core/fp/meta/combinators.ss")
(load "core/fp/numeric/numeric.ss")

;;; ============================================================
;;; BigInt: Arbitrary Precision Integers
;;; ============================================================
;;;
;;; Representation:
;;;   BigInt = (bigint sign digits)
;;;     sign: 1 (positive), -1 (negative), 0 (zero)
;;;     digits: vector of base-2^30 digits (little-endian)
;;;
;;; Why base 2^30?
;;;   - Fits in fixnum on most systems (63-bit fixnums can hold 2^30)
;;;   - Allows efficient multiplication (2^30 × 2^30 = 2^60, fits in 64 bits)
;;;   - Efficient conversion to/from base 10
;;;
;;; Invariants:
;;;   - No leading zeros in digits (except for zero itself)
;;;   - Zero is represented as (bigint 0 #())
;;;   - Sign is correct: positive = 1, negative = -1, zero = 0

(define bigint-base (expt 2 30))  ; Base for digit representation
(define bigint-base-mask (- bigint-base 1))  ; Mask for extracting single digit

;;; make-bigint : Int × Vector → BigInt
;;; Constructor with normalization.
(define (make-bigint sign digits)
  (let ([normalized (normalize-bigint sign digits)])
       (vector 'bigint (car normalized) (cadr normalized))))

;;; bigint? : Any → Boolean
(define (bigint? x)
  (and (vector? x)
       (= (vector-length x) 3)
       (eq? (vector-ref x 0) 'bigint)))

;;; bigint-sign : BigInt → Int
(define (bigint-sign b) (vector-ref b 1))

;;; bigint-digits : BigInt → Vector
(define (bigint-digits b) (vector-ref b 2))

;;; normalize-bigint : Int × Vector → (List Int Vector)
;;; Remove leading zeros and fix sign.
(define (normalize-bigint sign digits)
  (let ([len (vector-length digits)])
       (if (= len 0)
           (list 0 (vector))
           (let loop ([i (- len 1)])
                (cond
                 [(< i 0) (list 0 (vector))]  ; All zeros
                 [(> (vector-ref digits i) 0)
                  ;; Found non-zero digit, trim from here
                  (let ([new-len (+ i 1)])
                       (if (= new-len len)
                           (list sign digits)
                           (let ([trimmed (make-vector new-len)])
                                (do ([j 0 (+ j 1)])
                                    [(= j new-len) (list sign trimmed)]
                                    (vector-set! trimmed j (vector-ref digits j))))))]
                 [else (loop (- i 1))])))))

;;; bigint-zero? : BigInt → Boolean
(define (bigint-zero? b)
  (= (bigint-sign b) 0))

;;; bigint-positive? : BigInt → Boolean
(define (bigint-positive? b)
  (> (bigint-sign b) 0))

;;; bigint-negative? : BigInt → Boolean
(define (bigint-negative? b)
  (< (bigint-sign b) 0))

;;; ============================================================
;;; BigInt Construction from Scheme Numbers
;;; ============================================================

;;; integer->bigint : Integer → BigInt
;;; Convert Scheme integer to BigInt.
(define (integer->bigint n)
  (cond
   [(= n 0) (make-bigint 0 (vector))]
   [(< n 0) (let ([result (integer->bigint (- n))])
                 (make-bigint -1 (bigint-digits result)))]
   [else
    (let loop ([n n] [digits '()])
         (if (= n 0)
             (make-bigint 1 (list->vector (reverse digits)))
             (let ([q (quotient n bigint-base)]
                   [r (remainder n bigint-base)])
                  (loop q (cons r digits)))))]))

;;; bigint->integer : BigInt → Integer | #f
;;; Convert BigInt to Scheme integer if it fits, else #f.
(define (bigint->integer b)
  (let ([sign (bigint-sign b)]
        [digits (bigint-digits b)])
       (if (= sign 0)
           0
           (let ([len (vector-length digits)])
                ;; Check if it would overflow (rough heuristic: > 2 digits)
                (if (> len 2)
                    #f  ; Too large
                    (let ([magnitude
                           (do ([i 0 (+ i 1)]
                                [acc 0 (+ (* acc bigint-base)
                                          (vector-ref digits (- len 1 i)))])
                               [(= i len) acc])])
                         (* sign magnitude)))))))

;;; ============================================================
;;; BigInt Comparison
;;; ============================================================

;;; bigint-compare : BigInt × BigInt → Int
;;; Return -1 if a < b, 0 if a = b, 1 if a > b.
(define (bigint-compare a b)
  (let ([sign-a (bigint-sign a)]
        [sign-b (bigint-sign b)])
       (cond
        [(< sign-a sign-b) -1]
        [(> sign-a sign-b) 1]
        [(= sign-a 0) 0]  ; Both zero
        [else
         ;; Same sign, compare magnitudes
         (let ([cmp (bigint-compare-magnitude a b)])
              (if (= sign-a 1)
                  cmp
                  (- cmp)))])))

;;; bigint-compare-magnitude : BigInt × BigInt → Int
;;; Compare absolute values.
(define (bigint-compare-magnitude a b)
  (let ([digits-a (bigint-digits a)]
        [digits-b (bigint-digits b)]
        [len-a (vector-length (bigint-digits a))]
        [len-b (vector-length (bigint-digits b))])
       (cond
        [(< len-a len-b) -1]
        [(> len-a len-b) 1]
        [else
         ;; Same length, compare digit by digit from most significant
         (let loop ([i (- len-a 1)])
              (cond
               [(< i 0) 0]  ; All digits equal
               [(< (vector-ref digits-a i) (vector-ref digits-b i)) -1]
               [(> (vector-ref digits-a i) (vector-ref digits-b i)) 1]
               [else (loop (- i 1))]))])))

;;; bigint=? : BigInt × BigInt → Boolean
(define (bigint=? a b) (= (bigint-compare a b) 0))

;;; bigint<? : BigInt × BigInt → Boolean
(define (bigint<? a b) (< (bigint-compare a b) 0))

;;; bigint>? : BigInt × BigInt → Boolean
(define (bigint>? a b) (> (bigint-compare a b) 0))

;;; bigint<=? : BigInt × BigInt → Boolean
(define (bigint<=? a b) (<= (bigint-compare a b) 0))

;;; bigint>=? : BigInt × BigInt → Boolean
(define (bigint>=? a b) (>= (bigint-compare a b) 0))

;;; ============================================================
;;; BigInt Arithmetic: Addition
;;; ============================================================

;;; bigint-add : BigInt × BigInt → BigInt
(define (bigint-add a b)
  (let ([sign-a (bigint-sign a)]
        [sign-b (bigint-sign b)])
       (cond
        [(= sign-a 0) b]
        [(= sign-b 0) a]
        [(= sign-a sign-b)
         ;; Same sign: add magnitudes, keep sign
         (make-bigint sign-a (bigint-add-magnitude (bigint-digits a) (bigint-digits b)))]
        [else
         ;; Different signs: subtract magnitudes
         (let ([cmp (bigint-compare-magnitude a b)])
              (cond
               [(= cmp 0) (make-bigint 0 (vector))]  ; Equal magnitudes cancel
               [(> cmp 0)
                ;; |a| > |b|: result has sign of a
                (make-bigint sign-a (bigint-sub-magnitude (bigint-digits a) (bigint-digits b)))]
               [else
                ;; |a| < |b|: result has sign of b
                (make-bigint sign-b (bigint-sub-magnitude (bigint-digits b) (bigint-digits a)))]))])))

;;; bigint-add-magnitude : Vector × Vector → Vector
;;; Add two digit vectors (unsigned).
(define (bigint-add-magnitude a b)
  (let* ([len-a (vector-length a)]
         [len-b (vector-length b)]
         [max-len (max len-a len-b)]
         [result (make-vector (+ max-len 1) 0)])
        (let loop ([i 0] [carry 0])
             (if (= i max-len)
                 (begin
                  (when (> carry 0)
                        (vector-set! result i carry))
                  (cadr (normalize-bigint 1 result)))
                 (let* ([digit-a (if (< i len-a) (vector-ref a i) 0)]
                        [digit-b (if (< i len-b) (vector-ref b i) 0)]
                        [sum (+ digit-a digit-b carry)]
                        [new-carry (quotient sum bigint-base)]
                        [new-digit (remainder sum bigint-base)])
                       (vector-set! result i new-digit)
                       (loop (+ i 1) new-carry))))))

;;; ============================================================
;;; BigInt Arithmetic: Subtraction
;;; ============================================================

;;; bigint-sub : BigInt × BigInt → BigInt
(define (bigint-sub a b)
  (bigint-add a (bigint-negate b)))

;;; bigint-sub-magnitude : Vector × Vector → Vector
;;; Subtract b from a (assumes |a| >= |b|).
(define (bigint-sub-magnitude a b)
  (let* ([len-a (vector-length a)]
         [len-b (vector-length b)]
         [result (make-vector len-a 0)])
        (let loop ([i 0] [borrow 0])
             (if (= i len-a)
                 (cadr (normalize-bigint 1 result))
                 (let* ([digit-a (vector-ref a i)]
                        [digit-b (if (< i len-b) (vector-ref b i) 0)]
                        [diff (- digit-a digit-b borrow)])
                       (if (< diff 0)
                           (begin
                            (vector-set! result i (+ diff bigint-base))
                            (loop (+ i 1) 1))
                           (begin
                            (vector-set! result i diff)
                            (loop (+ i 1) 0))))))))

;;; bigint-negate : BigInt → BigInt
(define (bigint-negate b)
  (make-bigint (- (bigint-sign b)) (bigint-digits b)))

;;; ============================================================
;;; BigInt Arithmetic: Multiplication
;;; ============================================================

;;; bigint-mul : BigInt × BigInt → BigInt
(define (bigint-mul a b)
  (let ([sign-a (bigint-sign a)]
        [sign-b (bigint-sign b)])
       (if (or (= sign-a 0) (= sign-b 0))
           (make-bigint 0 (vector))
           (let ([result-sign (* sign-a sign-b)]
                 [result-digits (bigint-mul-magnitude (bigint-digits a) (bigint-digits b))])
                (make-bigint result-sign result-digits)))))

;;; bigint-mul-magnitude : Vector × Vector → Vector
;;; Multiply two digit vectors (grade-school algorithm).
(define (bigint-mul-magnitude a b)
  (let* ([len-a (vector-length a)]
         [len-b (vector-length b)]
         [result (make-vector (+ len-a len-b) 0)])
        (do ([i 0 (+ i 1)])
            [(= i len-a) (cadr (normalize-bigint 1 result))]
            (let ([digit-a (vector-ref a i)])
                 (when (> digit-a 0)
                       (let loop ([j 0] [carry 0])
                            (when (or (< j len-b) (> carry 0))
                                  (let* ([digit-b (if (< j len-b) (vector-ref b j) 0)]
                                         [product (+ (* digit-a digit-b)
                                                     (vector-ref result (+ i j))
                                                     carry)]
                                         [new-carry (quotient product bigint-base)]
                                         [new-digit (remainder product bigint-base)])
                                        (vector-set! result (+ i j) new-digit)
                                        (loop (+ j 1) new-carry)))))))))

;;; ============================================================
;;; BigInt Arithmetic: Division
;;; ============================================================

;;; bigint-divmod : BigInt × BigInt → (BigInt × BigInt)
;;; Return quotient and remainder.
;;; Uses long division algorithm.
(define (bigint-divmod a b)
  (let ([sign-a (bigint-sign a)]
        [sign-b (bigint-sign b)])
       (cond
        [(= sign-b 0) (error 'bigint-divmod "Division by zero")]
        [(= sign-a 0) (cons (make-bigint 0 (vector)) (make-bigint 0 (vector)))]
        [else
         (let-values ([(q r) (bigint-divmod-magnitude (bigint-digits a) (bigint-digits b))])
                     (let ([q-sign (* sign-a sign-b)]
                           [r-sign sign-a])
                          (cons (make-bigint q-sign q)
                                (make-bigint r-sign r))))])))

;;; bigint-divmod-magnitude : Vector × Vector → (Vector × Vector)
;;; Division for unsigned digit vectors.
;;; Uses repeated subtraction (naive but correct for small numbers).
;;; TODO: Implement Knuth's Algorithm D for efficiency.
(define (bigint-divmod-magnitude a b)
  ;; Use repeated subtraction on BigInt values
  (let ([dividend (make-bigint 1 a)]
        [divisor (make-bigint 1 b)]
        [zero (make-bigint 0 (vector))]
        [one (make-bigint 1 (vector 1))])
       (if (bigint=? divisor zero)
           (error 'bigint-divmod-magnitude "Division by zero")
           (let loop ([q zero] [r dividend])
                (if (bigint<? r divisor)
                    (values (bigint-digits q) (bigint-digits r))
                    (loop (bigint-add q one)
                          (bigint-sub r divisor)))))))

;;; bigint-quotient : BigInt × BigInt → BigInt
(define (bigint-quotient a b)
  (car (bigint-divmod a b)))

;;; bigint-remainder : BigInt × BigInt → BigInt
(define (bigint-remainder a b)
  (cdr (bigint-divmod a b)))

;;; ============================================================
;;; BigInt Other Operations
;;; ============================================================

;;; bigint-abs : BigInt → BigInt
(define (bigint-abs b)
  (if (bigint-negative? b)
      (bigint-negate b)
      b))

;;; bigint-signum : BigInt → Int
(define (bigint-signum b)
  (bigint-sign b))

;;; bigint-gcd : BigInt × BigInt → BigInt
;;; Euclidean algorithm.
(define (bigint-gcd a b)
  (let ([zero (make-bigint 0 (vector))])
       (let loop ([x (bigint-abs a)]
                  [y (bigint-abs b)])
            (if (bigint=? y zero)
                x
                (loop y (bigint-remainder x y))))))

;;; bigint-lcm : BigInt × BigInt → BigInt
(define (bigint-lcm a b)
  (let ([zero (make-bigint 0 (vector))])
       (if (or (bigint=? a zero) (bigint=? b zero))
           zero
           (bigint-abs (bigint-quotient (bigint-mul a b) (bigint-gcd a b))))))

;;; ============================================================
;;; BigInt Num Instance
;;; ============================================================

(define bigint-num
  (make-num bigint-add
            bigint-sub
            bigint-mul
            bigint-negate
            bigint-abs
            (lambda (b) (integer->bigint (bigint-signum b)))
            integer->bigint))

(define bigint-integral
  (make-integral bigint-num
                 bigint-quotient
                 bigint-remainder
                 bigint-quotient  ; div = quotient for BigInt
                 bigint-remainder  ; mod = remainder for BigInt
                 (lambda (b) (or (bigint->integer b)
                                 (error 'bigint-to-integer "BigInt too large")))))

;;; ============================================================
;;; BigRational: Exact Rational Numbers
;;; ============================================================
;;;
;;; Representation:
;;;   BigRational = (bigrational numerator denominator)
;;;     numerator: BigInt
;;;     denominator: BigInt (always positive, always in lowest terms)
;;;
;;; Invariants:
;;;   - denominator > 0
;;;   - gcd(numerator, denominator) = 1 (always reduced)
;;;   - Zero is represented as 0/1

;;; make-bigrational : BigInt × BigInt → BigRational
;;; Constructor with normalization.
(define (make-bigrational numer denom)
  (cond
   [(bigint-zero? denom) (error 'make-bigrational "Zero denominator")]
   [(bigint-zero? numer) (vector 'bigrational (make-bigint 0 (vector)) (integer->bigint 1))]
   [else
    (let* ([g (bigint-gcd numer denom)]
           [n (bigint-quotient numer g)]
           [d (bigint-quotient denom g)]
           ;; Ensure denominator is positive
           [sign-d (bigint-sign d)])
          (if (< sign-d 0)
              (vector 'bigrational (bigint-negate n) (bigint-negate d))
              (vector 'bigrational n d)))]))

;;; bigrational? : Any → Boolean
(define (bigrational? x)
  (and (vector? x)
       (= (vector-length x) 3)
       (eq? (vector-ref x 0) 'bigrational)))

;;; bigrational-numerator : BigRational → BigInt
(define (bigrational-numerator r) (vector-ref r 1))

;;; bigrational-denominator : BigRational → BigInt
(define (bigrational-denominator r) (vector-ref r 2))

;;; integer->bigrational : Integer → BigRational
(define (integer->bigrational n)
  (make-bigrational (integer->bigint n) (integer->bigint 1)))

;;; bigint->bigrational : BigInt → BigRational
(define (bigint->bigrational b)
  (make-bigrational b (integer->bigint 1)))

;;; ============================================================
;;; BigRational Arithmetic
;;; ============================================================

;;; bigrational-add : BigRational × BigRational → BigRational
;;; a/b + c/d = (ad + bc) / bd
(define (bigrational-add p q)
  (let ([a (bigrational-numerator p)]
        [b (bigrational-denominator p)]
        [c (bigrational-numerator q)]
        [d (bigrational-denominator q)])
       (make-bigrational (bigint-add (bigint-mul a d) (bigint-mul b c))
                         (bigint-mul b d))))

;;; bigrational-sub : BigRational × BigRational → BigRational
(define (bigrational-sub p q)
  (bigrational-add p (bigrational-negate q)))

;;; bigrational-mul : BigRational × BigRational → BigRational
;;; (a/b) * (c/d) = (ac) / (bd)
(define (bigrational-mul p q)
  (make-bigrational (bigint-mul (bigrational-numerator p) (bigrational-numerator q))
                    (bigint-mul (bigrational-denominator p) (bigrational-denominator q))))

;;; bigrational-div : BigRational × BigRational → BigRational
;;; (a/b) / (c/d) = (ad) / (bc)
(define (bigrational-div p q)
  (let ([c (bigrational-numerator q)])
       (if (bigint-zero? c)
           (error 'bigrational-div "Division by zero")
           (make-bigrational (bigint-mul (bigrational-numerator p) (bigrational-denominator q))
                             (bigint-mul (bigrational-denominator p) c)))))

;;; bigrational-negate : BigRational → BigRational
(define (bigrational-negate r)
  (make-bigrational (bigint-negate (bigrational-numerator r))
                    (bigrational-denominator r)))

;;; bigrational-abs : BigRational → BigRational
(define (bigrational-abs r)
  (if (bigint-negative? (bigrational-numerator r))
      (bigrational-negate r)
      r))

;;; bigrational-signum : BigRational → Int
(define (bigrational-signum r)
  (bigint-signum (bigrational-numerator r)))

;;; bigrational-recip : BigRational → BigRational
;;; 1 / (a/b) = b/a
(define (bigrational-recip r)
  (let ([n (bigrational-numerator r)]
        [d (bigrational-denominator r)])
       (if (bigint-zero? n)
           (error 'bigrational-recip "Division by zero")
           (make-bigrational d n))))

;;; ============================================================
;;; BigRational Num and Fractional Instances
;;; ============================================================

(define bigrational-num
  (make-num bigrational-add
            bigrational-sub
            bigrational-mul
            bigrational-negate
            bigrational-abs
            (lambda (r) (integer->bigrational (bigrational-signum r)))
            integer->bigrational))

(define bigrational-fractional
  (make-fractional bigrational-num
                   bigrational-div
                   bigrational-recip
                   (lambda (r) r)))  ; fromRational is identity

;;; ============================================================
;;; BigDecimal: Arbitrary Precision Fixed-Point Decimals
;;; ============================================================
;;;
;;; Representation:
;;;   BigDecimal = (bigdecimal coefficient scale)
;;;     coefficient: BigInt (the unscaled value)
;;;     scale: Int (number of decimal places)
;;;
;;; Value represented: coefficient * 10^(-scale)
;;; Example: 123.45 = (bigdecimal (bigint 12345) 2)
;;;
;;; Note: This is a sketch for future implementation.
;;; Full BigDecimal support requires rounding mode specifications.

;;; ============================================================
;;; Export Summary
;;; ============================================================
;;;
;;; BigInt:
;;;   make-bigint, bigint?, bigint-sign, bigint-digits
;;;   integer->bigint, bigint->integer
;;;   bigint-zero?, bigint-positive?, bigint-negative?
;;;   bigint-compare, bigint=?, bigint<?, bigint>?, bigint<=?, bigint>=?
;;;   bigint-add, bigint-sub, bigint-mul, bigint-divmod
;;;   bigint-quotient, bigint-remainder
;;;   bigint-abs, bigint-signum, bigint-gcd, bigint-lcm
;;;   bigint-num, bigint-integral (type class instances)
;;;
;;; BigRational:
;;;   make-bigrational, bigrational?, bigrational-numerator, bigrational-denominator
;;;   integer->bigrational, bigint->bigrational
;;;   bigrational-add, bigrational-sub, bigrational-mul, bigrational-div
;;;   bigrational-negate, bigrational-abs, bigrational-signum, bigrational-recip
;;;   bigrational-num, bigrational-fractional (type class instances)
