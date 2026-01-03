;;; core/linalg/numeric-instances.ss --- Num/Fractional/Floating instances for Vec and Matrix
;;;
;;; This module provides numeric type class instances for Vec and Matrix,
;;; enabling element-wise arithmetic operations:
;;;   (vec+ v1 v2)        ; element-wise addition
;;;   (vec* v1 v2)        ; element-wise multiplication (Hadamard)
;;;   (vec-sin v)         ; element-wise sin
;;;   (matrix+ m1 m2)     ; element-wise addition
;;;   (matrix-sin m)      ; element-wise sin
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - vec.ss
;;;   - matrix.ss

(load "core/base/prelude.ss")
(load "core/linalg/vec.ss")
(load "core/linalg/matrix.ss")

;;; ============================================================
;;; Vec Num Instance
;;; ============================================================

;;; vec+ : Vec Num × Vec Num → Vec Num | Error
;;; Element-wise addition.
(define vec+ vec-add)

;;; vec- : Vec Num × Vec Num → Vec Num | Error
;;; Element-wise subtraction.
(define vec- vec-sub)

;;; vec* : Vec Num × Vec Num → Vec Num | Error
;;; Element-wise multiplication (Hadamard product).
(define vec* vec-mul)

;;; vec-negate : Vec Num → Vec Num
;;; Negate all elements.
;;; (Already defined in vec.ss)

;;; vec-abs : Vec Num → Vec Num
;;; Element-wise absolute value.
(define (vec-abs v)
  (vec-map abs v))

;;; vec-signum : Vec Num → Vec Num
;;; Element-wise signum (-1, 0, or 1).
(define (vec-signum v)
  (vec-map (lambda (x)
                   (cond [(< x 0) -1]
                         [(> x 0) 1]
                         [else 0]))
           v))

;;; vec-from-integer : Nat × Int → Vec Int
;;; Create a vector of n copies of the integer.
(define (vec-from-integer n val)
  (make-vec n val))

;;; ============================================================
;;; Vec Fractional Instance
;;; ============================================================

;;; vec/ : Vec Num × Vec Num → Vec Num | Error
;;; Element-wise division.
(define vec/ vec-div)

;;; vec-recip : Vec Num → Vec Num
;;; Element-wise reciprocal (1/x).
(define (vec-recip v)
  (vec-map (lambda (x) (/ 1 x)) v))

;;; ============================================================
;;; Vec Floating Instance
;;; ============================================================

;;; vec-pi : Nat → Vec Num
;;; Create a vector of n copies of pi.
(define (vec-pi n)
  (make-vec n 3.141592653589793))

;;; vec-exp : Vec Num → Vec Num
;;; Element-wise exponential.
(define (vec-exp v)
  (vec-map exp v))

;;; vec-log : Vec Num → Vec Num
;;; Element-wise natural logarithm.
(define (vec-log v)
  (vec-map log v))

;;; vec-sqrt : Vec Num → Vec Num
;;; Element-wise square root.
(define (vec-sqrt v)
  (vec-map sqrt v))

;;; vec-pow : Vec Num × Vec Num → Vec Num | Error
;;; Element-wise exponentiation.
(define (vec-pow v1 v2)
  (vec-zip-with expt v1 v2))

;;; vec-sin : Vec Num → Vec Num
;;; Element-wise sine.
(define (vec-sin v)
  (vec-map sin v))

;;; vec-cos : Vec Num → Vec Num
;;; Element-wise cosine.
(define (vec-cos v)
  (vec-map cos v))

;;; vec-tan : Vec Num → Vec Num
;;; Element-wise tangent.
(define (vec-tan v)
  (vec-map tan v))

;;; vec-asin : Vec Num → Vec Num
;;; Element-wise arc sine.
(define (vec-asin v)
  (vec-map asin v))

;;; vec-acos : Vec Num → Vec Num
;;; Element-wise arc cosine.
(define (vec-acos v)
  (vec-map acos v))

;;; vec-atan : Vec Num → Vec Num
;;; Element-wise arc tangent.
(define (vec-atan v)
  (vec-map atan v))

;;; vec-sinh : Vec Num → Vec Num
;;; Element-wise hyperbolic sine.
(define (vec-sinh v)
  (vec-map (lambda (x) (/ (- (exp x) (exp (- x))) 2)) v))

;;; vec-cosh : Vec Num → Vec Num
;;; Element-wise hyperbolic cosine.
(define (vec-cosh v)
  (vec-map (lambda (x) (/ (+ (exp x) (exp (- x))) 2)) v))

;;; vec-tanh : Vec Num → Vec Num
;;; Element-wise hyperbolic tangent.
(define (vec-tanh v)
  (vec-map (lambda (x)
                   (let ([ex (exp x)]
                         [e-x (exp (- x))])
                        (/ (- ex e-x) (+ ex e-x))))
           v))

;;; vec-asinh : Vec Num → Vec Num
;;; Element-wise inverse hyperbolic sine.
(define (vec-asinh v)
  (vec-map (lambda (x) (log (+ x (sqrt (+ (* x x) 1))))) v))

;;; vec-acosh : Vec Num → Vec Num
;;; Element-wise inverse hyperbolic cosine.
(define (vec-acosh v)
  (vec-map (lambda (x) (log (+ x (sqrt (- (* x x) 1))))) v))

;;; vec-atanh : Vec Num → Vec Num
;;; Element-wise inverse hyperbolic tangent.
(define (vec-atanh v)
  (vec-map (lambda (x) (/ (log (/ (+ 1 x) (- 1 x))) 2)) v))

;;; ============================================================
;;; Matrix Num Instance
;;; ============================================================

;;; matrix+ : Matrix Num × Matrix Num → Matrix Num | Error
;;; Element-wise addition.
(define matrix+ matrix-add)

;;; matrix- : Matrix Num × Matrix Num → Matrix Num | Error
;;; Element-wise subtraction.
(define matrix- matrix-sub)

;;; matrix-hadamard : Matrix Num × Matrix Num → Matrix Num | Error
;;; Element-wise multiplication (Hadamard product).
(define (matrix-hadamard m1 m2)
  (let ([r1 (matrix-rows m1)]
        [c1 (matrix-cols m1)]
        [r2 (matrix-rows m2)]
        [c2 (matrix-cols m2)])
       (if (or (not (= r1 r2)) (not (= c1 c2)))
           `(error dimension-mismatch (,r1 ,c1) (,r2 ,c2))
           (let ([data1 (matrix-data m1)]
                 [data2 (matrix-data m2)]
                 [result (make-vector (* r1 c1) 0)])
                (do ([i 0 (+ i 1)])
                    ((= i (* r1 c1)))
                    (vector-set! result i (* (vector-ref data1 i)
                                             (vector-ref data2 i))))
                (list 'matrix r1 c1 result)))))

;;; Alias for consistency with Num class
(define matrix* matrix-hadamard)

;;; matrix-negate : Matrix Num → Matrix Num
;;; Negate all elements.
(define (matrix-negate m)
  (matrix-map - m))

;;; matrix-abs : Matrix Num → Matrix Num
;;; Element-wise absolute value.
(define (matrix-abs m)
  (matrix-map abs m))

;;; matrix-signum : Matrix Num → Matrix Num
;;; Element-wise signum.
(define (matrix-signum m)
  (matrix-map (lambda (x)
                      (cond [(< x 0) -1]
                            [(> x 0) 1]
                            [else 0]))
              m))

;;; matrix-from-integer : Nat × Nat × Int → Matrix Int
;;; Create a matrix filled with copies of the integer.
(define (matrix-from-integer rows cols val)
  (make-matrix rows cols val))

;;; ============================================================
;;; Matrix Fractional Instance
;;; ============================================================

;;; matrix/ : Matrix Num × Matrix Num → Matrix Num | Error
;;; Element-wise division.
(define (matrix/ m1 m2)
  (let ([r1 (matrix-rows m1)]
        [c1 (matrix-cols m1)]
        [r2 (matrix-rows m2)]
        [c2 (matrix-cols m2)])
       (if (or (not (= r1 r2)) (not (= c1 c2)))
           `(error dimension-mismatch (,r1 ,c1) (,r2 ,c2))
           (let ([data1 (matrix-data m1)]
                 [data2 (matrix-data m2)]
                 [result (make-vector (* r1 c1) 0)])
                (do ([i 0 (+ i 1)])
                    ((= i (* r1 c1)))
                    (vector-set! result i (/ (vector-ref data1 i)
                                             (vector-ref data2 i))))
                (list 'matrix r1 c1 result)))))

;;; matrix-recip : Matrix Num → Matrix Num
;;; Element-wise reciprocal.
(define (matrix-recip m)
  (matrix-map (lambda (x) (/ 1 x)) m))

;;; ============================================================
;;; Matrix Floating Instance
;;; ============================================================

;;; matrix-pi : Nat × Nat → Matrix Num
;;; Create a matrix filled with pi.
(define (matrix-pi rows cols)
  (make-matrix rows cols 3.141592653589793))

;;; matrix-exp : Matrix Num → Matrix Num
;;; Element-wise exponential.
(define (matrix-exp m)
  (matrix-map exp m))

;;; matrix-log : Matrix Num → Matrix Num
;;; Element-wise natural logarithm.
(define (matrix-log m)
  (matrix-map log m))

;;; matrix-sqrt : Matrix Num → Matrix Num
;;; Element-wise square root.
(define (matrix-sqrt m)
  (matrix-map sqrt m))

;;; matrix-pow : Matrix Num × Matrix Num → Matrix Num | Error
;;; Element-wise exponentiation.
(define (matrix-pow m1 m2)
  (let ([r1 (matrix-rows m1)]
        [c1 (matrix-cols m1)]
        [r2 (matrix-rows m2)]
        [c2 (matrix-cols m2)])
       (if (or (not (= r1 r2)) (not (= c1 c2)))
           `(error dimension-mismatch (,r1 ,c1) (,r2 ,c2))
           (let ([data1 (matrix-data m1)]
                 [data2 (matrix-data m2)]
                 [result (make-vector (* r1 c1) 0)])
                (do ([i 0 (+ i 1)])
                    ((= i (* r1 c1)))
                    (vector-set! result i (expt (vector-ref data1 i)
                                                (vector-ref data2 i))))
                (list 'matrix r1 c1 result)))))

;;; matrix-sin : Matrix Num → Matrix Num
;;; Element-wise sine.
(define (matrix-sin m)
  (matrix-map sin m))

;;; matrix-cos : Matrix Num → Matrix Num
;;; Element-wise cosine.
(define (matrix-cos m)
  (matrix-map cos m))

;;; matrix-tan : Matrix Num → Matrix Num
;;; Element-wise tangent.
(define (matrix-tan m)
  (matrix-map tan m))

;;; matrix-asin : Matrix Num → Matrix Num
;;; Element-wise arc sine.
(define (matrix-asin m)
  (matrix-map asin m))

;;; matrix-acos : Matrix Num → Matrix Num
;;; Element-wise arc cosine.
(define (matrix-acos m)
  (matrix-map acos m))

;;; matrix-atan : Matrix Num → Matrix Num
;;; Element-wise arc tangent.
(define (matrix-atan m)
  (matrix-map atan m))

;;; matrix-sinh : Matrix Num → Matrix Num
;;; Element-wise hyperbolic sine.
(define (matrix-sinh m)
  (matrix-map (lambda (x) (/ (- (exp x) (exp (- x))) 2)) m))

;;; matrix-cosh : Matrix Num → Matrix Num
;;; Element-wise hyperbolic cosine.
(define (matrix-cosh m)
  (matrix-map (lambda (x) (/ (+ (exp x) (exp (- x))) 2)) m))

;;; matrix-tanh : Matrix Num → Matrix Num
;;; Element-wise hyperbolic tangent.
(define (matrix-tanh m)
  (matrix-map (lambda (x)
                      (let ([ex (exp x)]
                            [e-x (exp (- x))])
                           (/ (- ex e-x) (+ ex e-x))))
              m))

;;; matrix-asinh : Matrix Num → Matrix Num
;;; Element-wise inverse hyperbolic sine.
(define (matrix-asinh m)
  (matrix-map (lambda (x) (log (+ x (sqrt (+ (* x x) 1))))) m))

;;; matrix-acosh : Matrix Num → Matrix Num
;;; Element-wise inverse hyperbolic cosine.
(define (matrix-acosh m)
  (matrix-map (lambda (x) (log (+ x (sqrt (- (* x x) 1))))) m))

;;; matrix-atanh : Matrix Num → Matrix Num
;;; Element-wise inverse hyperbolic tangent.
(define (matrix-atanh m)
  (matrix-map (lambda (x) (/ (log (/ (+ 1 x) (- 1 x))) 2)) m))

;;; ============================================================
;;; Scalar-Vector and Scalar-Matrix Operations
;;; ============================================================

;;; These allow mixing scalars and vectors/matrices in arithmetic.

;;; scalar-vec+ : Num × Vec Num → Vec Num
;;; Add scalar to each element.
(define (scalar-vec+ k v)
  (vec-map (lambda (x) (+ k x)) v))

;;; scalar-vec* : Num × Vec Num → Vec Num
;;; Multiply each element by scalar (same as vec-scale).
(define scalar-vec* vec-scale)

;;; scalar-matrix+ : Num × Matrix Num → Matrix Num
;;; Add scalar to each element.
(define (scalar-matrix+ k m)
  (matrix-map (lambda (x) (+ k x)) m))

;;; scalar-matrix* : Num × Matrix Num → Matrix Num
;;; Multiply each element by scalar (same as matrix-scale).
(define scalar-matrix* matrix-scale)

;;; ============================================================
;;; Functor Instance Aliases (for consistency)
;;; ============================================================

;;; vec-fmap : (a → b) × Vec a → Vec b
;;; Functor fmap for Vec.
(define vec-fmap vec-map)

;;; matrix-fmap : (a → b) × Matrix a → Matrix b
;;; Functor fmap for Matrix.
(define matrix-fmap matrix-map)

;;; ============================================================
;;; Applicative-style Operations
;;; ============================================================

;;; vec-pure : a → Vec a
;;; Wrap value in a singleton vector.
(define (vec-pure x)
  (vector x))

;;; vec-ap : Vec (a → b) × Vec a → Vec b | Error
;;; Apply a vector of functions to a vector of values.
;;; Supports broadcasting: if one operand is length-1, it broadcasts to match the other.
(define (vec-ap fs xs)
  (let ([nf (vec-length fs)]
        [nx (vec-length xs)])
       (cond
        ;; Case 1: Equal lengths - standard element-wise application
        [(= nf nx)
         (let ([result (make-vector nf 0)])
              (do ([i 0 (+ i 1)])
                  ((= i nf) result)
                  (vector-set! result i ((vector-ref fs i) (vector-ref xs i)))))]
        ;; Case 2: Single function broadcasts to all values
        [(= nf 1)
         (let ([f (vector-ref fs 0)]
               [result (make-vector nx 0)])
              (do ([i 0 (+ i 1)])
                  ((= i nx) result)
                  (vector-set! result i (f (vector-ref xs i)))))]
        ;; Case 3: Single value broadcasts to all functions
        [(= nx 1)
         (let ([x (vector-ref xs 0)]
               [result (make-vector nf 0)])
              (do ([i 0 (+ i 1)])
                  ((= i nf) result)
                  (vector-set! result i ((vector-ref fs i) x))))]
        ;; Case 4: Incompatible dimensions
        [else `(error dimension-mismatch ,nf ,nx)])))

;;; vec-lift2 : (a × b → c) × Vec a × Vec b → Vec c | Error
;;; Lift a binary function to work on vectors element-wise.
(define (vec-lift2 f v1 v2)
  (vec-zip-with f v1 v2))

;;; matrix-pure : Nat × Nat × a → Matrix a
;;; Create a matrix filled with a value.
(define (matrix-pure rows cols x)
  (make-matrix rows cols x))

;;; matrix-ap : Matrix (a → b) × Matrix a → Matrix b | Error
;;; Apply a matrix of functions to a matrix of values.
(define (matrix-ap mf mx)
  (let ([rf (matrix-rows mf)]
        [cf (matrix-cols mf)]
        [rx (matrix-rows mx)]
        [cx (matrix-cols mx)])
       (if (or (not (= rf rx)) (not (= cf cx)))
           `(error dimension-mismatch (,rf ,cf) (,rx ,cx))
           (let ([dataf (matrix-data mf)]
                 [datax (matrix-data mx)]
                 [result (make-vector (* rf cf) 0)])
                (do ([i 0 (+ i 1)])
                    ((= i (* rf cf)))
                    (vector-set! result i ((vector-ref dataf i) (vector-ref datax i))))
                (list 'matrix rf cf result)))))

;;; matrix-lift2 : (a × b → c) × Matrix a × Matrix b → Matrix c | Error
;;; Lift a binary function to work on matrices element-wise.
(define (matrix-lift2 f m1 m2)
  (let ([r1 (matrix-rows m1)]
        [c1 (matrix-cols m1)]
        [r2 (matrix-rows m2)]
        [c2 (matrix-cols m2)])
       (if (or (not (= r1 r2)) (not (= c1 c2)))
           `(error dimension-mismatch (,r1 ,c1) (,r2 ,c2))
           (let ([data1 (matrix-data m1)]
                 [data2 (matrix-data m2)]
                 [result (make-vector (* r1 c1) 0)])
                (do ([i 0 (+ i 1)])
                    ((= i (* r1 c1)))
                    (vector-set! result i (f (vector-ref data1 i)
                                             (vector-ref data2 i))))
                (list 'matrix r1 c1 result)))))
