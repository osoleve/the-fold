(load "core/base/prelude.ss")
(load "lattice/fp/numeric/transcendental.ss")

(doc 'module 'complex)
(doc 'description "Comprehensive complex number arithmetic for signal processing, quantum computing, and advanced mathematics")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ====
;;; Complex Number Representation
;;; ====

;;; Complex numbers are represented in rectangular form: a + bi
;;; where a is the real part and b is the imaginary part.
;;;
;;; Structure: (complex real imag)

;;; make-complex : Number × Number → Complex
;;; Create a complex number from real and imaginary parts.
(define (make-complex real imag)
  (list 'complex real imag))

;;; complex? : Any → Boolean
;;; Check if a value is a complex number.
(define (complex? x)
  (and (pair? x) (eq? (car x) 'complex)))

;;; complex-real : Complex → Number
;;; Extract the real part.
(define (complex-real c)
  (cadr c))

;;; complex-imag : Complex → Number
;;; Extract the imaginary part.
(define (complex-imag c)
  (caddr c))

;;; real-num->complex : Number → Complex
;;; Lift a real number to a complex number.
(define (real-num->complex x)
  (make-complex x 0))

;;; i-value : → Complex
;;; The imaginary unit i (0 + 1i).
(define (i-value)
  (make-complex 0 1))

;;; ====
;;; Polar Form
;;; ====

;;; make-polar : Number × Number → Complex
;;; Create a complex number from polar coordinates (magnitude, phase).
;;; z = r * e^(iθ) = r * (cos(θ) + i*sin(θ))
(define (make-polar r theta)
  (make-complex (* r (cos theta)) (* r (sin theta))))

;;; complex-magnitude : Complex → Number
;;; Compute the magnitude (absolute value) of a complex number.
;;; |a + bi| = sqrt(a² + b²)
(define (complex-magnitude c)
  (let ([a (complex-real c)]
        [b (complex-imag c)])
       (sqrt (+ (* a a) (* b b)))))

;;; complex-angle : Complex → Number
;;; Compute the phase angle (argument) of a complex number.
;;; arg(a + bi) = atan2(b, a)
;;; Range: (-π, π]
(define (complex-angle c)
  (let ([a (complex-real c)]
        [b (complex-imag c)])
       (atan b a)))

;;; complex->polar : Complex → (Number × Number)
;;; Convert to polar form (magnitude, angle).
(define (complex->polar c)
  (list (complex-magnitude c) (complex-angle c)))

;;; polar->complex : Number × Number → Complex
;;; Convert from polar form.
(define (polar->complex r theta)
  (make-polar r theta))

;;; ====
;;; Basic Arithmetic Operations
;;; ====

;;; complex-add : Complex × Complex → Complex
;;; Add two complex numbers.
;;; (a + bi) + (c + di) = (a+c) + (b+d)i
(define (complex-add z1 z2)
  (make-complex (+ (complex-real z1) (complex-real z2))
                (+ (complex-imag z1) (complex-imag z2))))

;;; complex-sub : Complex × Complex → Complex
;;; Subtract two complex numbers.
;;; (a + bi) - (c + di) = (a-c) + (b-d)i
(define (complex-sub z1 z2)
  (make-complex (- (complex-real z1) (complex-real z2))
                (- (complex-imag z1) (complex-imag z2))))

;;; complex-mul : Complex × Complex → Complex
;;; Multiply two complex numbers.
;;; (a + bi)(c + di) = (ac - bd) + (ad + bc)i
(define (complex-mul z1 z2)
  (let ([a (complex-real z1)]
        [b (complex-imag z1)]
        [c (complex-real z2)]
        [d (complex-imag z2)])
       (make-complex (- (* a c) (* b d))
                     (+ (* a d) (* b c)))))

;;; complex-div : Complex × Complex → Complex
;;; Divide two complex numbers.
;;; (a + bi) / (c + di) = [(ac + bd) + (bc - ad)i] / (c² + d²)
(define (complex-div z1 z2)
  (let ([a (complex-real z1)]
        [b (complex-imag z1)]
        [c (complex-real z2)]
        [d (complex-imag z2)])
       (let ([denom (+ (* c c) (* d d))])
            (if (= denom 0)
                (error 'complex-div "division by zero" z2)
                (make-complex (/ (+ (* a c) (* b d)) denom)
                              (/ (- (* b c) (* a d)) denom))))))

;;; complex-negate : Complex → Complex
;;; Negate a complex number.
;;; -(a + bi) = -a - bi
(define (complex-negate z)
  (make-complex (- (complex-real z))
                (- (complex-imag z))))

;;; complex-conjugate : Complex → Complex
;;; Complex conjugate.
;;; conj(a + bi) = a - bi
(define (complex-conjugate z)
  (make-complex (complex-real z)
                (- (complex-imag z))))

;;; complex-reciprocal : Complex → Complex
;;; Reciprocal of a complex number.
;;; 1 / (a + bi) = conj(a + bi) / |a + bi|²
(define (complex-reciprocal z)
  (complex-div (make-complex 1 0) z))

;;; ====
;;; Comparison and Equality
;;; ====

;;; complex-equal? : Complex × Complex → Boolean
;;; Test if two complex numbers are equal.
(define (complex-equal? z1 z2)
  (and (= (complex-real z1) (complex-real z2))
       (= (complex-imag z1) (complex-imag z2))))

;;; complex-zero? : Complex → Boolean
;;; Test if a complex number is zero.
(define (complex-zero? z)
  (and (= (complex-real z) 0)
       (= (complex-imag z) 0)))

;;; complex-real? : Complex → Boolean
;;; Test if a complex number is real (imaginary part is zero).
(define (complex-real? z)
  (= (complex-imag z) 0))

;;; complex-imaginary? : Complex → Boolean
;;; Test if a complex number is purely imaginary (real part is zero).
(define (complex-imaginary? z)
  (= (complex-real z) 0))

;;; ====
;;; Transcendental Functions
;;; ====

;;; complex-exp : Complex → Complex
;;; Complex exponential function.
;;; e^(a + bi) = e^a * (cos(b) + i*sin(b))  (Euler's formula)
(define (complex-exp z)
  (let ([a (complex-real z)]
        [b (complex-imag z)])
       (let ([exp-a (exp a)])
            (make-complex (* exp-a (cos b))
                          (* exp-a (sin b))))))

;;; complex-log : Complex → Complex
;;; Complex natural logarithm (principal branch).
;;; log(a + bi) = log|a + bi| + i*arg(a + bi)
(define (complex-log z)
  (if (complex-zero? z)
      (error 'complex-log "logarithm of zero is undefined" z)
      (make-complex (log (complex-magnitude z))
                    (complex-angle z))))

;;; complex-pow : Complex × Complex → Complex
;;; Complex exponentiation.
;;; z^w = e^(w * log(z))
;;; Note: 0^w is only defined when Re(w) > 0 and Im(w) = 0.
;;; When Im(w) != 0, the limit oscillates on the unit circle and is undefined.
(define (complex-pow z w)
  (if (complex-zero? z)
      (cond
       [(complex-zero? w)
        (error 'complex-pow "0^0 is undefined" (list z w))]
       [(< (complex-real w) 0)
        (error 'complex-pow "0^(-n) is undefined (division by zero)" (list z w))]
       [(and (= (complex-real w) 0) (not (= (complex-imag w) 0)))
        ;; 0^(bi) for b != 0 is undefined (oscillates on unit circle)
        (make-complex +nan.0 +nan.0)]
       [else (make-complex 0 0)])
      (complex-exp (complex-mul w (complex-log z)))))

;;; complex-sqrt : Complex → Complex
;;; Complex square root (principal branch).
;;; sqrt(a + bi) = sqrt((|z| + a)/2) + i*sgn(b)*sqrt((|z| - a)/2)
(define (complex-sqrt z)
  (let* ([a (complex-real z)]
         [b (complex-imag z)]
         [r (complex-magnitude z)])
        (if (= b 0)
            (if (>= a 0)
                (make-complex (sqrt a) 0)
                (make-complex 0 (sqrt (- a))))
            (let ([re (sqrt (/ (+ r a) 2))]
                  [im (* (if (>= b 0) 1 -1) (sqrt (/ (- r a) 2)))])
                 (make-complex re im)))))

;;; ====
;;; Trigonometric Functions
;;; ====

;;; complex-sin : Complex → Complex
;;; Complex sine.
;;; sin(a + bi) = sin(a)*cosh(b) + i*cos(a)*sinh(b)
(define (complex-sin z)
  (let ([a (complex-real z)]
        [b (complex-imag z)])
       (make-complex (* (sin a) (cosh-num b))
                     (* (cos a) (sinh-num b)))))

;;; complex-cos : Complex → Complex
;;; Complex cosine.
;;; cos(a + bi) = cos(a)*cosh(b) - i*sin(a)*sinh(b)
(define (complex-cos z)
  (let ([a (complex-real z)]
        [b (complex-imag z)])
       (make-complex (* (cos a) (cosh-num b))
                     (- (* (sin a) (sinh-num b))))))

;;; complex-tan : Complex → Complex
;;; Complex tangent.
;;; tan(z) = sin(z) / cos(z)
(define (complex-tan z)
  (complex-div (complex-sin z) (complex-cos z)))

;;; ====
;;; Hyperbolic Functions
;;; ====

;;; complex-sinh : Complex → Complex
;;; Complex hyperbolic sine.
;;; sinh(a + bi) = sinh(a)*cos(b) + i*cosh(a)*sin(b)
(define (complex-sinh z)
  (let ([a (complex-real z)]
        [b (complex-imag z)])
       (make-complex (* (sinh-num a) (cos b))
                     (* (cosh-num a) (sin b)))))

;;; complex-cosh : Complex → Complex
;;; Complex hyperbolic cosine.
;;; cosh(a + bi) = cosh(a)*cos(b) + i*sinh(a)*sin(b)
(define (complex-cosh z)
  (let ([a (complex-real z)]
        [b (complex-imag z)])
       (make-complex (* (cosh-num a) (cos b))
                     (* (sinh-num a) (sin b)))))

;;; complex-tanh : Complex → Complex
;;; Complex hyperbolic tangent.
;;; tanh(z) = sinh(z) / cosh(z)
(define (complex-tanh z)
  (complex-div (complex-sinh z) (complex-cosh z)))

;;; ====
;;; Utility Functions
;;; ====

;;; complex->string : Complex → String
;;; Convert complex number to string representation.
(define (complex->string z)
  (let ([a (complex-real z)]
        [b (complex-imag z)])
       (cond
        [(= b 0) (number->string a)]
        [(= a 0) (string-append (number->string b) "i")]
        [(< b 0) (string-append (number->string a) (number->string b) "i")]
        [else (string-append (number->string a) "+" (number->string b) "i")])))

;;; complex-scale : Number × Complex → Complex
;;; Multiply a complex number by a real scalar.
(define (complex-scale k z)
  (make-complex (* k (complex-real z))
                (* k (complex-imag z))))

;;; complex-dot : Complex × Complex → Number
;;; Complex dot product (treating as 2D vectors).
;;; Returns a real number: Re(z1) * Re(z2) + Im(z1) * Im(z2)
(define (complex-dot z1 z2)
  (+ (* (complex-real z1) (complex-real z2))
     (* (complex-imag z1) (complex-imag z2))))

;;; complex-hermitian-product : Complex × Complex → Complex
;;; Hermitian inner product: conj(z1) * z2
(define (complex-hermitian-product z1 z2)
  (complex-mul (complex-conjugate z1) z2))

;;; ====
;;; Constants
;;; ====

;;; complex-zero : → Complex
;;; The complex number 0 + 0i.
(define (complex-zero)
  (make-complex 0 0))

;;; complex-one : → Complex
;;; The complex number 1 + 0i.
(define (complex-one)
  (make-complex 1 0))

;;; complex-i : → Complex
;;; The imaginary unit i = 0 + 1i.
(define (complex-i)
  (make-complex 0 1))

;;; complex-pi : → Complex
;;; π as a complex number (π + 0i).
(define (complex-pi)
  (make-complex (pi-value) 0))

;;; complex-e : → Complex
;;; Euler's number e as a complex number (e + 0i).
(define (complex-e)
  (make-complex (e-value) 0))
