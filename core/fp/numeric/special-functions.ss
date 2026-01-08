;;; core/fp/numeric/special-functions.ss — Special Mathematical Functions
;;;
;;; Standard special functions for scientific computing.
;;; Provides error functions, gamma functions, beta functions, and more.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Usage:
;;;   (erf 1.0)          => 0.8427... (error function)
;;;   (erfc 1.0)         => 0.1573... (complementary error function)
;;;   (gamma 5)          => 24 (gamma(5) = 4!)
;;;   (lgamma 100)       => 359.13... (log-gamma for large values)
;;;   (beta 2 3)         => 0.0833... (beta function)
;;;   (digamma 1)        => -0.5772... (Euler-Mascheroni constant)
;;;
;;; Dependencies:
;;;   - transcendental.ss (for exp, log, sqrt, pi-value)

;;; ============================================================
;;; Constants
;;; ============================================================

;;; Euler-Mascheroni constant γ ≈ 0.5772156649...
(define *euler-gamma* 0.5772156649015329)

;;; sqrt(2π) for Stirling approximation
(define *sqrt-2pi* 2.5066282746310002)

;;; sqrt(π)
(define *sqrt-pi* 1.7724538509055159)

;;; ============================================================
;;; Error Functions
;;; ============================================================

;;; erf : Number → Number
;;; Error function: erf(x) = (2/√π) ∫₀ˣ e^(-t²) dt
;;; Uses Horner's method with Abramowitz & Stegun 7.1.26 approximation.
(define (erf x)
  (let* ((sign (if (< x 0) -1 1))
         (x (abs x))
         (p 0.3275911)
         (t (/ 1.0 (+ 1.0 (* p x))))
         (a1 0.254829592)
         (a2 -0.284496736)
         (a3 1.421413741)
         (a4 -1.453152027)
         (a5 1.061405429)
         (y (* t (+ a1 (* t (+ a2 (* t (+ a3 (* t (+ a4 (* t a5))))))))))
         (result (- 1.0 (* y (exp (- (* x x)))))))
        (* sign result)))

;;; erfc : Number → Number
;;; Complementary error function: erfc(x) = 1 - erf(x)
;;; More accurate than computing 1 - erf(x) for large x.
(define (erfc x)
  (if (< x 0)
      (- 2 (erfc (- x)))
      (let* ((p 0.3275911)
             (t (/ 1.0 (+ 1.0 (* p x))))
             (a1 0.254829592)
             (a2 -0.284496736)
             (a3 1.421413741)
             (a4 -1.453152027)
             (a5 1.061405429)
             (y (* t (+ a1 (* t (+ a2 (* t (+ a3 (* t (+ a4 (* t a5)))))))))))
            (* y (exp (- (* x x)))))))

;;; erfinv : Number → Number
;;; Inverse error function: erfinv(erf(x)) = x
;;; Uses rational approximation (Winitzki 2008).
;;; Domain: (-1, 1)
(define (erfinv x)
  (cond
   ((<= x -1) (error 'erfinv "domain error: x must be in (-1, 1)" x))
   ((>= x 1) (error 'erfinv "domain error: x must be in (-1, 1)" x))
   ((= x 0) 0)
   (else
    (let* ((sign (if (< x 0) -1 1))
           (x (abs x))
           (a 0.147)
           (ln-term (log (- 1 (* x x))))
           (term1 (/ 2 (* 3.14159265358979 a)))
           (term2 (/ ln-term 2))
           (sqrt-inner (- (+ term1 term2)
                          (sqrt (- (* (+ term1 term2) (+ term1 term2))
                                   (/ ln-term a))))))
          (* sign (sqrt (- sqrt-inner)))))))

;;; erfcinv : Number → Number
;;; Inverse complementary error function: erfcinv(erfc(x)) = x
;;; Domain: (0, 2)
(define (erfcinv x)
  (cond
   ((<= x 0) (error 'erfcinv "domain error: x must be in (0, 2)" x))
   ((>= x 2) (error 'erfcinv "domain error: x must be in (0, 2)" x))
   (else (erfinv (- 1 x)))))

;;; ============================================================
;;; Gamma Functions
;;; ============================================================

;;; *lanczos-g* and *lanczos-coeffs*
;;; Lanczos approximation coefficients for g=7
(define *lanczos-g* 7)
(define *lanczos-coeffs*
  '#(0.99999999999980993
     676.5203681218851
     -1259.1392167224028
     771.32342877765313
     -176.61502916214059
     12.507343278686905
     -0.13857109526572012
     9.9843695780195716e-6
     1.5056327351493116e-7))

;;; lanczos-sum : Number → Number
;;; Compute Lanczos series sum.
(define (lanczos-sum x)
  (let loop ((i 1) (ag (vector-ref *lanczos-coeffs* 0)))
       (if (>= i (vector-length *lanczos-coeffs*))
           ag
           (loop (+ i 1)
                 (+ ag (/ (vector-ref *lanczos-coeffs* i)
                          (+ x i)))))))

;;; gamma : Number → Number
;;; Gamma function: Γ(n) = (n-1)! for positive integers.
;;; Uses Lanczos approximation for general values.
(define (gamma x)
  (cond
   ;; Handle poles at non-positive integers
   ((and (<= x 0) (= x (truncate x)))
    (error 'gamma "pole at non-positive integer" x))
   ;; Use reflection formula for negative values
   ((< x 0.5)
    (let ((pi 3.141592653589793))
         (/ pi (* (sin (* pi x)) (gamma (- 1 x))))))
   ;; Lanczos approximation for x >= 0.5
   (else
    (let* ((x (- x 1))
           (ag (lanczos-sum x))
           (t (+ x *lanczos-g* 0.5)))
          (* *sqrt-2pi*
             (expt t (+ x 0.5))
             (exp (- t))
             ag)))))

;;; lgamma : Number → Number
;;; Log-gamma function: lgamma(x) = log(|Γ(x)|)
;;; More numerically stable than log(gamma(x)) for large x.
(define (lgamma x)
  (cond
   ((and (<= x 0) (= x (truncate x)))
    (error 'lgamma "pole at non-positive integer" x))
   ((< x 0.5)
    (let ((pi 3.141592653589793))
         (- (log (/ pi (abs (sin (* pi x)))))
            (lgamma (- 1 x)))))
   (else
    (let* ((x (- x 1))
           (ag (lanczos-sum x))
           (t (+ x *lanczos-g* 0.5)))
          (+ (* 0.5 (log (* 2 3.141592653589793)))
             (* (+ x 0.5) (log t))
             (- t)
             (log ag))))))

;;; factorial : Nat → Nat
;;; Compute n! = n × (n-1) × ... × 1
(define (factorial n)
  (cond
   ((< n 0) (error 'factorial "undefined for negative integers" n))
   ((= n (truncate n))
    (inexact->exact (round (gamma (+ n 1)))))
   (else (gamma (+ n 1)))))

;;; ============================================================
;;; Digamma and Polygamma Functions
;;; ============================================================

;;; digamma : Number → Number
;;; Digamma function: ψ(x) = d/dx log(Γ(x)) = Γ'(x)/Γ(x)
;;; Uses asymptotic expansion with more terms for precision.
(define (digamma x)
  (cond
   ((and (<= x 0) (= x (truncate x)))
    (error 'digamma "pole at non-positive integer" x))
   ((< x 0)
    (let ((pi 3.141592653589793))
         (- (digamma (- 1 x))
            (/ pi (tan (* pi x))))))
   ;; Use recurrence to shift to larger x for better asymptotic accuracy
   ((< x 8)
    (- (digamma (+ x 1)) (/ 1 x)))
   (else
    ;; Asymptotic expansion with Bernoulli numbers B₂ₙ/(2n·x^(2n))
    (let* ((x2 (* x x))
           (x4 (* x2 x2))
           (x6 (* x4 x2))
           (x8 (* x6 x2))
           (x10 (* x8 x2))
           ;; Bernoulli number coefficients: B₂/(2), B₄/(4), B₆/(6), ...
           (b2 (/ 1 12))      ; B₂/(2·1) = 1/6 / 2
           (b4 (/ -1 120))    ; B₄/(4·1) = -1/30 / 4
           (b6 (/ 1 252))     ; B₆/(6·1) = 1/42 / 6
           (b8 (/ -1 240))    ; B₈/(8·1)
           (b10 (/ 1 132)))   ; B₁₀/(10·1)
          (- (log x)
             (/ 1 (* 2 x))
             (/ b2 x2)
             (/ b4 x4)
             (/ b6 x6)
             (/ b8 x8)
             (/ b10 x10))))))

;;; trigamma : Number → Number
;;; Trigamma function: ψ₁(x) = d²/dx² log(Γ(x))
(define (trigamma x)
  (cond
   ((and (<= x 0) (= x (truncate x)))
    (error 'trigamma "pole at non-positive integer" x))
   ((< x 0)
    (let ((pi 3.141592653589793))
         (+ (trigamma (- 1 x))
            (/ (* pi pi) (* (sin (* pi x)) (sin (* pi x)))))))
   ((< x 6)
    (+ (trigamma (+ x 1)) (/ 1 (* x x))))
   (else
    (let* ((x2 (* x x))
           (x3 (* x2 x))
           (x5 (* x3 x2))
           (x7 (* x5 x2)))
          (+ (/ 1 x)
             (/ 1 (* 2 x2))
             (/ 1 (* 6 x3))
             (/ -1 (* 30 x5))
             (/ 1 (* 42 x7)))))))

;;; ============================================================
;;; Beta Function
;;; ============================================================

;;; beta : Number × Number → Number
;;; Beta function: B(a,b) = Γ(a)Γ(b)/Γ(a+b)
(define (beta a b)
  (if (or (<= a 0) (<= b 0))
      (error 'beta "requires positive arguments" (list a b))
      (exp (- (+ (lgamma a) (lgamma b))
              (lgamma (+ a b))))))

;;; lbeta : Number × Number → Number
;;; Log-beta function: log(B(a,b))
(define (lbeta a b)
  (if (or (<= a 0) (<= b 0))
      (error 'lbeta "requires positive arguments" (list a b))
      (- (+ (lgamma a) (lgamma b))
         (lgamma (+ a b)))))

;;; ============================================================
;;; Incomplete Gamma Functions
;;; ============================================================

;;; gammainc-lower : Number × Number → Number
;;; Lower incomplete gamma: γ(a,x) = ∫₀ˣ t^(a-1) e^(-t) dt
(define (gammainc-lower a x)
  (cond
   ((<= a 0) (error 'gammainc-lower "requires a > 0" a))
   ((< x 0) (error 'gammainc-lower "requires x >= 0" x))
   ((= x 0) 0)
   (else (gammainc-lower-series a x 200 1e-15))))

;;; gammainc-lower-series : series expansion helper
(define (gammainc-lower-series a x max-iter eps)
  (let loop ((n 0) (term 1.0) (sum 1.0))
       (if (or (>= n max-iter) (< (abs term) (* eps (abs sum))))
           (* (exp (- (* a (log x)) x)) (/ sum a))
           (let ((new-term (* term (/ x (+ a n 1)))))
                (loop (+ n 1) new-term (+ sum new-term))))))

;;; gammainc-upper : Number × Number → Number
;;; Upper incomplete gamma: Γ(a,x) = ∫ₓ^∞ t^(a-1) e^(-t) dt
(define (gammainc-upper a x)
  (- (gamma a) (gammainc-lower a x)))

;;; gammainc-reg : Number × Number → Number
;;; Regularized incomplete gamma: P(a,x) = γ(a,x) / Γ(a)
(define (gammainc-reg a x)
  (/ (gammainc-lower a x) (gamma a)))

;;; ============================================================
;;; Incomplete Beta Function
;;; ============================================================

;;; betainc : Number × Number × Number → Number
;;; Regularized incomplete beta: I_x(a,b) = B(x;a,b) / B(a,b)
(define (betainc a b x)
  (cond
   ((or (<= a 0) (<= b 0))
    (error 'betainc "requires a > 0 and b > 0" (list a b)))
   ((or (< x 0) (> x 1))
    (error 'betainc "requires x in [0, 1]" x))
   ((= x 0) 0)
   ((= x 1) 1)
   ((> x (/ (+ a 1) (+ a b 2)))
    (- 1 (betainc b a (- 1 x))))
   (else
    (betainc-cf a b x))))

;;; betainc-cf : continued fraction for betainc
(define (betainc-cf a b x)
  (let* ((qab (+ a b))
         (qap (+ a 1))
         (qam (- a 1))
         (d0 (- 1 (/ (* qab x) qap))))
        (let ((d0 (if (< (abs d0) 1e-30) 1e-30 d0)))
             (let ((d0 (/ 1 d0)))
                  (betainc-cf-loop a b x qab qap qam 1 d0 1.0 d0)))))

;;; betainc-cf-loop : iteration for continued fraction
(define (betainc-cf-loop a b x qab qap qam m h c d)
  (if (>= m 200)
      (betainc-cf-result a b x h)
      (let* ((m2 (* 2 m))
             (aa-even (/ (* m (- b m) x)
                         (* (+ qam m2) (+ a m2))))
             (d1 (+ 1 (* aa-even d)))
             (d1 (if (< (abs d1) 1e-30) 1e-30 d1))
             (c1 (+ 1 (/ aa-even c)))
             (c1 (if (< (abs c1) 1e-30) 1e-30 c1))
             (d1 (/ 1 d1))
             (h1 (* h d1 c1))
             (aa-odd (/ (* (- (+ a m)) (+ qab m) x)
                        (* (+ a m2) (+ qap m2))))
             (d2 (+ 1 (* aa-odd d1)))
             (d2 (if (< (abs d2) 1e-30) 1e-30 d2))
             (c2 (+ 1 (/ aa-odd c1)))
             (c2 (if (< (abs c2) 1e-30) 1e-30 c2))
             (d2 (/ 1 d2))
             (del (* d2 c2))
             (h2 (* h1 del)))
            (if (< (abs (- del 1)) 1e-10)
                (betainc-cf-result a b x h2)
                (betainc-cf-loop a b x qab qap qam (+ m 1) h2 c2 d2)))))

;;; betainc-cf-result : compute final result
(define (betainc-cf-result a b x h)
  (* h (exp (+ (* a (log x))
               (* b (log (- 1 x)))
               (- (lbeta a b))))))

;;; ============================================================
;;; Binomial Coefficient
;;; ============================================================

;;; binomial : Nat × Nat → Nat
;;; Binomial coefficient: C(n,k) = n! / (k! (n-k)!)
(define (binomial n k)
  (cond
   ((< k 0) 0)
   ((> k n) 0)
   ((or (= k 0) (= k n)) 1)
   (else
    (inexact->exact
     (round (exp (- (lgamma (+ n 1))
                    (lgamma (+ k 1))
                    (lgamma (+ (- n k) 1)))))))))

;;; ============================================================
;;; Pochhammer Symbol (Rising Factorial)
;;; ============================================================

;;; pochhammer : Number × Nat → Number
;;; Pochhammer symbol (rising factorial): (x)_n = x(x+1)...(x+n-1)
(define (pochhammer x n)
  (if (= n 0)
      1
      (exp (- (lgamma (+ x n)) (lgamma x)))))

;;; ============================================================
;;; Double Factorial
;;; ============================================================

;;; double-factorial : Nat → Nat
;;; Double factorial: n!! = n(n-2)(n-4)...
(define (double-factorial n)
  (cond
   ((< n 0) (error 'double-factorial "undefined for negative integers" n))
   ((<= n 1) 1)
   (else (* n (double-factorial (- n 2))))))
