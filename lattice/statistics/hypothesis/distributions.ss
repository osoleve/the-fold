;;; lattice/statistics/hypothesis/distributions.ss — Test Statistic Distributions
;;;
;;; CDF and quantile functions for t, chi-squared, and F distributions.
;;; Uses special-functions.ss for the underlying incomplete beta/gamma.
;;;
;;; This is Lattice code: pure, total, assumes reasonable input.
;;;
;;; Provides:
;;;   - t-cdf, t-quantile, t-pdf
;;;   - chi-squared-cdf, chi-squared-quantile, chi-squared-pdf
;;;   - f-cdf, f-quantile, f-pdf
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/numeric/special-functions.ss
;;;   - fp/numeric/transcendental.ss

(load "core/base/prelude.ss")
(load "lattice/fp/numeric/transcendental.ss")
(load "lattice/fp/numeric/special-functions.ss")

;;; ====
;;; Student's t-Distribution
;;; ====

;;; The t-distribution CDF is related to the regularized incomplete beta:
;;; F_t(x|df) = 1 - 0.5 * I_{df/(df+x^2)}(df/2, 0.5)  for x >= 0
;;;           = 0.5 * I_{df/(df+x^2)}(df/2, 0.5)      for x < 0

;;; t-cdf : Num × Num → Num
;;; CDF of Student's t-distribution with df degrees of freedom.
(define (t-cdf x df)
  (if (<= df 0)
      (error 't-cdf "df must be positive" df)
      (let* ([x2 (* x x)]
             [u (/ df (+ df x2))])
            (if (>= x 0)
                (- 1 (* 0.5 (betainc (/ df 2) 0.5 u)))
                (* 0.5 (betainc (/ df 2) 0.5 u))))))

;;; t-quantile : Num × Num → Num
;;; Quantile function (inverse CDF) of t-distribution.
;;; Uses Newton-Raphson iteration.
(define (t-quantile p df)
  (cond
   [(<= p 0) (error 't-quantile "p must be in (0, 1)" p)]
   [(>= p 1) (error 't-quantile "p must be in (0, 1)" p)]
   [(<= df 0) (error 't-quantile "df must be positive" df)]
   [else
    ;; Use normal approximation as starting point for large df
    (let* ([z0 (standard-normal-quantile p)]
           [x0 (if (> df 30) z0 (* z0 (sqrt (/ df (- df 2)))))])
          (newton-raphson-quantile
           (lambda (x) (t-cdf x df))
           (lambda (x) (t-pdf x df))
           p x0 50 1e-10))]))

;;; t-pdf : Num × Num → Num
;;; PDF of Student's t-distribution.
(define (t-pdf x df)
  (if (<= df 0)
      (error 't-pdf "df must be positive" df)
      (let* ([c (/ (gamma (/ (+ df 1) 2))
                   (* (sqrt (* df (pi-value))) (gamma (/ df 2))))]
             [base (+ 1 (/ (* x x) df))])
            (* c (expt base (/ (- (+ df 1)) 2))))))

;;; standard-normal-quantile : Num → Num
;;; Quantile function for N(0,1) using rational approximation.
(define (standard-normal-quantile p)
  (if (or (<= p 0) (>= p 1))
      (error 'standard-normal-quantile "p must be in (0, 1)" p)
      (let* ([a0 2.515517]
             [a1 0.802853]
             [a2 0.010328]
             [b1 1.432788]
             [b2 0.189269]
             [b3 0.001308]
             [sign (if (< p 0.5) -1 1)]
             [p* (if (< p 0.5) p (- 1 p))]
             [t (sqrt (* -2 (log p*)))]
             [num (+ a0 (* a1 t) (* a2 t t))]
             [den (+ 1 (* b1 t) (* b2 t t) (* b3 t t t))])
            (* sign (- t (/ num den))))))

;;; ====
;;; Chi-Squared Distribution
;;; ====

;;; The chi-squared distribution is a special case of gamma:
;;; chi-squared(df) = gamma(df/2, 2)
;;; CDF: F(x|df) = gammainc-reg(df/2, x/2)

;;; chi-squared-cdf : Num × Num → Num
;;; CDF of chi-squared distribution with df degrees of freedom.
(define (chi-squared-cdf x df)
  (cond
   [(<= df 0) (error 'chi-squared-cdf "df must be positive" df)]
   [(< x 0) 0]
   [(= x 0) 0]
   [else (gammainc-reg (/ df 2) (/ x 2))]))

;;; chi-squared-quantile : Num × Num → Num
;;; Quantile function of chi-squared distribution.
;;; Uses Newton-Raphson iteration.
(define (chi-squared-quantile p df)
  (cond
   [(<= p 0) (error 'chi-squared-quantile "p must be in (0, 1)" p)]
   [(>= p 1) (error 'chi-squared-quantile "p must be in (0, 1)" p)]
   [(<= df 0) (error 'chi-squared-quantile "df must be positive" df)]
   [else
    ;; Initial guess using Wilson-Hilferty approximation
    (let* ([z (standard-normal-quantile p)]
           [h (/ 2 (* 9 df))]
           [x0 (* df (expt (+ 1 (- h) (* z (sqrt h))) 3))]
           [x0 (max x0 0.001)])  ; Ensure positive
          (newton-raphson-quantile
           (lambda (x) (chi-squared-cdf x df))
           (lambda (x) (chi-squared-pdf x df))
           p x0 50 1e-10))]))

;;; chi-squared-pdf : Num × Num → Num
;;; PDF of chi-squared distribution.
(define (chi-squared-pdf x df)
  (cond
   [(<= df 0) (error 'chi-squared-pdf "df must be positive" df)]
   [(< x 0) 0]
   [(= x 0) (if (< df 2) +inf.0 (if (= df 2) 0.5 0))]
   [else
    (let* ([k2 (/ df 2)]
           [c (/ 1 (* (expt 2 k2) (gamma k2)))])
          (* c (expt x (- k2 1)) (exp (/ (- x) 2))))]))

;;; ====
;;; F-Distribution
;;; ====

;;; The F-distribution CDF is related to the regularized incomplete beta:
;;; F_F(x|df1,df2) = I_{df1*x/(df1*x+df2)}(df1/2, df2/2)

;;; f-cdf : Num × Num × Num → Num
;;; CDF of F-distribution with df1, df2 degrees of freedom.
(define (f-cdf x df1 df2)
  (cond
   [(<= df1 0) (error 'f-cdf "df1 must be positive" df1)]
   [(<= df2 0) (error 'f-cdf "df2 must be positive" df2)]
   [(< x 0) 0]
   [(= x 0) 0]
   [else
    (let ([u (/ (* df1 x) (+ (* df1 x) df2))])
         (betainc (/ df1 2) (/ df2 2) u))]))

;;; f-quantile : Num × Num × Num → Num
;;; Quantile function of F-distribution.
;;; Uses Newton-Raphson iteration.
(define (f-quantile p df1 df2)
  (cond
   [(<= p 0) (error 'f-quantile "p must be in (0, 1)" p)]
   [(>= p 1) (error 'f-quantile "p must be in (0, 1)" p)]
   [(<= df1 0) (error 'f-quantile "df1 must be positive" df1)]
   [(<= df2 0) (error 'f-quantile "df2 must be positive" df2)]
   [else
    ;; Initial guess: use chi-squared ratio approximation
    (let* ([x0 (/ (chi-squared-quantile p df1) df1
                  (/ (chi-squared-quantile 0.5 df2) df2))]
           [x0 (max x0 0.001)])
          (newton-raphson-quantile
           (lambda (x) (f-cdf x df1 df2))
           (lambda (x) (f-pdf x df1 df2))
           p x0 50 1e-10))]))

;;; f-pdf : Num × Num × Num → Num
;;; PDF of F-distribution.
(define (f-pdf x df1 df2)
  (cond
   [(<= df1 0) (error 'f-pdf "df1 must be positive" df1)]
   [(<= df2 0) (error 'f-pdf "df2 must be positive" df2)]
   [(< x 0) 0]
   [(= x 0) (if (< df1 2) +inf.0 (if (= df1 2) 1 0))]
   [else
    (let* ([a (/ df1 2)]
           [b (/ df2 2)]
           [c (/ (expt (/ df1 df2) a)
                 (beta a b))]
           [num (expt x (- a 1))]
           [den (expt (+ 1 (* (/ df1 df2) x)) (+ a b))])
          (* c (/ num den)))]))

;;; ====
;;; Newton-Raphson Quantile Inversion
;;; ====

;;; newton-raphson-quantile : (Num → Num) × (Num → Num) × Num × Num × Nat × Num → Num
;;; Find x such that cdf(x) = p using Newton-Raphson.
;;; cdf: cumulative distribution function
;;; pdf: probability density function (derivative of cdf)
;;; p: target probability
;;; x0: initial guess
;;; max-iter: maximum iterations
;;; tol: convergence tolerance
(define (newton-raphson-quantile cdf pdf p x0 max-iter tol)
  (let loop ([x x0] [iter 0])
       (if (>= iter max-iter)
           x  ; Return best estimate
           (let* ([fx (- (cdf x) p)]
                  [fpx (pdf x)])
                 (if (< (abs fpx) 1e-15)
                     x  ; Derivative too small, return current
                     (let* ([dx (/ fx fpx)]
                            [x-new (- x dx)])
                           ;; Ensure x stays positive for chi-sq and F
                           (let ([x-new (max x-new 1e-10)])
                                (if (< (abs dx) tol)
                                    x-new
                                    (loop x-new (+ iter 1))))))))))

;;; ====
;;; Two-Tailed P-Values
;;; ====

;;; t-pvalue-two-tailed : Num × Num → Num
;;; Two-tailed p-value for t-statistic.
(define (t-pvalue-two-tailed t-stat df)
  (* 2 (- 1 (t-cdf (abs t-stat) df))))

;;; chi-squared-pvalue : Num × Num → Num
;;; Upper-tail p-value for chi-squared statistic.
(define (chi-squared-pvalue chi-sq df)
  (- 1 (chi-squared-cdf chi-sq df)))

;;; f-pvalue : Num × Num × Num → Num
;;; Upper-tail p-value for F-statistic.
(define (f-pvalue f-stat df1 df2)
  (- 1 (f-cdf f-stat df1 df2)))
