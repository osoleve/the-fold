;;; lattice/statistics/timeseries/acf-pacf.ss — Autocorrelation Functions
;;;
;;; ACF and PACF computation for time series.
;;;
;;; This is Lattice code: pure, total, assumes reasonable input.
;;;
;;; Provides:
;;;   - acf: Autocorrelation function
;;;   - pacf: Partial autocorrelation function
;;;   - acf-plot-data: Data for ACF visualization
;;;   - ljung-box-test: Portmanteau test for residual autocorrelation
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - statistics/core/summary-stats.ss
;;;   - statistics/hypothesis/distributions.ss

(load "core/base/prelude.ss")
(load "lattice/statistics/core/summary-stats.ss")
(load "lattice/statistics/hypothesis/distributions.ss")

;;; ============================================================
;;; Autocovariance and Autocorrelation
;;; ============================================================

;;; autocovariance : Vec × Nat → Num
;;; Compute autocovariance at lag k: Cov(X_t, X_{t+k})
(define (autocovariance xs k)
  (let* ([n (vector-length xs)]
         [mean (vec-mean xs)]
         [sum (let loop ([i 0] [s 0])
                   (if (>= i (- n k))
                       s
                       (loop (+ i 1)
                             (+ s (* (- (vector-ref xs i) mean)
                                     (- (vector-ref xs (+ i k)) mean))))))])
        (/ sum n)))  ; Divide by n, not n-k, for consistency

;;; acf : Vec × Nat → Vec
;;; Compute autocorrelation function at lags 0, 1, ..., max-lag.
;;; ACF(k) = Cov(X_t, X_{t+k}) / Var(X)
;;; Returns vector of length max-lag + 1.
(define (acf xs max-lag)
  (let* ([n (vector-length xs)]
         [max-lag (min max-lag (- n 1))]
         [acf-vals (make-vector (+ max-lag 1))]
         [gamma0 (autocovariance xs 0)])  ; Variance
        (do ([k 0 (+ k 1)])
            [(> k max-lag) acf-vals]
            (let ([gammak (autocovariance xs k)])
                 (vector-set! acf-vals k
                              (if (= gamma0 0) 0 (/ gammak gamma0)))))))

;;; acf-standard-error : Nat → Num
;;; Approximate standard error for ACF at lag k.
;;; SE ≈ 1/sqrt(n) for white noise.
(define (acf-standard-error n)
  (/ 1 (sqrt n)))

;;; acf-confidence-bounds : Nat × Num → (Num × Num)
;;; Confidence bounds for ACF (assuming white noise).
;;; Returns (lower, upper) for given confidence level.
(define (acf-confidence-bounds n confidence)
  (let* ([z (standard-normal-quantile (+ 0.5 (/ confidence 2)))]
         [se (acf-standard-error n)]
         [bound (* z se)])
        (cons (- bound) bound)))

;;; ============================================================
;;; Partial Autocorrelation
;;; ============================================================

;;; pacf : Vec × Nat → Vec
;;; Compute partial autocorrelation function using Durbin-Levinson algorithm.
;;; PACF(k) is the correlation between X_t and X_{t+k} after removing
;;; the linear effect of X_{t+1}, ..., X_{t+k-1}.
(define (pacf xs max-lag)
  (let* ([n (vector-length xs)]
         [max-lag (min max-lag (- n 1))]
         [acf-vals (acf xs max-lag)]
         [pacf-vals (make-vector (+ max-lag 1))])
        ;; PACF(0) = 1 by definition
        (vector-set! pacf-vals 0 1)
        ;; PACF(1) = ACF(1)
        (when (>= max-lag 1)
              (vector-set! pacf-vals 1 (vector-ref acf-vals 1)))
        ;; Use Durbin-Levinson recursion for higher lags
        (when (>= max-lag 2)
              (durbin-levinson acf-vals pacf-vals max-lag))
        pacf-vals))

;;; durbin-levinson : Vec × Vec × Nat → Void
;;; Durbin-Levinson algorithm for computing PACF.
(define (durbin-levinson acf-vals pacf-vals max-lag)
  (let ([phi (make-vector max-lag 0)])
       ;; Initialize phi_1,1 = rho_1
       (vector-set! phi 0 (vector-ref acf-vals 1))
       ;; Iterate for k = 2, ..., max-lag
       (do ([k 2 (+ k 1)])
           [(> k max-lag)]
           (let* (;; Compute numerator: rho_k - sum(phi_{k-1,j} * rho_{k-j})
                  [num (- (vector-ref acf-vals k)
                          (let loop ([j 1] [s 0])
                               (if (>= j k)
                                   s
                                   (loop (+ j 1)
                                         (+ s (* (vector-ref phi (- j 1))
                                                 (vector-ref acf-vals (- k j))))))))]
                  ;; Compute denominator: 1 - sum(phi_{k-1,j} * rho_j)
                  [den (- 1
                          (let loop ([j 1] [s 0])
                               (if (>= j k)
                                   s
                                   (loop (+ j 1)
                                         (+ s (* (vector-ref phi (- j 1))
                                                 (vector-ref acf-vals j)))))))]
                  ;; phi_{k,k} = num / den
                  [phi-kk (if (= den 0) 0 (/ num den))])
                 ;; Store PACF value
                 (vector-set! pacf-vals k phi-kk)
                 ;; Update phi coefficients for next iteration
                 (let ([phi-new (make-vector max-lag)])
                      ;; phi_{k,j} = phi_{k-1,j} - phi_{k,k} * phi_{k-1,k-j}
                      (do ([j 1 (+ j 1)])
                          [(>= j k)]
                          (vector-set! phi-new (- j 1)
                                       (- (vector-ref phi (- j 1))
                                          (* phi-kk (vector-ref phi (- k j 1))))))
                      (vector-set! phi-new (- k 1) phi-kk)
                      ;; Copy new phi values
                      (do ([j 0 (+ j 1)])
                          [(= j k)]
                          (vector-set! phi j (vector-ref phi-new j))))))))

;;; ============================================================
;;; Ljung-Box Test
;;; ============================================================

;;; ljung-box-test : Vec × Nat → TestResult
;;; Portmanteau test for autocorrelation in residuals.
;;; Tests H0: rho_1 = ... = rho_m = 0 (no autocorrelation up to lag m).
;;; Q = n(n+2) * sum_{k=1}^m (rho_k^2 / (n-k)) ~ chi-sq(m) under H0.
(define (ljung-box-test xs m)
  (let* ([n (vector-length xs)]
         [m (min m (- n 1))]
         [acf-vals (acf xs m)]
         ;; Compute Q statistic
         [Q (let loop ([k 1] [s 0])
                 (if (> k m)
                     (* n (+ n 2) s)
                     (let ([rho-k (vector-ref acf-vals k)])
                          (loop (+ k 1)
                                (+ s (/ (* rho-k rho-k)
                                        (- n k)))))))]
         [p-value (chi-squared-pvalue Q m)])
        (make-test-result 'ljung-box Q m p-value #f #f)))

;;; box-pierce-test : Vec × Nat → TestResult
;;; Simpler version of portmanteau test.
;;; Q = n * sum_{k=1}^m rho_k^2 ~ chi-sq(m)
(define (box-pierce-test xs m)
  (let* ([n (vector-length xs)]
         [m (min m (- n 1))]
         [acf-vals (acf xs m)]
         [Q (* n (let loop ([k 1] [s 0])
                      (if (> k m)
                          s
                          (let ([rho-k (vector-ref acf-vals k)])
                               (loop (+ k 1) (+ s (* rho-k rho-k)))))))]
         [p-value (chi-squared-pvalue Q m)])
        (make-test-result 'box-pierce Q m p-value #f #f)))

;;; ============================================================
;;; Cross-Correlation
;;; ============================================================

;;; ccf : Vec × Vec × Nat → Vec
;;; Cross-correlation function between xs and ys.
;;; CCF(k) = Cor(X_t, Y_{t+k})
;;; Returns vector for lags -max-lag, ..., 0, ..., max-lag.
(define (ccf xs ys max-lag)
  (let* ([n (min (vector-length xs) (vector-length ys))]
         [max-lag (min max-lag (- n 1))]
         [ccf-vals (make-vector (+ (* 2 max-lag) 1))]
         [mx (vec-mean xs)]
         [my (vec-mean ys)]
         [sx (vec-std-dev xs)]
         [sy (vec-std-dev ys)])
        ;; Compute cross-covariance at each lag
        (do ([k (- max-lag) (+ k 1)])
            [(> k max-lag) ccf-vals]
            (let* ([idx (+ k max-lag)]
                   [cov (cross-covariance xs ys mx my k n)]
                   [cor (if (or (= sx 0) (= sy 0)) 0 (/ cov (* sx sy)))])
                  (vector-set! ccf-vals idx cor)))))

;;; cross-covariance : Vec × Vec × Num × Num × Int × Nat → Num
(define (cross-covariance xs ys mx my k n)
  (let* ([start (max 0 (- k))]
         [end (min n (- n k))]
         [sum (let loop ([t start] [s 0])
                   (if (>= t end)
                       s
                       (loop (+ t 1)
                             (+ s (* (- (vector-ref xs t) mx)
                                     (- (vector-ref ys (+ t k)) my))))))])
        (/ sum n)))

;;; ============================================================
;;; Standard Normal Quantile (helper)
;;; ============================================================

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
