(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module ma
;;; @requires prelude result-types summary-stats acf-pacf iteration stat/distributions
(require 'prelude)
(require 'result-types)
(require 'summary-stats)
(require 'acf-pacf)
(require 'iteration)
(require 'stat/distributions)

(doc 'module 'ma)
(doc 'description "Moving Average Models — MA(q) model fitting and forecasting")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'description "ma-fit: Fit MA(q) model via innovations algorithm")
(doc 'description "ma-predict: One-step ahead prediction")
(doc 'description "ma-forecast: Multi-step forecasting")
(doc 'description "moving-average: Simple moving average smoother")

(doc 'section 'simple-moving-average-smoother)
(define (moving-average xs window)
  (doc 'export #t)
  (doc 'type '(-> Vec Nat Vec))
  (doc 'description "Simple moving average smoother")
  (doc 'note "Returns smoothed series of length n - window + 1")
  (let* ([n (vector-length xs)]
         [m (- n window -1)]
         [result (make-vector m)])
        (if (< n window)
            result
            (begin
             ;; Compute first window
             (let ([first-sum (let loop ([i 0] [s 0])
                                   (if (= i window)
                                       s
                                       (loop (+ i 1) (+ s (vector-ref xs i)))))])
                  (vector-set! result 0 (/ first-sum window))
                  ;; Rolling update for efficiency
                  (let loop ([i 1] [sum first-sum])
                       (when (< i m)
                             (let ([new-sum (- (+ sum (vector-ref xs (+ i window -1)))
                                               (vector-ref xs (- i 1)))])
                                  (vector-set! result i (/ new-sum window))
                                  (loop (+ i 1) new-sum))))
                  result)))))

;;; weighted-moving-average : Vec × Vec → Vec
;;; Weighted moving average using custom weights.
;;; Weight vector determines the window size.
(define (weighted-moving-average xs weights)
  (doc 'export #t)
  (let* ([n (vector-length xs)]
         [k (vector-length weights)]
         [m (- n k -1)]
         ;; Normalize weights
         [weight-sum (let loop ([i 0] [s 0])
                          (if (= i k)
                              s
                              (loop (+ i 1) (+ s (vector-ref weights i)))))]
         [norm-weights (vec-tabulate k i
                        (/ (vector-ref weights i)
                           (max weight-sum 1e-10)))])
        (vec-tabulate m t
          (let loop ([j 0] [s 0])
               (if (= j k)
                   s
                   (loop (+ j 1)
                         (+ s (* (vector-ref norm-weights j)
                                 (vector-ref xs (+ t j))))))))))

;;; exponential-moving-average : Vec × Num → Vec
;;; Exponential moving average (same length as input).
;;; Alpha in (0, 1): higher = more weight on recent.
(define (exponential-moving-average xs alpha)
  (doc 'export #t)
  (let ([n (vector-length xs)])
       (vec-scan n (vector-ref xs 0) i prev
         (+ (* alpha (vector-ref xs i))
            (* (- 1 alpha) prev)))))

;;; ====
;;; MA(q) Model Fitting
;;; ====

;;; MA(q) model: X_t = mu + epsilon_t + theta_1*epsilon_{t-1} + ... + theta_q*epsilon_{t-q}
;;; For centered series: X_t = epsilon_t + theta_1*epsilon_{t-1} + ... + theta_q*epsilon_{t-q}

;;; ma-fit : Vec × Nat → MAResult | Error
;;; Fit MA(q) model using the innovations algorithm.
;;; The innovations algorithm recursively estimates MA coefficients from ACF.
(define (ma-fit xs q)
  (doc 'export #t)
  (let* ([n (vector-length xs)]
         [mean (vec-mean xs)])
        (if (< n (+ q 3))
            (list 'error 'insufficient-data n q)
            (let* (;; Center the series
                   [centered (center-series-ma xs mean)]
                   ;; Compute ACF
                   [acf-vals (acf centered q)]
                   ;; Compute autocovariances (gamma)
                   [gamma0 (autocovariance-ma centered 0)]
                   [gammas (vec-tabulate (+ q 1) k (autocovariance-ma centered k))]
                   ;; Run innovations algorithm
                   [result (innovations-algorithm gammas q)])
                  (if (and (pair? result) (eq? (car result) 'error))
                      result
                      (let* ([theta result]
                             ;; Compute residuals
                             [residuals (ma-compute-residuals centered theta q)]
                             ;; Compute residual variance
                             [sigma (ma-residual-sigma residuals q n)])
                            (make-ma-result theta mean residuals sigma q)))))))

;;; center-series-ma : Vec × Num → Vec
(define (center-series-ma xs mean)
  (vec-tabulate (vector-length xs) i (- (vector-ref xs i) mean)))

;;; autocovariance-ma : Vec × Nat → Num
(define (autocovariance-ma xs k)
  (let* ([n (vector-length xs)]
         [mean (/ (let loop ([i 0] [s 0])
                       (if (= i n)
                           s
                           (loop (+ i 1) (+ s (vector-ref xs i)))))
                  n)]
         [sum (let loop ([i 0] [s 0])
                   (if (>= i (- n k))
                       s
                       (loop (+ i 1)
                             (+ s (* (- (vector-ref xs i) mean)
                                     (- (vector-ref xs (+ i k)) mean))))))])
        (/ sum n)))

;;; innovations-algorithm : Vec × Nat → Vec | Error
;;; Innovations algorithm for MA parameter estimation.
;;; Given autocovariances gamma_0, ..., gamma_q, estimate theta_1, ..., theta_q.
(define (innovations-algorithm gammas q)
  (let* ([theta (make-vector q 0)]
         [v (make-vector (+ q 1) 0)])
        ;; v_0 = gamma_0
        (vector-set! v 0 (vector-ref gammas 0))
        ;; Iterate
        (do ([n 1 (+ n 1)])
            [(> n q)]
            ;; Compute theta_{n,j} for j = 1, ..., n
            (let ([theta-n (make-vector n 0)])
                 ;; theta_{n,n-1} first (j = n-1, corresponds to lag n)
                 ;; For MA(q): theta_{n,n-j} = (gamma_j - sum_{k=0}^{j-1} theta_{j,j-k} theta_{n,n-k} v_k) / v_j
                 (do ([j 0 (+ j 1)])
                     [(= j n)]
                     (let* ([k-max j]
                            [sum (let loop ([k 0] [s 0])
                                      (if (= k k-max)
                                          s
                                          (let* ([theta-j-k (if (= j 0) 0
                                                                (if (< (- j k 1) 0) 0
                                                                    (vector-ref theta-n (- j k 1))))]
                                                 [theta-n-k (if (< (- n k 1) 0) 0
                                                                (vector-ref theta-n (- n k 1)))]
                                                 [vk (vector-ref v k)])
                                                (loop (+ k 1)
                                                      (+ s (* theta-j-k theta-n-k vk))))))]
                            [gamma-nj (if (< (- n j) (vector-length gammas))
                                          (vector-ref gammas (- n j))
                                          0)]
                            [vj (vector-ref v j)]
                            [theta-val (if (< (abs vj) 1e-10)
                                           0
                                           (/ (- gamma-nj sum) vj))])
                           (when (< (- n j 1) n)
                                 (vector-set! theta-n (- n j 1) theta-val))))
                 ;; Update v_n
                 (let* ([sum (let loop ([j 0] [s 0])
                                  (if (= j n)
                                      s
                                      (let ([theta-nj (vector-ref theta-n j)]
                                            [vj (vector-ref v j)])
                                           (loop (+ j 1)
                                                 (+ s (* theta-nj theta-nj vj))))))]
                        [vn (- (vector-ref gammas 0) sum)])
                       (vector-set! v n (max vn 1e-10)))
                 ;; Store final theta values
                 (when (= n q)
                       (do ([j 0 (+ j 1)])
                           [(= j q)]
                           (vector-set! theta j (vector-ref theta-n j))))))
        theta))

;;; ma-compute-residuals : Vec × Vec × Nat → Vec
;;; Compute residuals from fitted MA model.
(define (ma-compute-residuals centered theta q)
  (let* ([n (vector-length centered)]
         [residuals (make-vector n 0)])
        ;; Use recursive formula: epsilon_t = x_t - sum_{j=1}^{min(t,q)} theta_j * epsilon_{t-j}
        (do ([t 0 (+ t 1)])
            [(= t n) residuals]
            (let* ([xt (vector-ref centered t)]
                   [sum (let loop ([j 1] [s 0])
                             (if (or (> j q) (> j t))
                                 s
                                 (loop (+ j 1)
                                       (+ s (* (vector-ref theta (- j 1))
                                               (vector-ref residuals (- t j)))))))]
                   [et (- xt sum)])
                  (vector-set! residuals t et)))))

;;; ma-residual-sigma : Vec × Nat × Nat → Num
(define (ma-residual-sigma residuals q n)
  (let* ([sse (let loop ([t q] [s 0])
                   (if (= t n)
                       s
                       (loop (+ t 1)
                             (+ s (expt (vector-ref residuals t) 2)))))])
        (sqrt (/ sse (max (- n q) 1)))))

;;; make-ma-result : Vec × Num × Vec × Num × Nat → MAResult
(define (make-ma-result theta mean residuals sigma q)
  (list 'ma-result theta mean residuals sigma q))

;;; ma-result? : Any → Bool
(define (ma-result? x)
  (and (pair? x) (eq? (car x) 'ma-result)))

;;; Accessors
(define (ma-coefficients model) (cadr model))
(define (ma-intercept model) (caddr model))
(define (ma-residuals model) (cadddr model))
(define (ma-sigma model) (list-ref model 4))
(define (ma-order model) (list-ref model 5))

;;; ====
;;; MA Forecasting
;;; ====

;;; ma-forecast : MAResult × Vec × Nat → ForecastResult
;;; Multi-step ahead forecasting for MA(q).
;;; Note: MA(q) forecasts revert to mean after q steps.
(define (ma-forecast model xs h)
  (doc 'export #t)
  (let* ([theta (ma-coefficients model)]
         [mean (ma-intercept model)]
         [sigma (ma-sigma model)]
         [q (ma-order model)]
         [n (vector-length xs)]
         ;; Get recent residuals
         [centered (center-series-ma xs mean)]
         [residuals (ma-compute-residuals centered theta q)]
         ;; Extend residuals for forecasting (future residuals = 0)
         [extended-res (vec-tabulate (+ n h) i
                         (if (< i n) (vector-ref residuals i) 0))]
         ;; Generate forecasts
         [forecasts (vec-tabulate h t
                      (+ mean
                         (let loop ([j 1] [s 0])
                              (if (or (> j q) (> j (+ n t)))
                                  s
                                  (let ([res-idx (- (+ n t) j)])
                                       (if (< res-idx 0)
                                           s
                                           (loop (+ j 1)
                                                 (+ s (* (vector-ref theta (- j 1))
                                                         (vector-ref extended-res res-idx))))))))))])
        ;; Compute forecast standard errors
        (let* ([forecast-se (ma-forecast-se theta sigma q h)]
               [z (standard-normal-quantile 0.975)]
               [lower (make-vector h)]
               [upper (make-vector h)])
              (do ([t 0 (+ t 1)])
                  [(= t h)]
                  (let ([fc (vector-ref forecasts t)]
                        [se (vector-ref forecast-se t)])
                       (vector-set! lower t (- fc (* z se)))
                       (vector-set! upper t (+ fc (* z se)))))
              (make-forecast-result forecasts lower upper 0.95))))

;;; ma-forecast-se : Vec × Num × Nat × Nat → Vec
;;; Compute forecast standard errors for MA(q).
(define (ma-forecast-se theta sigma q h)
  ;; For MA(q), forecast error variance at horizon h:
  ;; Var(e_{n+h}) = sigma^2 * (1 + theta_1^2 + ... + theta_{min(h-1,q)}^2)
  ;; But beyond q steps, it's just sigma^2 * (1 + sum of all theta^2)
  (vec-tabulate h t
    (let* ([max-j (min t q)]
           [sum (let loop ([j 0] [s 0])
                     (if (>= j max-j)
                         s
                         (loop (+ j 1)
                               (+ s (expt (vector-ref theta j) 2)))))])
          (* sigma (sqrt (+ 1 sum))))))

;;; make-forecast-result : Vec × Vec × Vec × Num → ForecastResult
(define (make-forecast-result forecasts lower upper confidence)
  (list 'forecast-result forecasts lower upper confidence))

;;; standard-normal-quantile : provided by (require 'distributions)

;;; ====
;;; ARMA (Combined AR and MA)
;;; ====

;;; Note: Full ARMA requires iterative estimation.
;;; This is a simplified implementation using conditional least squares.

;;; arma-fit-simple : Vec × Nat × Nat → ARMAResult | Error
;;; Fit ARMA(p,q) using a two-stage approach:
;;; 1. Fit AR(p+q) to get preliminary residuals
;;; 2. Estimate MA(q) parameters from residuals
(define (arma-fit-simple xs p q)
  (doc 'export #t)
  (let* ([n (vector-length xs)])
        (if (< n (+ p q 5))
            (list 'error 'insufficient-data n p q)
            (let* ([mean (vec-mean xs)]
                   [centered (center-series-ma xs mean)]
                   ;; Get ACF for AR estimation
                   [acf-vals (acf centered (+ p q))]
                   ;; Build Toeplitz matrix for AR(p)
                   [R (make-toeplitz-ar acf-vals p)]
                   [r (make-r-vector acf-vals p)]
                   ;; Solve for AR coefficients
                   [phi (solve-toeplitz R r p)])
                  (if (and (pair? phi) (eq? (car phi) 'error))
                      phi
                      (let* (;; Compute AR residuals
                             [ar-residuals (compute-ar-residuals centered phi p)]
                             ;; Fit MA to residuals
                             [ma-model (ma-fit ar-residuals q)])
                            (if (and (pair? ma-model) (eq? (car ma-model) 'error))
                                ma-model
                                (let* ([theta (ma-coefficients ma-model)]
                                       [sigma (ma-sigma ma-model)])
                                      (list 'arma-result phi theta mean sigma p q)))))))))

;;; Helper functions for ARMA
(define (make-toeplitz-ar acf-vals p)
  (let ([data (make-vector (* p p))])
       (do ([i 0 (+ i 1)])
           [(= i p)]
           (do ([j 0 (+ j 1)])
               [(= j p)]
               (vector-set! data (+ (* i p) j)
                            (vector-ref acf-vals (abs (- i j))))))
       (list 'matrix p p data)))

(define (make-r-vector acf-vals p)
  (vec-tabulate p k (vector-ref acf-vals (+ k 1))))

(define (solve-toeplitz R r p)
  ;; Simple Gaussian elimination for small systems
  (let* ([A (cadddr R)]
         [b (vec-tabulate p i (vector-ref r i))]
         [x (make-vector p 0)])
        ;; Forward elimination (simplified, not production-grade)
        ;; For now, use iterative refinement for small systems
        (do ([iter 0 (+ iter 1)])
            [(= iter 100) x]
            (do ([i 0 (+ i 1)])
                [(= i p)]
                (let* ([sum (let loop ([j 0] [s 0])
                                 (if (= j p)
                                     s
                                     (if (= i j)
                                         (loop (+ j 1) s)
                                         (loop (+ j 1)
                                               (+ s (* (vector-ref A (+ (* i p) j))
                                                       (vector-ref x j)))))))]
                       [aii (vector-ref A (+ (* i p) i))]
                       [bi (vector-ref b i)]
                       [xi-new (if (< (abs aii) 1e-10)
                                   0
                                   (/ (- bi sum) aii))])
                      (vector-set! x i xi-new))))))

(define (compute-ar-residuals centered phi p)
  (vec-tabulate (vector-length centered) t
    (if (< t p)
        (vector-ref centered t)
        (let ([sum (let loop ([k 0] [s 0])
                        (if (= k p)
                            s
                            (loop (+ k 1)
                                  (+ s (* (vector-ref phi k)
                                          (vector-ref centered (- t k 1)))))))])
             (- (vector-ref centered t) sum)))))

