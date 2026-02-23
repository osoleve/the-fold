(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module ar
;;; @requires prelude iteration matrix matrix-decomp matrix-solvers result-types summary-stats acf-pacf
(require 'prelude)
(require 'iteration)
(require 'matrix)
(require 'matrix-decomp)
(require 'matrix-solvers)
(require 'result-types)
(require 'summary-stats)
(require 'acf-pacf)

(doc 'module 'ar)
(doc 'description "Autoregressive Models — AR(p) model fitting and forecasting")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'description "ar-fit: Fit AR(p) model using Yule-Walker equations")
(doc 'description "ar-predict: One-step ahead prediction")
(doc 'description "ar-forecast: Multi-step forecasting")
(doc 'description "ar-aic: Model selection via AIC")

(doc 'section 'ar-model-fitting)

(doc 'note "AR(p) model: X_t = c + phi_1*X_{t-1} + ... + phi_p*X_{t-p} + epsilon_t")
(doc 'note "For centered series (mean 0): X_t = phi_1*X_{t-1} + ... + phi_p*X_{t-p} + epsilon_t")
(doc 'note "The Yule-Walker equations relate autocorrelations to AR coefficients: R * phi = r")
(doc 'note "where R is the Toeplitz matrix of autocorrelations and r = [rho_1, ..., rho_p]'")
(doc 'note "Uses Levinson-Durbin algorithm for O(p²) performance instead of O(p³)")
(define (ar-fit xs p)
  (doc 'export #t)
  (doc 'type '(-> Vec Nat (or ARResult Error)))
  (doc 'description "Fit AR(p) model using Yule-Walker equations")
  (let* ([n (vector-length xs)]
         [mean (vec-mean xs)])
        (if (< n (+ p 2))
            (list 'error 'insufficient-data n p)
            (let* (;; Center the series
                   [centered (center-series xs mean)]
                   ;; Compute ACF
                   [acf-vals (acf centered p)]
                   ;; Solve Yule-Walker equations using Levinson-Durbin O(p²)
                   [phi (levinson-durbin acf-vals)])
                  (if (and (pair? phi) (eq? (car phi) 'error))
                      phi
                      ;; Compute residuals and diagnostics
                      (ar-compute-diagnostics xs centered phi mean p n))))))

;;; center-series : Vec × Num → Vec
(define (center-series xs mean)
  (let ([n (vector-length xs)])
       (vec-tabulate n i (- (vector-ref xs i) mean))))

;;; make-toeplitz-matrix : Vec × Nat → Matrix
;;; Create symmetric Toeplitz matrix from ACF.
;;; R[i,j] = rho_{|i-j|}
(define (make-toeplitz-matrix acf-vals p)
  (let* ([data (make-vector (* p p))])
        (do ([i 0 (+ i 1)])
            [(= i p)]
            (do ([j 0 (+ j 1)])
                [(= j p)]
                (vector-set! data (+ (* i p) j)
                             (vector-ref acf-vals (abs (- i j))))))
        (list 'matrix p p data)))

;;; ar-compute-diagnostics : Vec × Vec × Vec × Num × Nat × Nat → ARResult
(define (ar-compute-diagnostics xs centered phi mean p n)
  (let* (;; Compute fitted values and residuals
         [fitted (make-vector n 0)]
         [residuals (make-vector n 0)]
         ;; Fill initial values with actual data
         [_ (do ([t 0 (+ t 1)])
                [(= t p)]
                (vector-set! fitted t (vector-ref xs t))
                (vector-set! residuals t 0))]
         ;; Compute predictions for t >= p
         [_ (do ([t p (+ t 1)])
                [(= t n)]
                (let ([pred (+ mean (ar-predict-one centered phi t p))])
                     (vector-set! fitted t pred)
                     (vector-set! residuals t (- (vector-ref xs t) pred))))]
         ;; Compute residual variance
         [sse (let loop ([t p] [s 0])
                   (if (= t n)
                       s
                       (loop (+ t 1)
                             (+ s (expt (vector-ref residuals t) 2)))))]
         [sigma (sqrt (/ sse (max (- n p) 1)))]
         ;; Compute AIC
         [aic (ar-aic-value xs phi sigma p n)])
        (make-ar-result phi mean residuals fitted sigma aic p)))

;;; ar-predict-one : Vec × Vec × Nat × Nat → Num
;;; Predict X_t from X_{t-1}, ..., X_{t-p} (centered).
(define (ar-predict-one centered phi t p)
  (let loop ([k 0] [s 0])
       (if (= k p)
           s
           (loop (+ k 1)
                 (+ s (* (vector-ref phi k)
                         (vector-ref centered (- t k 1))))))))

;;; ====
;;; Forecasting
;;; ====

;;; ar-forecast : ARResult × Vec × Nat → ForecastResult
;;; Multi-step ahead forecasting.
;;; Returns point forecasts and confidence intervals.
(define (ar-forecast model xs h)
  (doc 'export #t)
  (let* ([phi (ar-coefficients model)]
         [mean (ar-intercept model)]
         [sigma (ar-sigma model)]
         [p (ar-order model)]
         [n (vector-length xs)]
         ;; Center the series
         [centered (center-series xs mean)]
         ;; Extend series for forecasting
         [extended (make-vector (+ n h) 0)]
         [_ (do ([i 0 (+ i 1)])
                [(= i n)]
                (vector-set! extended i (vector-ref centered i)))]
         ;; Generate forecasts
         [forecasts (make-vector h)]
         [_ (do ([t 0 (+ t 1)])
                [(= t h)]
                (let ([pred (let loop ([k 0] [s 0])
                                 (if (= k p)
                                     s
                                     (loop (+ k 1)
                                           (+ s (* (vector-ref phi k)
                                                   (vector-ref extended (- (+ n t) k 1)))))))])
                     (vector-set! extended (+ n t) pred)
                     (vector-set! forecasts t (+ mean pred))))]
         ;; Compute forecast variances (simplified)
         [forecast-se (ar-forecast-se phi sigma h)]
         ;; Confidence intervals (95%)
         [z (standard-normal-quantile 0.975)]
         [lower (make-vector h)]
         [upper (make-vector h)])
        (do ([t 0 (+ t 1)])
            [(= t h)]
            (let ([fc (vector-ref forecasts t)]
                  [se (vector-ref forecast-se t)])
                 (vector-set! lower t (- fc (* z se)))
                 (vector-set! upper t (+ fc (* z se)))))
        (make-forecast-result forecasts lower upper 0.95)))

;;; ar-forecast-se : Vec × Num × Nat → Vec
;;; Compute forecast standard errors for AR(p).
;;; Uses psi-weights (impulse response function).
(define (ar-forecast-se phi sigma h)
  (let* ([p (vector-length phi)]
         [psi (make-vector h 0)])
        ;; Compute psi weights recursively
        ;; psi_0 = 1, psi_j = sum_{k=1}^{min(j,p)} phi_k * psi_{j-k}
        (vector-set! psi 0 1)
        (do ([j 1 (+ j 1)])
            [(= j h)]
            (let ([sum (let loop ([k 1] [s 0])
                            (if (or (> k p) (> k j))
                                s
                                (loop (+ k 1)
                                      (+ s (* (vector-ref phi (- k 1))
                                              (vector-ref psi (- j k)))))))])
                 (vector-set! psi j sum)))
        ;; Forecast variance at horizon h: sigma^2 * sum_{j=0}^{h-1} psi_j^2
        (vec-tabulate h t
          (sqrt (* sigma sigma
                   (range-fold s 0 j 0 (+ t 1)
                     (+ s (expt (vector-ref psi j) 2))))))))

;;; ====
;;; Model Selection
;;; ====

;;; ar-aic-value : Vec × Vec × Num × Nat × Nat → Num
;;; Compute AIC for AR(p) model.
;;; AIC = n * log(sigma^2) + 2*(p + 1)
(define (ar-aic-value xs phi sigma p n)
  (+ (* n (log (* sigma sigma)))
     (* 2 (+ p 1))))

;;; ar-select-order : Vec × Nat → Nat
;;; Select optimal AR order using AIC.
;;; Fits AR(1), AR(2), ..., AR(max-p) and returns best order.
(define (ar-select-order xs max-p)
  (doc 'export #t)
  (let* ([n (vector-length xs)]
         [max-p (min max-p (quotient n 3))]  ; Limit by data size
         [aics (make-vector (+ max-p 1))])
        ;; Compute AIC for each order
        (do ([p 1 (+ p 1)])
            [(> p max-p)]
            (let ([model (ar-fit xs p)])
                 (if (and (pair? model) (eq? (car model) 'error))
                     (vector-set! aics p +inf.0)
                     (vector-set! aics p (ar-aic model)))))
        ;; Find minimum AIC
        (let loop ([p 1] [best-p 1] [best-aic (vector-ref aics 1)])
             (if (> p max-p)
                 best-p
                 (let ([aic (vector-ref aics p)])
                      (if (< aic best-aic)
                          (loop (+ p 1) p aic)
                          (loop (+ p 1) best-p best-aic)))))))

;;; ar-bic : ARResult → Num
;;; Bayesian Information Criterion.
;;; BIC = n * log(sigma^2) + log(n) * (p + 1)
(define (ar-bic model)
  (doc 'export #t)
  (let* ([n (vector-length (ar-fitted model))]
         [p (ar-order model)]
         [sigma (ar-sigma model)])
        (+ (* n (log (* sigma sigma)))
           (* (log n) (+ p 1)))))

;;; ====
;;; Standard Normal Quantile (helper)
;;; ====

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
