(load "core/base/prelude.ss")

(doc 'module 'forecast)
(doc 'description "Forecasting Utilities — Forecast accuracy metrics and utilities")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'description "mape: Mean Absolute Percentage Error")
(doc 'description "rmse: Root Mean Squared Error")
(doc 'description "mae: Mean Absolute Error")
(doc 'description "mse: Mean Squared Error")
(doc 'description "forecast-accuracy: Comprehensive accuracy report")
(doc 'description "naive-forecast: Baseline naive forecaster")
(doc 'description "seasonal-naive-forecast: Seasonal naive forecaster")
(doc 'description "forecast-intervals: Compute prediction intervals")

(doc 'section 'forecast-accuracy-metrics)
(define (mae actual forecast)
  (let* ([n (min (vector-length actual) (vector-length forecast))]
         [sum (let loop ([i 0] [s 0])
                   (if (= i n)
                       s
                       (loop (+ i 1)
                             (+ s (abs (- (vector-ref actual i)
                                          (vector-ref forecast i)))))))])
        (/ sum (max n 1))))

;;; mse : Vec × Vec → Num
;;; Mean Squared Error: (1/n) * sum((actual - forecast)^2)
(define (mse actual forecast)
  (let* ([n (min (vector-length actual) (vector-length forecast))]
         [sum (let loop ([i 0] [s 0])
                   (if (= i n)
                       s
                       (loop (+ i 1)
                             (+ s (expt (- (vector-ref actual i)
                                           (vector-ref forecast i))
                                        2)))))])
        (/ sum (max n 1))))

;;; rmse : Vec × Vec → Num
;;; Root Mean Squared Error: sqrt(MSE)
(define (rmse actual forecast)
  (sqrt (mse actual forecast)))

;;; mape : Vec × Vec → Num
;;; Mean Absolute Percentage Error: (100/n) * sum(|actual - forecast| / |actual|)
;;; Note: Excludes zero values to avoid division by zero.
(define (mape actual forecast)
  (let* ([n (min (vector-length actual) (vector-length forecast))])
        (let loop ([i 0] [s 0] [count 0])
             (if (= i n)
                 (if (= count 0) 0 (/ (* 100 s) count))
                 (let ([a (vector-ref actual i)]
                       [f (vector-ref forecast i)])
                      (if (< (abs a) 1e-10)
                          (loop (+ i 1) s count)  ; Skip zero actuals
                          (loop (+ i 1)
                                (+ s (/ (abs (- a f)) (abs a)))
                                (+ count 1))))))))

;;; smape : Vec × Vec → Num
;;; Symmetric Mean Absolute Percentage Error.
;;; SMAPE = (100/n) * sum(|actual - forecast| / ((|actual| + |forecast|) / 2))
(define (smape actual forecast)
  (let* ([n (min (vector-length actual) (vector-length forecast))])
        (let loop ([i 0] [s 0] [count 0])
             (if (= i n)
                 (if (= count 0) 0 (/ (* 100 s) count))
                 (let* ([a (vector-ref actual i)]
                        [f (vector-ref forecast i)]
                        [denom (/ (+ (abs a) (abs f)) 2)])
                       (if (< denom 1e-10)
                           (loop (+ i 1) s count)
                           (loop (+ i 1)
                                 (+ s (/ (abs (- a f)) denom))
                                 (+ count 1))))))))

;;; mase : Vec × Vec × Vec → Num
;;; Mean Absolute Scaled Error.
;;; MASE = MAE / MAE_naive where naive is one-step seasonal naive.
(define (mase actual forecast training-data period)
  (let* ([mae-forecast (mae actual forecast)]
         ;; Compute in-sample MAE of seasonal naive
         [n (vector-length training-data)]
         [naive-mae (if (<= n period)
                        1  ; Default to 1 if insufficient data
                        (let loop ([i period] [s 0] [count 0])
                             (if (= i n)
                                 (/ s (max count 1))
                                 (loop (+ i 1)
                                       (+ s (abs (- (vector-ref training-data i)
                                                    (vector-ref training-data (- i period)))))
                                       (+ count 1)))))])
        (/ mae-forecast (max naive-mae 1e-10))))

;;; ====
;;; Naive Forecasters (Baselines)
;;; ====

;;; naive-forecast : Vec × Nat → Vec
;;; Naive forecast: predict last observed value for all horizons.
(define (naive-forecast xs h)
  (let* ([n (vector-length xs)]
         [last-val (vector-ref xs (- n 1))]
         [forecasts (make-vector h)])
        (do ([i 0 (+ i 1)])
            [(= i h) forecasts]
            (vector-set! forecasts i last-val))))

;;; drift-forecast : Vec × Nat → Vec
;;; Drift forecast: extend the line from first to last observation.
;;; f_{n+h} = x_n + h * (x_n - x_1) / (n - 1)
(define (drift-forecast xs h)
  (let* ([n (vector-length xs)]
         [first-val (vector-ref xs 0)]
         [last-val (vector-ref xs (- n 1))]
         [slope (/ (- last-val first-val) (max (- n 1) 1))]
         [forecasts (make-vector h)])
        (do ([i 1 (+ i 1)])
            [(> i h) forecasts]
            (vector-set! forecasts (- i 1)
                         (+ last-val (* i slope))))))

;;; seasonal-naive-forecast : Vec × Nat × Nat → Vec
;;; Seasonal naive: forecast = value from same season last period.
(define (seasonal-naive-forecast xs period h)
  (let* ([n (vector-length xs)]
         [forecasts (make-vector h)])
        (do ([i 0 (+ i 1)])
            [(= i h) forecasts]
            (let ([idx (- n period (- period (modulo (+ i 1) period)))])
                 (vector-set! forecasts i
                              (vector-ref xs (max 0 (min idx (- n 1)))))))))

;;; mean-forecast : Vec × Nat → Vec
;;; Mean forecast: predict the mean of all observations.
(define (mean-forecast xs h)
  (let* ([n (vector-length xs)]
         [mean (/ (let loop ([i 0] [s 0])
                       (if (= i n)
                           s
                           (loop (+ i 1) (+ s (vector-ref xs i)))))
                  n)]
         [forecasts (make-vector h)])
        (do ([i 0 (+ i 1)])
            [(= i h) forecasts]
            (vector-set! forecasts i mean))))

;;; ====
;;; Forecast Intervals
;;; ====

;;; forecast-intervals : Vec × Vec × Num → ForecastIntervalResult
;;; Compute prediction intervals given forecasts and residuals.
;;; Uses residual standard deviation and normality assumption.
(define (forecast-intervals forecasts residuals confidence)
  (let* ([h (vector-length forecasts)]
         [n (vector-length residuals)]
         ;; Compute residual standard deviation
         [sigma (sqrt (/ (let loop ([i 0] [s 0])
                              (if (= i n)
                                  s
                                  (loop (+ i 1)
                                        (+ s (expt (vector-ref residuals i) 2)))))
                         (max (- n 1) 1)))]
         ;; Get z-score for confidence level
         [z (standard-normal-quantile-approx (+ 0.5 (/ confidence 2)))]
         [lower (make-vector h)]
         [upper (make-vector h)])
        ;; Intervals widen with horizon (simple approximation)
        (do ([i 0 (+ i 1)])
            [(= i h)]
            (let* ([fc (vector-ref forecasts i)]
                   ;; SE grows with sqrt of horizon for random walk
                   [se (* sigma (sqrt (+ i 1)))]
                   [margin (* z se)])
                  (vector-set! lower i (- fc margin))
                  (vector-set! upper i (+ fc margin))))
        (list 'forecast-interval-result forecasts lower upper confidence)))

;;; standard-normal-quantile-approx : Num → Num
;;; Approximate inverse normal CDF.
(define (standard-normal-quantile-approx p)
  (if (or (<= p 0) (>= p 1))
      (if (<= p 0) -1000 1000)  ; Approximation for extremes
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
;;; Forecast Evaluation
;;; ====

;;; forecast-accuracy : Vec × Vec → AccuracyResult
;;; Compute all accuracy metrics for a forecast.
(define (forecast-accuracy actual forecast)
  (list 'accuracy-result
        (cons 'mae (mae actual forecast))
        (cons 'mse (mse actual forecast))
        (cons 'rmse (rmse actual forecast))
        (cons 'mape (mape actual forecast))
        (cons 'smape (smape actual forecast))))

;;; theil-u : Vec × Vec × Vec → Num
;;; Theil's U statistic comparing forecast to naive.
;;; U < 1 means forecast is better than naive.
;;; U = 1 means forecast equals naive.
;;; U > 1 means forecast is worse than naive.
(define (theil-u actual forecast naive-forecast)
  (let* ([mse-forecast (mse actual forecast)]
         [mse-naive (mse actual naive-forecast)])
        (sqrt (/ mse-forecast (max mse-naive 1e-10)))))

;;; tracking-signal : Vec × Vec → Num
;;; Tracking signal for forecast bias detection.
;;; TS = Cumulative Forecast Error / MAD
;;; |TS| > 4 suggests systematic bias.
(define (tracking-signal actual forecast)
  (let* ([n (min (vector-length actual) (vector-length forecast))]
         [errors (make-vector n)]
         [_ (do ([i 0 (+ i 1)])
                [(= i n)]
                (vector-set! errors i
                             (- (vector-ref actual i)
                                (vector-ref forecast i))))]
         [cfe (let loop ([i 0] [s 0])
                   (if (= i n)
                       s
                       (loop (+ i 1) (+ s (vector-ref errors i)))))]
         [mad (/ (let loop ([i 0] [s 0])
                      (if (= i n)
                          s
                          (loop (+ i 1) (+ s (abs (vector-ref errors i))))))
                 (max n 1))])
        (/ cfe (max mad 1e-10))))

;;; ====
;;; Cross-Validation for Time Series
;;; ====

;;; time-series-cv : Vec × Nat × Nat × (Vec → Model) × (Model × Nat → Vec) → Vec
;;; Rolling window cross-validation for time series.
;;; initial: initial training window size
;;; horizon: forecast horizon
;;; fit-fn: function to fit model
;;; forecast-fn: function to generate forecasts
;;; Returns vector of RMSE values for each fold.
(define (time-series-cv xs initial horizon fit-fn forecast-fn)
  (let* ([n (vector-length xs)]
         [num-folds (max 1 (- n initial horizon))]
         [rmses (make-vector num-folds)])
        (do ([i 0 (+ i 1)])
            [(= i num-folds) rmses]
            (let* ([train-end (+ initial i)]
                   [test-end (min (+ train-end horizon) n)]
                   [test-len (- test-end train-end)]
                   ;; Extract training data
                   [train (let ([v (make-vector train-end)])
                               (do ([j 0 (+ j 1)])
                                   [(= j train-end) v]
                                   (vector-set! v j (vector-ref xs j))))]
                   ;; Fit model and generate forecast
                   [model (fit-fn train)]
                   [forecast (forecast-fn model test-len)]
                   ;; Extract test data
                   [test (let ([v (make-vector test-len)])
                              (do ([j 0 (+ j 1)])
                                  [(= j test-len) v]
                                  (vector-set! v j (vector-ref xs (+ train-end j)))))]
                   ;; Compute RMSE
                   [fold-rmse (rmse test forecast)])
                  (vector-set! rmses i fold-rmse)))))

;;; ====
;;; Combination Forecasts
;;; ====

;;; combine-forecasts-mean : (List Vec) → Vec
;;; Simple average of multiple forecasts.
(define (combine-forecasts-mean forecasts)
  (if (null? forecasts)
      (vector)
      (let* ([h (vector-length (car forecasts))]
             [k (length forecasts)]
             [combined (make-vector h 0)])
            (for-each
             (lambda (fc)
                     (do ([i 0 (+ i 1)])
                         [(= i h)]
                         (vector-set! combined i
                                      (+ (vector-ref combined i)
                                         (/ (vector-ref fc i) k)))))
             forecasts)
            combined)))

;;; combine-forecasts-weighted : (List Vec) × Vec → Vec
;;; Weighted combination of forecasts.
(define (combine-forecasts-weighted forecasts weights)
  (if (null? forecasts)
      (vector)
      (let* ([h (vector-length (car forecasts))]
             [combined (make-vector h 0)])
            (let loop ([fcs forecasts] [wts 0])
                 (if (null? fcs)
                     combined
                     (let ([fc (car fcs)]
                           [w (vector-ref weights wts)])
                          (do ([i 0 (+ i 1)])
                              [(= i h)]
                              (vector-set! combined i
                                           (+ (vector-ref combined i)
                                              (* w (vector-ref fc i)))))
                          (loop (cdr fcs) (+ wts 1))))))))

