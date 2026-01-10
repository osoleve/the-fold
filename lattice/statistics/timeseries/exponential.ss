;;; lattice/statistics/timeseries/exponential.ss — Exponential Smoothing
;;;
;;; Simple, double, and triple (Holt-Winters) exponential smoothing.
;;;
;;; This is Lattice code: pure, total, assumes reasonable input.
;;;
;;; Provides:
;;;   - simple-exponential-smooth: SES (alpha parameter)
;;;   - holt-smooth: Double exponential (trend)
;;;   - holt-winters: Triple exponential (trend + seasonality)
;;;   - optimize-ses-alpha: Find optimal alpha via grid search
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "core/base/prelude.ss")

;;; ============================================================
;;; Simple Exponential Smoothing (SES)
;;; ============================================================

;;; simple-exponential-smooth : Vec × Num → SESResult
;;; Single exponential smoothing: s_t = alpha * x_t + (1-alpha) * s_{t-1}
;;; Alpha in (0, 1): higher alpha = more weight on recent observations.
(define (simple-exponential-smooth xs alpha)
  (let* ([n (vector-length xs)]
         [smoothed (make-vector n)]
         ;; Initialize with first observation
         [s0 (vector-ref xs 0)])
        (vector-set! smoothed 0 s0)
        (do ([t 1 (+ t 1)])
            [(= t n)]
            (let ([prev (vector-ref smoothed (- t 1))]
                  [curr (vector-ref xs t)])
                 (vector-set! smoothed t
                              (+ (* alpha curr)
                                 (* (- 1 alpha) prev)))))
        (list 'ses-result alpha smoothed (vector-ref smoothed (- n 1)))))

;;; ses-forecast : SESResult × Nat → Vec
;;; Forecast h steps ahead using SES (all forecasts are the last smoothed value).
(define (ses-forecast result h)
  (let* ([last-level (cadddr result)]
         [forecasts (make-vector h)])
        (do ([i 0 (+ i 1)])
            [(= i h) forecasts]
            (vector-set! forecasts i last-level))))

;;; optimize-ses-alpha : Vec → Num
;;; Find optimal alpha minimizing SSE via grid search.
(define (optimize-ses-alpha xs)
  (let* ([n (vector-length xs)]
         [best-alpha 0.1]
         [best-sse +inf.0])
        (do ([alpha 0.01 (+ alpha 0.01)])
            [(> alpha 0.99) best-alpha]
            (let* ([result (simple-exponential-smooth xs alpha)]
                   [smoothed (caddr result)]
                   [sse (let loop ([t 1] [s 0])
                             (if (= t n)
                                 s
                                 (loop (+ t 1)
                                       (+ s (expt (- (vector-ref xs t)
                                                     (vector-ref smoothed (- t 1)))
                                                  2)))))])
                  (when (< sse best-sse)
                        (set! best-alpha alpha)
                        (set! best-sse sse))))))

;;; ============================================================
;;; Holt's Double Exponential Smoothing
;;; ============================================================

;;; holt-smooth : Vec × Num × Num → HoltResult
;;; Double exponential smoothing for data with trend.
;;; alpha: level smoothing (0, 1)
;;; beta: trend smoothing (0, 1)
;;; Level: l_t = alpha * x_t + (1-alpha) * (l_{t-1} + b_{t-1})
;;; Trend: b_t = beta * (l_t - l_{t-1}) + (1-beta) * b_{t-1}
(define (holt-smooth xs alpha beta)
  (let* ([n (vector-length xs)]
         [level (make-vector n)]
         [trend (make-vector n)]
         ;; Initialize level with first observation
         [l0 (vector-ref xs 0)]
         ;; Initialize trend as difference of first two points (if available)
         [b0 (if (> n 1)
                 (- (vector-ref xs 1) (vector-ref xs 0))
                 0)])
        (vector-set! level 0 l0)
        (vector-set! trend 0 b0)
        (do ([t 1 (+ t 1)])
            [(= t n)]
            (let* ([lt-1 (vector-ref level (- t 1))]
                   [bt-1 (vector-ref trend (- t 1))]
                   [xt (vector-ref xs t)]
                   ;; Update level
                   [lt (+ (* alpha xt)
                          (* (- 1 alpha) (+ lt-1 bt-1)))]
                   ;; Update trend
                   [bt (+ (* beta (- lt lt-1))
                          (* (- 1 beta) bt-1))])
                  (vector-set! level t lt)
                  (vector-set! trend t bt)))
        (list 'holt-result alpha beta level trend
              (vector-ref level (- n 1))
              (vector-ref trend (- n 1)))))

;;; holt-forecast : HoltResult × Nat → Vec
;;; Forecast h steps ahead using Holt's method.
;;; Forecast: f_{t+h} = l_t + h * b_t
(define (holt-forecast result h)
  (let* ([last-level (list-ref result 4)]
         [last-trend (list-ref result 5)]
         [forecasts (make-vector h)])
        (do ([i 1 (+ i 1)])
            [(> i h) forecasts]
            (vector-set! forecasts (- i 1)
                         (+ last-level (* i last-trend))))))

;;; holt-fitted : HoltResult → Vec
;;; Get fitted values from Holt model.
(define (holt-fitted result)
  (let* ([level (list-ref result 2)]
         [trend (list-ref result 3)]
         [n (vector-length level)]
         [fitted (make-vector n)])
        (vector-set! fitted 0 (vector-ref level 0))
        (do ([t 1 (+ t 1)])
            [(= t n) fitted]
            (vector-set! fitted t
                         (+ (vector-ref level (- t 1))
                            (vector-ref trend (- t 1)))))))

;;; ============================================================
;;; Holt-Winters Triple Exponential Smoothing
;;; ============================================================

;;; holt-winters : Vec × Num × Num × Num × Nat × Symbol → HWResult
;;; Triple exponential smoothing with seasonality.
;;; alpha: level smoothing
;;; beta: trend smoothing
;;; gamma: seasonal smoothing
;;; period: seasonal period (e.g., 12 for monthly, 4 for quarterly)
;;; type: 'additive or 'multiplicative
(define (holt-winters xs alpha beta gamma period type)
  (if (eq? type 'multiplicative)
      (holt-winters-multiplicative xs alpha beta gamma period)
      (holt-winters-additive xs alpha beta gamma period)))

;;; holt-winters-additive : Vec × Num × Num × Num × Nat → HWResult
;;; Additive Holt-Winters: x_t = l_t + b_t + s_t + noise
(define (holt-winters-additive xs alpha beta gamma period)
  (let* ([n (vector-length xs)]
         [level (make-vector n)]
         [trend (make-vector n)]
         [seasonal (make-vector n)])
        (if (< n (* 2 period))
            (list 'error 'insufficient-data n period)
            (let* (;; Initialize: average first period for level
                   [l0 (/ (let loop ([i 0] [s 0])
                               (if (= i period)
                                   s
                                   (loop (+ i 1) (+ s (vector-ref xs i)))))
                          period)]
                   ;; Initialize trend from first two periods
                   [b0 (/ (- (/ (let loop ([i period] [s 0])
                                     (if (= i (* 2 period))
                                         s
                                         (loop (+ i 1) (+ s (vector-ref xs i)))))
                                period)
                             (/ (let loop ([i 0] [s 0])
                                     (if (= i period)
                                         s
                                         (loop (+ i 1) (+ s (vector-ref xs i)))))
                                period))
                          period)]
                   ;; Initialize seasonal from first period
                   [_ (do ([i 0 (+ i 1)])
                          [(= i period)]
                          (vector-set! seasonal i (- (vector-ref xs i) l0)))])
                  ;; Set initial values for the first period
                  (vector-set! level 0 l0)
                  (vector-set! trend 0 b0)
                  ;; Main loop
                  (do ([t 1 (+ t 1)])
                      [(= t n)]
                      (let* ([xt (vector-ref xs t)]
                             [lt-1 (vector-ref level (- t 1))]
                             [bt-1 (vector-ref trend (- t 1))]
                             [st-p (if (>= (- t period) 0)
                                       (vector-ref seasonal (- t period))
                                       (vector-ref seasonal (modulo t period)))]
                             ;; Update level
                             [lt (+ (* alpha (- xt st-p))
                                    (* (- 1 alpha) (+ lt-1 bt-1)))]
                             ;; Update trend
                             [bt (+ (* beta (- lt lt-1))
                                    (* (- 1 beta) bt-1))]
                             ;; Update seasonal
                             [st (+ (* gamma (- xt lt))
                                    (* (- 1 gamma) st-p))])
                            (vector-set! level t lt)
                            (vector-set! trend t bt)
                            (vector-set! seasonal t st)))
                  (list 'hw-result 'additive alpha beta gamma period
                        level trend seasonal
                        (vector-ref level (- n 1))
                        (vector-ref trend (- n 1)))))))

;;; holt-winters-multiplicative : Vec × Num × Num × Num × Nat → HWResult
;;; Multiplicative Holt-Winters: x_t = (l_t + b_t) * s_t + noise
(define (holt-winters-multiplicative xs alpha beta gamma period)
  (let* ([n (vector-length xs)]
         [level (make-vector n)]
         [trend (make-vector n)]
         [seasonal (make-vector n)])
        (if (< n (* 2 period))
            (list 'error 'insufficient-data n period)
            (let* (;; Initialize: average first period for level
                   [l0 (/ (let loop ([i 0] [s 0])
                               (if (= i period)
                                   s
                                   (loop (+ i 1) (+ s (vector-ref xs i)))))
                          period)]
                   ;; Initialize trend from first two periods
                   [b0 (/ (- (/ (let loop ([i period] [s 0])
                                     (if (= i (* 2 period))
                                         s
                                         (loop (+ i 1) (+ s (vector-ref xs i)))))
                                period)
                             l0)
                          period)]
                   ;; Initialize seasonal from first period (ratios)
                   [_ (do ([i 0 (+ i 1)])
                          [(= i period)]
                          (vector-set! seasonal i
                                       (/ (vector-ref xs i) (max l0 1e-10))))])
                  ;; Set initial values
                  (vector-set! level 0 l0)
                  (vector-set! trend 0 b0)
                  ;; Main loop
                  (do ([t 1 (+ t 1)])
                      [(= t n)]
                      (let* ([xt (vector-ref xs t)]
                             [lt-1 (vector-ref level (- t 1))]
                             [bt-1 (vector-ref trend (- t 1))]
                             [st-p (if (>= (- t period) 0)
                                       (vector-ref seasonal (- t period))
                                       (vector-ref seasonal (modulo t period)))]
                             ;; Update level
                             [lt (+ (* alpha (/ xt (max st-p 1e-10)))
                                    (* (- 1 alpha) (+ lt-1 bt-1)))]
                             ;; Update trend
                             [bt (+ (* beta (- lt lt-1))
                                    (* (- 1 beta) bt-1))]
                             ;; Update seasonal
                             [st (+ (* gamma (/ xt (max lt 1e-10)))
                                    (* (- 1 gamma) st-p))])
                            (vector-set! level t lt)
                            (vector-set! trend t bt)
                            (vector-set! seasonal t st)))
                  (list 'hw-result 'multiplicative alpha beta gamma period
                        level trend seasonal
                        (vector-ref level (- n 1))
                        (vector-ref trend (- n 1)))))))

;;; hw-forecast : HWResult × Nat → Vec
;;; Forecast h steps ahead using Holt-Winters.
(define (hw-forecast result h)
  (let* ([type (cadr result)]
         [period (list-ref result 4)]
         [seasonal (list-ref result 7)]
         [last-level (list-ref result 8)]
         [last-trend (list-ref result 9)]
         [n (vector-length seasonal)]
         [forecasts (make-vector h)])
        (do ([i 1 (+ i 1)])
            [(> i h) forecasts]
            (let* ([trend-component (* i last-trend)]
                   ;; Get seasonal component from the last cycle
                   [seasonal-idx (- n period (- period (modulo i period)))]
                   [seasonal-component (vector-ref seasonal
                                                   (max 0 (min seasonal-idx (- n 1))))])
                  (if (eq? type 'additive)
                      (vector-set! forecasts (- i 1)
                                   (+ last-level trend-component seasonal-component))
                      (vector-set! forecasts (- i 1)
                                   (* (+ last-level trend-component) seasonal-component)))))))

;;; hw-fitted : HWResult → Vec
;;; Get fitted values from Holt-Winters model.
(define (hw-fitted result)
  (let* ([type (cadr result)]
         [period (list-ref result 4)]
         [level (list-ref result 5)]
         [trend (list-ref result 6)]
         [seasonal (list-ref result 7)]
         [n (vector-length level)]
         [fitted (make-vector n)])
        ;; First period uses initial values
        (do ([t 0 (+ t 1)])
            [(= t n) fitted]
            (if (= t 0)
                (if (eq? type 'additive)
                    (vector-set! fitted t
                                 (+ (vector-ref level 0) (vector-ref seasonal 0)))
                    (vector-set! fitted t
                                 (* (vector-ref level 0) (vector-ref seasonal 0))))
                (let* ([lt-1 (vector-ref level (- t 1))]
                       [bt-1 (vector-ref trend (- t 1))]
                       [st-p (if (>= (- t period) 0)
                                 (vector-ref seasonal (- t period))
                                 (vector-ref seasonal t))])
                      (if (eq? type 'additive)
                          (vector-set! fitted t (+ lt-1 bt-1 st-p))
                          (vector-set! fitted t (* (+ lt-1 bt-1) st-p))))))))

;;; ============================================================
;;; Damped Trend Holt's Method
;;; ============================================================

;;; holt-damped : Vec × Num × Num × Num → HoltDampedResult
;;; Holt's method with damped trend for more conservative forecasts.
;;; phi: damping parameter (0, 1), typically 0.8-0.98
(define (holt-damped xs alpha beta phi)
  (let* ([n (vector-length xs)]
         [level (make-vector n)]
         [trend (make-vector n)]
         [l0 (vector-ref xs 0)]
         [b0 (if (> n 1)
                 (- (vector-ref xs 1) (vector-ref xs 0))
                 0)])
        (vector-set! level 0 l0)
        (vector-set! trend 0 b0)
        (do ([t 1 (+ t 1)])
            [(= t n)]
            (let* ([lt-1 (vector-ref level (- t 1))]
                   [bt-1 (vector-ref trend (- t 1))]
                   [xt (vector-ref xs t)]
                   ;; Damped trend update
                   [lt (+ (* alpha xt)
                          (* (- 1 alpha) (+ lt-1 (* phi bt-1))))]
                   [bt (+ (* beta (- lt lt-1))
                          (* (- 1 beta) (* phi bt-1)))])
                  (vector-set! level t lt)
                  (vector-set! trend t bt)))
        (list 'holt-damped-result alpha beta phi level trend
              (vector-ref level (- n 1))
              (vector-ref trend (- n 1)))))

;;; holt-damped-forecast : HoltDampedResult × Nat → Vec
;;; Forecast with damped trend.
;;; f_{t+h} = l_t + (phi + phi^2 + ... + phi^h) * b_t
(define (holt-damped-forecast result h)
  (let* ([phi (list-ref result 2)]
         [last-level (list-ref result 5)]
         [last-trend (list-ref result 6)]
         [forecasts (make-vector h)])
        (do ([i 1 (+ i 1)])
            [(> i h) forecasts]
            ;; Sum of geometric series: phi + phi^2 + ... + phi^i = phi * (1 - phi^i) / (1 - phi)
            (let ([phi-sum (if (< (abs (- phi 1)) 1e-10)
                               (exact->inexact i)
                               (* phi (/ (- 1 (expt phi i)) (- 1 phi))))])
                 (vector-set! forecasts (- i 1)
                              (+ last-level (* phi-sum last-trend)))))))

