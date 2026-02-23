(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module exponential
;;; @requires prelude iteration
(require 'prelude)
(require 'iteration)

(doc 'module 'exponential)
(doc 'description "Exponential Smoothing — Simple, double, and triple (Holt-Winters) exponential smoothing")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'description "simple-exponential-smooth: SES (alpha parameter)")
(doc 'description "holt-smooth: Double exponential (trend)")
(doc 'description "holt-winters: Triple exponential (trend + seasonality)")
(doc 'description "optimize-ses-alpha: Find optimal alpha via grid search")

(doc 'section 'simple-exponential-smoothing-ses)
(define (simple-exponential-smooth xs alpha)
  (doc 'export #t)
  (doc 'type '(-> Vec Num SESResult))
  (doc 'description "Single exponential smoothing: s_t = alpha * x_t + (1-alpha) * s_{t-1}")
  (doc 'note "Alpha in (0, 1): higher alpha = more weight on recent observations")
  (let* ([n (vector-length xs)]
         [s0 (vector-ref xs 0)]
         [smoothed (vec-scan n s0 t prev
                    (+ (* alpha (vector-ref xs t))
                       (* (- 1 alpha) prev)))])
        (list 'ses-result alpha smoothed (vector-ref smoothed (- n 1)))))

;;; ses-forecast : SESResult × Nat → Vec
;;; Forecast h steps ahead using SES (all forecasts are the last smoothed value).
(define (ses-forecast result h)
  (doc 'export #t)
  (let ([last-level (cadddr result)])
       (vec-tabulate h i last-level)))

;;; optimize-ses-alpha : Vec → Num
;;; Find optimal alpha minimizing SSE via grid search.
(define (optimize-ses-alpha xs)
  (doc 'export #t)
  (let ([n (vector-length xs)])
       (car (range-fold best (cons 0.1 +inf.0) i 1 100
              (let* ([alpha (* i 0.01)]
                     [result (simple-exponential-smooth xs alpha)]
                     [smoothed (caddr result)]
                     [sse (range-fold s 0 t 1 n
                            (+ s (expt (- (vector-ref xs t)
                                          (vector-ref smoothed (- t 1)))
                                       2)))])
                    (if (< sse (cdr best))
                        (cons alpha sse)
                        best))))))

;;; ====
;;; Holt's Double Exponential Smoothing
;;; ====

;;; holt-smooth : Vec × Num × Num → HoltResult
;;; Double exponential smoothing for data with trend.
;;; alpha: level smoothing (0, 1)
;;; beta: trend smoothing (0, 1)
;;; Level: l_t = alpha * x_t + (1-alpha) * (l_{t-1} + b_{t-1})
;;; Trend: b_t = beta * (l_t - l_{t-1}) + (1-beta) * b_{t-1}
(define (holt-smooth xs alpha beta)
  (doc 'export #t)
  (let* ([n (vector-length xs)]
         ;; Initialize level with first observation
         [l0 (vector-ref xs 0)]
         ;; Initialize trend as difference of first two points (if available)
         [b0 (if (> n 1)
                 (- (vector-ref xs 1) (vector-ref xs 0))
                 0)]
         ;; Scan with pair accumulator (level . trend)
         [state (vec-scan n (cons l0 b0) t st
                  (let* ([lt-1 (car st)]
                         [bt-1 (cdr st)]
                         [xt (vector-ref xs t)]
                         [lt (+ (* alpha xt)
                                (* (- 1 alpha) (+ lt-1 bt-1)))]
                         [bt (+ (* beta (- lt lt-1))
                                (* (- 1 beta) bt-1))])
                    (cons lt bt)))]
         ;; Extract level and trend vectors
         [level (vec-tabulate n i (car (vector-ref state i)))]
         [trend (vec-tabulate n i (cdr (vector-ref state i)))])
        (list 'holt-result alpha beta level trend
              (vector-ref level (- n 1))
              (vector-ref trend (- n 1)))))

;;; holt-forecast : HoltResult × Nat → Vec
;;; Forecast h steps ahead using Holt's method.
;;; Forecast: f_{t+h} = l_t + h * b_t
(define (holt-forecast result h)
  (doc 'export #t)
  (let ([last-level (list-ref result 4)]
        [last-trend (list-ref result 5)])
       (vec-tabulate h i (+ last-level (* (+ i 1) last-trend)))))

;;; holt-fitted : HoltResult → Vec
;;; Get fitted values from Holt model.
(define (holt-fitted result)
  (let* ([level (list-ref result 2)]
         [trend (list-ref result 3)]
         [n (vector-length level)])
        (vec-tabulate n t
          (if (= t 0)
              (vector-ref level 0)
              (+ (vector-ref level (- t 1))
                 (vector-ref trend (- t 1)))))))

;;; ====
;;; Holt-Winters Triple Exponential Smoothing
;;; ====

;;; holt-winters : Vec × Num × Num × Num × Nat × Symbol → HWResult
;;; Triple exponential smoothing with seasonality.
;;; alpha: level smoothing
;;; beta: trend smoothing
;;; gamma: seasonal smoothing
;;; period: seasonal period (e.g., 12 for monthly, 4 for quarterly)
;;; type: 'additive or 'multiplicative
(define (holt-winters xs alpha beta gamma period type)
  (doc 'export #t)
  (if (eq? type 'multiplicative)
      (holt-winters-multiplicative xs alpha beta gamma period)
      (holt-winters-additive xs alpha beta gamma period)))

;;; holt-winters-additive : Vec × Num × Num × Num × Nat → HWResult
;;; Additive Holt-Winters: x_t = l_t + b_t + s_t + noise
;;; Note: seasonal component has period-step lag dependency, so the main
;;; loop uses range-do! with three output vectors rather than vec-scan.
(define (holt-winters-additive xs alpha beta gamma period)
  (let ([n (vector-length xs)])
       (if (< n (* 2 period))
           (list 'error 'insufficient-data n period)
           (let* (;; Initialize: average first period for level
                  [l0 (/ (range-fold s 0 i 0 period
                           (+ s (vector-ref xs i)))
                         period)]
                  ;; Average of second period
                  [avg2 (/ (range-fold s 0 i period (* 2 period)
                             (+ s (vector-ref xs i)))
                           period)]
                  ;; Initialize trend from first two periods
                  [b0 (/ (- avg2 l0) period)]
                  ;; Output vectors
                  [level (make-vector n)]
                  [trend (make-vector n)]
                  ;; Initialize seasonal from first period
                  [seasonal (vec-tabulate n i
                              (if (< i period)
                                  (- (vector-ref xs i) l0)
                                  0))])
                 (vector-set! level 0 l0)
                 (vector-set! trend 0 b0)
                 ;; Main loop: level and trend depend on t-1, seasonal on t-period
                 (range-do! t 1 n
                   (let* ([xt (vector-ref xs t)]
                          [lt-1 (vector-ref level (- t 1))]
                          [bt-1 (vector-ref trend (- t 1))]
                          [st-p (if (>= (- t period) 0)
                                    (vector-ref seasonal (- t period))
                                    (vector-ref seasonal (modulo t period)))]
                          [lt (+ (* alpha (- xt st-p))
                                 (* (- 1 alpha) (+ lt-1 bt-1)))]
                          [bt (+ (* beta (- lt lt-1))
                                 (* (- 1 beta) bt-1))]
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
;;; Note: same lag structure as additive variant (see above).
(define (holt-winters-multiplicative xs alpha beta gamma period)
  (let ([n (vector-length xs)])
       (if (< n (* 2 period))
           (list 'error 'insufficient-data n period)
           (let* (;; Initialize: average first period for level
                  [l0 (/ (range-fold s 0 i 0 period
                           (+ s (vector-ref xs i)))
                         period)]
                  ;; Average of second period
                  [avg2 (/ (range-fold s 0 i period (* 2 period)
                             (+ s (vector-ref xs i)))
                           period)]
                  ;; Initialize trend from first two periods
                  [b0 (/ (- avg2 l0) period)]
                  ;; Output vectors
                  [level (make-vector n)]
                  [trend (make-vector n)]
                  ;; Initialize seasonal from first period (ratios)
                  [seasonal (vec-tabulate n i
                              (if (< i period)
                                  (/ (vector-ref xs i) (max l0 1e-10))
                                  0))])
                 (vector-set! level 0 l0)
                 (vector-set! trend 0 b0)
                 ;; Main loop
                 (range-do! t 1 n
                   (let* ([xt (vector-ref xs t)]
                          [lt-1 (vector-ref level (- t 1))]
                          [bt-1 (vector-ref trend (- t 1))]
                          [st-p (if (>= (- t period) 0)
                                    (vector-ref seasonal (- t period))
                                    (vector-ref seasonal (modulo t period)))]
                          [lt (+ (* alpha (/ xt (max st-p 1e-10)))
                                 (* (- 1 alpha) (+ lt-1 bt-1)))]
                          [bt (+ (* beta (- lt lt-1))
                                 (* (- 1 beta) bt-1))]
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
  (doc 'export #t)
  (let* ([type (cadr result)]
         [period (list-ref result 4)]
         [seasonal (list-ref result 7)]
         [last-level (list-ref result 8)]
         [last-trend (list-ref result 9)]
         [n (vector-length seasonal)])
        (vec-tabulate h i
          (let* ([step (+ i 1)]
                 [trend-component (* step last-trend)]
                 ;; Get seasonal component from the last cycle
                 [seasonal-idx (- n period (- period (modulo step period)))]
                 [seasonal-component (vector-ref seasonal
                                                 (max 0 (min seasonal-idx (- n 1))))])
                (if (eq? type 'additive)
                    (+ last-level trend-component seasonal-component)
                    (* (+ last-level trend-component) seasonal-component))))))

;;; hw-fitted : HWResult → Vec
;;; Get fitted values from Holt-Winters model.
(define (hw-fitted result)
  (let* ([type (cadr result)]
         [period (list-ref result 4)]
         [level (list-ref result 5)]
         [trend (list-ref result 6)]
         [seasonal (list-ref result 7)]
         [n (vector-length level)])
        (vec-tabulate n t
          (if (= t 0)
              (if (eq? type 'additive)
                  (+ (vector-ref level 0) (vector-ref seasonal 0))
                  (* (vector-ref level 0) (vector-ref seasonal 0)))
              (let* ([lt-1 (vector-ref level (- t 1))]
                     [bt-1 (vector-ref trend (- t 1))]
                     [st-p (if (>= (- t period) 0)
                               (vector-ref seasonal (- t period))
                               (vector-ref seasonal t))])
                    (if (eq? type 'additive)
                        (+ lt-1 bt-1 st-p)
                        (* (+ lt-1 bt-1) st-p)))))))

;;; ====
;;; Damped Trend Holt's Method
;;; ====

;;; holt-damped : Vec × Num × Num × Num → HoltDampedResult
;;; Holt's method with damped trend for more conservative forecasts.
;;; phi: damping parameter (0, 1), typically 0.8-0.98
(define (holt-damped xs alpha beta phi)
  (doc 'export #t)
  (let* ([n (vector-length xs)]
         [l0 (vector-ref xs 0)]
         [b0 (if (> n 1)
                 (- (vector-ref xs 1) (vector-ref xs 0))
                 0)]
         ;; Scan with pair accumulator (level . trend)
         [state (vec-scan n (cons l0 b0) t st
                  (let* ([lt-1 (car st)]
                         [bt-1 (cdr st)]
                         [xt (vector-ref xs t)]
                         [lt (+ (* alpha xt)
                                (* (- 1 alpha) (+ lt-1 (* phi bt-1))))]
                         [bt (+ (* beta (- lt lt-1))
                                (* (- 1 beta) (* phi bt-1)))])
                    (cons lt bt)))]
         ;; Extract level and trend vectors
         [level (vec-tabulate n i (car (vector-ref state i)))]
         [trend (vec-tabulate n i (cdr (vector-ref state i)))])
        (list 'holt-damped-result alpha beta phi level trend
              (vector-ref level (- n 1))
              (vector-ref trend (- n 1)))))

;;; holt-damped-forecast : HoltDampedResult × Nat → Vec
;;; Forecast with damped trend.
;;; f_{t+h} = l_t + (phi + phi^2 + ... + phi^h) * b_t
(define (holt-damped-forecast result h)
  (let ([phi (list-ref result 2)]
        [last-level (list-ref result 5)]
        [last-trend (list-ref result 6)])
       (vec-tabulate h i
         (let* ([step (+ i 1)]
                ;; Sum of geometric series: phi + phi^2 + ... + phi^step
                [phi-sum (if (< (abs (- phi 1)) 1e-10)
                             (exact->inexact step)
                             (* phi (/ (- 1 (expt phi step)) (- 1 phi))))])
               (+ last-level (* phi-sum last-trend))))))

