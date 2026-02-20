;;; lattice/control-systems/kalman.ss — Scalar Kalman Filter
;;;
;;; SCALAR Kalman filter for online estimation of a single hidden variable.
;;; NOT the full matrix Kalman filter used in state-space control systems.
;;;
;;; For matrix-based Kalman filtering in control applications, see:
;;;   - lqg in controller-design.ss (LQR + Kalman observer design)
;;;   - solve-care for algebraic Riccati equations
;;;
;;; This scalar filter is used by adaptive fuel allocation to refine cost
;;; estimates at runtime. It maintains a belief (mean, variance) about a
;;; hidden scalar state and updates it as observations arrive:
;;;   - State: expected cost per element
;;;   - Observations: actual measured costs
;;;   - Process noise (Q): cost variability between elements
;;;   - Measurement noise (R): instrumentation uncertainty
;;;
;;; Log-space variant handles heavy-tailed, strictly-positive distributions
;;; better than linear Kalman (costs often vary by orders of magnitude).
;;;
;;; This is lattice code: pure, no side effects.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)

(doc 'module 'kalman)
(doc 'purity 'total)
(doc 'description "Scalar Kalman filter for online estimation with log-space variant")
(doc 'layer 'lattice)

;;; ====
;;; Scalar Kalman Filter
;;; ====

;;; A Kalman filter is: (kalman mean variance Q R)
;;; where:
;;;   mean     — current estimate of the hidden state
;;;   variance — uncertainty in the estimate (P in literature)
;;;   Q        — process noise variance (how much state drifts)
;;;   R        — measurement noise variance

;;; kalman? : Any → Boolean
(define (kalman? kf)
  (and (pair? kf)
       (eq? (car kf) 'kalman)
       (= (length kf) 5)))

;;; make-kalman-filter : Num × Num × Num × Num → Kalman
;;; Create a new Kalman filter with initial state estimate.
(define (make-kalman-filter initial-mean initial-variance process-noise measurement-noise)
  (list 'kalman initial-mean initial-variance process-noise measurement-noise))

;;; Accessors

;;; kalman-mean : Kalman → Num
(define (kalman-mean kf) (cadr kf))

;;; kalman-variance : Kalman → Num
(define (kalman-variance kf) (caddr kf))

;;; kalman-Q : Kalman → Num
(define (kalman-Q kf) (cadddr kf))

;;; kalman-R : Kalman → Num
(define (kalman-R kf) (car (cddddr kf)))

;;; ====
;;; Kalman Filter Operations
;;; ====

;;; kalman-predict : Kalman → Kalman
;;; Predict step: propagate state forward with no observation.
;;; For random walk model: mean stays same, variance increases by Q.
;;;
;;;   μ̂_{k|k-1} = μ̂_{k-1|k-1}
;;;   P_{k|k-1} = P_{k-1|k-1} + Q
(define (kalman-predict kf)
  (let ([mean (kalman-mean kf)]
        [var (kalman-variance kf)]
        [Q (kalman-Q kf)]
        [R (kalman-R kf)])
       (make-kalman-filter mean (+ var Q) Q R)))

;;; kalman-update : Kalman × Num → Kalman
;;; Update step: incorporate an observation to refine estimate.
;;;
;;;   K_k = P_{k|k-1} / (P_{k|k-1} + R)        ; Kalman gain
;;;   μ̂_{k|k} = μ̂_{k|k-1} + K_k(z_k - μ̂_{k|k-1})  ; updated mean
;;;   P_{k|k} = (1 - K_k) P_{k|k-1}            ; updated variance
(define (kalman-update kf observation)
  (let* ([mean (kalman-mean kf)]
         [var (kalman-variance kf)]
         [Q (kalman-Q kf)]
         [R (kalman-R kf)]
         [K (if (zero? (+ var R)) 0 (/ var (+ var R)))]  ; Kalman gain
         [new-mean (+ mean (* K (- observation mean)))]
         [new-var (* (- 1 K) var)])
        (make-kalman-filter new-mean (max 0.0001 new-var) Q R)))

;;; kalman-estimate : Kalman × Num → Kalman
;;; Combined predict + update in one step (most common usage).
(define (kalman-estimate kf observation)
  (kalman-update (kalman-predict kf) observation))

;;; kalman-gain : Kalman → Num
;;; Compute current Kalman gain (how much to trust new observations).
;;; High gain = trust observations more. Low gain = trust prior more.
(define (kalman-gain kf)
  (let ([var (kalman-variance kf)]
        [R (kalman-R kf)])
       (if (zero? (+ var R)) 0 (/ var (+ var R)))))

;;; kalman-batch : Kalman × List[Num] → Kalman
;;; Process a list of observations sequentially.
(define (kalman-batch kf observations)
  (fold-left kalman-estimate kf observations))

;;; kalman-residual : Kalman × Num → Num
;;; Compute the innovation/residual (observation - prediction).
;;; Useful for diagnostics and outlier detection.
(define (kalman-residual kf observation)
  (- observation (kalman-mean kf)))

;;; kalman-mahalanobis : Kalman × Num → Num
;;; Compute Mahalanobis distance of observation from predicted distribution.
;;; Values > 3 indicate potential outliers.
(define (kalman-mahalanobis kf observation)
  (let ([residual (kalman-residual kf observation)]
        [var (kalman-variance kf)]
        [R (kalman-R kf)])
       (if (zero? (+ var R))
           0
           (abs (/ residual (sqrt (+ var R)))))))

;;; ====
;;; Log-Space Kalman Filter
;;; ====
;;;
;;; For heavy-tailed, strictly positive distributions (like fuel costs),
;;; operating in log-space provides better behavior:
;;;   - A spike from 10 to 1000 is 2.3 → 6.9 in log-space (manageable)
;;;   - In linear space, mean jumps to ~500, variance explodes
;;;
;;; Transform: z → log(z) before update, exp(x̂) for prediction

;;; make-log-kalman-filter : Num × Num × Num × Num → Kalman
;;; Create a log-space Kalman filter.
;;; initial-mean is in natural units, internally stored as log.
(define (make-log-kalman-filter initial-mean initial-variance Q R)
  (make-kalman-filter (log (max 1 initial-mean))
                      initial-variance
                      Q
                      R))

;;; log-kalman-update : Kalman × Num → Kalman
;;; Update with an observation in natural units (transforms to log internally).
(define (log-kalman-update kf observed-cost)
  (kalman-update kf (log (max 1 observed-cost))))

;;; log-kalman-estimate : Kalman × Num → Kalman
;;; Combined predict + update for log-space filter.
(define (log-kalman-estimate kf observed-cost)
  (kalman-estimate kf (log (max 1 observed-cost))))

;;; log-kalman-mean : Kalman → Num
;;; Get the mean estimate in natural units (exponentiates from log).
(define (log-kalman-mean kf)
  (exp (kalman-mean kf)))

;;; log-kalman-predict-cost : Kalman × Num → Num
;;; Predict cost with confidence margin.
;;; confidence-sigmas: number of standard deviations for safety margin
;;;   - 1.0 = 68% confidence
;;;   - 2.0 = 95% confidence
;;;   - 3.0 = 99.7% confidence
(define (log-kalman-predict-cost kf confidence-sigmas)
  (let* ([log-mean (kalman-mean kf)]
         [log-var (kalman-variance kf)]
         [log-sigma (sqrt (max 0.0001 log-var))])
        (exp (+ log-mean (* confidence-sigmas log-sigma)))))

;;; log-kalman-confidence-interval : Kalman × Num → (Num . Num)
;;; Get confidence interval in natural units.
(define (log-kalman-confidence-interval kf confidence-sigmas)
  (let* ([log-mean (kalman-mean kf)]
         [log-var (kalman-variance kf)]
         [log-sigma (sqrt (max 0.0001 log-var))]
         [lower (exp (- log-mean (* confidence-sigmas log-sigma)))]
         [upper (exp (+ log-mean (* confidence-sigmas log-sigma)))])
        (cons lower upper)))

;;; ====
;;; Adaptive Q Tuning
;;; ====
;;;
;;; Process noise Q controls how quickly the filter adapts.
;;; Too low: filter becomes rigid, can't track changing costs
;;; Too high: filter is noisy, over-reacts to outliers
;;;
;;; These helpers allow dynamic Q adjustment based on residuals.

;;; kalman-with-Q : Kalman × Num → Kalman
;;; Return a copy of the filter with updated Q.
(define (kalman-with-Q kf new-Q)
  (make-kalman-filter (kalman-mean kf)
                      (kalman-variance kf)
                      new-Q
                      (kalman-R kf)))

;;; kalman-boost-Q : Kalman × Num → Kalman
;;; Multiply Q by a factor (useful when residuals are consistently large).
(define (kalman-boost-Q kf factor)
  (kalman-with-Q kf (* (kalman-Q kf) factor)))

;;; ====
;;; Summary Statistics
;;; ====

;;; kalman-summary : Kalman → Alist
;;; Return a diagnostic summary of the filter state.
(define (kalman-summary kf)
  `((mean . ,(kalman-mean kf))
    (variance . ,(kalman-variance kf))
    (std-dev . ,(sqrt (max 0 (kalman-variance kf))))
    (Q . ,(kalman-Q kf))
    (R . ,(kalman-R kf))
    (gain . ,(kalman-gain kf))))

;;; log-kalman-summary : Kalman → Alist
;;; Summary for log-space filter (values in natural units).
(define (log-kalman-summary kf)
  (let ([ci (log-kalman-confidence-interval kf 2.0)])
       `((mean . ,(log-kalman-mean kf))
         (log-mean . ,(kalman-mean kf))
         (log-variance . ,(kalman-variance kf))
         (ci-95-lower . ,(car ci))
         (ci-95-upper . ,(cdr ci))
         (Q . ,(kalman-Q kf))
         (R . ,(kalman-R kf))
         (gain . ,(kalman-gain kf)))))
