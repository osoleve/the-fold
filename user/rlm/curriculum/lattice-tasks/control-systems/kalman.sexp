;;; kalman.sexp — Development tasks for lattice/control-systems/kalman.ss
;;;
;;; Module: kalman (tier 0, depends on prelude)
;;; 243 lines, ~20 exported functions
;;; Scalar Kalman filter for online Bayesian estimation.
;;; Maintains belief (mean, variance) about hidden state.
;;; Predict-update cycle, batch processing, Mahalanobis outlier detection.
;;; Log-space variant for heavy-tailed distributions.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement kalman-predict
  ;; ──────────────────────────────────────────────
  ;; The predict step — uncertainty grows without observation.

  (task
    (id "control-systems/kalman/kalman-predict-impl")
    (module kalman)
    (tier 0)
    (type implement)
    (description "Implement kalman-predict: propagate the filter state forward one time step without an observation. For a random walk model, the mean stays unchanged and the variance increases by the process noise Q. This reflects growing uncertainty: the longer we go without observing, the less certain we are. Returns a new Kalman filter with updated variance. Use make-kalman-filter to construct the result, preserving the same Q and R.")
    (context "Available: kalman-mean, kalman-variance, kalman-Q, kalman-R, make-kalman-filter, +. Four accessors, one addition, one constructor.")
    (before "(define (kalman-predict kf)
  ;; TODO: mean unchanged, variance += Q
  kf)")
    (test-file "lattice/control-systems/test-kalman.ss")
    (test-forms (";; Mean unchanged after predict
(let* ([kf (make-kalman-filter 10 5 0.1 1.0)]
       [predicted (kalman-predict kf)])
  (assert-equal 10 (kalman-mean predicted)))"
                 ";; Variance increases by Q
(let* ([kf (make-kalman-filter 10 5 0.1 1.0)]
       [predicted (kalman-predict kf)])
  (assert-equal 5.1 (kalman-variance predicted)))"
                 ";; Q and R preserved
(let* ([kf (make-kalman-filter 10 5 0.1 1.0)]
       [predicted (kalman-predict kf)])
  (assert-equal 0.1 (kalman-Q predicted))
  (assert-equal 1.0 (kalman-R predicted)))"))
    (available-modules (prelude))
    (hints ("Read: mean, var, Q, R from kf"
            "New variance = (+ var Q)"
            "Return (make-kalman-filter mean (+ var Q) Q R)"))
    (reference-solution "(define (kalman-predict kf)
  (let ([mean (kalman-mean kf)]
        [var (kalman-variance kf)]
        [Q (kalman-Q kf)]
        [R (kalman-R kf)])
       (make-kalman-filter mean (+ var Q) Q R)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement kalman-update
  ;; ──────────────────────────────────────────────
  ;; The update step — fusing observation with prior belief.

  (task
    (id "control-systems/kalman/kalman-update-impl")
    (module kalman)
    (tier 0)
    (type implement)
    (description "Implement kalman-update: incorporate an observation to refine the state estimate. The Kalman gain K = P/(P+R) determines how much to trust the observation vs the prior. New mean = old_mean + K*(observation - old_mean). New variance = (1-K)*P. Guard against division by zero: if P+R = 0, set K = 0. Also clamp new variance to minimum 0.0001 to prevent numerical collapse. This is the core of Bayesian filtering: every observation shrinks uncertainty.")
    (context "Available: kalman-mean, kalman-variance, kalman-Q, kalman-R, make-kalman-filter, zero?, +, -, *, /, if, max, let*. Kalman gain formula + mean/variance update.")
    (before "(define (kalman-update kf observation)
  ;; TODO: K = P/(P+R), new-mean = mean + K*(obs - mean), new-var = (1-K)*P
  kf)")
    (test-file "lattice/control-systems/test-kalman.ss")
    (test-forms (";; Update moves mean toward observation
(let* ([kf (make-kalman-filter 10 1 0.1 1.0)]
       [updated (kalman-update kf 20)])
  (assert-true (> (kalman-mean updated) 10))
  (assert-true (< (kalman-mean updated) 20)))"
                 ";; Update reduces variance
(let* ([kf (make-kalman-filter 10 10 0.1 1.0)]
       [updated (kalman-update kf 12)])
  (assert-true (< (kalman-variance updated) 10)))"
                 ";; With equal P and R, gain = 0.5, mean = midpoint
(let* ([kf (make-kalman-filter 10 1 0.1 1.0)]
       [updated (kalman-update kf 20)])
  (assert-equal 15.0 (kalman-mean updated)))"))
    (available-modules (prelude))
    (hints ("Read mean, var (=P), Q, R"
            "K = (if (zero? (+ var R)) 0 (/ var (+ var R)))"
            "new-mean = (+ mean (* K (- observation mean)))"
            "new-var = (* (- 1 K) var)"
            "Clamp: (max 0.0001 new-var)"
            "Return (make-kalman-filter new-mean clamped-var Q R)"))
    (reference-solution "(define (kalman-update kf observation)
  (let* ([mean (kalman-mean kf)]
         [var (kalman-variance kf)]
         [Q (kalman-Q kf)]
         [R (kalman-R kf)]
         [K (if (zero? (+ var R)) 0 (/ var (+ var R)))]
         [new-mean (+ mean (* K (- observation mean)))]
         [new-var (* (- 1 K) var)])
        (make-kalman-filter new-mean (max 0.0001 new-var) Q R)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement kalman-gain
  ;; ──────────────────────────────────────────────
  ;; The Kalman gain — how much to trust new observations.

  (task
    (id "control-systems/kalman/kalman-gain-impl")
    (module kalman)
    (tier 0)
    (type implement)
    (description "Implement kalman-gain: compute the current Kalman gain K = P/(P+R) where P is the state variance and R is the measurement noise. The gain ranges from 0 to 1. High gain (K near 1) means high state uncertainty relative to measurement noise — trust the observation. Low gain (K near 0) means low state uncertainty — trust the prior. Guard: if P+R = 0, return 0. The gain is diagnostic: it tells you whether the filter is observation-driven or prior-driven.")
    (context "Available: kalman-variance, kalman-R, zero?, +, /, if. One formula with a guard.")
    (before "(define (kalman-gain kf)
  ;; TODO: K = P / (P + R)
  0)")
    (test-file "lattice/control-systems/test-kalman.ss")
    (test-forms (";; High variance, low R → high gain
(let ([kf (make-kalman-filter 10 100 0.1 1.0)])
  (assert-true (> (kalman-gain kf) 0.9)))"
                 ";; Low variance, high R → low gain
(let ([kf (make-kalman-filter 10 0.1 0.1 100)])
  (assert-true (< (kalman-gain kf) 0.01)))"
                 ";; Equal P and R → gain = 0.5
(let ([kf (make-kalman-filter 10 1 0.1 1)])
  (assert-equal 1/2 (kalman-gain kf)))"))
    (available-modules (prelude))
    (hints ("Read var = (kalman-variance kf), R = (kalman-R kf)"
            "Guard: (if (zero? (+ var R)) 0 ...)"
            "Return: (/ var (+ var R))"))
    (reference-solution "(define (kalman-gain kf)
  (let ([var (kalman-variance kf)]
        [R (kalman-R kf)])
       (if (zero? (+ var R)) 0 (/ var (+ var R)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement kalman-batch
  ;; ──────────────────────────────────────────────
  ;; Process a sequence of observations — the practical usage pattern.

  (task
    (id "control-systems/kalman/kalman-batch-impl")
    (module kalman)
    (tier 0)
    (type implement)
    (description "Implement kalman-batch: process a list of observations sequentially through the filter. Each observation triggers a predict+update cycle via kalman-estimate. Use fold-left to thread the filter state through the observation list. This is the most common usage pattern: given historical data, compute the posterior estimate. The filter converges: starting far from the true mean, it approaches it as observations accumulate.")
    (context "Available: kalman-estimate (combined predict+update), fold-left. One fold.")
    (before "(define (kalman-batch kf observations)
  ;; TODO: fold observations through kalman-estimate
  kf)")
    (test-file "lattice/control-systems/test-kalman.ss")
    (test-forms (";; Converges to true mean
(let* ([kf (make-kalman-filter 50 1000 0.5 10)]
       [final (kalman-batch kf '(95 105 98 102 100 101 99 103 97 100))])
  (assert-true (< (abs (- (kalman-mean final) 100)) 10)))"
                 ";; Variance decreases over time
(let* ([kf0 (make-kalman-filter 10 100 0.1 1)]
       [kf-after (kalman-batch kf0 '(12 11 10))])
  (assert-true (< (kalman-variance kf-after) 100)))"
                 ";; Stable with constant observations
(let* ([kf (make-kalman-filter 10 50 0.1 1)]
       [final (kalman-batch kf (make-list 100 15))])
  (assert-true (< (abs (- (kalman-mean final) 15)) 0.1)))"))
    (available-modules (prelude))
    (hints ("fold-left threads state through a list"
            "Return (fold-left kalman-estimate kf observations)"))
    (reference-solution "(define (kalman-batch kf observations)
  (fold-left kalman-estimate kf observations))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement kalman-mahalanobis
  ;; ──────────────────────────────────────────────
  ;; Outlier detection via standardized distance.

  (task
    (id "control-systems/kalman/kalman-mahalanobis-impl")
    (module kalman)
    (tier 0)
    (type implement)
    (description "Implement kalman-mahalanobis: compute the Mahalanobis distance of an observation from the filter's predicted distribution. This is |residual| / sqrt(P + R), where residual = observation - mean and P + R is the innovation variance (state uncertainty + measurement noise). Values > 3 indicate potential outliers (3-sigma rule). Guard: if P+R = 0, return 0. This is the key diagnostic for adaptive filtering — large Mahalanobis distances trigger Q-boosting in practice.")
    (context "Available: kalman-residual (obs - mean), kalman-variance, kalman-R, zero?, abs, /, sqrt, +, if. Standardize the residual.")
    (before "(define (kalman-mahalanobis kf observation)
  ;; TODO: |residual| / sqrt(P + R)
  0)")
    (test-file "lattice/control-systems/test-kalman.ss")
    (test-forms (";; Observation 1 sigma away
(let* ([kf (make-kalman-filter 100 25 0.1 1)]
       [distance (kalman-mahalanobis kf 105)])
  ;; sqrt(25 + 1) ≈ 5.1, residual = 5, distance ≈ 0.98
  (assert-true (< (abs (- distance 1.0)) 0.5)))"
                 ";; Outlier detection: far observation
(let* ([kf (make-kalman-filter 100 25 0.1 1)]
       [d (kalman-mahalanobis kf 200)])
  (assert-true (> d 3)))"
                 ";; Zero residual → distance = 0
(let* ([kf (make-kalman-filter 10 5 0.1 1)]
       [d (kalman-mahalanobis kf 10)])
  (assert-equal 0 d))"))
    (available-modules (prelude))
    (hints ("Compute residual = (kalman-residual kf observation)"
            "Read var = (kalman-variance kf), R = (kalman-R kf)"
            "Guard: (if (zero? (+ var R)) 0 ...)"
            "Return: (abs (/ residual (sqrt (+ var R))))"))
    (reference-solution "(define (kalman-mahalanobis kf observation)
  (let ([residual (kalman-residual kf observation)]
        [var (kalman-variance kf)]
        [R (kalman-R kf)])
       (if (zero? (+ var R))
           0
           (abs (/ residual (sqrt (+ var R)))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

) ;; end tasks
