;;; lattice/fp/control-systems/test-kalman.ss — Tests for Kalman Filter
;;;
;;; Run from project root: scheme --script lattice/fp/control-systems/test-kalman.ss

(load "core/testing/test-framework.ss")
(load "lattice/fp/control-systems/kalman.ss")

(display "\n")
(display "====\n")
(display "         KALMAN FILTER TESTS\n")
(display "====\n")

;;; Helper: approximately equal within tolerance
(define (approx= expected actual tolerance)
  (< (abs (- expected actual)) tolerance))

;;; Assert approximately equal
(define (assert-approx expected actual tolerance)
  (assert-true (approx= expected actual tolerance)))

;;; ====
;;; Construction Tests
;;; ====

(test-group construction
            (define-test make-kalman-basic
              (let ([kf (make-kalman-filter 10 5 0.1 1.0)])
                   (assert-true (kalman? kf))
                   (assert-equal 10 (kalman-mean kf))
                   (assert-equal 5 (kalman-variance kf))
                   (assert-equal 0.1 (kalman-Q kf))
                   (assert-equal 1.0 (kalman-R kf))))
            
            (define-test initial-gain
              ;; With high variance relative to R, gain should be high
              (let ([kf (make-kalman-filter 10 100 0.1 1.0)])
                   (assert-true (> (kalman-gain kf) 0.9))))
            
            (define-test low-initial-gain
              ;; With low variance relative to R, gain should be low
              (let ([kf (make-kalman-filter 10 0.1 0.1 100)])
                   (assert-true (< (kalman-gain kf) 0.01)))))

;;; ====
;;; Predict Tests
;;; ====

(test-group predict
            (define-test predict-mean-unchanged
              ;; Random walk: mean stays the same during predict
              (let* ([kf (make-kalman-filter 10 5 0.1 1.0)]
                     [predicted (kalman-predict kf)])
                    (assert-equal 10 (kalman-mean predicted))))
            
            (define-test predict-variance-increases
              ;; Variance increases by Q during predict
              (let* ([kf (make-kalman-filter 10 5 0.1 1.0)]
                     [predicted (kalman-predict kf)])
                    (assert-equal 5.1 (kalman-variance predicted)))))

;;; ====
;;; Update Tests
;;; ====

(test-group update
            (define-test update-moves-toward-observation
              ;; Observation higher than mean should increase mean
              (let* ([kf (make-kalman-filter 10 1 0.1 1.0)]
                     [updated (kalman-update kf 20)])
                    (assert-true (> (kalman-mean updated) 10))
                    (assert-true (< (kalman-mean updated) 20))))
            
            (define-test update-reduces-variance
              ;; Observation should reduce variance
              (let* ([kf (make-kalman-filter 10 10 0.1 1.0)]
                     [updated (kalman-update kf 12)])
                    (assert-true (< (kalman-variance updated) 10))))
            
            (define-test exact-observation-high-gain
              ;; With very low R (trusted observation), mean should nearly match observation
              (let* ([kf (make-kalman-filter 10 100 0.1 0.001)]
                     [updated (kalman-update kf 20)])
                    (assert-approx 20 (kalman-mean updated) 0.1))))

;;; ====
;;; Estimate (Predict + Update) Tests
;;; ====

(test-group estimate
            (define-test estimate-combines-predict-update
              (let* ([kf (make-kalman-filter 10 5 0.1 1.0)]
                     [estimated (kalman-estimate kf 15)]
                     [predicted (kalman-predict kf)]
                     [updated (kalman-update predicted 15)])
                    (assert-equal (kalman-mean updated) (kalman-mean estimated))
                    (assert-equal (kalman-variance updated) (kalman-variance estimated)))))

;;; ====
;;; Batch Processing Tests
;;; ====

(test-group batch
            (define-test converges-to-true-mean
              ;; Given observations around true mean, filter should converge
              (let* ([true-mean 100]
                     ;; Observations: 95, 105, 98, 102, 100, 101, 99, 103, 97, 100
                     [observations '(95 105 98 102 100 101 99 103 97 100)]
                     [kf (make-kalman-filter 50 1000 0.5 10)]  ; start far from truth
                     [final (kalman-batch kf observations)])
                    ;; Should converge close to true mean
                    (assert-approx true-mean (kalman-mean final) 10)))
            
            (define-test variance-decreases-over-time
              (let* ([kf0 (make-kalman-filter 10 100 0.1 1)]
                     [kf1 (kalman-estimate kf0 12)]
                     [kf2 (kalman-estimate kf1 11)]
                     [kf3 (kalman-estimate kf2 10)])
                    (assert-true (< (kalman-variance kf1) (kalman-variance kf0)))
                    (assert-true (< (kalman-variance kf2) (kalman-variance kf1)))
                    (assert-true (< (kalman-variance kf3) (kalman-variance kf2)))))
            
            (define-test stable-with-many-observations
              ;; Process many observations, filter should remain stable
              (let* ([observations (make-list 100 15)]  ; all same value
                     [kf (make-kalman-filter 10 50 0.1 1)]
                     [final (kalman-batch kf observations)])
                    (assert-approx 15 (kalman-mean final) 0.1)
                    (assert-true (> (kalman-variance final) 0)))))  ; shouldn't collapse to zero

;;; ====
;;; Log-Space Kalman Tests
;;; ====

(test-group log-kalman
            (define-test log-kalman-construction
              (let ([kf (make-log-kalman-filter 100 1 0.1 0.5)])
                   (assert-true (kalman? kf))
                   ;; Internal mean should be log(100) ≈ 4.6
                   (assert-approx (log 100) (kalman-mean kf) 0.01)))
            
            (define-test log-kalman-prediction
              (let* ([kf (make-log-kalman-filter 100 1 0.1 0.5)]
                     [predicted (log-kalman-predict-cost kf 0)])  ; 0 sigmas = just mean
                    ;; Should be approximately 100
                    (assert-approx 100 predicted 10)))
            
            (define-test log-kalman-with-margin
              (let* ([kf (make-log-kalman-filter 100 1 0.1 0.5)]
                     [no-margin (log-kalman-predict-cost kf 0)]
                     [with-margin (log-kalman-predict-cost kf 2)])
                    ;; With margin should be larger
                    (assert-true (> with-margin no-margin))))
            
            (define-test log-kalman-update
              (let* ([kf (make-log-kalman-filter 100 1 0.1 0.5)]
                     [updated (log-kalman-estimate kf 200)])
                    ;; Mean should move toward 200 but stay closer to 100
                    (assert-true (> (log-kalman-mean updated) 100))
                    (assert-true (< (log-kalman-mean updated) 200))))
            
            (define-test log-kalman-handles-spike
              ;; A spike shouldn't destroy the estimate in log space
              (let* ([kf (make-log-kalman-filter 100 1 0.1 0.5)]
                     [after-spike (log-kalman-estimate kf 10000)])
                    ;; Mean should increase but not to 10000 (that would be bad)
                    ;; Log-space dampens: 10000 = e^9.2, 100 = e^4.6, avg ~e^6.9 = 1000
                    (assert-true (< (log-kalman-mean after-spike) 5000))
                    ;; Mean should be more than 100 (moved toward spike)
                    (assert-true (> (log-kalman-mean after-spike) 100))))
            
            (define-test log-kalman-confidence-interval
              (let* ([kf (make-log-kalman-filter 100 1 0.1 0.5)]
                     [ci (log-kalman-confidence-interval kf 2)])
                    ;; Lower bound should be less than 100
                    (assert-true (< (car ci) 100))
                    ;; Upper bound should be greater than 100
                    (assert-true (> (cdr ci) 100))
                    ;; Should be reasonable bounds
                    (assert-true (> (car ci) 0))
                    (assert-true (< (cdr ci) 10000)))))

;;; ====
;;; Mahalanobis Distance Tests
;;; ====

(test-group mahalanobis
            (define-test normal-observation
              (let* ([kf (make-kalman-filter 100 25 0.1 1)]  ; variance=25, so sigma=5
                     [distance (kalman-mahalanobis kf 105)])  ; 1 sigma away
                    ;; Should be approximately 1
                    (assert-approx 1 distance 0.5)))
            
            (define-test outlier-detection
              (let* ([kf (make-kalman-filter 100 25 0.1 1)]
                     [outlier-distance (kalman-mahalanobis kf 200)])  ; way outside
                    ;; Should be > 3 (outlier threshold)
                    (assert-true (> outlier-distance 3)))))

;;; ====
;;; Q Tuning Tests
;;; ====

(test-group q-tuning
            (define-test with-Q-changes-Q
              (let* ([kf (make-kalman-filter 10 5 0.1 1)]
                     [new-kf (kalman-with-Q kf 0.5)])
                    (assert-equal 0.5 (kalman-Q new-kf))
                    ;; Other params unchanged
                    (assert-equal 10 (kalman-mean new-kf))
                    (assert-equal 5 (kalman-variance new-kf))))
            
            (define-test boost-Q
              (let* ([kf (make-kalman-filter 10 5 0.1 1)]
                     [boosted (kalman-boost-Q kf 2)])
                    (assert-equal 0.2 (kalman-Q boosted)))))

;;; ====
;;; Summary Tests
;;; ====

(test-group summary
            (define-test kalman-summary-keys
              (let* ([kf (make-kalman-filter 10 5 0.1 1)]
                     [s (kalman-summary kf)])
                    (assert-true (pair? (assq 'mean s)))
                    (assert-true (pair? (assq 'variance s)))
                    (assert-true (pair? (assq 'std-dev s)))
                    (assert-true (pair? (assq 'Q s)))
                    (assert-true (pair? (assq 'R s)))
                    (assert-true (pair? (assq 'gain s)))))
            
            (define-test log-kalman-summary-keys
              (let* ([kf (make-log-kalman-filter 100 1 0.1 0.5)]
                     [s (log-kalman-summary kf)])
                    (assert-true (pair? (assq 'mean s)))
                    (assert-true (pair? (assq 'log-mean s)))
                    (assert-true (pair? (assq 'ci-95-lower s)))
                    (assert-true (pair? (assq 'ci-95-upper s))))))

;;; ====
;;; Run Tests
;;; ====

(run-all-tests)
