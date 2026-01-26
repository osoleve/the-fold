(load "lattice/fp/control-systems/kalman.ss")
(load "core/lang/eval.ss")

(doc 'module 'adaptive-allocator)
(doc 'description "Runtime-adaptive fuel allocation using Kalman filtering. Wraps the core evaluator to dynamically estimate and allocate fuel based on observed execution costs.")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(lattice/fp/control-systems/kalman core/lang/eval))

(doc 'note "Key insight: Instead of guessing fuel upfront, observe early iterations and refine estimates. Uses log-space Kalman filter for heavy-tailed cost distributions (execution costs often vary by orders of magnitude).")

(doc 'section 'allocator-state)

(doc 'note "An allocator maintains: estimator (log-space Kalman filter for cost estimation), confidence (number of sigmas for safety margin, 2.0 = 95%), observations (history for diagnostics, kept bounded), total-allocated (cumulative fuel allocated), total-consumed (cumulative fuel actually used), max-history (maximum observations to keep)")

(doc *default-max-history* 'type 'Nat)
(doc *default-max-history* 'description "Default maximum number of observations to keep in history")
(define *default-max-history* 100)

(define (allocator? a)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if value is an adaptive allocator")
  (doc 'export #t)
  (and (pair? a)
       (eq? (car a) 'adaptive-allocator)))

(define (make-adaptive-allocator initial-estimate process-noise measurement-noise confidence)
  (doc 'type (-> Num Num Num Num Allocator))
  (doc 'description "Create an adaptive fuel allocator")
  (doc 'param 'initial-estimate "Expected cost per element (natural units)")
  (doc 'param 'process-noise "Q - how much cost varies between elements")
  (doc 'param 'measurement-noise "R - instrumentation uncertainty")
  (doc 'param 'confidence "Number of sigmas for safety margin (2.0 = 95%)")
  (doc 'export #t)
  (list 'adaptive-allocator
        (make-log-kalman-filter initial-estimate
                                1.0          ; initial variance in log-space
                                process-noise
                                measurement-noise)
        confidence
        '()                                   ; observations history
        0                                     ; history-len (track explicitly for O(1) check)
        0                                     ; total-allocated
        0                                     ; total-consumed
        *default-max-history*))               ; max-history

(doc 'section 'accessors)

(define (allocator-estimator a)
  (doc 'type (-> Allocator LogKalmanFilter))
  (doc 'description "Extract Kalman filter estimator from allocator")
  (doc 'export #t)
  (cadr a))

(define (allocator-confidence a)
  (doc 'type (-> Allocator Num))
  (doc 'description "Extract confidence level (sigmas) from allocator")
  (doc 'export #t)
  (caddr a))

(define (allocator-observations a)
  (doc 'type (-> Allocator (List Num)))
  (doc 'description "Extract observation history from allocator")
  (doc 'export #t)
  (cadddr a))

(define (allocator-history-len a)
  (doc 'type (-> Allocator Nat))
  (doc 'description "Get observation history length in O(1)")
  (doc 'export #t)
  (car (cddddr a)))

(define (allocator-total-allocated a)
  (doc 'type (-> Allocator Nat))
  (doc 'description "Get cumulative fuel allocated")
  (doc 'export #t)
  (cadr (cddddr a)))

(define (allocator-total-consumed a)
  (doc 'type (-> Allocator Nat))
  (doc 'description "Get cumulative fuel consumed")
  (doc 'export #t)
  (caddr (cddddr a)))

(define (allocator-max-history a)
  (doc 'type (-> Allocator Nat))
  (doc 'description "Get maximum history size")
  (doc 'export #t)
  (cadddr (cddddr a)))

(doc 'section 'mutators)
(doc 'note "Functional mutators - return new allocator")

(define (allocator-set-estimator a est)
  (doc 'type (-> Allocator LogKalmanFilter Allocator))
  (doc 'description "Return new allocator with updated estimator")
  (doc 'export #t)
  (list 'adaptive-allocator
        est
        (allocator-confidence a)
        (allocator-observations a)
        (allocator-history-len a)
        (allocator-total-allocated a)
        (allocator-total-consumed a)
        (allocator-max-history a)))

(define (allocator-push-observation a obs)
  (doc 'type (-> Allocator Num Allocator))
  (doc 'description "Return new allocator with observation added to history")
  (doc 'note "O(1) cons, only truncates when at limit (amortized)")
  (doc 'export #t)
  (let* ([history (allocator-observations a)]
         [len (allocator-history-len a)]
         [max-h (allocator-max-history a)]
         [new-history (if (>= len max-h)
                          (cons obs (take (- max-h 1) history))
                          (cons obs history))]
         [new-len (if (>= len max-h) max-h (+ len 1))])
        (list 'adaptive-allocator
              (allocator-estimator a)
              (allocator-confidence a)
              new-history
              new-len
              (allocator-total-allocated a)
              (allocator-total-consumed a)
              max-h)))

(define (allocator-add-allocated a amount)
  (doc 'type (-> Allocator Nat Allocator))
  (doc 'description "Return new allocator with allocated fuel incremented")
  (doc 'export #t)
  (list 'adaptive-allocator
        (allocator-estimator a)
        (allocator-confidence a)
        (allocator-observations a)
        (allocator-history-len a)
        (+ (allocator-total-allocated a) amount)
        (allocator-total-consumed a)
        (allocator-max-history a)))

(define (allocator-add-consumed a amount)
  (doc 'type (-> Allocator Nat Allocator))
  (doc 'description "Return new allocator with consumed fuel incremented")
  (doc 'export #t)
  (list 'adaptive-allocator
        (allocator-estimator a)
        (allocator-confidence a)
        (allocator-observations a)
        (allocator-history-len a)
        (allocator-total-allocated a)
        (+ (allocator-total-consumed a) amount)
        (allocator-max-history a)))

(doc 'section 'fuel-allocation)

(define (allocator-request-fuel alloc)
  (doc 'type (-> Allocator Nat))
  (doc 'description "Request fuel allocation for ONE element. Uses log-space Kalman prediction with confidence margin.")
  (doc 'export #t)
  (let* ([est (allocator-estimator alloc)]
         [conf (allocator-confidence alloc)]
         [overhead 10]  ; fixed overhead for call machinery
         [predicted (log-kalman-predict-cost est conf)])
        (max 1 (ceiling (+ overhead predicted)))))

(define (allocator-observe alloc observed-cost)
  (doc 'type (-> Allocator Num Allocator))
  (doc 'description "Update allocator with observed actual cost. Returns new allocator with refined estimate.")
  (doc 'export #t)
  (let* ([est (allocator-estimator alloc)]
         [new-est (log-kalman-estimate est observed-cost)]
         [a1 (allocator-set-estimator alloc new-est)]
         [a2 (allocator-push-observation a1 observed-cost)]
         [a3 (allocator-add-consumed a2 observed-cost)])
        a3))

(define (allocator-mark-allocated alloc amount)
  (doc 'type (-> Allocator Num Allocator))
  (doc 'description "Track that fuel was allocated (for efficiency metrics)")
  (doc 'export #t)
  (allocator-add-allocated alloc amount))

(doc 'section 'diagnostics)

(define (allocator-efficiency alloc)
  (doc 'type (-> Allocator Num))
  (doc 'description "Ratio of consumed to allocated fuel (1.0 = perfect, <1 = over-allocated)")
  (doc 'export #t)
  (let ([allocated (allocator-total-allocated alloc)]
        [consumed (allocator-total-consumed alloc)])
       (if (zero? allocated)
           1.0
           (/ consumed allocated))))

(define (allocator-summary alloc)
  (doc 'type (-> Allocator Alist))
  (doc 'description "Return diagnostic summary of allocator state")
  (doc 'export #t)
  (let* ([est (allocator-estimator alloc)]
         [kf-summary (log-kalman-summary est)])
        `((mean-estimate . ,(cdr (assq 'mean kf-summary)))
          (ci-95-lower . ,(cdr (assq 'ci-95-lower kf-summary)))
          (ci-95-upper . ,(cdr (assq 'ci-95-upper kf-summary)))
          (kalman-gain . ,(cdr (assq 'gain kf-summary)))
          (efficiency . ,(allocator-efficiency alloc))
          (total-allocated . ,(allocator-total-allocated alloc))
          (total-consumed . ,(allocator-total-consumed alloc))
          (observations . ,(allocator-history-len alloc)))))

(define (allocator-recent-costs alloc n)
  (doc 'type (-> Allocator Nat (List Num)))
  (doc 'description "Get the N most recent observed costs")
  (doc 'export #t)
  (take (min n (allocator-history-len alloc))
        (allocator-observations alloc)))

(doc 'section 'convenience)

(define (default-adaptive-allocator)
  (doc 'type (-> Allocator))
  (doc 'description "Create an allocator with reasonable defaults: initial estimate 100 fuel units, process noise Q = 0.1 (moderate drift), measurement noise R = 0.5 (moderate instrumentation noise), confidence 2 sigmas (95%)")
  (doc 'export #t)
  (make-adaptive-allocator 100 0.1 0.5 2.0))

(doc 'section 'core-eval-integration)

(define (result-status result)
  (doc 'type (-> Result Symbol))
  (doc 'description "Extract status from eval result: 'ok, 'suspended, or 'error")
  (doc 'export #t)
  (car result))

(define (result-value result)
  (doc 'type (-> Result Any))
  (doc 'description "Extract value from (ok value remaining) result")
  (doc 'export #t)
  (cadr result))

(define (result-remaining-fuel result)
  (doc 'type (-> Result Nat))
  (doc 'description "Extract remaining fuel from (ok value remaining) result")
  (doc 'export #t)
  (if (eq? (car result) 'ok)
      (caddr result)
      0))

(define (result-suspended-expr result)
  (doc 'type (-> Result Expr))
  (doc 'description "Extract suspended expression from (suspended expr env) result")
  (doc 'export #t)
  (cadr result))

(define (result-suspended-env result)
  (doc 'type (-> Result Env))
  (doc 'description "Extract suspended environment from (suspended expr env) result")
  (doc 'export #t)
  (caddr result))

;; take is provided by prelude (via kalman.ss and eval.ss)
