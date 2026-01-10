;;; shell/fuel/adaptive-allocator.ss — Adaptive Fuel Allocation
;;;
;;; Runtime-adaptive fuel allocation using Kalman filtering.
;;; Wraps the core evaluator to dynamically estimate and allocate fuel
;;; based on observed execution costs.
;;;
;;; Key insight: Instead of guessing fuel upfront, observe early iterations
;;; and refine estimates. Uses log-space Kalman filter for heavy-tailed
;;; cost distributions (execution costs often vary by orders of magnitude).
;;;
;;; This is shell code: may have side effects for state management.
;;;
;;; Dependencies:
;;;   - lattice/fp/control-systems/kalman.ss
;;;   - core/lang/eval.ss

(load "lattice/fp/control-systems/kalman.ss")
(load "core/lang/eval.ss")

;;; ============================================================
;;; Allocator State
;;; ============================================================

;;; An allocator maintains:
;;;   - estimator: log-space Kalman filter for cost estimation
;;;   - confidence: number of sigmas for safety margin (2.0 = 95%)
;;;   - observations: history for diagnostics (kept bounded)
;;;   - total-allocated: cumulative fuel allocated
;;;   - total-consumed: cumulative fuel actually used
;;;   - max-history: maximum observations to keep

(define *default-max-history* 100)

;;; allocator? : Any → Boolean
(define (allocator? a)
  (and (pair? a)
       (eq? (car a) 'adaptive-allocator)))

;;; make-adaptive-allocator : Num × Num × Num × Num → Allocator
;;; Create an adaptive fuel allocator.
;;;   initial-estimate: expected cost per element (natural units)
;;;   process-noise: Q - how much cost varies between elements
;;;   measurement-noise: R - instrumentation uncertainty
;;;   confidence: number of sigmas for safety margin
(define (make-adaptive-allocator initial-estimate process-noise measurement-noise confidence)
  (list 'adaptive-allocator
        ;; Estimator: log-space Kalman filter
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

;;; Accessors

(define (allocator-estimator a) (cadr a))
(define (allocator-confidence a) (caddr a))
(define (allocator-observations a) (cadddr a))
(define (allocator-history-len a) (car (cddddr a)))      ; O(1) length tracking
(define (allocator-total-allocated a) (cadr (cddddr a)))
(define (allocator-total-consumed a) (caddr (cddddr a)))
(define (allocator-max-history a) (cadddr (cddddr a)))

;;; Mutators (functional - return new allocator)

(define (allocator-set-estimator a est)
  (list 'adaptive-allocator
        est
        (allocator-confidence a)
        (allocator-observations a)
        (allocator-history-len a)
        (allocator-total-allocated a)
        (allocator-total-consumed a)
        (allocator-max-history a)))

(define (allocator-push-observation a obs)
  (let* ([history (allocator-observations a)]
         [len (allocator-history-len a)]
         [max-h (allocator-max-history a)]
         ;; O(1) cons, only truncate when at limit (amortized)
         [new-history (if (>= len max-h)
                          (cons obs (take history (- max-h 1)))
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
  (list 'adaptive-allocator
        (allocator-estimator a)
        (allocator-confidence a)
        (allocator-observations a)
        (allocator-history-len a)
        (+ (allocator-total-allocated a) amount)
        (allocator-total-consumed a)
        (allocator-max-history a)))

(define (allocator-add-consumed a amount)
  (list 'adaptive-allocator
        (allocator-estimator a)
        (allocator-confidence a)
        (allocator-observations a)
        (allocator-history-len a)
        (allocator-total-allocated a)
        (+ (allocator-total-consumed a) amount)
        (allocator-max-history a)))

;;; ============================================================
;;; Fuel Allocation
;;; ============================================================

;;; allocator-request-fuel : Allocator → Nat
;;; Request fuel allocation for ONE element.
;;; Uses log-space Kalman prediction with confidence margin.
(define (allocator-request-fuel alloc)
  (let* ([est (allocator-estimator alloc)]
         [conf (allocator-confidence alloc)]
         [overhead 10]  ; fixed overhead for call machinery
         [predicted (log-kalman-predict-cost est conf)])
        (max 1 (ceiling (+ overhead predicted)))))

;;; allocator-observe : Allocator × Num → Allocator
;;; Update allocator with observed actual cost.
;;; Returns new allocator with refined estimate.
(define (allocator-observe alloc observed-cost)
  (let* ([est (allocator-estimator alloc)]
         [new-est (log-kalman-estimate est observed-cost)]
         [a1 (allocator-set-estimator alloc new-est)]
         [a2 (allocator-push-observation a1 observed-cost)]
         [a3 (allocator-add-consumed a2 observed-cost)])
        a3))

;;; allocator-mark-allocated : Allocator × Num → Allocator
;;; Track that fuel was allocated (for efficiency metrics).
(define (allocator-mark-allocated alloc amount)
  (allocator-add-allocated alloc amount))

;;; ============================================================
;;; Diagnostics
;;; ============================================================

;;; allocator-efficiency : Allocator → Num
;;; Ratio of consumed to allocated fuel (1.0 = perfect, <1 = over-allocated).
(define (allocator-efficiency alloc)
  (let ([allocated (allocator-total-allocated alloc)]
        [consumed (allocator-total-consumed alloc)])
       (if (zero? allocated)
           1.0
           (/ consumed allocated))))

;;; allocator-summary : Allocator → Alist
;;; Return diagnostic summary of allocator state.
(define (allocator-summary alloc)
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

;;; allocator-recent-costs : Allocator × Nat → List[Num]
;;; Get the N most recent observed costs.
(define (allocator-recent-costs alloc n)
  (take (allocator-observations alloc)
        (min n (allocator-history-len alloc))))

;;; ============================================================
;;; Convenience: Default Allocator
;;; ============================================================

;;; default-adaptive-allocator : → Allocator
;;; Create an allocator with reasonable defaults.
;;; Initial estimate: 100 fuel units
;;; Process noise: Q = 0.1 (moderate drift)
;;; Measurement noise: R = 0.5 (moderate instrumentation noise)
;;; Confidence: 2 sigmas (95%)
(define (default-adaptive-allocator)
  (make-adaptive-allocator 100 0.1 0.5 2.0))

;;; ============================================================
;;; Integration with Core Eval
;;; ============================================================

;;; result-status : Result → Symbol
;;; Extract status from eval result: 'ok, 'suspended, or 'error
(define (result-status result)
  (car result))

;;; result-value : Result → Any
;;; Extract value from (ok value remaining) result.
(define (result-value result)
  (cadr result))

;;; result-remaining-fuel : Result → Nat
;;; Extract remaining fuel from (ok value remaining) result.
(define (result-remaining-fuel result)
  (if (eq? (car result) 'ok)
      (caddr result)
      0))

;;; result-suspended-expr : Result → Expr
;;; Extract suspended expression from (suspended expr env) result.
(define (result-suspended-expr result)
  (cadr result))

;;; result-suspended-env : Result → Env
;;; Extract suspended environment from (suspended expr env) result.
(define (result-suspended-env result)
  (caddr result))

;;; Helper to take first n elements
(define (take lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))
