;;; shell/fuel/adaptive-hof.ss — Adaptive Higher-Order Functions
;;;
;;; HOFs with runtime-adaptive fuel allocation.
;;; Uses Kalman filtering to refine cost estimates as elements are processed.
;;;
;;; Features:
;;;   - Per-element fuel allocation (not batch)
;;;   - Log-space Kalman filter for heavy-tailed costs
;;;   - Accumulates cost across retries (no filter pollution)
;;;   - Returns stats alongside results for observability
;;;
;;; This is shell code: orchestrates evaluation with fuel management.
;;;
;;; Dependencies:
;;;   - shell/fuel/adaptive-allocator.ss

(load "shell/fuel/adaptive-allocator.ss")

;;; ============================================================
;;; Options Parsing
;;; ============================================================

;;; get-opt : Alist × Symbol × Any → Any
;;; Get option value or default.
(define (get-opt opts key default)
  (let ([entry (assq key opts)])
       (if entry (cdr entry) default)))

;;; ============================================================
;;; Core: Adaptive Evaluation
;;; ============================================================

;;; adaptive-eval-element : Allocator × Expr × Env × Nat → (Values Any Allocator)
;;; Evaluate a single element with adaptive retry.
;;; Accumulates fuel across retries, updates filter only on success.
;;; Returns (values result-or-error updated-allocator)
(define (adaptive-eval-element alloc expr env max-retries)
  (let loop ([fuel-limit (allocator-request-fuel alloc)]
             [accumulated-cost 0]
             [retries 0]
             [alloc alloc])
       (if (> retries max-retries)
           (values `(error max-retries-exceeded ,expr ,accumulated-cost) alloc)
           (let* ([alloc-tracked (allocator-mark-allocated alloc fuel-limit)]
                  [result (eval-expr expr env fuel-limit)])
                 (case (result-status result)
                       [(ok)
                        ;; Success: update filter with total cost
                        (let* ([step-cost (- fuel-limit (result-remaining-fuel result))]
                               [total-cost (+ accumulated-cost step-cost)]
                               [alloc-updated (allocator-observe alloc-tracked total-cost)])
                              (values `(ok ,(result-value result) ,total-cost) alloc-updated))]
                       [(suspended)
                        ;; Ran out of fuel: retry with doubled budget
                        ;; Don't update filter - this is censored data
                        (loop (* 2 fuel-limit)
                              (+ accumulated-cost fuel-limit)
                              (+ retries 1)
                              alloc-tracked)]
                       [else
                        ;; Error: propagate
                        (values result alloc-tracked)])))))

;;; ============================================================
;;; Adaptive Map
;;; ============================================================

;;; adaptive-map : (α → β) × List[α] × Alist → (Values List[β] Alist)
;;; Map with adaptive fuel allocation.
;;;
;;; Options:
;;;   initial-estimate - starting cost estimate (default: 100)
;;;   confidence - sigmas for safety margin (default: 2.0)
;;;   process-noise - Q parameter (default: 0.1)
;;;   measurement-noise - R parameter (default: 0.5)
;;;   max-retries - per-element retry limit (default: 10)
;;;
;;; Returns:
;;;   (values results stats)
;;;   where stats is an alist with efficiency metrics
(define (adaptive-map f xs . opts)
  (let* ([opts (if (null? opts) '() (car opts))]
         [initial-estimate (get-opt opts 'initial-estimate 100)]
         [confidence (get-opt opts 'confidence 2.0)]
         [Q (get-opt opts 'process-noise 0.1)]
         [R (get-opt opts 'measurement-noise 0.5)]
         [max-retries (get-opt opts 'max-retries 10)]
         [alloc (make-adaptive-allocator initial-estimate Q R confidence)])
        
        ;; Build the call expression for f applied to an element
        ;; We'll quote the element and apply f
        (define (make-call elem)
          `(call (quote ,f) (quote ,elem)))
        
        (let loop ([remaining xs]
                   [results '()]
                   [alloc alloc])
             (if (null? remaining)
                 (values (reverse results) (allocator-summary alloc))
                 (let ([elem (car remaining)])
                      (let-values ([(result alloc*)
                                    (adaptive-eval-element alloc
                                                           (make-call elem)
                                                           empty-env
                                                           max-retries)])
                                  (case (car result)
                                        [(ok)
                                         (loop (cdr remaining)
                                               (cons (cadr result) results)
                                               alloc*)]
                                        [else
                                         ;; Error - return partial results with error
                                         (values `(error ,result
                                                   (completed . ,(reverse results))
                                                   (remaining . ,(length (cdr remaining))))
                                                 (allocator-summary alloc*))])))))))

;;; ============================================================
;;; Adaptive Map (Scheme-native functions)
;;; ============================================================

;;; adaptive-map-native : (α → β) × List[α] × Alist → (Values List[β] Alist)
;;; Map with adaptive fuel allocation for native Scheme functions.
;;; This version applies the function directly, measuring wall time as "cost".
;;; Useful when function isn't in Core DSL.
(define (adaptive-map-native f xs . opts)
  (let* ([opts (if (null? opts) '() (car opts))]
         [initial-estimate (get-opt opts 'initial-estimate 100)]
         [confidence (get-opt opts 'confidence 2.0)]
         [Q (get-opt opts 'process-noise 0.1)]
         [R (get-opt opts 'measurement-noise 0.5)]
         [alloc (make-adaptive-allocator initial-estimate Q R confidence)])
        
        (let loop ([remaining xs]
                   [results '()]
                   [alloc alloc])
             (if (null? remaining)
                 (values (reverse results) (allocator-summary alloc))
                 (let* ([elem (car remaining)]
                        [start-time (get-time-micros)]
                        [result (f elem)]
                        [end-time (get-time-micros)]
                        ;; Use microseconds as cost proxy
                        [cost (max 1 (time-diff-micros end-time start-time))]
                        [alloc* (allocator-observe alloc cost)])
                       (loop (cdr remaining)
                             (cons result results)
                             alloc*))))))

;;; ============================================================
;;; Adaptive Filter
;;; ============================================================

;;; adaptive-filter : (α → Bool) × List[α] × Alist → (Values List[α] Alist)
;;; Filter with adaptive fuel allocation.
(define (adaptive-filter pred xs . opts)
  (let-values ([(results stats)
                (apply adaptive-map
                       (lambda (x) (cons (pred x) x))
                       xs
                       opts)])
              (if (and (pair? results) (eq? (car results) 'error))
                  (values results stats)  ; propagate error
                  (values (map cdr (filter (lambda (p) (car p)) results))
                          stats))))

;;; adaptive-filter-native : (α → Bool) × List[α] × Alist → (Values List[α] Alist)
;;; Filter with native Scheme predicates.
(define (adaptive-filter-native pred xs . opts)
  (let-values ([(results stats)
                (apply adaptive-map-native
                       (lambda (x) (cons (pred x) x))
                       xs
                       opts)])
              (values (map cdr (filter (lambda (p) (car p)) results))
                      stats)))

;;; ============================================================
;;; Adaptive Fold
;;; ============================================================

;;; adaptive-fold-left : (β × α → β) × β × List[α] × Alist → (Values β Alist)
;;; Left fold with adaptive fuel allocation.
(define (adaptive-fold-left f init xs . opts)
  (let* ([opts (if (null? opts) '() (car opts))]
         [initial-estimate (get-opt opts 'initial-estimate 100)]
         [confidence (get-opt opts 'confidence 2.0)]
         [Q (get-opt opts 'process-noise 0.1)]
         [R (get-opt opts 'measurement-noise 0.5)]
         [max-retries (get-opt opts 'max-retries 10)]
         [alloc (make-adaptive-allocator initial-estimate Q R confidence)])
        
        ;; Build call expression for f applied to accumulator and element
        (define (make-call acc elem)
          `(call (quote ,f) (quote ,acc) (quote ,elem)))
        
        (let loop ([remaining xs]
                   [acc init]
                   [alloc alloc])
             (if (null? remaining)
                 (values acc (allocator-summary alloc))
                 (let ([elem (car remaining)])
                      (let-values ([(result alloc*)
                                    (adaptive-eval-element alloc
                                                           (make-call acc elem)
                                                           empty-env
                                                           max-retries)])
                                  (case (car result)
                                        [(ok)
                                         (loop (cdr remaining)
                                               (cadr result)
                                               alloc*)]
                                        [else
                                         (values `(error ,result
                                                   (accumulated . ,acc)
                                                   (remaining . ,(length remaining)))
                                                 (allocator-summary alloc*))])))))))

;;; adaptive-fold-left-native : (β × α → β) × β × List[α] × Alist → (Values β Alist)
;;; Left fold with native Scheme functions.
(define (adaptive-fold-left-native f init xs . opts)
  (let* ([opts (if (null? opts) '() (car opts))]
         [initial-estimate (get-opt opts 'initial-estimate 100)]
         [confidence (get-opt opts 'confidence 2.0)]
         [Q (get-opt opts 'process-noise 0.1)]
         [R (get-opt opts 'measurement-noise 0.5)]
         [alloc (make-adaptive-allocator initial-estimate Q R confidence)])
        
        (let loop ([remaining xs]
                   [acc init]
                   [alloc alloc])
             (if (null? remaining)
                 (values acc (allocator-summary alloc))
                 (let* ([elem (car remaining)]
                        [start-time (get-time-micros)]
                        [new-acc (f acc elem)]
                        [end-time (get-time-micros)]
                        [cost (max 1 (time-diff-micros end-time start-time))]
                        [alloc* (allocator-observe alloc cost)])
                       (loop (cdr remaining)
                             new-acc
                             alloc*))))))

;;; ============================================================
;;; Time Utilities (for native function timing)
;;; ============================================================

;;; get-time-micros : → Nat
;;; Get current time in microseconds (Chez Scheme specific).
(define (get-time-micros)
  (let ([t (current-time 'time-utc)])
       (+ (* (time-second t) 1000000)
          (quotient (time-nanosecond t) 1000))))

;;; time-difference : Nat × Nat → Nat
;;; Difference between two times in microseconds.
(define (time-diff-micros end start)
  (max 0 (- end start)))

;;; ============================================================
;;; Convenience: Quick Adaptive Evaluation
;;; ============================================================

;;; adaptive-eval : Expr → (Values Any Alist)
;;; Evaluate a single expression with adaptive fuel allocation.
;;; Useful for one-off evaluations where you don't know the cost.
(define (adaptive-eval expr)
  (let ([alloc (default-adaptive-allocator)])
       (let-values ([(result alloc*)
                     (adaptive-eval-element alloc expr empty-env 10)])
                   (values (if (eq? (car result) 'ok)
                               (cadr result)
                               result)
                           (allocator-summary alloc*)))))
