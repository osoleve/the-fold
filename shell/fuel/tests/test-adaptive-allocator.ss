;;; shell/fuel/tests/test-adaptive-allocator.ss — Integration Tests for Adaptive Fuel Allocation
;;;
;;; Run from project root: scheme --script shell/fuel/tests/test-adaptive-allocator.ss

(load "core/testing/test-framework.ss")
(load "shell/fuel/adaptive-hof.ss")

(display "\n")
(display "==============================================================\n")
(display "     ADAPTIVE FUEL ALLOCATOR INTEGRATION TESTS\n")
(display "==============================================================\n")

;;; Helper: approximately equal within tolerance
(define (approx= expected actual tolerance)
  (< (abs (- expected actual)) tolerance))

(define (assert-approx expected actual tolerance)
  (assert-true (approx= expected actual tolerance)))

;;; Helper: observe multiple times
(define (observe-many alloc n val)
  (if (<= n 0) alloc
      (observe-many (allocator-observe alloc val) (- n 1) val)))

;;; ============================================================
;;; Allocator Construction and Accessors
;;; ============================================================

(test-group allocator-basics
            (define-test make-allocator
              (let ([a (make-adaptive-allocator 100 0.1 0.5 2.0)])
                   (assert-true (allocator? a))
                   (assert-equal 0 (allocator-history-len a))
                   (assert-equal 0 (allocator-total-allocated a))
                   (assert-equal 0 (allocator-total-consumed a))))
            
            (define-test default-allocator
              (let ([a (default-adaptive-allocator)])
                   (assert-true (allocator? a))
                   (assert-true (> (allocator-request-fuel a) 0)))))

;;; ============================================================
;;; Fuel Request Convergence
;;; ============================================================

(test-group fuel-convergence
            (define-test converges-to-stable-cost
              ;; Repeatedly observe cost=50, fuel request should converge toward ~50+margin
              (let* ([a0 (make-adaptive-allocator 200 0.1 0.5 2.0)]  ; start high
                     [a1 (observe-many a0 20 50)])
                    ;; Mean estimate should be near 50
                    (let ([summary (allocator-summary a1)])
                         (assert-approx 50 (cdr (assq 'mean-estimate summary)) 10))))
            
            (define-test adapts-to-changing-costs
              ;; First observe low costs, then high costs - should adapt
              (let* ([a0 (make-adaptive-allocator 100 0.1 0.5 2.0)]
                     [a1 (observe-many a0 10 20)]   ; low costs
                     [a2 (observe-many a1 10 200)]) ; high costs
                    (let ([summary (allocator-summary a2)])
                         ;; Should be somewhere between 20 and 200, biased toward recent
                         (assert-true (> (cdr (assq 'mean-estimate summary)) 50))
                         (assert-true (< (cdr (assq 'mean-estimate summary)) 200))))))

;;; ============================================================
;;; History Management
;;; ============================================================

(test-group history-management
            (define-test history-tracks-length
              (let* ([a0 (make-adaptive-allocator 100 0.1 0.5 2.0)]
                     [a1 (allocator-observe a0 50)]
                     [a2 (allocator-observe a1 60)]
                     [a3 (allocator-observe a2 70)])
                    (assert-equal 1 (allocator-history-len a1))
                    (assert-equal 2 (allocator-history-len a2))
                    (assert-equal 3 (allocator-history-len a3))))
            
            (define-test history-caps-at-max
              (let* ([a0 (make-adaptive-allocator 100 0.1 0.5 2.0)]
                     [a1 (observe-many a0 150 75)])  ; max-history default is 100
                    (assert-equal 100 (allocator-history-len a1))
                    (assert-equal 100 (length (allocator-observations a1)))))
            
            (define-test recent-costs-correct
              (let* ([a0 (make-adaptive-allocator 100 0.1 0.5 2.0)]
                     [a1 (allocator-observe a0 10)]
                     [a2 (allocator-observe a1 20)]
                     [a3 (allocator-observe a2 30)])
                    ;; Most recent first
                    (assert-equal '(30 20 10) (allocator-recent-costs a3 3))
                    (assert-equal '(30 20) (allocator-recent-costs a3 2)))))

;;; ============================================================
;;; Mark Allocated / Consumed Tracking
;;; ============================================================

(test-group tracking
            (define-test mark-allocated
              (let* ([a0 (make-adaptive-allocator 100 0.1 0.5 2.0)]
                     [a1 (allocator-mark-allocated a0 500)])
                    (assert-equal 500 (allocator-total-allocated a1))))
            
            (define-test consumed-accumulated
              (let* ([a0 (make-adaptive-allocator 100 0.1 0.5 2.0)]
                     [a1 (allocator-observe a0 30)]
                     [a2 (allocator-observe a1 40)]
                     [a3 (allocator-observe a2 50)])
                    (assert-equal 120 (allocator-total-consumed a3))))
            
            (define-test efficiency-calculation
              (let* ([a0 (make-adaptive-allocator 100 0.1 0.5 2.0)]
                     [a1 (allocator-mark-allocated a0 200)]
                     [a2 (allocator-observe a1 100)])
                    ;; Efficiency = consumed/allocated = 100/200 = 0.5
                    (assert-approx 0.5 (allocator-efficiency a2) 0.01))))

;;; ============================================================
;;; Adaptive Map Native
;;; ============================================================

(test-group adaptive-map-native
            (define-test basic-map
              (let-values ([(results stats)
                            (adaptive-map-native (lambda (x) (* x x))
                                                 '(1 2 3 4 5))])
                          (assert-equal '(1 4 9 16 25) results)
                          (assert-true (pair? (assq 'mean-estimate stats)))
                          (assert-equal 5 (cdr (assq 'observations stats)))))
            
            (define-test empty-list
              (let-values ([(results stats)
                            (adaptive-map-native (lambda (x) x) '())])
                          (assert-equal '() results)))
            
            (define-test large-list-efficiency
              ;; Process 100 elements, should achieve reasonable efficiency
              (let-values ([(results stats)
                            (adaptive-map-native (lambda (x) (* x x))
                                                 (iota 100))])
                          (assert-equal 100 (length results))
                          (assert-equal 100 (cdr (assq 'observations stats))))))

;;; ============================================================
;;; Adaptive Filter Native
;;; ============================================================

(test-group adaptive-filter-native
            (define-test basic-filter
              (let-values ([(results stats)
                            (adaptive-filter-native even? '(1 2 3 4 5 6 7 8 9 10))])
                          (assert-equal '(2 4 6 8 10) results))))

;;; ============================================================
;;; Adaptive Fold Native
;;; ============================================================

(test-group adaptive-fold-native
            (define-test sum-fold
              (let-values ([(result stats)
                            (adaptive-fold-left-native + 0 '(1 2 3 4 5))])
                          (assert-equal 15 result)))
            
            (define-test list-reverse-fold
              (let-values ([(result stats)
                            (adaptive-fold-left-native (lambda (acc x) (cons x acc))
                                                       '()
                                                       '(1 2 3))])
                          (assert-equal '(3 2 1) result))))

;;; ============================================================
;;; Summary Statistics
;;; ============================================================

(test-group summary
            (define-test summary-keys
              (let* ([a (make-adaptive-allocator 100 0.1 0.5 2.0)]
                     [a1 (allocator-observe a 50)]
                     [s (allocator-summary a1)])
                    (assert-true (pair? (assq 'mean-estimate s)))
                    (assert-true (pair? (assq 'ci-95-lower s)))
                    (assert-true (pair? (assq 'ci-95-upper s)))
                    (assert-true (pair? (assq 'kalman-gain s)))
                    (assert-true (pair? (assq 'efficiency s)))
                    (assert-true (pair? (assq 'total-allocated s)))
                    (assert-true (pair? (assq 'total-consumed s)))
                    (assert-true (pair? (assq 'observations s))))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests)
