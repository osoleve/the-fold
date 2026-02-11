(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'des/event)
(require 'des/world)
(require 'des/engine)
(require 'des/schedule)
(require 'des/replicate)
(require 'queue)
(require 'avl-tree)

;;;===========================================================================
;;; DES SDK Tests
;;;===========================================================================

;; Shorthand: empty world with no entities/metrics
(define (empty-world . args)
  (let ([clock (if (null? args) 0 (car args))]
        [rng (if (or (null? args) (null? (cdr args))) 'no-rng (cadr args))])
    (make-des-world clock avl-empty eq-empty avl-empty rng)))

;;;---------------------------------------------------------------------------
;;; Event tests
;;;---------------------------------------------------------------------------

(test-group "des-event"

  (define-test "event construction and accessors"
    (let ([e (make-des-event 5.0 'arrive '(customer-1))])
      (assert-true (des-event? e))
      (assert-equal 5.0 (des-event-time e))
      (assert-equal 'arrive (des-event-type e))
      (assert-equal '(customer-1) (des-event-payload e))))

  (define-test "event-with-time"
    (let* ([e (make-des-event 5.0 'arrive '())]
           [e2 (des-event-with-time e 10.0)])
      (assert-equal 10.0 (des-event-time e2))
      (assert-equal 'arrive (des-event-type e2))))

  (define-test "event queue empty"
    (assert-true (eq-empty? eq-empty))
    (assert-equal 0 (eq-size eq-empty)))

  (define-test "event queue schedule and peek"
    (let* ([q (eq-schedule eq-empty (make-des-event 3.0 'b '()))]
           [q (eq-schedule q (make-des-event 1.0 'a '()))]
           [q (eq-schedule q (make-des-event 2.0 'c '()))])
      (assert-equal 3 (eq-size q))
      (assert-equal 'a (des-event-type (eq-next q)))))

  (define-test "event queue pop ordering"
    (let* ([q (eq-from-list
               (list (make-des-event 3.0 'c '())
                     (make-des-event 1.0 'a '())
                     (make-des-event 2.0 'b '())))])
      (let-values ([(e1 q) (eq-pop q)])
        (assert-equal 'a (des-event-type e1))
        (let-values ([(e2 q) (eq-pop q)])
          (assert-equal 'b (des-event-type e2))
          (let-values ([(e3 q) (eq-pop q)])
            (assert-equal 'c (des-event-type e3))
            (assert-true (eq-empty? q)))))))

  (define-test "event queue to-list is sorted"
    (let* ([q (eq-from-list
               (list (make-des-event 5.0 'e '())
                     (make-des-event 1.0 'a '())
                     (make-des-event 3.0 'c '())))]
           [lst (eq-to-list q)])
      (assert-equal 3 (length lst))
      (assert-equal 1.0 (des-event-time (car lst)))
      (assert-equal 3.0 (des-event-time (cadr lst)))
      (assert-equal 5.0 (des-event-time (caddr lst)))))

  (define-test "event queue cancel by type"
    (let* ([q (eq-from-list
               (list (make-des-event 1.0 'arrive '())
                     (make-des-event 2.0 'depart '())
                     (make-des-event 3.0 'arrive '())))]
           [q2 (eq-cancel-type 'arrive q)])
      (assert-equal 1 (eq-size q2))
      (assert-equal 'depart (des-event-type (eq-next q2))))))

;;;---------------------------------------------------------------------------
;;; World tests
;;;---------------------------------------------------------------------------

(test-group "des-world"

  (define-test "world construction"
    (let ([w (empty-world)])
      (assert-true (des-world? w))
      (assert-equal 0 (des-world-clock w))
      (assert-equal 0 (world-entity-count w))))

  (define-test "world construction from alists"
    (let ([w (des-world 0 '((a . 1) (b . 2)) eq-empty '((x . 10)) 'no-rng)])
      (assert-equal 1 (world-entity w 'a))
      (assert-equal 2 (world-entity w 'b))
      (assert-equal 10 (world-metric w 'x))))

  (define-test "world entity CRUD"
    (let* ([w (empty-world)]
           [w (world-set-entity w 'server '(idle))]
           [w (world-set-entity w 'wait-queue '())])
      (assert-equal '(idle) (world-entity w 'server))
      (assert-equal '() (world-entity w 'wait-queue))
      (assert-equal 2 (world-entity-count w))
      ;; Update
      (let ([w2 (world-update-entity w 'server (lambda (_) '(busy)))])
        (assert-equal '(busy) (world-entity w2 'server)))
      ;; Remove
      (let ([w3 (world-remove-entity w 'wait-queue)])
        (assert-equal 1 (world-entity-count w3))
        (assert-false (world-entity w3 'wait-queue)))))

  (define-test "world metrics"
    (let* ([w (empty-world)]
           [w (world-set-metric w 'arrivals 0)]
           [w (world-inc-metric w 'arrivals)]
           [w (world-inc-metric w 'arrivals)]
           [w (world-add-metric w 'arrivals 3)])
      (assert-equal 5 (world-metric w 'arrivals))))

  (define-test "world scheduling convenience"
    (let* ([w (empty-world 10.0)]
           [w (world-schedule-at w 15.0 'tick '())]
           [w (world-schedule-after w 3.0 'check '())])
      (assert-equal 2 (eq-size (des-world-queue w)))
      (assert-equal 13.0 (des-event-time (eq-next (des-world-queue w))))))

  (define-test "world lenses work with view/set-lens/over"
    (let* ([w (empty-world)]
           [t (view world-clock w)]
           [w2 (set-lens world-clock 42 w)])
      (assert-equal 0 t)
      (assert-equal 42 (des-world-clock w2))
      (let ([w3 (over world-clock (lambda (t) (+ t 10)) w)])
        (assert-equal 10 (des-world-clock w3))))))

;;;---------------------------------------------------------------------------
;;; Engine tests
;;;---------------------------------------------------------------------------

(test-group "des-engine"

  (define-test "handler table lookup"
    (let ([table (make-handler-table
                  'arrive (lambda (w e) w)
                  'depart (lambda (w e) w))])
      (assert-true (procedure? (handler-lookup table 'arrive)))
      (assert-true (procedure? (handler-lookup table 'depart)))
      (assert-false (handler-lookup table 'unknown))))

  (define-test "single step advances clock"
    (let* ([handlers (make-handler-table
                      'tick (lambda (w e) w))]
           [config (make-des-config handlers 100.0 1000)]
           [w0 (empty-world)]
           [w0 (world-schedule-at w0 5.0 'tick '())]
           [w1 (des-step config w0)])
      (assert-equal 5.0 (des-world-clock w1))))

  (define-test "step returns #f on empty queue"
    (let* ([config (make-des-config '() 100.0 1000)]
           [w0 (empty-world)])
      (assert-false (des-step config w0))))

  (define-test "step returns #f past stop-time"
    (let* ([config (make-des-config '() 10.0 1000)]
           [w0 (empty-world)]
           [w0 (world-schedule-at w0 20.0 'tick '())])
      (assert-false (des-step config w0))))

  (define-test "deterministic simulation run"
    (let* ([tick-handler
            (lambda (w e)
              (let* ([w (world-inc-metric w 'ticks)]
                     [count (world-metric w 'ticks)])
                (if (< count 5)
                    (world-schedule-after w 1.0 'tick '())
                    w)))]
           [handlers (make-handler-table 'tick tick-handler)]
           [config (make-des-config handlers 100.0 1000)]
           [w0 (empty-world)]
           [w0 (world-schedule-at w0 1.0 'tick '())]
           [wf (des-run-final config w0)])
      (assert-equal 5 (world-metric wf 'ticks))
      (assert-equal 5.0 (des-world-clock wf))))

  (define-test "des-run produces stream of states"
    (let* ([tick-handler
            (lambda (w e)
              (world-schedule-after w 1.0 'tick '()))]
           [handlers (make-handler-table 'tick tick-handler)]
           [config (make-des-config handlers 5.0 100)]
           [w0 (empty-world)]
           [w0 (world-schedule-at w0 1.0 'tick '())]
           [states (des-run-list config w0)])
      (assert-equal 5 (length states))
      (assert-equal 1.0 (des-world-clock (car states)))
      (assert-equal 5.0 (des-world-clock (list-ref states 4))))))

;;;---------------------------------------------------------------------------
;;; Schedule tests
;;;---------------------------------------------------------------------------

(test-group "des-schedule"

  (define-test "schedule-periodic"
    (let* ([w0 (empty-world)]
           [w1 (schedule-periodic w0 2.0 'tick '() 3)]
           [events (eq-to-list (des-world-queue w1))])
      (assert-equal 3 (length events))
      (assert-equal 2.0 (des-event-time (car events)))
      (assert-equal 4.0 (des-event-time (cadr events)))
      (assert-equal 6.0 (des-event-time (caddr events)))))

  (define-test "schedule-at-times"
    (let* ([w0 (empty-world)]
           [w1 (schedule-at-times w0 '(10.0 5.0 15.0) 'check '())]
           [events (eq-to-list (des-world-queue w1))])
      (assert-equal 3 (length events))
      (assert-equal 5.0 (des-event-time (car events)))))

  (define-test "schedule-after-exp uses RNG"
    (let* ([rng (make-pcg 42 1)]
           [w0 (empty-world 0 rng)]
           [w1 (schedule-after-exp w0 1.0 'arrive '())])
      (assert-equal 1 (eq-size (des-world-queue w1)))
      (assert-true (> (des-event-time (eq-next (des-world-queue w1))) 0))))

  (define-test "make-poisson-arrival-handler self-schedules"
    (let* ([on-arrive (lambda (w e)
                        (world-inc-metric w 'arrivals))]
           [handler (make-poisson-arrival-handler 1.0 'arrive on-arrive)]
           [rng (make-pcg 42 1)]
           [w0 (empty-world 0 rng)]
           [w1 (handler w0 (make-des-event 0 'arrive '()))])
      (assert-equal 1 (world-metric w1 'arrivals))
      (assert-equal 1 (eq-size (des-world-queue w1))))))

;;;---------------------------------------------------------------------------
;;; M/M/1 Queue — canonical DES example (using Okasaki queue)
;;;---------------------------------------------------------------------------

(test-group "mm1-queue"

  (define mm1-arrival-rate 0.8)
  (define mm1-service-rate 1.0)

  ;; Entity: server is '(idle) or '(busy customer-id)
  ;; Entity: wait-queue is an Okasaki FIFO queue (amortized O(1) enqueue/dequeue)

  (define (mm1-handle-arrive w e)
    (let* ([customer-id (world-metric w 'next-id)]
           [w (world-set-metric w 'next-id (+ customer-id 1))]
           [w (world-inc-metric w 'total-arrivals)]
           [server (world-entity w 'server)])
      (if (eq? (car server) 'idle)
          ;; Server free: start service immediately
          (let-values ([(svc-time w) (world-run-random w (random-exponential mm1-service-rate))])
            (let* ([w (world-set-entity w 'server (list 'busy customer-id))]
                   [w (world-schedule-after w svc-time 'depart customer-id)])
              w))
          ;; Server busy: join queue
          (let* ([q (world-entity w 'wait-queue)]
                 [w (world-set-entity w 'wait-queue (queue-enqueue customer-id q))]
                 [w (world-set-metric w 'max-queue-length
                      (max (world-metric w 'max-queue-length)
                           (queue-size (world-entity w 'wait-queue))))])
            w))))

  (define (mm1-handle-depart w e)
    (let* ([w (world-inc-metric w 'total-departures)]
           [q (world-entity w 'wait-queue)])
      (if (queue-empty? q)
          ;; Queue empty: server goes idle
          (world-set-entity w 'server '(idle))
          ;; Serve next in queue
          (let-values ([(rest-q next-customer) (queue-dequeue q)])
            (let ([w (world-set-entity w 'wait-queue rest-q)])
              (let-values ([(svc-time w) (world-run-random w (random-exponential mm1-service-rate))])
                (let* ([w (world-set-entity w 'server (list 'busy next-customer))]
                       [w (world-schedule-after w svc-time 'depart next-customer)])
                  w)))))))

  (define mm1-arrive-handler
    (make-poisson-arrival-handler mm1-arrival-rate 'arrive mm1-handle-arrive))

  (define mm1-handlers
    (make-handler-table
     'arrive mm1-arrive-handler
     'depart mm1-handle-depart))

  (define (make-mm1-world rng)
    (let* ([w (empty-world 0 rng)]
           [w (world-set-entity w 'server '(idle))]
           [w (world-set-entity w 'wait-queue queue-empty)]
           [w (world-set-metric w 'total-arrivals 0)]
           [w (world-set-metric w 'total-departures 0)]
           [w (world-set-metric w 'max-queue-length 0)]
           [w (world-set-metric w 'next-id 1)]
           [w (schedule-after-exp w mm1-arrival-rate 'arrive '())])
      w))

  (define-test "mm1 single run completes"
    (let* ([config (make-des-config mm1-handlers 100.0 10000)]
           [rng (make-pcg 42 1)]
           [w0 (make-mm1-world rng)]
           [wf (des-run-final config w0)])
      (assert-true (> (world-metric wf 'total-arrivals) 0))
      (assert-true (> (world-metric wf 'total-departures) 0))
      (assert-true (> (world-metric wf 'total-arrivals) 50))
      (assert-true (< (world-metric wf 'total-arrivals) 120))))

  (define-test "mm1 utilization approaches theoretical"
    (let* ([config (make-des-config mm1-handlers 1000.0 100000)]
           [rng (make-pcg 12345 1)]
           [w0 (make-mm1-world rng)]
           [wf (des-run-final config w0)]
           [arrivals (world-metric wf 'total-arrivals)]
           [departures (world-metric wf 'total-departures)]
           [throughput (/ departures arrivals)])
      (assert-true (> throughput 0.9)))))

;;;---------------------------------------------------------------------------
;;; Replication tests
;;;---------------------------------------------------------------------------

(test-group "des-replicate"

  (define-test "replications produce independent results"
    (let* ([handlers (make-handler-table
                      'tick (lambda (w e) (world-inc-metric w 'count)))]
           [config (make-des-config handlers 10.0 100)]
           [make-w (lambda (rng)
                     (let ([w (empty-world 0 rng)])
                       (schedule-periodic w 1.0 'tick '() 5)))]
           [finals (des-replicate config make-w 3 42)])
      (assert-equal 3 (length finals))
      (assert-equal 5 (world-metric (car finals) 'count))
      (assert-equal 5 (world-metric (cadr finals) 'count))))

  (define-test "replicate-summary computes stats"
    (let* ([tick-handler
            (lambda (w e)
              (let-values ([(val w) (world-run-random w (random-exponential 1.0))])
                (world-add-metric w 'total val)))]
           [handlers (make-handler-table 'tick tick-handler)]
           [config (make-des-config handlers 10.0 1000)]
           [make-w (lambda (rng)
                     (let ([w (empty-world 0 rng)])
                       (schedule-periodic w 0.1 'tick '() 50)))]
           [summary (des-replicate-summary config make-w 10 42 'total)])
      (assert-true (> (cdr (assq 'mean summary)) 0))
      (assert-equal 10 (cdr (assq 'n summary))))))

;;;===========================================================================

(run-all-tests)
