(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
;;; @module des/schedule
;;; @requires prelude des/event des/world prng distributions
(require 'prelude)
(require 'des/event)
(require 'des/world)
(require 'prng)
(require 'distributions)

(doc 'module 'des/schedule)
(doc 'description "Discrete Event Simulation — stochastic and deterministic event scheduling combinators. Generators produce events using the world's RNG state (threaded purely via the State monad).")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;;---------------------------------------------------------------------------
;;; Deterministic scheduling
;;;---------------------------------------------------------------------------

(doc 'section 'deterministic)

(define (schedule-periodic world interval type payload count)
  (doc 'export #t)
  (doc 'type '(-> DESWorld Number Symbol Any Nat DESWorld))
  (doc 'description "Schedule count events at regular intervals starting from current clock + interval.")
  (let ([t0 (des-world-clock world)])
    (world-schedule* world
      (map (lambda (i)
             (make-des-event (+ t0 (* interval (+ i 1))) type payload))
           (iota count)))))

(define (schedule-at-times world times type payload)
  (doc 'export #t)
  (doc 'type '(-> DESWorld (List Number) Symbol Any DESWorld))
  (doc 'description "Schedule events at specific absolute times.")
  (world-schedule* world
    (map (lambda (t) (make-des-event t type payload)) times)))

;;;---------------------------------------------------------------------------
;;; Stochastic scheduling (single event)
;;;---------------------------------------------------------------------------

(doc 'section 'stochastic)

(define (schedule-after-exp world rate type payload)
  (doc 'export #t)
  (doc 'type '(-> DESWorld Number Symbol Any DESWorld))
  (doc 'description "Schedule one event after an exponentially-distributed delay. Commonly used for Poisson process arrivals. Advances world RNG state.")
  (let-values ([(delay w1) (world-run-random world (random-exponential rate))])
    (world-schedule-after w1 delay type payload)))

(define (schedule-after-normal world mean stddev type payload)
  (doc 'export #t)
  (doc 'type '(-> DESWorld Number Number Symbol Any DESWorld))
  (doc 'description "Schedule one event after a normally-distributed delay (clamped to >= 0).")
  (let-values ([(delay w1) (world-run-random world (random-normal mean stddev))])
    (world-schedule-after w1 (max 0 delay) type payload)))

(define (schedule-after-uniform world lo hi type payload)
  (doc 'export #t)
  (doc 'type '(-> DESWorld Number Number Symbol Any DESWorld))
  (doc 'description "Schedule one event after a uniformly-distributed delay in [lo, hi).")
  (let-values ([(delay w1) (world-run-random world (random-float-range lo hi))])
    (world-schedule-after w1 delay type payload)))

;;;---------------------------------------------------------------------------
;;; Self-scheduling patterns
;;;---------------------------------------------------------------------------

(doc 'section 'self-scheduling)
(doc 'note "These are patterns for event handlers that re-schedule themselves. The handler processes the current event then schedules the next occurrence. This is the standard DES pattern for recurring processes (arrivals, periodic checks, heartbeats).")

(define (make-poisson-arrival-handler rate type on-arrive)
  (doc 'export #t)
  (doc 'type '(-> Number Symbol (-> DESWorld DESEvent DESWorld) (-> DESWorld DESEvent DESWorld)))
  (doc 'description "Create a handler that processes an arrival event (via on-arrive) then self-schedules the next arrival with exponential inter-arrival time. This produces a Poisson process.")
  (lambda (world event)
    (let ([w1 (on-arrive world event)])
      (schedule-after-exp w1 rate type (des-event-payload event)))))

(define (make-periodic-handler interval type on-tick)
  (doc 'export #t)
  (doc 'type '(-> Number Symbol (-> DESWorld DESEvent DESWorld) (-> DESWorld DESEvent DESWorld)))
  (doc 'description "Create a handler that processes a periodic tick (via on-tick) then self-schedules the next tick after a fixed interval.")
  (lambda (world event)
    (let ([w1 (on-tick world event)])
      (world-schedule-after w1 interval type (des-event-payload event)))))
