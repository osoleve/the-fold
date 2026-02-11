(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
;;; @module des/world
;;; @requires prelude optics des/event state
(require 'prelude)
(require 'optics)
(require 'des/event)
(require 'state)

(doc 'module 'des/world)
(doc 'description "Discrete Event Simulation — world state container with optics. A DES world holds the simulation clock, an entity store (alist), the event queue, accumulated metrics, and RNG state. Lenses provide composable access into all fields.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;;---------------------------------------------------------------------------
;;; World state
;;;---------------------------------------------------------------------------

(doc 'section 'world-state)

(define (make-des-world clock entities event-queue metrics rng)
  (doc 'type '(-> Number Alist EventQueue Alist Any DESWorld))
  (doc 'description "Create a simulation world. Entities is an alist of (id . entity-data). Metrics is an alist of (name . value) for accumulators.")
  (list 'des-world clock entities event-queue metrics rng))

(define (des-world? x)
  (doc 'type '(-> Any Boolean))
  (and (pair? x) (eq? (car x) 'des-world)))

(define (des-world-clock w)     (list-ref w 1))
(define (des-world-entities w)  (list-ref w 2))
(define (des-world-queue w)     (list-ref w 3))
(define (des-world-metrics w)   (list-ref w 4))
(define (des-world-rng w)       (list-ref w 5))

;;;---------------------------------------------------------------------------
;;; Lenses
;;;---------------------------------------------------------------------------

(doc 'section 'world-lenses)

(doc 'type '(Lens DESWorld Number))
(define world-clock
  (make-lens des-world-clock
             (lambda (t w) (make-des-world t (des-world-entities w) (des-world-queue w) (des-world-metrics w) (des-world-rng w)))))

(doc 'type '(Lens DESWorld Alist))
(define world-entities
  (make-lens des-world-entities
             (lambda (es w) (make-des-world (des-world-clock w) es (des-world-queue w) (des-world-metrics w) (des-world-rng w)))))

(doc 'type '(Lens DESWorld EventQueue))
(define world-queue
  (make-lens des-world-queue
             (lambda (q w) (make-des-world (des-world-clock w) (des-world-entities w) q (des-world-metrics w) (des-world-rng w)))))

(doc 'type '(Lens DESWorld Alist))
(define world-metrics
  (make-lens des-world-metrics
             (lambda (m w) (make-des-world (des-world-clock w) (des-world-entities w) (des-world-queue w) m (des-world-rng w)))))

(doc 'type '(Lens DESWorld Any))
(define world-rng
  (make-lens des-world-rng
             (lambda (r w) (make-des-world (des-world-clock w) (des-world-entities w) (des-world-queue w) (des-world-metrics w) r))))

;;;---------------------------------------------------------------------------
;;; Entity operations (via entities alist)
;;;---------------------------------------------------------------------------

(doc 'section 'entity-ops)

(define (world-entity w id)
  (doc 'type '(-> DESWorld Symbol Any))
  (doc 'description "Look up entity by id. Returns #f if not found.")
  (let ([pair (assq id (des-world-entities w))])
    (if pair (cdr pair) #f)))

(define (world-set-entity w id data)
  (doc 'type '(-> DESWorld Symbol Any DESWorld))
  (doc 'description "Set or add entity. Replaces if id exists, appends if new.")
  (let* ([es (des-world-entities w)]
         [new-es (cons (cons id data)
                       (filter (lambda (p) (not (eq? (car p) id))) es))])
    (set-lens world-entities new-es w)))

(define (world-update-entity w id f)
  (doc 'type '(-> DESWorld Symbol (-> Any Any) DESWorld))
  (doc 'description "Apply f to entity data. No-op if entity not found.")
  (let ([cur (world-entity w id)])
    (if cur (world-set-entity w id (f cur)) w)))

(define (world-remove-entity w id)
  (doc 'type '(-> DESWorld Symbol DESWorld))
  (doc 'description "Remove entity by id.")
  (over world-entities
        (lambda (es) (filter (lambda (p) (not (eq? (car p) id))) es))
        w))

(define (world-entity-ids w)
  (doc 'type '(-> DESWorld (List Symbol)))
  (map car (des-world-entities w)))

(define (world-entity-count w)
  (doc 'type '(-> DESWorld Nat))
  (length (des-world-entities w)))

;;;---------------------------------------------------------------------------
;;; Event scheduling (convenience wrappers)
;;;---------------------------------------------------------------------------

(doc 'section 'scheduling)

(define (world-schedule w event)
  (doc 'type '(-> DESWorld DESEvent DESWorld))
  (doc 'description "Schedule an event into the world's event queue.")
  (over world-queue (lambda (q) (eq-schedule q event)) w))

(define (world-schedule* w events)
  (doc 'type '(-> DESWorld (List DESEvent) DESWorld))
  (doc 'description "Schedule multiple events.")
  (over world-queue (lambda (q) (eq-schedule* q events)) w))

(define (world-schedule-at w time type payload)
  (doc 'type '(-> DESWorld Number Symbol Any DESWorld))
  (doc 'description "Convenience: schedule an event by time, type, and payload.")
  (world-schedule w (make-des-event time type payload)))

(define (world-schedule-after w delay type payload)
  (doc 'type '(-> DESWorld Number Symbol Any DESWorld))
  (doc 'description "Schedule an event relative to current clock time.")
  (world-schedule-at w (+ (des-world-clock w) delay) type payload))

(define (world-cancel-events w type)
  (doc 'type '(-> DESWorld Symbol DESWorld))
  (doc 'description "Cancel all pending events of a given type.")
  (over world-queue (lambda (q) (eq-cancel-type type q)) w))

;;;---------------------------------------------------------------------------
;;; Metrics (running accumulators)
;;;---------------------------------------------------------------------------

(doc 'section 'metrics)

(define (world-metric w name)
  (doc 'type '(-> DESWorld Symbol Any))
  (doc 'description "Get metric value. Returns 0 if not found.")
  (let ([pair (assq name (des-world-metrics w))])
    (if pair (cdr pair) 0)))

(define (world-set-metric w name value)
  (doc 'type '(-> DESWorld Symbol Any DESWorld))
  (over world-metrics
        (lambda (ms)
          (cons (cons name value)
                (filter (lambda (p) (not (eq? (car p) name))) ms)))
        w))

(define (world-inc-metric w name)
  (doc 'type '(-> DESWorld Symbol DESWorld))
  (doc 'description "Increment a numeric metric by 1.")
  (world-set-metric w name (+ 1 (world-metric w name))))

(define (world-add-metric w name delta)
  (doc 'type '(-> DESWorld Symbol Number DESWorld))
  (doc 'description "Add delta to a numeric metric.")
  (world-set-metric w name (+ delta (world-metric w name))))

;;;---------------------------------------------------------------------------
;;; RNG integration
;;;---------------------------------------------------------------------------

(doc 'section 'rng)

(define (world-run-random w computation)
  (doc 'type '(-> DESWorld (State RNG a) (Values a DESWorld)))
  (doc 'description "Run a State RNG computation against the world's RNG. Returns (values result new-world) with updated RNG state.")
  (let* ([result (run-state computation (des-world-rng w))]
         [val (car result)]
         [new-rng (cdr result)])
    (values val (set-lens world-rng new-rng w))))
