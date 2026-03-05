(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
;;; @module des/world
;;; @requires prelude des/event fp/control/state avl-tree templates
(require 'prelude)
(require 'des/event)
(require 'fp/control/state)
(require 'avl-tree)
(require 'templates)

(doc 'module 'des/world)
(doc 'description "Discrete Event Simulation — world state container with optics. A DES world holds the simulation clock, entity and metric stores (AVL trees for O(log n) access), the event queue, and RNG state. Lenses provide composable access into all fields.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;;---------------------------------------------------------------------------
;;; Symbol comparator for AVL trees
;;;---------------------------------------------------------------------------

(define (symbol<? a b)
  (doc 'export #t)
  (doc 'type '(-> Symbol Symbol Boolean))
  (string<? (symbol->string a) (symbol->string b)))

;;;---------------------------------------------------------------------------
;;; AVL-based symbol map helpers (internal)
;;;---------------------------------------------------------------------------

(define (sym-avl-lookup key tree)
  (doc 'export #t)
  (avl-lookup-by symbol<? key tree))

(define (sym-avl-insert key value tree)
  (doc 'export #t)
  (avl-insert-by symbol<? key value tree))

(define (sym-avl-delete key tree)
  (doc 'export #t)
  (avl-delete-by symbol<? key tree))

(define (alist->sym-avl alist)
  (doc 'export #t)
  (fold-left (lambda (tree pair) (sym-avl-insert (car pair) (cdr pair) tree))
             avl-empty alist))

;;;---------------------------------------------------------------------------
;;; World state
;;;---------------------------------------------------------------------------

(doc 'section 'world-state)

(define (make-des-world clock entities event-queue metrics rng)
  (doc 'export #t)
  (doc 'type '(-> Number AVL EventQueue AVL Any DESWorld))
  (doc 'description "Create a simulation world. Entities and metrics are AVL trees (symbol-keyed). Use des-world for alist convenience constructor.")
  (list 'des-world clock entities event-queue metrics rng))

(define (des-world clock entity-alist event-queue metric-alist rng)
  (doc 'export #t)
  (doc 'type '(-> Number Alist EventQueue Alist Any DESWorld))
  (doc 'description "Convenience constructor: accepts alists for entities and metrics, converts to AVL trees internally.")
  (make-des-world clock (alist->sym-avl entity-alist) event-queue (alist->sym-avl metric-alist) rng))

(define (des-world? x)
  (doc 'export #t)
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

(doc 'type '(Lens DESWorld AVL))
(define world-entities
  (make-lens des-world-entities
             (lambda (es w) (make-des-world (des-world-clock w) es (des-world-queue w) (des-world-metrics w) (des-world-rng w)))))

(doc 'type '(Lens DESWorld EventQueue))
(define world-queue
  (make-lens des-world-queue
             (lambda (q w) (make-des-world (des-world-clock w) (des-world-entities w) q (des-world-metrics w) (des-world-rng w)))))

(doc 'type '(Lens DESWorld AVL))
(define world-metrics
  (make-lens des-world-metrics
             (lambda (m w) (make-des-world (des-world-clock w) (des-world-entities w) (des-world-queue w) m (des-world-rng w)))))

(doc 'type '(Lens DESWorld Any))
(define world-rng
  (make-lens des-world-rng
             (lambda (r w) (make-des-world (des-world-clock w) (des-world-entities w) (des-world-queue w) (des-world-metrics w) r))))

;;;---------------------------------------------------------------------------
;;; Entity operations (AVL tree, O(log n))
;;;---------------------------------------------------------------------------

(doc 'section 'entity-ops)

(define (world-entity w id)
  (doc 'export #t)
  (doc 'type '(-> DESWorld Symbol Any))
  (doc 'description "Look up entity by id. O(log n). Returns #f if not found.")
  (sym-avl-lookup id (des-world-entities w)))

(define (world-set-entity w id data)
  (doc 'export #t)
  (doc 'type '(-> DESWorld Symbol Any DESWorld))
  (doc 'description "Set or add entity. O(log n).")
  (set-lens world-entities
            (sym-avl-insert id data (des-world-entities w))
            w))

(define (world-update-entity w id f)
  (doc 'export #t)
  (doc 'type '(-> DESWorld Symbol (-> Any Any) DESWorld))
  (doc 'description "Apply f to entity data. No-op if entity not found. O(log n).")
  (let ([cur (world-entity w id)])
    (if cur (world-set-entity w id (f cur)) w)))

(define (world-remove-entity w id)
  (doc 'export #t)
  (doc 'type '(-> DESWorld Symbol DESWorld))
  (doc 'description "Remove entity by id. O(log n).")
  (set-lens world-entities
            (sym-avl-delete id (des-world-entities w))
            w))

(define (world-entity-ids w)
  (doc 'export #t)
  (doc 'type '(-> DESWorld (List Symbol)))
  (avl-keys (des-world-entities w)))

(define (world-entity-count w)
  (doc 'export #t)
  (doc 'type '(-> DESWorld Nat))
  (avl-size (des-world-entities w)))

;;;---------------------------------------------------------------------------
;;; Event scheduling (convenience wrappers)
;;;---------------------------------------------------------------------------

(doc 'section 'scheduling)

(define (world-schedule w event)
  (doc 'export #t)
  (doc 'type '(-> DESWorld DESEvent DESWorld))
  (doc 'description "Schedule an event into the world's event queue.")
  (over world-queue (lambda (q) (eq-schedule q event)) w))

(define (world-schedule* w events)
  (doc 'export #t)
  (doc 'type '(-> DESWorld (List DESEvent) DESWorld))
  (doc 'description "Schedule multiple events.")
  (over world-queue (lambda (q) (eq-schedule* q events)) w))

(define (world-schedule-at w time type payload)
  (doc 'export #t)
  (doc 'type '(-> DESWorld Number Symbol Any DESWorld))
  (doc 'description "Convenience: schedule an event by time, type, and payload.")
  (world-schedule w (make-des-event time type payload)))

(define (world-schedule-after w delay type payload)
  (doc 'export #t)
  (doc 'type '(-> DESWorld Number Symbol Any DESWorld))
  (doc 'description "Schedule an event relative to current clock time.")
  (world-schedule-at w (+ (des-world-clock w) delay) type payload))

(define (world-cancel-events w type)
  (doc 'export #t)
  (doc 'type '(-> DESWorld Symbol DESWorld))
  (doc 'description "Cancel all pending events of a given type.")
  (over world-queue (lambda (q) (eq-cancel-type type q)) w))

;;;---------------------------------------------------------------------------
;;; Metrics (AVL tree, O(log n))
;;;---------------------------------------------------------------------------

(doc 'section 'metrics)

(define (world-metric w name)
  (doc 'export #t)
  (doc 'type '(-> DESWorld Symbol Any))
  (doc 'description "Get metric value. O(log n). Returns 0 if not found.")
  (let ([v (sym-avl-lookup name (des-world-metrics w))])
    (if v v 0)))

(define (world-set-metric w name value)
  (doc 'export #t)
  (doc 'type '(-> DESWorld Symbol Any DESWorld))
  (doc 'description "Set a metric value. O(log n).")
  (set-lens world-metrics
            (sym-avl-insert name value (des-world-metrics w))
            w))

(define (world-inc-metric w name)
  (doc 'export #t)
  (doc 'type '(-> DESWorld Symbol DESWorld))
  (doc 'description "Increment a numeric metric by 1.")
  (world-set-metric w name (+ 1 (world-metric w name))))

(define (world-add-metric w name delta)
  (doc 'export #t)
  (doc 'type '(-> DESWorld Symbol Number DESWorld))
  (doc 'description "Add delta to a numeric metric.")
  (world-set-metric w name (+ delta (world-metric w name))))

;;;---------------------------------------------------------------------------
;;; RNG integration
;;;---------------------------------------------------------------------------

(doc 'section 'rng)

(define (world-run-random w computation)
  (doc 'export #t)
  (doc 'type '(-> DESWorld (State RNG a) (Values a DESWorld)))
  (doc 'description "Run a State RNG computation against the world's RNG. Returns (values result new-world) with updated RNG state.")
  (let* ([result (run-state computation (des-world-rng w))]
         [val (car result)]
         [new-rng (cdr result)])
    (values val (set-lens world-rng new-rng w))))
