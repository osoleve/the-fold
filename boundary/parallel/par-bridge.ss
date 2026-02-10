;;; boundary/parallel/par-bridge.ss — Connect par to work-stealing pool
;;;
;;; Installs a backend into *par-backend* (core/lang/eval.ss) that routes
;;; par's background branch through the work-stealing thread pool instead
;;; of raw fork-thread.  Main benefit: pool reuse, work-stealing, and
;;; work-helping on await.
;;;
;;; No timeout needed: eval-core is total (fuel-bounded).  Fuel IS the
;;; timeout mechanism.

(load "boundary/parallel/scheduler.ss")

(doc 'module 'par-bridge)
(doc 'description "Bridge between core par and boundary work-stealing pool.")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'requires '(scheduler eval))

(define (pool-par-backend a-thunk b-thunk)
  (doc 'type '(-> (-> Result) (-> Result) Result))
  (doc 'description "Par backend using work-stealing pool. Submits a to pool, runs b on caller thread, inspects a's result.")
  (let ([a-future (spawn a-thunk)])
    (let ([b-result (b-thunk)])
      ;; await does work-helping (steals from pool deques) while waiting
      (guard (exn [else b-result])  ; pool-level errors → return b
        (let ([a-result (await a-future)])
          ;; a-result is a core Result: (ok val fuel ctx) | (error ...) | (suspended ...)
          (cond
            [(and (pair? a-result) (eq? (car a-result) 'error))
             a-result]
            [(and (pair? a-result) (eq? (car a-result) 'suspended))
             `(error par-suspended
                     "Background expression suspended (out of fuel)"
                     ,(cadr a-result))]
            [else b-result]))))))

(define (install-par-bridge!)
  (doc 'type '(-> Void))
  (doc 'description "Install pool-backed par backend. Replaces raw fork-thread par.")
  (doc 'export #t)
  (set-box! *par-backend* pool-par-backend))

(define (uninstall-par-bridge!)
  (doc 'type '(-> Void))
  (doc 'description "Remove pool-backed par backend. Restores default fork-thread par.")
  (doc 'export #t)
  (set-box! *par-backend* #f))
