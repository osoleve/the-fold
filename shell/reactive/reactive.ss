;;; shell/reactive/reactive.ss — Reactive Derivations from Optic Dependencies
;;;
;;; Track which optics were used to compute a value for automatic reactivity.
;;; When an optic's target changes (via traced-set), derived values that
;;; depend on that optic are marked stale and recomputed on next access.
;;;
;;; This is the pattern behind lens-based state management (MobX, Recoil, Jotai).
;;; The optic graph becomes a dependency graph.
;;;
;;; Usage:
;;;   ;; Define a reactive derivation
;;;   (define-reactive 'player-health
;;;     world-state
;;;     (lambda (world)
;;;       (traced-view world (>>> (body-lens 'player) health-lens))))
;;;
;;;   ;; Get value (computed lazily, cached until stale)
;;;   (reactive-value 'player-health)  ; => 100
;;;
;;;   ;; Modify through optic - derivation auto-invalidates
;;;   (reactive-set! (>>> (body-lens 'player) health-lens) 80 world-state)
;;;
;;;   ;; Next access recomputes
;;;   (reactive-value 'player-health)  ; => 80 (recomputed)
;;;
;;; This is Shell code: uses mutable state for derivation tracking.
;;;
;;; Dependencies:
;;;   - shell/provenance/traced-optics.ss (for optic registry and tracing)

(load "shell/provenance/traced-optics.ss")

;;; ============================================================
;;; Access Tracking
;;; ============================================================
;;;
;;; During computation of a derivation, we track which optics are accessed.
;;; This builds the dependency graph automatically.

;;; *tracking-accesses?* : Boolean
;;; Are we currently tracking optic accesses?
(define *tracking-accesses?* #f)

;;; *access-log* : (List Symbol)
;;; List of optic names accessed during current tracking session.
(define *access-log* '())

;;; ============================================================
;;; Unregistered Optic Handling (fold-zxpe fix)
;;; ============================================================
;;;
;;; When using anonymous/ad-hoc optics (not registered via register-optic!),
;;; dependency tracking silently fails. These settings control the behavior.

;;; *warn-unregistered-optics?* : Boolean
;;; When #t, emit a warning when an unregistered optic is used in reactive
;;; operations. Helps catch missing registrations during development.
;;; Default: #t
(define *warn-unregistered-optics?* #t)

;;; *strict-optic-registration?* : Boolean
;;; When #t, raise an error if an unregistered optic is used in reactive
;;; operations. Use this for strict enforcement in production code.
;;; Default: #f
(define *strict-optic-registration?* #f)

;;; *warned-optics* : Hashtable (Optic -> Boolean)
;;; Cache to avoid repeated warnings for the same optic.
;;; NOTE: Uses eq-hashtable keyed on optic closures. For long-running processes
;;; with many ephemeral anonymous optics, consider periodic reset-optic-warnings!
;;; calls to prevent unbounded growth. A weak-eq-hashtable would be ideal but
;;; the eq? identity of closures makes GC behavior unpredictable.
(define *warned-optics* (make-eq-hashtable))

;;; reset-optic-warnings! : -> Void
;;; Clear the warning cache (useful for testing or session reset).
(define (reset-optic-warnings!)
  (set! *warned-optics* (make-eq-hashtable)))

;;; handle-unregistered-optic! : Optic Symbol -> Void
;;; Handle an unregistered optic according to current configuration.
;;; - If strict mode: raises an error
;;; - If warn mode: emits a warning (once per optic)
;;; - Otherwise: silent (backward compatible)
;;;
;;; Arguments:
;;;   optic     - The unregistered optic
;;;   operation - The reactive operation being performed (for error messages)
(define (handle-unregistered-optic! optic operation)
  (cond
    [*strict-optic-registration?*
     (error operation
            "unregistered optic - use register-optic! to enable dependency tracking"
            optic)]
    [*warn-unregistered-optics?*
     (unless (hashtable-ref *warned-optics* optic #f)
       (hashtable-set! *warned-optics* optic #t)
       (display "Warning: ")
       (display operation)
       (display " used with unregistered optic - reactivity skipped\n")
       (display "  Hint: Use (register-optic! 'name optic) for dependency tracking\n"))]))

;;; with-access-tracking : (-> a) -> (Values a (List Symbol))
;;; Execute thunk while tracking optic accesses.
;;; Returns the result and the list of optic names accessed.
(define (with-access-tracking thunk)
  (let ([old-tracking *tracking-accesses?*]
        [old-log *access-log*])
    (dynamic-wind
      (lambda ()
        (set! *tracking-accesses?* #t)
        (set! *access-log* '()))
      (lambda ()
        (let ([result (thunk)])
          (values result (reverse *access-log*))))
      (lambda ()
        (set! *tracking-accesses?* old-tracking)
        (set! *access-log* old-log)))))

;;; log-optic-access! : Symbol -> Void
;;; Record that an optic was accessed (for dependency tracking).
(define (log-optic-access! optic-name)
  (when (and *tracking-accesses?* optic-name)
    (unless (memq optic-name *access-log*)
      (set! *access-log* (cons optic-name *access-log*)))))

;;; ============================================================
;;; Derivation Store
;;; ============================================================

;;; derivation record: vector #(tag source dependencies compute-fn cached-value stale?)
;;; - source: the root data structure being observed
;;; - dependencies: (List Symbol) optic names this derivation depends on
;;; - compute-fn: (source -> value) function to compute the value
;;; - cached-value: last computed value
;;; - stale?: Boolean, whether cached value needs recomputation

(define derivation-tag 'reactive/derivation)

(define (make-derivation-record source dependencies compute-fn cached-value stale?)
  (vector derivation-tag source dependencies compute-fn cached-value stale?))

(define (derivation-record? x)
  (and (vector? x)
       (> (vector-length x) 0)
       (eq? (vector-ref x 0) derivation-tag)))

(define (derivation-source d) (vector-ref d 1))
(define (derivation-dependencies d) (vector-ref d 2))
(define (derivation-compute-fn d) (vector-ref d 3))
(define (derivation-cached-value d) (vector-ref d 4))
(define (derivation-stale? d) (vector-ref d 5))

(define (derivation-set-source! d v) (vector-set! d 1 v))
(define (derivation-set-dependencies! d v) (vector-set! d 2 v))
(define (derivation-set-cached-value! d v) (vector-set! d 4 v))
(define (derivation-set-stale! d v) (vector-set! d 5 v))

;;; *derivations* : Hashtable (Symbol -> Derivation)
(define *derivations* (make-hashtable symbol-hash eq?))

;;; *optic-to-derivations* : Hashtable (Symbol -> (List Symbol))
;;; Maps optic names to the derivation names that depend on them.
(define *optic-to-derivations* (make-hashtable symbol-hash eq?))

;;; ============================================================
;;; Derivation Management
;;; ============================================================

;;; define-reactive : Symbol Any (Any -> Any) -> Void
;;; Define a reactive derivation.
;;; The compute-fn should use traced optic operations to auto-discover dependencies.
(define (define-reactive name source compute-fn)
  ;; Compute initial value while tracking accesses
  (call-with-values
    (lambda ()
      (with-access-tracking
       (lambda () (compute-fn source))))
    (lambda (value deps)
      ;; Create derivation record
      (let ([record (make-derivation-record source deps compute-fn value #f)])
        (hashtable-set! *derivations* name record)
        ;; Register in reverse index
        (for-each
         (lambda (optic-name)
           (let ([existing (hashtable-ref *optic-to-derivations* optic-name '())])
             (unless (memq name existing)
               (hashtable-set! *optic-to-derivations* optic-name (cons name existing)))))
         deps)))))

;;; reactive-value : Symbol -> Any
;;; Get the current value of a derivation.
;;; Recomputes if stale.
(define (reactive-value name)
  (let ([record (hashtable-ref *derivations* name #f)])
    (unless record
      (error 'reactive-value "unknown derivation" name))
    (if (derivation-stale? record)
        (reactive-recompute! name record)
        (derivation-cached-value record))))

;;; reactive-recompute! : Symbol Derivation -> Any
;;; Recompute a derivation and update its dependencies.
;;; If computation fails, dependency graph is restored to consistent state.
(define (reactive-recompute! name record)
  (let ([old-deps (derivation-dependencies record)])
    ;; Remove from old dependency mappings (and clean up empty entries)
    (for-each
     (lambda (optic-name)
       (let* ([existing (hashtable-ref *optic-to-derivations* optic-name '())]
              [remaining (filter (lambda (n) (not (eq? n name))) existing)])
         (if (null? remaining)
             (hashtable-delete! *optic-to-derivations* optic-name)
             (hashtable-set! *optic-to-derivations* optic-name remaining))))
     old-deps)
    ;; Compute with tracking, restoring deps on error
    (guard (ex [else
                ;; Restore old dependency mappings on error
                (for-each
                 (lambda (optic-name)
                   (let ([existing (hashtable-ref *optic-to-derivations* optic-name '())])
                     (unless (memq name existing)
                       (hashtable-set! *optic-to-derivations* optic-name (cons name existing)))))
                 old-deps)
                (raise ex)])
      (call-with-values
        (lambda ()
          (with-access-tracking
           (lambda () ((derivation-compute-fn record) (derivation-source record)))))
        (lambda (value new-deps)
          ;; Update record
          (derivation-set-dependencies! record new-deps)
          (derivation-set-cached-value! record value)
          (derivation-set-stale! record #f)
          ;; Register new dependencies
          (for-each
           (lambda (optic-name)
             (let ([existing (hashtable-ref *optic-to-derivations* optic-name '())])
               (unless (memq name existing)
                 (hashtable-set! *optic-to-derivations* optic-name (cons name existing)))))
           new-deps)
          value)))))

;;; reactive-refresh! : Symbol -> Any
;;; Force recomputation of a derivation.
(define (reactive-refresh! name)
  (let ([record (hashtable-ref *derivations* name #f)])
    (unless record
      (error 'reactive-refresh! "unknown derivation" name))
    (reactive-recompute! name record)))

;;; reactive-stale? : Symbol -> Boolean
;;; Check if a derivation needs recomputation.
(define (reactive-stale? name)
  (let ([record (hashtable-ref *derivations* name #f)])
    (and record (derivation-stale? record))))

;;; reactive-dependencies : Symbol -> (List Symbol)
;;; Get the optic dependencies of a derivation.
(define (reactive-dependencies name)
  (let ([record (hashtable-ref *derivations* name #f)])
    (if record
        (derivation-dependencies record)
        '())))

;;; undefine-reactive : Symbol -> Void
;;; Remove a derivation.
(define (undefine-reactive name)
  (let ([record (hashtable-ref *derivations* name #f)])
    (when record
      ;; Remove from dependency mappings (and clean up empty entries)
      (for-each
       (lambda (optic-name)
         (let* ([existing (hashtable-ref *optic-to-derivations* optic-name '())]
                [remaining (filter (lambda (n) (not (eq? n name))) existing)])
           (if (null? remaining)
               (hashtable-delete! *optic-to-derivations* optic-name)
               (hashtable-set! *optic-to-derivations* optic-name remaining))))
       (derivation-dependencies record))
      (hashtable-delete! *derivations* name))))

;;; ============================================================
;;; Invalidation
;;; ============================================================

;;; do-invalidate-optic! : Symbol -> Void
;;; Immediately mark all derivations depending on this optic as stale.
;;; Internal use - prefer invalidate-optic! which respects batching.
(define (do-invalidate-optic! optic-name)
  (let ([affected (hashtable-ref *optic-to-derivations* optic-name '())])
    (for-each
     (lambda (derivation-name)
       (let ([record (hashtable-ref *derivations* derivation-name #f)])
         (when record
           (derivation-set-stale! record #t))))
     affected)))

;;; invalidate-optic! : Symbol -> Void
;;; Mark all derivations depending on this optic as stale.
;;; If batching, defers invalidation until batch completes.
(define (invalidate-optic! optic-name)
  (if *batching?*
      (unless (memq optic-name *batch-invalidations*)
        (set! *batch-invalidations* (cons optic-name *batch-invalidations*)))
      (do-invalidate-optic! optic-name)))

;;; invalidate-optics! : (List Symbol) -> Void
;;; Mark all derivations depending on any of these optics as stale.
(define (invalidate-optics! optic-names)
  (for-each invalidate-optic! optic-names))

;;; ============================================================
;;; Reactive Optic Operations
;;; ============================================================
;;;
;;; These wrap the traced operations to:
;;; 1. Log accesses when tracking
;;; 2. Invalidate derivations on writes

;;; reactive-view : Any Optic -> Any
;;; View through an optic with access tracking and provenance.
;;; Warns or errors on unregistered optics (configurable).
(define (reactive-view s optic)
  (let ([name (lookup-optic-name optic)])
    (if name
        (log-optic-access! name)
        (when *tracking-accesses?*
          (handle-unregistered-optic! optic 'reactive-view))))
  (traced-view s optic))

;;; reactive-preview : Any Optic -> Maybe Any
;;; Preview through an optic with access tracking and provenance.
;;; Warns or errors on unregistered optics (configurable).
(define (reactive-preview s optic)
  (let ([name (lookup-optic-name optic)])
    (if name
        (log-optic-access! name)
        (when *tracking-accesses?*
          (handle-unregistered-optic! optic 'reactive-preview))))
  (traced-preview s optic))

;;; reactive-to-list : Any Optic -> (List Any)
;;; To-list through an optic with access tracking and provenance.
;;; Warns or errors on unregistered optics (configurable).
(define (reactive-to-list s optic)
  (let ([name (lookup-optic-name optic)])
    (if name
        (log-optic-access! name)
        (when *tracking-accesses?*
          (handle-unregistered-optic! optic 'reactive-to-list))))
  (traced-to-list s optic))

;;; reactive-set! : Optic Any Any -> Any
;;; Set through an optic with invalidation and provenance.
;;; Warns or errors on unregistered optics (configurable).
;;; Returns the new structure.
(define (reactive-set! optic val s)
  (let ([name (lookup-optic-name optic)])
    (if name
        (invalidate-optic! name)
        (handle-unregistered-optic! optic 'reactive-set!)))
  (traced-set optic val s))

;;; reactive-over! : Optic (Any -> Any) Any -> Any
;;; Modify through an optic with invalidation and provenance.
;;; Warns or errors on unregistered optics (configurable).
;;; Returns the new structure.
(define (reactive-over! optic f s)
  (let ([name (lookup-optic-name optic)])
    (if name
        (invalidate-optic! name)
        (handle-unregistered-optic! optic 'reactive-over!)))
  (traced-over optic f s))

;;; ============================================================
;;; Source Updates
;;; ============================================================

;;; reactive-update-source! : Symbol Any -> Void
;;; Update the source structure for a derivation.
;;; Does NOT automatically invalidate - use with reactive-set! for that.
(define (reactive-update-source! name new-source)
  (let ([record (hashtable-ref *derivations* name #f)])
    (when record
      (derivation-set-source! record new-source)
      (derivation-set-stale! record #t))))

;;; ============================================================
;;; Batch Operations
;;; ============================================================

;;; with-batch : (-> a) -> a
;;; Execute thunk, deferring all invalidation until the end.
;;; Useful for making multiple changes without intermediate recomputation.
(define *batching?* #f)
(define *batch-invalidations* '())

(define (with-batch thunk)
  (if *batching?*
      (thunk)  ; Already batching, just run
      (let ([old-invalidations *batch-invalidations*])
        (dynamic-wind
          (lambda ()
            (set! *batching?* #t)
            (set! *batch-invalidations* '()))
          (lambda ()
            (let ([result (thunk)])
              ;; Apply all collected invalidations using direct invalidation
              ;; (bypasses batching check since we're still in batch mode)
              (for-each do-invalidate-optic! (delete-duplicates *batch-invalidations*))
              result))
          (lambda ()
            (set! *batching?* #f)
            (set! *batch-invalidations* old-invalidations))))))

;;; delete-duplicates : (List a) -> (List a)
(define (delete-duplicates lst)
  (let loop ([lst lst] [seen '()])
    (cond
      [(null? lst) (reverse seen)]
      [(memq (car lst) seen) (loop (cdr lst) seen)]
      [else (loop (cdr lst) (cons (car lst) seen))])))

;;; ============================================================
;;; Introspection
;;; ============================================================

;;; list-derivations : -> (List Symbol)
;;; List all defined derivations.
(define (list-derivations)
  (vector->list (hashtable-keys *derivations*)))

;;; derivation-info : Symbol -> Alist | #f
;;; Get information about a derivation.
(define (derivation-info name)
  (let ([record (hashtable-ref *derivations* name #f)])
    (and record
         `((name . ,name)
           (dependencies . ,(derivation-dependencies record))
           (stale? . ,(derivation-stale? record))
           (cached-value . ,(derivation-cached-value record))))))

;;; dependency-graph : -> Alist
;;; Get the full optic -> derivation dependency graph.
(define (dependency-graph)
  (let ([keys (vector->list (hashtable-keys *optic-to-derivations*))])
    (map (lambda (k)
           (cons k (hashtable-ref *optic-to-derivations* k '())))
         keys)))

;;; ============================================================
;;; Filter helper (if not already available)
;;; ============================================================

(define (filter pred lst)
  (cond
    [(null? lst) '()]
    [(pred (car lst)) (cons (car lst) (filter pred (cdr lst)))]
    [else (filter pred (cdr lst))]))

;;; ============================================================
;;; Exports
;;; ============================================================
;;;
;;; Derivation Management:
;;;   define-reactive, reactive-value, reactive-refresh!
;;;   reactive-stale?, reactive-dependencies, undefine-reactive
;;;
;;; Access Tracking:
;;;   with-access-tracking, log-optic-access!
;;;   *tracking-accesses?*, *access-log*
;;;
;;; Unregistered Optic Configuration (fold-zxpe):
;;;   *warn-unregistered-optics?*   - Emit warnings (default #t)
;;;   *strict-optic-registration?*  - Raise errors (default #f)
;;;   reset-optic-warnings!         - Clear warning cache
;;;   handle-unregistered-optic!    - Called when optic not registered
;;;
;;; Invalidation:
;;;   invalidate-optic!, invalidate-optics!
;;;
;;; Reactive Operations:
;;;   reactive-view, reactive-preview, reactive-to-list
;;;   reactive-set!, reactive-over!
;;;
;;; Source Updates:
;;;   reactive-update-source!
;;;
;;; Batch Operations:
;;;   with-batch
;;;
;;; Introspection:
;;;   list-derivations, derivation-info, dependency-graph
