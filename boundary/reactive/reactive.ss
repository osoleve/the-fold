(load "boundary/provenance/traced-optics.ss")

(doc 'module 'reactive/reactive)
(doc 'description "Reactive Derivations from Optic Dependencies - Track which optics were used to compute a value for automatic reactivity. When an optic's target changes (via traced-set), derived values that depend on that optic are marked stale and recomputed on next access.")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(boundary/provenance/traced-optics))

(doc 'section 'access-tracking)

(doc *tracking-accesses?* 'type 'Boolean)
(doc *tracking-accesses?* 'description "Are we currently tracking optic accesses?")
(define *tracking-accesses?* #f)

(doc *access-log* 'type '(List Symbol))
(doc *access-log* 'description "List of optic names accessed during current tracking session.")
(define *access-log* '())

(doc 'section 'unregistered-optic-handling)

(doc *warn-unregistered-optics?* 'type 'Boolean)
(doc *warn-unregistered-optics?* 'description "When #t, emit a warning when an unregistered optic is used in reactive operations. Helps catch missing registrations during development.")
(define *warn-unregistered-optics?* #t)

(doc *strict-optic-registration?* 'type 'Boolean)
(doc *strict-optic-registration?* 'description "When #t, raise an error if an unregistered optic is used in reactive operations. Use this for strict enforcement in production code.")
(define *strict-optic-registration?* #f)

(doc *warned-optics-limit* 'type 'Nat)
(doc *warned-optics-limit* 'description "Maximum number of optics to track before auto-clearing the cache. This prevents unbounded memory growth with ephemeral anonymous optics.")
(define *warned-optics-limit* 1000)

(doc *warned-optics* 'type '(Hashtable Optic Boolean))
(doc *warned-optics* 'description "Cache to avoid repeated warnings for the same optic. Auto-clears when size exceeds *warned-optics-limit* to prevent unbounded growth.")
(define *warned-optics* (make-eq-hashtable))

(define (reset-optic-warnings!)
  (doc 'type (-> Void))
  (doc 'description "Clear the warning cache (useful for testing or session reset).")
  (doc 'export #t)
  (set! *warned-optics* (make-eq-hashtable)))

(define (handle-unregistered-optic! optic operation)
  (doc 'type (-> Optic Symbol Void))
  (doc 'description "Handle an unregistered optic according to current configuration. If strict mode: raises an error. If warn mode: emits a warning (once per optic). Otherwise: silent (backward compatible).")
  (doc 'param 'optic "The unregistered optic")
  (doc 'param 'operation "The reactive operation being performed (for error messages)")
  (doc 'export #t)
  (cond
    [*strict-optic-registration?*
     (error operation
            "unregistered optic - use register-optic! to enable dependency tracking"
            optic)]
    [*warn-unregistered-optics?*
     (unless (hashtable-ref *warned-optics* optic #f)
       (when (>= (hashtable-size *warned-optics*) *warned-optics-limit*)
         (set! *warned-optics* (make-eq-hashtable)))
       (hashtable-set! *warned-optics* optic #t)
       (display "Warning: ")
       (display operation)
       (display " used with unregistered optic - reactivity skipped\n")
       (display "  Hint: Use (register-optic! 'name optic) for dependency tracking\n"))]))

(define (with-access-tracking thunk)
  (doc 'type (-> (-> Any) (Values Any (List Symbol) (List Symbol))))
  (doc 'description "Execute thunk while tracking optic and derivation accesses. Returns the result, the list of optic names accessed, and the list of derivation names accessed.")
  (doc 'export #t)
  (let ([old-tracking *tracking-accesses?*]
        [old-log *access-log*]
        [old-deriv-log *derivation-access-log*])
    (dynamic-wind
      (lambda ()
        (set! *tracking-accesses?* #t)
        (set! *access-log* '())
        (set! *derivation-access-log* '()))
      (lambda ()
        (let ([result (thunk)])
          (values result (reverse *access-log*) (reverse *derivation-access-log*))))
      (lambda ()
        (set! *tracking-accesses?* old-tracking)
        (set! *access-log* old-log)
        (set! *derivation-access-log* old-deriv-log)))))

(define (log-optic-access! optic-name)
  (doc 'type (-> Symbol Void))
  (doc 'description "Record that an optic was accessed (for dependency tracking).")
  (doc 'export #t)
  (when (and *tracking-accesses?* optic-name)
    (unless (memq optic-name *access-log*)
      (set! *access-log* (cons optic-name *access-log*)))))

(doc 'section 'derivation-store)

(doc derivation-tag 'description "Tag for derivation record vectors")
(define derivation-tag 'reactive/derivation)

(doc make-derivation-record 'description "Derivation record: vector #(tag source optic-deps derivation-deps compute-fn cached-value stale?)")
(define (make-derivation-record source optic-deps derivation-deps compute-fn cached-value stale?)
  (vector derivation-tag source optic-deps derivation-deps compute-fn cached-value stale?))

(define (derivation-record? x)
  (and (vector? x)
       (>= (vector-length x) 7)
       (eq? (vector-ref x 0) derivation-tag)))

(define (derivation-source d) (vector-ref d 1))
(define (derivation-optic-dependencies d) (vector-ref d 2))
(define (derivation-derivation-dependencies d) (vector-ref d 3))
(define (derivation-compute-fn d) (vector-ref d 4))
(define (derivation-cached-value d) (vector-ref d 5))
(define (derivation-stale? d) (vector-ref d 6))

;; Backward compatibility alias
(define (derivation-dependencies d) (derivation-optic-dependencies d))

(define (derivation-set-source! d v) (vector-set! d 1 v))
(define (derivation-set-optic-dependencies! d v) (vector-set! d 2 v))
(define (derivation-set-derivation-dependencies! d v) (vector-set! d 3 v))
(define (derivation-set-cached-value! d v) (vector-set! d 5 v))
(define (derivation-set-stale! d v) (vector-set! d 6 v))

;; Backward compatibility alias
(define (derivation-set-dependencies! d v) (derivation-set-optic-dependencies! d v))

(doc *derivations* 'type '(Hashtable Symbol Derivation))
(define *derivations* (make-hashtable symbol-hash eq?))

(doc *optic-to-derivations* 'type '(Hashtable Symbol (List Symbol)))
(doc *optic-to-derivations* 'description "Maps optic names to the derivation names that depend on them.")
(define *optic-to-derivations* (make-hashtable symbol-hash eq?))

(doc 'section 'derivation-to-derivation-deps)

(doc *derivation-access-log* 'type '(List Symbol))
(doc *derivation-access-log* 'description "List of derivation names accessed during current tracking session.")
(define *derivation-access-log* '())

(doc *currently-computing* 'type '(List Symbol))
(doc *currently-computing* 'description "Stack of derivation names currently being computed. Used for cycle detection.")
(define *currently-computing* '())

(doc *derivation-to-derivations* 'type '(Hashtable Symbol (List Symbol)))
(doc *derivation-to-derivations* 'description "Maps derivation names to other derivation names that depend on them. Enables cascade invalidation when a derivation's value changes.")
(define *derivation-to-derivations* (make-hashtable symbol-hash eq?))

(define (log-derivation-access! derivation-name)
  (doc 'type (-> Symbol Void))
  (doc 'description "Record that a derivation was accessed (for inter-derivation dependency tracking).")
  (doc 'export #t)
  (when (and *tracking-accesses?* derivation-name)
    (unless (memq derivation-name *derivation-access-log*)
      (set! *derivation-access-log* (cons derivation-name *derivation-access-log*)))))

(doc 'section 'derivation-management)

(define (define-reactive name source compute-fn)
  (doc 'type (-> Symbol Any (-> Any Any) Void))
  (doc 'description "Define a reactive derivation. The compute-fn should use traced optic operations and reactive-value calls to auto-discover dependencies on both optics and other derivations.")
  (doc 'export #t)
  (call-with-values
    (lambda ()
      (with-access-tracking
       (lambda () (compute-fn source))))
    (lambda (value optic-deps deriv-deps)
      ;; Filter out self-references in derivation deps
      (let ([filtered-deriv-deps (filter (lambda (d) (not (eq? d name))) deriv-deps)])
        (let ([record (make-derivation-record source optic-deps filtered-deriv-deps compute-fn value #f)])
          (hashtable-set! *derivations* name record)
          ;; Register optic dependencies
          (for-each
           (lambda (optic-name)
             (let ([existing (hashtable-ref *optic-to-derivations* optic-name '())])
               (unless (memq name existing)
                 (hashtable-set! *optic-to-derivations* optic-name (cons name existing)))))
           optic-deps)
          ;; Register derivation dependencies
          (for-each
           (lambda (deriv-name)
             (let ([existing (hashtable-ref *derivation-to-derivations* deriv-name '())])
               (unless (memq name existing)
                 (hashtable-set! *derivation-to-derivations* deriv-name (cons name existing)))))
           filtered-deriv-deps))))))

(define (reactive-value name)
  (doc 'type (-> Symbol Any))
  (doc 'description "Get the current value of a derivation. Recomputes if stale. When called during access tracking, logs the dependency so the caller derivation will be invalidated when this derivation changes.")
  (doc 'export #t)
  ;; Log that we're accessing this derivation (for inter-derivation dependency tracking)
  (log-derivation-access! name)
  (let ([record (hashtable-ref *derivations* name #f)])
    (unless record
      (error 'reactive-value "unknown derivation" name))
    (if (derivation-stale? record)
        (reactive-recompute! name record)
        (derivation-cached-value record))))

(define (reactive-recompute! name record)
  (doc 'type (-> Symbol Derivation Any))
  (doc 'description "Recompute a derivation and update its optic and derivation dependencies. If computation fails, dependency graph is restored to consistent state. Detects circular dependencies and raises an error.")
  ;; Cycle detection: if we're already computing this derivation, it's a cycle
  (when (memq name *currently-computing*)
    (error 'reactive-recompute!
           (string-append "Circular dependency detected: "
                          (symbol->string name)
                          " depends on itself through: "
                          (let loop ([stack *currently-computing*] [acc ""])
                            (if (null? stack) acc
                                (loop (cdr stack)
                                      (string-append acc " -> " (symbol->string (car stack)))))))
           name))
  (let ([old-optic-deps (derivation-optic-dependencies record)]
        [old-deriv-deps (derivation-derivation-dependencies record)])
    ;; Remove old optic dependencies
    (for-each
     (lambda (optic-name)
       (let* ([existing (hashtable-ref *optic-to-derivations* optic-name '())]
              [remaining (filter (lambda (n) (not (eq? n name))) existing)])
         (if (null? remaining)
             (hashtable-delete! *optic-to-derivations* optic-name)
             (hashtable-set! *optic-to-derivations* optic-name remaining))))
     old-optic-deps)
    ;; Remove old derivation dependencies
    (for-each
     (lambda (deriv-name)
       (let* ([existing (hashtable-ref *derivation-to-derivations* deriv-name '())]
              [remaining (filter (lambda (n) (not (eq? n name))) existing)])
         (if (null? remaining)
             (hashtable-delete! *derivation-to-derivations* deriv-name)
             (hashtable-set! *derivation-to-derivations* deriv-name remaining))))
     old-deriv-deps)
    (guard (ex [else
                ;; Restore old optic dependencies on error
                (for-each
                 (lambda (optic-name)
                   (let ([existing (hashtable-ref *optic-to-derivations* optic-name '())])
                     (unless (memq name existing)
                       (hashtable-set! *optic-to-derivations* optic-name (cons name existing)))))
                 old-optic-deps)
                ;; Restore old derivation dependencies on error
                (for-each
                 (lambda (deriv-name)
                   (let ([existing (hashtable-ref *derivation-to-derivations* deriv-name '())])
                     (unless (memq name existing)
                       (hashtable-set! *derivation-to-derivations* deriv-name (cons name existing)))))
                 old-deriv-deps)
                (raise ex)])
      ;; Track that we're computing this derivation (for cycle detection)
      (dynamic-wind
        (lambda () (set! *currently-computing* (cons name *currently-computing*)))
        (lambda ()
          (call-with-values
            (lambda ()
              (with-access-tracking
               (lambda () ((derivation-compute-fn record) (derivation-source record)))))
        (lambda (value new-optic-deps new-deriv-deps)
          ;; Filter out self-references
          (let ([filtered-deriv-deps (filter (lambda (d) (not (eq? d name))) new-deriv-deps)])
            (derivation-set-optic-dependencies! record new-optic-deps)
            (derivation-set-derivation-dependencies! record filtered-deriv-deps)
            (derivation-set-cached-value! record value)
            (derivation-set-stale! record #f)
            ;; Register new optic dependencies
            (for-each
             (lambda (optic-name)
               (let ([existing (hashtable-ref *optic-to-derivations* optic-name '())])
                 (unless (memq name existing)
                   (hashtable-set! *optic-to-derivations* optic-name (cons name existing)))))
             new-optic-deps)
            ;; Register new derivation dependencies
            (for-each
             (lambda (deriv-name)
               (let ([existing (hashtable-ref *derivation-to-derivations* deriv-name '())])
                 (unless (memq name existing)
                   (hashtable-set! *derivation-to-derivations* deriv-name (cons name existing)))))
             filtered-deriv-deps)
            value))))
        ;; Pop from currently-computing stack
        (lambda () (set! *currently-computing* (cdr *currently-computing*)))))))

(define (reactive-refresh! name)
  (doc 'type (-> Symbol Any))
  (doc 'description "Force recomputation of a derivation.")
  (doc 'export #t)
  (let ([record (hashtable-ref *derivations* name #f)])
    (unless record
      (error 'reactive-refresh! "unknown derivation" name))
    (reactive-recompute! name record)))

(define (reactive-stale? name)
  (doc 'type (-> Symbol Boolean))
  (doc 'description "Check if a derivation needs recomputation.")
  (doc 'export #t)
  (let ([record (hashtable-ref *derivations* name #f)])
    (and record (derivation-stale? record))))

(define (reactive-dependencies name)
  (doc 'type (-> Symbol (List Symbol)))
  (doc 'description "Get the optic dependencies of a derivation.")
  (doc 'export #t)
  (let ([record (hashtable-ref *derivations* name #f)])
    (if record
        (derivation-optic-dependencies record)
        '())))

(define (reactive-derivation-dependencies name)
  (doc 'type (-> Symbol (List Symbol)))
  (doc 'description "Get the derivation dependencies of a derivation (other derivations it depends on).")
  (doc 'export #t)
  (let ([record (hashtable-ref *derivations* name #f)])
    (if record
        (derivation-derivation-dependencies record)
        '())))

(define (undefine-reactive name)
  (doc 'type (-> Symbol Void))
  (doc 'description "Remove a derivation and clean up all its dependencies.")
  (doc 'export #t)
  (let ([record (hashtable-ref *derivations* name #f)])
    (when record
      ;; Remove from optic dependency graph
      (for-each
       (lambda (optic-name)
         (let* ([existing (hashtable-ref *optic-to-derivations* optic-name '())]
                [remaining (filter (lambda (n) (not (eq? n name))) existing)])
           (if (null? remaining)
               (hashtable-delete! *optic-to-derivations* optic-name)
               (hashtable-set! *optic-to-derivations* optic-name remaining))))
       (derivation-optic-dependencies record))
      ;; Remove from derivation dependency graph
      (for-each
       (lambda (deriv-name)
         (let* ([existing (hashtable-ref *derivation-to-derivations* deriv-name '())]
                [remaining (filter (lambda (n) (not (eq? n name))) existing)])
           (if (null? remaining)
               (hashtable-delete! *derivation-to-derivations* deriv-name)
               (hashtable-set! *derivation-to-derivations* deriv-name remaining))))
       (derivation-derivation-dependencies record))
      ;; Also remove any derivations that depend on this one from the reverse index
      (hashtable-delete! *derivation-to-derivations* name)
      (hashtable-delete! *derivations* name))))

(doc 'section 'invalidation)

(define (do-invalidate-derivation! derivation-name)
  (doc 'type (-> Symbol Void))
  (doc 'description "Mark a derivation as stale and cascade to all derivations that depend on it.")
  (let ([record (hashtable-ref *derivations* derivation-name #f)])
    (when (and record (not (derivation-stale? record)))
      (derivation-set-stale! record #t)
      ;; Cascade to derivations that depend on this one
      (let ([dependents (hashtable-ref *derivation-to-derivations* derivation-name '())])
        (for-each do-invalidate-derivation! dependents)))))

(define (do-invalidate-optic! optic-name)
  (doc 'type (-> Symbol Void))
  (doc 'description "Immediately mark all derivations depending on this optic as stale, cascading through the derivation dependency graph.")
  (let ([affected (hashtable-ref *optic-to-derivations* optic-name '())])
    (for-each do-invalidate-derivation! affected)))

(define (invalidate-optic! optic-name)
  (doc 'type (-> Symbol Void))
  (doc 'description "Mark all derivations depending on this optic as stale. If batching, defers invalidation until batch completes.")
  (doc 'export #t)
  (if *batching?*
      (unless (memq optic-name *batch-invalidations*)
        (set! *batch-invalidations* (cons optic-name *batch-invalidations*)))
      (do-invalidate-optic! optic-name)))

(define (invalidate-optics! optic-names)
  (doc 'type (-> (List Symbol) Void))
  (doc 'description "Mark all derivations depending on any of these optics as stale.")
  (doc 'export #t)
  (for-each invalidate-optic! optic-names))

(doc 'section 'reactive-optic-operations)

(define (reactive-view s optic)
  (doc 'type (-> Any Optic Any))
  (doc 'description "View through an optic with access tracking and provenance. Warns or errors on unregistered optics (configurable).")
  (doc 'export #t)
  (let ([name (lookup-optic-name optic)])
    (if name
        (log-optic-access! name)
        (when *tracking-accesses?*
          (handle-unregistered-optic! optic 'reactive-view))))
  (traced-view s optic))

(define (reactive-preview s optic)
  (doc 'type (-> Any Optic (Maybe Any)))
  (doc 'description "Preview through an optic with access tracking and provenance. Warns or errors on unregistered optics (configurable).")
  (doc 'export #t)
  (let ([name (lookup-optic-name optic)])
    (if name
        (log-optic-access! name)
        (when *tracking-accesses?*
          (handle-unregistered-optic! optic 'reactive-preview))))
  (traced-preview s optic))

(define (reactive-to-list s optic)
  (doc 'type (-> Any Optic (List Any)))
  (doc 'description "To-list through an optic with access tracking and provenance. Warns or errors on unregistered optics (configurable).")
  (doc 'export #t)
  (let ([name (lookup-optic-name optic)])
    (if name
        (log-optic-access! name)
        (when *tracking-accesses?*
          (handle-unregistered-optic! optic 'reactive-to-list))))
  (traced-to-list s optic))

(define (reactive-set! optic val s)
  (doc 'type (-> Optic Any Any Any))
  (doc 'description "Set through an optic with invalidation and provenance. Warns or errors on unregistered optics (configurable). Returns the new structure.")
  (doc 'export #t)
  (let ([name (lookup-optic-name optic)])
    (if name
        (invalidate-optic! name)
        (handle-unregistered-optic! optic 'reactive-set!)))
  (traced-set optic val s))

(define (reactive-over! optic f s)
  (doc 'type (-> Optic (-> Any Any) Any Any))
  (doc 'description "Modify through an optic with invalidation and provenance. Warns or errors on unregistered optics (configurable). Returns the new structure.")
  (doc 'export #t)
  (let ([name (lookup-optic-name optic)])
    (if name
        (invalidate-optic! name)
        (handle-unregistered-optic! optic 'reactive-over!)))
  (traced-over optic f s))

(doc 'section 'source-updates)

(define (reactive-update-source! name new-source)
  (doc 'type (-> Symbol Any Void))
  (doc 'description "Update the source structure for a derivation. Does NOT automatically invalidate - use with reactive-set! for that.")
  (doc 'export #t)
  (let ([record (hashtable-ref *derivations* name #f)])
    (when record
      (derivation-set-source! record new-source)
      (derivation-set-stale! record #t))))

(doc 'section 'batch-operations)

(doc *batching?* 'description "Internal flag for batch mode")
(define *batching?* #f)

(doc *batch-invalidations* 'description "Accumulated invalidations during batch")
(define *batch-invalidations* '())

(define (with-batch thunk)
  (doc 'type (-> (-> Any) Any))
  (doc 'description "Execute thunk, deferring all invalidation until the end. Useful for making multiple changes without intermediate recomputation.")
  (doc 'export #t)
  (if *batching?*
      (thunk)
      (let ([old-invalidations *batch-invalidations*])
        (dynamic-wind
          (lambda ()
            (set! *batching?* #t)
            (set! *batch-invalidations* '()))
          (lambda ()
            (let ([result (thunk)])
              (for-each do-invalidate-optic! (delete-duplicates *batch-invalidations*))
              result))
          (lambda ()
            (set! *batching?* #f)
            (set! *batch-invalidations* old-invalidations))))))

(define (delete-duplicates lst)
  (doc 'type (-> (List Any) (List Any)))
  (doc 'description "Remove duplicate elements from list")
  (let loop ([lst lst] [seen '()])
    (cond
      [(null? lst) (reverse seen)]
      [(memq (car lst) seen) (loop (cdr lst) seen)]
      [else (loop (cdr lst) (cons (car lst) seen))])))

(doc 'section 'introspection)

(define (list-derivations)
  (doc 'type (-> (List Symbol)))
  (doc 'description "List all defined derivations.")
  (doc 'export #t)
  (vector->list (hashtable-keys *derivations*)))

(define (derivation-info name)
  (doc 'type (-> Symbol (U Alist Boolean)))
  (doc 'description "Get information about a derivation.")
  (doc 'export #t)
  (let ([record (hashtable-ref *derivations* name #f)])
    (and record
         `((name . ,name)
           (optic-dependencies . ,(derivation-optic-dependencies record))
           (derivation-dependencies . ,(derivation-derivation-dependencies record))
           (stale? . ,(derivation-stale? record))
           (cached-value . ,(derivation-cached-value record))))))

(define (dependency-graph)
  (doc 'type (-> Alist))
  (doc 'description "Get the full optic -> derivation dependency graph.")
  (doc 'export #t)
  (let ([keys (vector->list (hashtable-keys *optic-to-derivations*))])
    (map (lambda (k)
           (cons k (hashtable-ref *optic-to-derivations* k '())))
         keys)))

(define (derivation-dependency-graph)
  (doc 'type (-> Alist))
  (doc 'description "Get the full derivation -> derivation dependency graph (shows which derivations depend on which other derivations).")
  (doc 'export #t)
  (let ([keys (vector->list (hashtable-keys *derivation-to-derivations*))])
    (map (lambda (k)
           (cons k (hashtable-ref *derivation-to-derivations* k '())))
         keys)))

(doc 'section 'filter-helper)

(define (filter pred lst)
  (cond
    [(null? lst) '()]
    [(pred (car lst)) (cons (car lst) (filter pred (cdr lst)))]
    [else (filter pred (cdr lst))]))
